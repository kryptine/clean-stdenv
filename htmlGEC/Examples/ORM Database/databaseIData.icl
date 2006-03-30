implementation module databaseIData

import StdEnv, StdMaybe
import StdHtml

import databaseDef, support

academicTable	:: FormId [Academic]
academicTable	= pFormId "academicTable" [] 

departmentTable	:: FormId [Department]
departmentTable	= pFormId "departmentTable" [] 

/*
swapTable [STable attr table] = swapTable` [STable attr table] [[],[]]
where
	swapTable` [STable attr [[Input iattr str,body]]] [fields,bodies] = [STable attr [Input iattr str:fields],[body:bodies]]
	swapTable` [STable attr [left,right]] accu = swapTable` right (swapTable` left accu)
	swapTable` [x:xs] accu 	= [x : swapTable` accu]
	swapTable` [] accu 		= accu
swapTable else = else
*/
//turnTable table = [STable attr [[field][value]] \\ (STable attr oneitem) <- table, [field,value:_] <- oneitem ]

gForm {|Academics|} (init,formid) hst 
	= specialize myeditor (init,formid) hst
where
	myeditor (init,formid) hst
	# (academicForm,hst)	= vertlistFormButs 10 True (init,academicTable) hst
	# (nacademicForm,hst)	= vertlistFormButs 10 True (setID academicTable (pred academicForm.value)) hst
	# (error,ok)			= testUnique (gCollect {|*|} nacademicForm.value [])
	# (_,hst)				= Alert (if (not ok ) (\_ -> (True,error)) id) hst
	= (	{ changed			= academicForm.changed
		, value 			= Academic nacademicForm.value 
		, form 				= nacademicForm.form
		}
		,hst )

	pred acadTable = [adj mem \\ mem <- acadTable ] // Context sensitive restrictions on Academics
	where 
		adj mem=:{	appointment = Professor {heads = Yes a}
				, 	person 		= {Person | phone = No}}		= { mem & rank = Prof 
																		, person.Person.phone = Yes 0 }
		adj mem=:{	appointment = Professor _ }					= { mem & rank = Prof }
		adj mem=:{	appointment = TeachingAndProf {professor = {heads = Yes a}}
				,	person 		= {Person | phone = No}} 		= {mem 	& rank = Prof
																		, person.Person.phone = Yes 0}
		adj mem=:{	appointment = TeachingAndProf _ }			= {mem 	& rank = Prof }
		adj mem = mem

	
gForm {|Departments|} (init,formid) hst 
	= specialize myeditor (init,formid) hst
where
	myeditor (init,formid) hst
	# (departmentForm,hst)	= vertlistFormButs 5 True (init,departmentTable) hst
	# (error,ok)			= testUnique (gCollect {|*|} departmentForm.value [])
	# (_,hst)				= Alert (if (not ok ) (\_ -> (True,error)) id) hst
	= (	{ changed			= departmentForm.changed
		, value 			= Department departmentForm.value 
		, form 				= departmentForm.form
		}
		,hst )


// references to persistent tables:

gForm {|RefDept|} (init,formid) hst = specialize myeditor (init,formid) hst
where
	myeditor (init,formid) hst
	# (depForm,hst)				= mkEditForm (init,{departmentTable & mode = Display}) hst
	| length depForm.value == 0	= ({changed=False,value = RefDept 0,form = [toHtml NoDepartment]},hst)
	# (pdform,hst)				=  pullDownStore id hst
	# (PullDown _ (idx,depnames)) = pdform.value
	# chosenDepartment			= depnames!!idx
	# newidx					= hd ([let (Uniq nr) = dep.dnr in nr \\ dep <- depForm.value | printToString dep.departmentName == chosenDepartment] ++ [0]) 
	# newmenu					= [printToString rec.departmentName	\\ rec <- depForm.value]
	# newchosen					= hd ([i \\ i <- [0..] & newelem <- newmenu | newelem == chosenDepartment]++[0])
	# pulldownmenu				= PullDown (1,defpixel) (newchosen, newmenu)
	# (pdform,hst)				= pullDownStore (\_ -> pulldownmenu) hst
	= ({changed=pdform.changed || depForm.changed,value = RefDept newidx , form = [[toHtml newidx] <=> pdform.form]},hst)

	(RefDept currPtr) = formid.ival

	pullDownStore :: (PullDownMenu -> PullDownMenu) !*HSt -> (Form PullDownMenu,!*HSt)
	pullDownStore cbf hst = mkStoreForm (init,pulldownId) cbf hst
	where
		pulldownId = subFormId formid "sub" (PullDown (1,defpixel) (0,[printToString NoDepartment]))

// to ensure a unique identifier, each time a Uniq value is created, it will get a new unique value

gForm {|Uniq|} (init,formid) hst = specialize myeditor (init,formid) hst
where
	myeditor c hst 
	= case formid.ival of
		(Uniq 0)
			# (unqform,hst)	= mkStoreForm (init,uniqueId) inc hst // side effects are very dangerous !!! THIS DOES NOT INCREMENTED WITH 1
			= ({changed=True ,value = Uniq unqform.value,form = [toHtml unqform.value]},hst)
		(Uniq n)
			= ({changed=False,value = Uniq n, form = [toHtml n]},hst)
	where
		uniqueId :: FormId Int
		uniqueId = pdFormId "uniqueId" 1 

gForm {|Others|}  (init,formid) hst = specialize myeditor (init,formid) hst
where
	myeditor (init,formid) hst
	# (listForm,hst) = vertlistFormButs 5 True (init,reuseFormId formid intlist) hst
	= (	{ changed = listForm.changed, value = Other listForm.value, form = listForm.form},hst)
	(Other intlist) = formid.ival

gForm {|Topics|} (init,formid) hst = specialize myeditor (init,formid) hst
where
	myeditor (init,formid) hst
	# (listForm,hst) = vertlistFormButs 5 True (init,reuseFormId formid intlist) hst
	= (	{ changed = listForm.changed, value = Topic listForm.value, form = listForm.form},hst)
	(Topic intlist) = formid.ival

gForm {|Degrees|} (init,formid) hst = specialize myeditor (init,formid) hst //blue gives rise to run time error
where
	myeditor (init,formid) hst
	# (listForm,hst) = vertlistFormButs 5 True (init,reuseFormId formid intlist) hst
	= (	{ changed = listForm.changed, value = Degree listForm.value, form = listForm.form},hst)
	(Degree intlist) = formid.ival
	ot (Degree list) = list	
	to list = Degree list

derive gForm 	[],Maybe,
				Academic, X_DepartmentName, Phone, Access_Level, Contract, University, X_Degree,Rank, Chair,
				Appointment, Teacher, Professor, TeachingAndProf, Person, Room, Building,
				/* Uniq, */Opt,/* Others, */ Topic, X_TopicCode, /* Degrees, Topics,*/
				Department, MyDatabase, RefAcademicus//, Academics, Departments, RefDept

derive gUpd 	[], Maybe,
				Academic, X_DepartmentName, Phone, Access_Level, Contract, University, X_Degree, Rank, Chair,
				Appointment, Teacher, Professor, TeachingAndProf, Person, Room, Building,
				Uniq, Opt, Others, Topic, X_TopicCode, Degrees, Topics,
				Department, MyDatabase, RefAcademicus, Academics, Departments,RefDept

derive gPrint 	Maybe,
				Academic, X_DepartmentName, Phone, Access_Level, Contract, University, X_Degree, Rank, Chair,
				Appointment, Teacher, Professor, TeachingAndProf, Person, Room, Building,
				Uniq, Opt, Others, Topic, X_TopicCode, Degrees, Topics,
				Department, MyDatabase, RefAcademicus, Academics, Departments,RefDept

derive gParse 	Maybe,
				Academic, X_DepartmentName, Phone, Access_Level, Contract, University, X_Degree, Rank, Chair,
				Appointment, Teacher, Professor, TeachingAndProf, Person, Room, Building,
				Uniq, Opt, Others, Topic, X_TopicCode, Degrees, Topics,
				Department, MyDatabase, RefAcademicus, Academics, Departments,RefDept
	
derive gCollect	[], Maybe, (,), HtmlDate, 
				Academic, X_DepartmentName, Phone, Access_Level, Contract, University, X_Degree, Rank, Chair,
				Appointment, Teacher, Professor, TeachingAndProf, Person, Room, Building,
				Uniq, Opt, Others, Topic, X_TopicCode, Degrees, Topics,
				Department, MyDatabase, RefAcademicus, Academics, Departments,RefDept
	
// toHtml Uniq gives a crash ??	