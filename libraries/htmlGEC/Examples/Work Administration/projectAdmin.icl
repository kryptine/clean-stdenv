module projectAdmin

import StdEnv, StdHtml

derive gForm  	Worker, Project, DailyWork, ProjectPlan, Status, WorkerPlan
derive gUpd 	Worker, Project, DailyWork, ProjectPlan, Status, WorkerPlan, Date, []
derive gPrint	Worker, Project, DailyWork, ProjectPlan, Status, WorkerPlan, Date
derive gParse	Worker, Project, DailyWork, ProjectPlan, Status, WorkerPlan, Date

//	List elements need to be displayed below each other, left aligned:
gForm {|[]|} gHa formid [] hst
	= ({changed = False, value = [], form =[EmptyBody]},hst)
gForm {|[]|} gHa formid [x:xs] hst 
	# (formx, hst) = gHa formid x hst
	# (formxs,hst) = gForm {|*->*|} gHa formid xs hst
	= ({changed = False, value = [x:xs], form = [formx.form <||> formxs.form]},hst)
//	Date should be displayed as a triplet of pull down menus, displaying (day, month, year) selections:
gForm {|Date|} formid date=:(Date day month year) hst 
	= specialize myeditor {formid & lifespan = Page} date hst
where
	myeditor formid date hst = mkBimapEditor formid date bimap hst
	where
		bimap = {map_to = toPullDown, map_from = fromPullDown}
		where
			toPullDown (Date day month year) = (pddays,pdmonths,pdyears)
			where
				pddays 		= PullDown (1,  defpixel/2) (day-1,  [toString i \\ i <- [1..31]])
				pdmonths 	= PullDown (1,  defpixel/2) (month-1,[toString i \\ i <- [1..12]])
				pdyears 	= PullDown (1,2*defpixel/3) (year-1, [toString i \\ i <- [2005..2010]])
		
			fromPullDown (pddays,pdmonths,pdyears) = Date (convert pddays) (convert pdmonths) (convert pdyears)
			where
				convert x	= toInt (toString x)

Start world  = doHtmlServer MyPage world
//Start world  = doHtml MyPage world

:: Project		= 	{ 	plan		:: ProjectPlan
					, 	status		:: Status
					,	members		:: [Worker]
					}	
:: ProjectPlan	= 	{ 	name		:: String
					, 	hours		:: Int
					}
:: Status		=	{ 	total		:: Int
					, 	left		:: Int
					}
:: Worker		=	{ 	name		:: String
					, 	status		:: Status
					, 	work		:: [Work]
					}
:: Work			:== (Date,Int)
:: Date 		= 	Date Int Int Int
:: WorkerPlan	= 	{ 	project		:: ProjectList
					,	name		:: String
					, 	hours		:: Int
					}
:: DailyWork	=	{ 	projectId 	:: ProjectList
					, 	myName		:: WorkersList
					, 	date		:: Date
					, 	hoursWorked	:: Int
					}
:: ProjectList	:== PullDownMenu
:: WorkersList	:== PullDownMenu

//	Form creation/update functions:
workAdminStore :: ([Project] -> [Project]) *HSt -> (Form [Project], *HSt)
workAdminStore update hst = mkStoreForm (pdFormId "workadmin") initProjects update hst

addProjectForm :: *HSt -> (Form ProjectPlan, *HSt)
addProjectForm hst = mkEditForm (nFormId "addproject") (initProjectPlan "" 0) hst

addWorkerForm :: (WorkerPlan -> WorkerPlan) *HSt -> (Form WorkerPlan, *HSt)
addWorkerForm update hst = mkSelf2Form  (nFormId "addworker") (initWorkerPlan "" 0 0 initProjects) update hst

dailyWorkForm :: (DailyWork -> DailyWork) *HSt -> (Form DailyWork, *HSt)
dailyWorkForm update hst = mkSelf2Form  (pFormId "daylog") (initDailyWork 0 0 initProjects) update hst

myButtonsForm :: DailyWork WorkerPlan ProjectPlan *HSt -> (Form ([Project] -> [Project]), *HSt)
myButtonsForm daylog workplan project hst = ListFuncBut False (nFormId "mybuttons") myButtons hst
where
	myButtons = [ (LButton defpixel "addProject", addNewProject  project )
				, (LButton defpixel "addWorker",  addNewWorkplan workplan)
				, (LButton defpixel "addHours",   addDailyWork   daylog  )
				]
	
	addNewProject :: ProjectPlan -> [Project] -> [Project]
	addNewProject {ProjectPlan|name,hours} = flip (:^) (initProject name hours)
	
	addNewWorkplan :: WorkerPlan -> [Project] -> [Project]
	addNewWorkplan worker=:{project,name,hours}
		= updateElt (\{plan={ProjectPlan|name,hours}} -> name == toString project)
		            (\p -> {p & members = [initWorker name hours:p.members]})
	
	addDailyWork :: DailyWork [Project] -> [Project]
	addDailyWork daylog projects
		| daylog.hoursWorked == 0 || toString daylog.myName == "" || isEmpty projects
						= projects
		| otherwise 	= updateAt (toInt daylog.projectId) updatedProject projects
	where
		{status,plan=plan=:{ProjectPlan|hours},members}
					 	= projects!!(toInt daylog.projectId)
		totalHoursSpent = status.total + daylog.hoursWorked
		remainingHours	= hours - totalHoursSpent
		updatedProject 	= { status  = initStatus remainingHours totalHoursSpent
						  , members = addDay daylog members
						  , plan    = plan
						  }
		nworklog		= { name    = daylog.myName
						  , work    = [(daylog.date,daylog.hoursWorked)]
						  , status  = initStatus 0 0
						  }
	
		addDay :: DailyWork -> [Worker] -> [Worker]
		addDay nwork=:{myName,date,hoursWorked}
			= updateElt (\owork=:{Worker|name} -> name == toString myName)
			            (\owork=:{Worker|status,work}
			                  -> {owork & work   = work ++ [(date,hoursWorked)]
			                            , status = {status & total=status.total+hoursWorked
			                                               , left =status.left -hoursWorked
			                     }                 })

MyPage hst
	# (projects,hst) = workAdminStore id hst
	# (project, hst) = addProjectForm    hst
	# (worker,  hst) = addWorkerForm  id hst
	# (daylog,  hst) = dailyWorkForm  id hst
	# (update,  hst) = myButtonsForm  daylog.value worker.value project.value hst
	# (projects,hst) = workAdminStore update.value hst
	# (daylog,  hst) = dailyWorkForm (adjDailyWork projects.value) hst
	# (worker,  hst) = addWorkerForm (adjWorkers   projects.value) hst
	# no_projects    = isEmpty projects.value
	= mkHtml "table test"
		[ H1 [] "Project Administration"
		, Br
		, STable []
			[ [ STable [] 
					[ [ lTxt "Add New Project:"]
					, [ toBody project ]
					, [ update.form!!0 ]
					, [ lTxt "Add New Worker:"]
					, [ toBody worker  ]
					, [ update.form!!1 ]
					: if no_projects [] 
					[ [lTxt "Administrate Worked Hours:"]
					, [ toBody daylog  ]
					, [ update.form!!2 ]
					]
					]
			  : if no_projects []
			  [ STable []
					[ [ lTxt "Current Status of Project: \t\t" ]
					, [ toHtml (projects.value!!(toInt daylog.value.projectId)) ]
					]
			  ]]
			]
		] hst
where
	lTxt s = B [] s
	
	adjDailyWork :: [Project] DailyWork  -> DailyWork
	adjDailyWork projects daylog=:{projectId}
		= {	daylog & projectId = addProjectList projectId projects
		           , myName    = initWorkersList (toInt daylog.myName) (toInt projectId) projects
		  }
	
	adjWorkers :: [Project] WorkerPlan -> WorkerPlan
	adjWorkers projects worker = {worker & project = addProjectList worker.project projects}
	
	addProjectList :: PullDownMenu [Project] -> PullDownMenu
	addProjectList (PullDown pxls (i,_)) projects = PullDown pxls (i,[name \\ {plan={ProjectPlan|name}} <- projects])

//	Initial values of the work administration's data structures:
initProjects :: [Project]
initProjects = []

initProject :: String Int -> Project
initProject name hours
	= 	{ plan			= initProjectPlan name hours
		, status		= initStatus hours 0
		, members		= [] }
	
initProjectPlan :: String Int -> ProjectPlan
initProjectPlan name hours
	= 	{ProjectPlan
		| name			= name
		, hours			= hours }

initStatus :: Int Int -> Status
initStatus todo done
	=	{ total			= done
		, left			= todo }

initWorkerPlan :: String Int Int [Project] -> WorkerPlan
initWorkerPlan name hours i projects
	= 	{ project		= initProjectList i projects
		, name			= name
		, hours			= hours }

initWorker :: String Int -> Worker
initWorker name hours
	=	{ name			= name
		, status		= initStatus hours 0
		, work			= [] }

initDailyWork :: Int Int [Project] -> DailyWork
initDailyWork i j projects
	=	{ myName 		= initWorkersList i j projects
		, projectId 	= initProjectList i projects
		, date			= initDate
		, hoursWorked	= 0 }

initDate :: Date	
initDate = Date 1 1 2005

initWorkersList :: Int Int [Project] -> PullDownMenu
initWorkersList i j []			= PullDown (1,defpixel) (0,[])
initWorkersList i j projects	= PullDown (1,defpixel) (i,[name \\ {Worker|name} <- (projects!!j).members])

initProjectList :: Int [Project] -> PullDownMenu
initProjectList i projects		= PullDown (1,defpixel) (i,[name \\ {plan={ProjectPlan|name}} <- projects])

//	Useful list operations:
updateElt :: (a -> Bool) (a -> a) [a] -> [a]
updateElt c f [] = []
updateElt c f [a:as]
	| c a		= [f a:as]
	| otherwise	= [a:updateElt c f as]

updateElts :: (a -> Bool) (a -> a) [a] -> [a]
updateElts c f [] = []
updateElts c f [a:as]
	| c a		= [f a:updateElts c f as]
	| otherwise	= [  a:updateElts c f as]

(^:) infixr 5 :: a [a] -> [a]
(^:) a as = [a:as]

(:^) infixl 5 :: [a] a -> [a]
(:^) as a = as ++ [a]

//	Monadic digression:
::	StM st a :== st -> .(!a,!st)

(>>=) infixr 5 :: !u:(StM .st .a) !v:(.a -> .(StM .st .b)) -> w:(StM .st .b), [w<=u,w<=v]
(>>=) fA to_mB = mbind` fA to_mB
where
	mbind` fA to_mB st
		# (a,st)	= fA st
		= to_mB a st

(>>-) infixr 5 :: !u:(StM .st .a) !v:(StM .st .b) -> w:(StM .st .b), [w <= u,w <= v]
(>>-) fA fB = mbind_` fA fB
where
	mbind_` fA fB st
		# (_,st)	= fA st
		= fB st

mreturn :: !u:a -> v:(StM .st u:a), [v<=u]
mreturn x = mreturn` x
where
	mreturn` x st = (x,st)
