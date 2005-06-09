module main

import StdEnv, StdHtml

derive gForm  	Login, ProjectWorker,Project,DailyWork, ProjectPlan, Status, WorkerPlan
derive gUpd 	[], Login, ProjectWorker,Project,DailyWork, ProjectPlan, Date, Status, WorkerPlan
derive gPrint	Login, ProjectWorker,Project,DailyWork,ProjectPlan, Date, Status, WorkerPlan
derive gParse	Login, ProjectWorker,Project,DailyWork,ProjectPlan, Date, Status, WorkerPlan

gForm {|[]|} gHa formid [] hst = ({changed = False, value = [], form =[EmptyBody]},hst)
gForm {|[]|} gHa formid [x:xs] hst 
# (formx,hst)  = gHa formid x hst
# (formxs,hst) = gForm {|*->*|} gHa formid xs hst
= ({changed = False, value = [x:xs], form = [formx.form <||> formxs.form]},hst)

gForm {|Date|} formid date=:(Date day month year) hst 
	= specialize myeditor {formid & lifespan = Page} date hst
where
	myeditor formid date hst = mkBimapEditor formid date bimap hst
	where
		bimap = {map_to = toPullDown, map_from = fromPullDown}
		where
			toPullDown (Date day month year) = (pddays,pdmonths,pdyears)
			where
				pddays 		= PullDown (1,defpixel/2) (day-1,[toString i \\ i <- [1..31]])
				pdmonths 	= PullDown (1,defpixel/2) (month-1,[toString i \\ i <- [1..12]])
				pdyears 	= PullDown (1,60) (year-1,[toString i \\ i <- [2005..2010]])
		
			fromPullDown (pddays,pdmonths,pdyears) = Date (convert pddays) (convert pdmonths) (convert pdyears)
			where
				convert x = toInt (toString x)

Start world  = doHtml page world

:: ProjectAdmin	:== [Project]
:: Project
	= 	{ 	projectPlan		:: ProjectPlan
		, 	projectStatus	:: Status
		,	workers			:: [ProjectWorker]
		}	
:: ProjectPlan
	= 	{ 	projectName		:: String
		, 	hoursPlanned	:: Int
		}
:: Status
	=	{ 	totalHoursWorked:: Int
		, 	remainingHours	:: Int
		}
:: ProjectWorker
	=	{ 	nameWorker		:: String
		, 	statusWorker	:: Status
		, 	investedHours	:: [WorkingTable]
		}
:: WorkingTable	:== (Date,Int)
:: Date 		
	= 	Date Int Int Int

:: WorkerPlan
	= 	{ 	project			:: ProjectList
		,	workerName		:: String
		, 	hoursToWork		:: Int
		}
:: DailyWork
	=	{ 	projectId 		:: ProjectList
		, 	myName			:: WorkersList
		, 	date			:: Date
		, 	hoursWorked		:: Int
		}
:: ProjectList	:== PullDownMenu
:: WorkersList	:== PullDownMenu

workAdminStore :: (ProjectAdmin -> ProjectAdmin) *HSt -> (Form ProjectAdmin, *HSt)
workAdminStore update hst = mkStoreForm (pdFormId "workadmin") initProjectAdmin update hst

addProjectForm :: *HSt -> (Form ProjectPlan, *HSt)
addProjectForm hst = mkEditForm  (nFormId "addproject") (initProjectPlan "" 0) hst

addWorkerForm :: (WorkerPlan -> WorkerPlan) *HSt -> (Form WorkerPlan, *HSt)
addWorkerForm update hst = mkSelf2Form  (nFormId "addworker") (initWorkerPlan "" 0 0 initProjectAdmin) update hst

dailyWorkForm :: (DailyWork -> DailyWork) *HSt -> (Form DailyWork, *HSt)
dailyWorkForm update hst = mkSelf2Form  (pFormId "daylog") (initDailyWork 0 0 initProjectAdmin) update hst

buttonsForm :: DailyWork WorkerPlan ProjectPlan *HSt -> (Form (ProjectAdmin -> ProjectAdmin), *HSt)
buttonsForm daylog workplan project hst = ListFuncBut False (nFormId "mybuttons") myButtons hst
where
	myButtons = [ (LButton defpixel "addProject", addNewProject  project)
				, (LButton defpixel "addWorker",  addNewWorkplan workplan)
				, (LButton defpixel "addHours",   addDailyWork   daylog)
				]

page :: *HSt -> (Html,*HSt)
page hst = let (forms,hst1) = updateForms hst in (updatePage forms,hst1)
where
	updateForms :: *HSt -> ((Form ProjectAdmin,Form ProjectPlan,Form WorkerPlan,Form DailyWork,Form (ProjectAdmin -> ProjectAdmin)),*HSt)
	updateForms hst
	# (projects,hst) = workAdminStore id hst
	# (project, hst) = addProjectForm    hst
	# (worker,  hst) = addWorkerForm  id hst
	# (daylog,  hst) = dailyWorkForm  id hst
	# (update,  hst) = buttonsForm  daylog.value worker.value project.value hst
	# (projects,hst) = workAdminStore update.value hst
	# (daylog,  hst) = dailyWorkForm (adjDailyWork projects.value) hst
	# (worker,  hst) = addWorkerForm (adjWorkers   projects.value) hst
	= ((projects,project,worker,daylog,update),hst)
	
	updatePage (projects,project,worker,daylog,update)
	= simpleHtml "Work Table"
		[ H1 [] "Work Table"
		, Hr []
		, STable [] 
			[ [ lTxt "Add New Project:", toBody project, lTxt "Add New Worker:", toBody worker]
			, [ EmptyBody, update.form!!0, EmptyBody, update.form!!1]
			]
		, Hr []
		, if (isEmpty projects.value) 
		     EmptyBody
		     (BodyTag [	STable []
						[ [ lTxt "Administrate Worked Hours:", toBody daylog]
						, [ EmptyBody, update.form!!2]
						]
					  , Hr []
					  , lTxt "Project Status:" <.=.> toHtml (projects.value!!(toInt daylog.value.projectId))
					  ])
		]
	where
		lTxt s	= Txt s <.||.> (Hr [Hr_Size (Pixels 4)])

initProjectAdmin :: ProjectAdmin
initProjectAdmin = []

initProject :: String Int -> Project
initProject name hours
	= 	{ projectPlan		= initProjectPlan name hours
		, projectStatus		= initStatus hours 0
		, workers			= []
		}	
initProjectPlan :: String Int -> ProjectPlan
initProjectPlan name hours
	= 	{ projectName	= name
		, hoursPlanned	= hours
		}
initStatus :: Int Int -> Status
initStatus todo done
	=	{ totalHoursWorked	= done
		, remainingHours	= todo
		}
initWorkerPlan :: String Int Int ProjectAdmin -> WorkerPlan
initWorkerPlan name hours i projects
	= 	{ 	project			= initProjectList i projects
		,	workerName		= name
		, 	hoursToWork		= hours
		}
initProjectWorker :: String Int -> ProjectWorker
initProjectWorker name hours
	=	{ 	nameWorker		= name
		, 	statusWorker	= initStatus hours 0
		, 	investedHours	= []
		}
initDailyWork :: Int Int ProjectAdmin -> DailyWork
initDailyWork i j projects
	=	{ myName 				= initWorkersList i j projects
		, projectId 			= initProjectList i projects
		, date					= initDate
		, hoursWorked			= 0
		}
where
	initDate :: Date	
	initDate = Date 1 1 2005

initProjectList :: Int ProjectAdmin -> ProjectList
initProjectList i projects = PullDown (1,defpixel) (i,[projectPlan.projectName \\ {projectPlan} <- projects])

initWorkersList :: Int Int ProjectAdmin -> WorkersList
initWorkersList i j [] = PullDown (1,defpixel) (0,[])
initWorkersList i j projects = PullDown (1,defpixel) (i,[nameWorker \\ {nameWorker} <- (projects!!j).workers])

adjDailyWork :: ProjectAdmin DailyWork  -> DailyWork
adjDailyWork projects daylog=:{projectId} = {	daylog 
											& 	projectId = addProjectList projects projectId
											,	myName = initWorkersList (toInt daylog.myName) (toInt projectId) projects
											}

addProjectList :: ProjectAdmin ProjectList -> ProjectList
addProjectList projects (PullDown pxls (i,_)) = (PullDown pxls (i,[projectPlan.projectName \\ {projectPlan} <- projects]))

addNewProject :: ProjectPlan ProjectAdmin -> ProjectAdmin
addNewProject {projectName,hoursPlanned} projects = projects ++ [initProject projectName hoursPlanned]

addDailyWork :: DailyWork ProjectAdmin -> ProjectAdmin
addDailyWork daylog [] = []
addDailyWork daylog projects
| daylog.hoursWorked == 0 || toString daylog.myName == "" = projects
| otherwise = updateAt (toInt daylog.projectId) updatedProject projects
where
	thisProject 		= projects!!(toInt daylog.projectId)
	totalHoursSpended 	= thisProject.projectStatus.totalHoursWorked + daylog.hoursWorked
	remainingHours		= thisProject.projectPlan.hoursPlanned - totalHoursSpended
	updatedProject 		= {thisProject	
							& projectStatus = initStatus remainingHours totalHoursSpended
							, workers 		= addDay daylog thisProject.workers}
	nworklog = { nameWorker = daylog.myName
				, investedHours = [(daylog.date,daylog.hoursWorked)]
				, statusWorker = initStatus 0 0
				}

	addDay :: DailyWork [ProjectWorker] -> [ProjectWorker]
	addDay {myName,date,hoursWorked} [] = []
	addDay nwork=:{myName,date,hoursWorked} [owork=:{nameWorker,statusWorker,investedHours}:worklogs]
	| toString myName == nameWorker  = [{owork & investedHours = investedHours ++ [(date,hoursWorked)]
									  , statusWorker.totalHoursWorked = statusWorker.totalHoursWorked + hoursWorked
									  , statusWorker.remainingHours = statusWorker.remainingHours - hoursWorked
									  }:worklogs]
	| otherwise 			= [owork: addDay nwork worklogs]

addNewWorkplan :: WorkerPlan ProjectAdmin -> ProjectAdmin
addNewWorkplan {project,workerName,hoursToWork} [] = []
addNewWorkplan worker=:{project,workerName,hoursToWork} [p:ps]
| p.projectPlan.projectName == toString project 
					= [{p & workers = [initProjectWorker workerName hoursToWork:p.workers]}:ps]
| otherwise = [p: addNewWorkplan worker ps]


adjWorkers :: ProjectAdmin WorkerPlan -> WorkerPlan
adjWorkers projects worker = {worker & project = addProjectList projects worker.project}

:: Login 
	= 	{ loginName :: String
		, password	:: String
		}
		
mklogin name password
	= 	{ loginName = name
		, password	= password
		}
/*
MyPage  hst
# (login,hst)		= mkEditForm (nFormId "login") (mklogin "" "") hst
# (addlogin,hst)	= addLoginButton login.value hst
# (loginfile,hst)	= mkStoreForm (pFormId "logindatabase")	[mklogin "root" "secret"] 
						(addLogin login.value (addlogin.value False)) hst
= mkHtml "login test"
	[ H1 [] "Login test programme"
	, Br
	, if (isMember login.value loginfile.value) 
			(Txt ("Welcome " +++ login.value.name)) 
			(BodyTag
				[ Txt "Please log in ..."
				, toBody login, toBody addlogin
				])
	, Br
	] hst
where
	addLoginButton value hst = ListFuncBut False (formid "addlogin") pagebuttons hst
	where
		pagebuttons  = 
			[ (LButton defpixel "addLogin", \b -> True)
			]
		formid = if (value.loginName <> "" && value.password <> "") nFormId ndFormId
	

	
	addLogin newname False logindb = logindb
	addLogin newname True  logindb 
		= if (newname.loginName <> "" && newname.password <> "" && not (isMember newname logindb))
			[newname:logindb]
			logindb


instance == Login
where
	(==) login1 login2 = login1.loginName == login2.loginName && login1.password == login2.password
*/