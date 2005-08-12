module projectAdmin

import StdEnv, StdHtml

derive gForm  	ProjectWorker,Project,DailyWork, ProjectPlan, Status, WorkerPlan
derive gUpd 	ProjectWorker,Project,DailyWork, ProjectPlan, Status, WorkerPlan, Date, []
derive gPrint	ProjectWorker,Project,DailyWork, ProjectPlan, Status, WorkerPlan, Date
derive gParse	ProjectWorker,Project,DailyWork, ProjectPlan, Status, WorkerPlan, Date

//	List elements need to be displayed below each other, left aligned:
gForm {|[]|} gHa formid [] hst
	= ({changed = False, value = [], form =[EmptyBody]},hst)
gForm {|[]|} gHa formid [x:xs] hst 
	# (formx,hst)  = gHa formid x hst
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
workAdminStore update hst = mkStoreForm (pdFormId "workadmin") myProjects update hst

addProjectForm :: *HSt -> (Form ProjectPlan, *HSt)
addProjectForm hst = mkEditForm (nFormId "addproject") (initProjectPlan "" 0) hst

addWorkerForm :: (WorkerPlan -> WorkerPlan) *HSt -> (Form WorkerPlan, *HSt)
addWorkerForm update hst = mkSelf2Form  (nFormId "addworker") (initWorkerPlan "" 0 0 myProjects) update hst

dailyWorkForm :: (DailyWork -> DailyWork) *HSt -> (Form DailyWork, *HSt)
dailyWorkForm update hst = mkSelf2Form  (pFormId "daylog") (initDailyWork 0 0 myProjects) update hst

myButtonsForm :: DailyWork WorkerPlan ProjectPlan *HSt -> (Form (ProjectAdmin -> ProjectAdmin), *HSt)
myButtonsForm daylog workplan project hst = ListFuncBut False (nFormId "mybuttons") myButtons hst
where
	myButtons = [ (LButton defpixel "addProject", addNewProject  project )
				, (LButton defpixel "addWorker",  addNewWorkplan workplan)
				, (LButton defpixel "addHours",   addDailyWork   daylog  )
				]

MyPage hst
	# (projects,hst) = workAdminStore id hst
	# (project, hst) = addProjectForm    hst
	# (worker,  hst) = addWorkerForm  id hst
	# (daylog,  hst) = dailyWorkForm  id hst
	# (update,  hst) = myButtonsForm daylog.value worker.value project.value hst
	# (projects,hst) = workAdminStore update.value hst
	# (daylog,  hst) = dailyWorkForm (adjDailyWork projects.value) hst
	# (worker,  hst) = addWorkerForm (adjWorkers   projects.value) hst
	# no_projects    = isEmpty projects.value
	= mkHtml "table test"
		[ H1 [] "Project Administration"
		, Br
		, STable [] 
		  [ [ lTxt "Add New Project:",lTxt "Add New Worker:": if no_projects [] [lTxt "Administrate Worked Hours:"]]
		  , [ toBody project,         toBody worker         : if no_projects [] [toBody daylog                    ]]
		  , [ update.form!!0,         update.form!!1        : if no_projects [] [update.form!!2                   ]]
		  ]
		, Br, Br
		: if no_projects []
		[ Br, Br, BodyTag [ lTxt "Current Status of Project: \t\t"
		                               <.||.>
		                    toHtml (projects.value!!(toInt daylog.value.projectId))
		]                 ]
		] hst
where
	lTxt s = B [] s

myProjects :: [Project]
myProjects = []

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

initProjectList :: Int ProjectAdmin -> PullDownMenu
initProjectList i projects = PullDown (1,defpixel) (i,[projectPlan.projectName \\ {projectPlan} <- projects])

initWorkersList :: Int Int ProjectAdmin -> PullDownMenu
initWorkersList i j [] = PullDown (1,defpixel) (0,[])
initWorkersList i j projects = PullDown (1,defpixel) (i,[nameWorker \\ {nameWorker} <- (projects!!j).workers])

adjDailyWork :: ProjectAdmin DailyWork  -> DailyWork
adjDailyWork projects daylog=:{projectId} = {	daylog 
											& 	projectId = addProjectList projectId projects
											,	myName = initWorkersList (toInt daylog.myName) (toInt projectId) projects
											}

addProjectList :: PullDownMenu ProjectAdmin -> PullDownMenu
addProjectList (PullDown pxls (i,_)) projects = (PullDown pxls (i,[projectPlan.projectName \\ {projectPlan} <- projects]))

addNewProject :: ProjectPlan ProjectAdmin -> ProjectAdmin
addNewProject {projectName,hoursPlanned} projects = projects ++ [initProject projectName hoursPlanned]

addDailyWork :: DailyWork [Project] -> [Project]
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
		| toString myName == nameWorker
			= [{owork & investedHours = investedHours ++ [(date,hoursWorked)]
			          , statusWorker.totalHoursWorked = statusWorker.totalHoursWorked + hoursWorked
			          , statusWorker.remainingHours = statusWorker.remainingHours - hoursWorked
			   }
			  :worklogs
			  ]
		| otherwise
			= [owork: addDay nwork worklogs]

addNewWorkplan :: WorkerPlan [Project] -> [Project]
addNewWorkplan {project,workerName,hoursToWork} [] = []
addNewWorkplan worker=:{project,workerName,hoursToWork} [p:ps]
	| p.projectPlan.projectName == toString project 
				= [{p & workers = [initProjectWorker workerName hoursToWork:p.workers]}:ps]
	| otherwise = [p: addNewWorkplan worker ps]

adjWorkers :: [Project] WorkerPlan -> WorkerPlan
adjWorkers projects worker = {worker & project = addProjectList worker.project projects}


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
