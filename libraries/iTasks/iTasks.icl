implementation module iTasks

// (c) iTask & iData Concept and Implementation by Rinus Plasmeijer, 2006,2007 - MJP

// iTasks library for defining interactive multi-user workflow tasks (iTask) for the web.
// Defined on top of the iData library.

import StdEnv, StdBimap, StdOrdList
import iDataSettings, iDataHandler, iDataTrivial, iDataButtons, iDataFormlib, iDataStylelib
import dynamic_string, graph_to_string_with_descriptors

derive gForm 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, VersionInfo, TaskThread, Maybe, []
derive gUpd 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, VersionInfo, TaskThread, Maybe, []
derive gParse 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, VersionInfo, TaskThread, Maybe
derive gPrint 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, VersionInfo, TaskThread, Maybe
derive gerda 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, VersionInfo, TaskThread
derive read 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, VersionInfo, TaskThread
derive write 	Void, Options, Lifespan, Mode, StorageFormat, GarbageCollect, VersionInfo, TaskThread

:: *TSt 		=	{ tasknr 		:: !TaskNr			// for generating unique form-id's
					, activated		:: !Bool   			// if true activate task, if set as result task completed	
					, userId		:: !Int				// id of user to which task is assigned
					, staticInfo	:: !StaticInfo		// info which does not change during a run
					, html			:: !HtmlTree		// accumulator for html code
					, options		:: !Options			// iData lifespan and storage format
					, trace			:: !Maybe [Trace]	// for displaying task trace
					, hst			:: !HSt				// iData state
					}
:: TaskNr		:== [Int]								// task nr i.j is adminstrated as [j,i]
:: HtmlTree		=	BT [BodyTag]						// simple code
				|	(@@:) infix  0 (Int,String) HtmlTree// code with id of user attached to it
				|	(-@:) infix  0 Int 			HtmlTree// skip code with this id if it is the id of the user 
				|	(+-+) infixl 1 HtmlTree HtmlTree	// code to be placed next to each other				
				|	(+|+) infixl 1 HtmlTree HtmlTree	// code to be placed below each other				
				|	DivCode String HtmlTree				// code that should be labeled with a div, used for Ajax and Client technology
:: Options		=	{ tasklife		:: !Lifespan		// default: Session		
					, taskstorage	:: !StorageFormat	// default: PlainString
					, taskmode		:: !Mode			// default: Edit
					, gc			:: !GarbageCollect	// default: Collect
					}
:: StaticInfo	=	{ currentUserId	:: !Int				// id of application user 
					, threadTableLoc:: !Lifespan		// where to store the server thread table, default is Session
					}

:: GarbageCollect =	Collect | NoCollect

:: Trace		=	Trace !TraceInfo ![Trace]			// traceinfo with possibly subprocess
:: TraceInfo	:== Maybe !(!Bool,!(!Int,!TaskNr,!String,!String))	// Task finished? who did it, task nr, task name (for tracing) value produced

:: ThreadTable	:== [TaskThread]						// thread table is used for Ajax and OnClient options
:: TaskThread	=	{ thrTaskNr			:: !TaskNr		// task number to recover
					, thrUserId			:: !Int			// which user has to perform the task
					, thrOptions		:: !Options		// options of the task
					, thrCallback		:: !String		// serialized callback function for the server
					, thrCallbackClient	:: !String		// serialized callback function for the client (optional, empty if not applicable)
					, thrDone			:: !Bool		// thread task is ready
					}

:: VersionInfo	=	{ versionNr			:: !Int			// latest querie number of a user
					, newThread			:: !Bool		// is a new thread assigned to this user (used for Ajax)?
					, deletedThreads	:: ![TaskNr]	// are there threads deleted (used for Ajax)?
					}

// Initial values for Task State TSt

initTst thisUser location hst
				=	{ tasknr		= [-1]
					, activated 	= True
					, staticInfo	= initStaticInfo thisUser location
					, userId		= if (thisUser >= 0) defaultUser thisUser
					, html 			= BT []
					, trace			= Nothing
					, hst 			= hst
					, options 		= initialOptions
					}

initialOptions	=	{ tasklife 		= Session
					, taskstorage 	= PlainString
					, taskmode 		= Edit 
					, gc			= Collect
					}
initStaticInfo thisUser location
=					{ currentUserId	= thisUser 
					, threadTableLoc= location
					}

// *** First some small utility functions

instance == GarbageCollect
where
	(==) Collect Collect 		= True
	(==) NoCollect NoCollect 	= True
	(==) _ _ 					= False

class 	(<<@) infixl 3 b ::  !(Task a) !b  -> (Task a)
instance <<@  Lifespan
where   (<<@) task lifespan			= \tst -> task {tst & options.tasklife = lifespan}
instance <<@  StorageFormat
where   (<<@) task storageformat 	= \tst -> task {tst & options.taskstorage = storageformat}
instance <<@  Mode
where   (<<@) task mode 			= \tst -> task {tst & options.taskmode = mode}
instance <<@  GarbageCollect
where   (<<@) task gc 				= \tst -> task {tst & options.gc = gc}

class 	(@>>) infixl 7 b ::  !b !(Task a)   -> (Task a) | iData a
instance @>>  SubPage
where   (@>>) UseAjax task			= \tst -> IF_Ajax 
												(mkTaskThread UseAjax task tst)
												(task tst) 
		(@>>) OnClient  task 		= \tst -> IF_Ajax 
												(mkTaskThread OnClient task tst)
												(task tst) 

// Lifting HSt domain to the TSt domain, for convenience

LiftHst fun tst=:{hst}
# (form,hst) = fun hst
= (form,{tst & hst = hst})

isNil [] = True
isNil _ = False

// Storages utility functions used

cFormId  		{tasklife,taskstorage,taskmode} s d = {sFormId  s d & lifespan = tasklife, storage = taskstorage, mode = taskmode} 

sessionFormId  	options s d = cFormId options s d <@ Session
pageFormId  	options s d = cFormId options s d <@ Page
storageFormId  	options s d = cFormId options s d <@ NoForm

// mkTask is an important wrapper function which should be wrapped around any task
// It takes care of
//		- deciding whether the task should be called (activated) or not
//		- adding of trace information
//		- generating task numbers in a systematic way
// It is very important that the numbering of the tasks is done systematically
// Every task should have a unique number
// Every sequential task should increase the task number
// If a task j is a subtask of task i, than it will get number i.j in reverse order
	
mkTask :: !String !(Task a) -> (Task a) | iCreateAndPrint a
mkTask taskname mytask = mkTaskNoInc taskname mytask o incTaskNr

mkTaskNoInc :: !String !(Task a) -> (Task a) | iCreateAndPrint a			// common second part of task wrappers
mkTaskNoInc taskname mytask = mkTaskNoInc`
where
	mkTaskNoInc` tst=:{activated,tasknr,userId}		
	| not activated							= (createDefault,tst)	// not active, don't call task, return default value
	# (val,tst=:{activated,trace})			= mytask tst			// active, so perform task and get its result
	| isNothing trace || taskname == ""		= (val,tst)				// no trace, just return value
	= (val,{tst & tasknr = tasknr
				, trace = Just (InsertTrace activated tasknr userId taskname (printToString val) (fromJust trace))}) // adjust trace

incTaskNr tst 		= {tst & tasknr = incNr tst.tasknr}
newSubTaskNr tst	= {tst & tasknr = [-1:tst.tasknr]}

incNr [] = [0]
incNr [i:is] = [i+1:is]

addTasknr [] j = [j]
addTasknr [i:is] j = [i+j:is]

showTaskNr [] 		= ""
showTaskNr [i] 		= toString i
showTaskNr [i:is] 	= showTaskNr is <+++ "." <+++ toString i 

showThreadNr [-1]		= "Root"
showThreadNr [-1:is]	= showTaskNr is
showThreadNr else		= "*" <+++ showTaskNr else

iTaskId :: !Int !TaskNr !String -> String
iTaskId userid tasknr postfix 
# postfix	=	{ c \\ c <-: postfix | not (isMember c ['\\\"/:*?<>|"']) }			// throw away characters not allowed in a file name
| postfix == ""
	| userid < 0	= "iLog_"  <+++ (showTaskNr tasknr) 
	| otherwise		= "iTask_" <+++ (showTaskNr tasknr) 
| userid < 0		= "iLog_"  <+++ (showTaskNr tasknr) <+++ "-" <+++ postfix
| otherwise			= "iTask_" <+++ (showTaskNr tasknr) <+++ "-" <+++ postfix <+++ "+" <+++ userid

internEditSTask tracename prompt task = \tst -> mkTask tracename ((editTask` prompt task <<@ Page) <<@ Edit) tst

// Garbage collection on iTask administration is done dependening on gc option chosen

deleteSubTasks :: !TaskNr TSt -> TSt
deleteSubTasks tasknr tst 
# tst=:{hst,userId,options}	= IF_Ajax (deleteThreads tasknr tst) tst
| options.gc == NoCollect 	= tst
| otherwise					= {tst & hst = deleteIData (iTaskId userId tasknr "") hst}

// *** wrappers for the end user, to be used in combination with an iData wrapper...

singleUserTask :: ![StartUpOptions] !(Task a) !*HSt -> (Html,*HSt) | iCreate a 
singleUserTask startUpOptions task hst 
# (traceOn,tableloc,_,versionsOn) 	= checkOptions startUpOptions 
# (html,hst) 			= startTstTask 0 False traceOn versionsOn (False,[])  task (initTst 0 tableloc hst)
= mkHtml "singleUser" html hst

checkOptions :: ![StartUpOptions] -> (!Bool,!Lifespan,!Int,!Bool)	// traceOn, threadtable location, max number of users resp,checkversion.
checkOptions startUpOptions = checkOptions` startUpOptions (True,TxtFile,5,False)
where
	checkOptions` [] options 										= options
	checkOptions` [TraceOn:xs] 				(trace,tableloc,n,chk)	= checkOptions` xs (True,tableloc,n,chk)
	checkOptions` [TraceOff:xs] 			(trace,tableloc,n,chk)	= checkOptions` xs (False,tableloc,n,chk)
	checkOptions` [ThreadStorage nloc:xs] 	(trace,tableloc,n,chk) 	= checkOptions` xs (trace,nloc,n,chk)
	checkOptions` [ShowUsers max:xs] 		(trace,tableloc,n,chk) 	= checkOptions` xs (trace,tableloc,max,chk)
	checkOptions` [VersionCheck:xs] 		(trace,tableloc,n,chk) 	= checkOptions` xs (trace,tableloc,n,True)
	checkOptions` [VersionNoCheck:xs] 		(trace,tableloc,n,chk) 	= checkOptions` xs (trace,tableloc,n,False)

multiUserTask :: ![StartUpOptions] !(Task a) !*HSt -> (Html,*HSt) | iCreate a 
multiUserTask startUpOptions task  hst 
# (traceOn,tableloc,nusers,versionsOn) = checkOptions startUpOptions 
# (idform,hst) 	= FuncMenu (Init,nFormId "User_Selected" 
						(0,[("User " +++ toString i,\_ -> i) \\ i<-[0..nusers - 1] ])) hst
# currentWorker	= snd idform.value
# (html,hst) 	= startTstTask currentWorker True traceOn versionsOn  (if traceOn (idform.changed,idform.form) (False,[])) task (initTst currentWorker tableloc hst)
= mkHtml "multiUser" html hst

workFlowTask :: ![StartUpOptions] !(Task (Int,a)) !((Int,a) -> Task b) !*HSt -> (Html,*HSt) | iCreate a 
workFlowTask  startUpOptions taska iataskb hst 
# (traceOn,tableloc,_,chk) 			= checkOptions startUpOptions 
# ((i,a),tst=:{activated,html,hst})	= taska (initTst -1 tableloc hst)		// for doing the login 
| not activated
	# iTaskHeader					= [BCTxt Aqua "i-Task", CTxt Yellow " - Multi-User Workflow System ",Hr []]
	# iTaskInfo						= mkDiv "iTaskInfo" [Txt "Login procedure... ", Hr []]
	= mkHtml "workFlow" [Ajax 	[ ("thePage",iTaskHeader ++ iTaskInfo ++ noFilter html)
								]
						] hst
# (body,hst) 				= startTstTask i True traceOn True (False,[]) (iataskb (i,a)) (initTst i tableloc hst)
= mkHtml "workFlow" body hst
where
	noFilter :: HtmlTree -> [BodyTag]
	noFilter (BT body) 			= body
	noFilter (_ @@: html) 		= noFilter html
	noFilter (_ -@: html) 		= noFilter html
	noFilter (htmlL +-+ htmlR) 	= [noFilter htmlL  <=>  noFilter htmlR]
	noFilter (htmlL +|+ htmlR) 	= noFilter htmlL <|.|> noFilter htmlR
	noFilter (DivCode str html) = noFilter html

// Version number control for multiple user workflows
// To support Ajax calls, it is remembered which of the threads of a user has been deleted by someone else
// 		if information from that thread still arives, the input is thrown away since the thread does not exists anymore.
// 		if information from another thread is received, the task tree is calculated starting from the root
// To support Ajax calls, it is remembered whther new threads have been created for the user by other users
//		if so, the task tree is calculated starting from the root 

traceId							= "User_Trace" 
refreshId						= "User_refresh"
applicationVersionNr			= ThisExe <+++ "_Version" 

userVersionNr thisUser			= "User" <+++ thisUser <+++ "_VersionPNr"
usersessionVersionNr thisUser	= "User" <+++ thisUser <+++ "_VersionSNr" 

setAppversion :: !(Int -> Int) !*HSt -> (Form !Int,!*HSt) 
setAppversion    f hst	= mkStoreForm (Init, pFormId applicationVersionNr 0) 	 f hst

setPUser :: !Int !(VersionInfo -> VersionInfo) !*HSt -> (Form !VersionInfo,!*HSt) 
setPUser user f hst	= mkStoreForm (Init, pFormId (userVersionNr user) 	{ versionNr 	= 0
																		, newThread		= False
																		, deletedThreads= []
																		} <@ NoForm) 	 f hst

addPUserDeletedThread :: !Int !TaskNr !*HSt -> (Form !VersionInfo,!*HSt) 
addPUserDeletedThread user thread hst	= setPUser user (\r -> {r & deletedThreads = [thread:r.deletedThreads]}) hst

setPUserNr :: !Int !(Int -> Int) !*HSt -> (Form !VersionInfo,!*HSt) 
setPUserNr user f hst	= setPUser user (\r -> {r & versionNr = f r.versionNr}) hst

setPUserNewThread :: !Int !*HSt -> (Form !VersionInfo,!*HSt) 
setPUserNewThread user hst	= setPUser user (\r -> {r & newThread = True}) hst

//clearPUser :: !Int !*HSt -> (Form !VersionInfo,!*HSt) 
//clearPUser user hst	= setPUser user (\r -> {r & newThread = False, deletedThreads = []}) hst

clearIncPUser :: !Int !(Int -> Int) !*HSt -> (Form !VersionInfo,!*HSt) 
clearIncPUser user f hst	= setPUser user (\r -> {r & newThread = False, deletedThreads = [], versionNr = f r.versionNr}) hst

setSVersionNr :: !Int !(Int -> Int) !*HSt -> (Form !Int,!*HSt) 
setSVersionNr user f hst	= mkStoreForm (Init, nFormId (usersessionVersionNr user) 0 <@ NoForm) f hst

// Main routine for the creation of the workflow page

startTstTask :: !Int !Bool !Bool !Bool !(!Bool,![BodyTag])  !(Task a) !*TSt -> (![BodyTag],!*HSt) //| iCreate a 
startTstTask thisUser multiuser traceOn versionsOn (userchanged,multiuserform)  taska tst=:{hst,tasknr,staticInfo}

// prolog

| thisUser < 0 			= abort "Users should have id's >= 0 !"
# (refresh,hst) 		= simpleButton refreshId "Refresh" id hst
# (traceAsked,hst) 		= simpleButton traceId "ShowTrace" (\_ -> True) hst
# doTrace				= traceAsked.value False
# (appversion,hst)	 	= setAppversion id hst
# nonewversion			= not versionsOn || refresh.changed || traceAsked.changed || userchanged 
# (appversion,hst)	 	= setAppversion (mbinc nonewversion) hst
# (pversion,hst)	 	= setPUserNr thisUser id hst
# (sversion,hst)	 	= setSVersionNr thisUser (if nonewversion /*refresh.changed*/ (\_ -> pversion.value.versionNr) id) hst
# versionconflict		= sversion.value < pversion.value.versionNr && versionsOn				
# iTaskHeader			=	[Table [Tbl_Width (Percent 100)] [Tr [] 
							[ Td [] [Img [Img_Src (ThisExe +++ "/scleanlogo.jpg"),Img_Align Alo_Middle]
									,BCTxt Aqua "i -Task", CTxt Yellow " Workflow System "]
							, Td [Td_Align Aln_Right] (multiuserform ++ refresh.form ++ ifTraceOn traceAsked.form)] ]]++
							[Hr []]
| versionconflict	 
/*
	# iTaskHeader		=	[Table [Tbl_Width (Percent 100)] [Tr [] 
							[ Td [] [BCTxt Aqua "i-Task", Br, CTxt Yellow " - Multi-User Workflow System "]
							, Td [Td_Align Aln_Right] (multiuserform ++ refresh.form ++ ifTraceOn traceAsked.form)] ]]++
							[Hr []]
*/	# iTaskInfo			= mkDiv "iTaskInfo" [CTxt Yellow "Cannot apply request. Version conflict. Please refresh the page!", Hr []]
	= ([Ajax 	[ ("thePage",iTaskHeader ++ iTaskInfo)
						]
				],hst)


// Here the iTask starts...
													    
# ((_,thrOwner,event,thrinfo,onserver,threads),tst=:{html,hst,trace})	
						= (IF_Ajax (startAjaxApplication thisUser pversion.value) startMainTask)
							taska {tst & hst = hst, trace = if doTrace (Just []) Nothing, activated = True, html = BT []}

// epilog
# (_,hst)				= clearIncPUser thisUser (mbinc nonewversion) hst									// clear administration
# (sversion,hst)	 	= setSVersionNr thisUser (mbinc nonewversion) hst

# showCompletePage		= IF_Ajax (hd threads == [-1]) True
# (threadtrace,tst=:{hst})	
						= IF_Ajax (if TraceThreads showThreadTable (\tst -> ([],tst)) {tst & hst = hst}) ([],{tst & hst = hst})
# threadsText			= if showCompletePage thrinfo ((thrinfo +++ ": ") +++ foldl (+++) "" [showThreadNr tasknrs +++ " + " \\ tasknrs <- reverse threads])
# (threadcode,selbuts,selname,seltask,hst)	
						= Filter showCompletePage thrOwner html hst

/*
# iTaskHeader			=	[Table [Tbl_Width (Percent 100)] [Tr [] 
							[ Td [] [Img [Img_Src (ThisExe +++ "/scleanlogo.jpg"),Img_Align Alo_Middle]
							,BCTxt Aqua "i-Task", CTxt Yellow " - Multi-User Workflow System "]
							, Td [Td_Align Aln_Right] (multiuserform ++ refresh.form ++ ifTraceOn traceAsked.form)] ]]++
							[Hr []]
*/
# iTaskInfo				= 	mkDiv "iTaskInfo" 
							(	if multiuser 
									[Txt "User: " , CTxt Silver thisUser, Txt " - ", Txt "Querie: " , CTxt Silver sversion.value, Txt " - "] [] ++
								if versionsOn [Txt "iTask Querie: ", CTxt Silver appversion.value] [Txt "iTask Querie: - "] ++
								IF_Ajax
									[Txt " - Task: ", CTxt Silver (showTaskNr  event),Txt (if onserver " - Server Updated: " " - Client Updated: "), CTxt Silver threadsText,Br] [] ++
								[Hr []]
							)
# iTaskTraceInfo		=	showOptions staticInfo.threadTableLoc ++ threadtrace ++ [printTrace2 trace ]
| showCompletePage		=	([Ajax [("thePage",	iTaskHeader ++
														iTaskInfo  ++
														if (doTrace && traceOn)
																iTaskTraceInfo
																[ STable []	[ [BodyTag  selbuts, selname <||>  seltask ]
																			]
																]
											)]
									] 
							,hst)
# (newthread,oldthreads)=	(hd threads, tl threads)
| otherwise				=	([Ajax (	[("iTaskInfo", iTaskInfo)] ++			// header ino
											[(showTaskNr childthreads,[Txt " "]) \\ childthreads <- oldthreads] ++ //clear childthreads, since parent thread don't need to be on this page
											[(showTaskNr newthread, if (isNil threadcode) seltask threadcode)]	// task info
										   )
									]
							,hst)
where
	startMainTask :: !(Task a) !*TSt -> ((!Maybe a,!Int,!TaskNr,!String,!Bool,![TaskNr]),*TSt) 	// No threads, always start from scratch		
	startMainTask task tst
	# (a,tst) = task tst
	= ((Just a,defaultUser,[0],"",True,[]),tst)

	mbinc True = id
	mbinc _ = inc

	ifTraceOn form = if traceOn form []

	showOptions location
	= [Txt "Version nr: ", CTxt Silver iTaskVersion] ++
	  [Txt " - iTask Options: "] ++
	  [CTxt Silver (IF_Ajax 	(" Ajax (" <+++ location <+++ ") ") " No Ajax ")] ++
	  [CTxt Silver (IF_OnClient	(IF_Ajax " + Client" "") "")] ++
	  [CTxt Silver (IF_Database " + Database" "")] ++
	  [CTxt Silver (IF_DataFile " + Datafile" "")] ++
	  [Br,Hr []]


	mkSTable2 :: [[BodyTag]] -> BodyTag
	mkSTable2 table
	= Table []	(mktable table)
	where
		mktable table 	= [Tr [] (mkrow rows) \\ rows <- table]	
		mkrow rows 		= [Td [Td_VAlign Alo_Top] [row] \\ row <- rows] 

	Filter wholepage thrOwner tree hst
	# startuser			= if wholepage defaultUser thrOwner
	# (threadcode,accu) = Collect thisUser startuser [] ((startuser,"Main") @@: tree) 
	| isNil accu		= (threadcode,[],[],[],hst)
	# (names,tasks) 	= unzip accu
	# info				= initialOptions
	# ((selected,buttons,chosenname),hst) = mkTaskButtons "Main Tasks:" ("User " <+++ thisUser) thisUser [] info names hst 
	= (threadcode,buttons,chosenname,tasks!!if (selected > length accu) 0 selected,hst)

	Collect thisuser taskuser accu ((ntaskuser,taskname) @@: tree) 	// Collect returns the wanted code, and the remaining code
	# (myhtml,accu)	= Collect thisuser ntaskuser accu tree			// Collect all code of this user belonging to this task
	| thisuser == ntaskuser && not (isNil myhtml)
							= ([],[(taskname,myhtml):accu])
	| otherwise				= ([],accu)
	Collect thisuser taskuser accu (nuser -@: tree)
	| thisuser == nuser 	= ([],accu)
	| otherwise				= Collect thisuser taskuser accu tree
	Collect thisuser taskuser accu (tree1 +|+ tree2)
	# (lhtml,accu)	= Collect thisuser taskuser accu tree1
	# (rhtml,accu)	= Collect thisuser taskuser accu tree2
	= (lhtml <|.|> rhtml,accu)
	Collect thisuser taskuser accu (tree1 +-+ tree2)
	# (lhtml,accu)	= Collect thisuser taskuser accu tree1
	# (rhtml,accu)	= Collect thisuser taskuser accu tree2
	= ([lhtml <=> rhtml],accu)
	Collect thisuser taskuser accu (BT bdtg)
	| thisuser == taskuser	= (bdtg,accu)
	| otherwise				= ([],accu)
	Collect thisuser taskuser accu (DivCode id tree)
	# (html,accu)			= Collect thisuser taskuser accu tree
	| thisuser == taskuser 	= (mkDiv id html,accu)
	= ([],accu)


mkTaskButtons :: !String !String !Int !TaskNr !Options ![String] *HSt -> ((Int,[BodyTag],[BodyTag]),*HSt)
mkTaskButtons header myid userId tasknr info btnnames hst
# btnsId			= iTaskId userId tasknr (myid <+++ "genBtns")
# myidx				= length btnnames
# (chosen,hst)		= SelectStore (myid,myidx) tasknr info id hst						// which choice was made in the past
# (buttons,hst)		= SelectButtons Init btnsId info (chosen,btnnames) hst				// create buttons
# (chosen,hst)		= SelectStore (myid,myidx) tasknr info  buttons.value hst			// maybe a new button was pressed
# (buttons,hst)		= SelectButtons Set btnsId info (chosen,btnnames) hst				// adjust look of that button
= ((chosen,[CTxt Red header, Br: buttons.form],[CTxt Yellow (btnnames!!chosen),Br,Br]),hst)
where
	SelectButtons init id info (idx,btnnames) hst = TableFuncBut2 (init,pageFormId info id 
															[[(mode idx n, but txt,\_ -> n)] \\ txt <- btnnames & n <- [0..]]) hst
	but i = LButton defpixel i

	mode i j
	| i==j = Display

	= Edit

	SelectStore :: !(String,Int) !TaskNr !Options (Int -> Int) *HSt -> (Int,*HSt)
	SelectStore (myid,idx) tasknr info fun hst 
	# storeId 			= iTaskId userId tasknr (myid <+++ "BtnsS" <+++ idx)
	# (storeform,hst)	= mkStoreForm (Init,storageFormId info storeId 0) fun hst
	= (storeform.value,hst)

// Below we define the iTask Combinators...

// make an iTask editor

editTask :: !String !a -> (Task a) | iData a 
editTask prompt a = mkTask "editTask" (editTask` prompt a)

editTask` prompt a tst=:{tasknr,html,hst,userId}
# taskId			= iTaskId userId tasknr "EdFin"
# editId			= iTaskId userId tasknr "EdVal"
# buttonId			= iTaskId userId tasknr "EdBut"
# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) id hst  	// remember if the task has been done
| taskdone.value																			// test if task has completed
	# (editor,hst) 	= (mkEditForm  (Init,cFormId tst.options editId a <@ Display) hst)		// yes, read out current value, make editor passive
	= (editor.value,{tst & activated = True, html = html +|+ BT editor.form, hst = hst})	// return result task
# (editor,hst) 		= mkEditForm  (Init,cFormId tst.options editId a) hst					// no, read out current value from active editor
# (finbut,hst)  	= simpleButton buttonId prompt (\_ -> True) hst							// add button for marking task as done
# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) finbut.value hst 	// remember task status for next time
| taskdone.value	= editTask` prompt a {tst & hst = hst}									// task is now completed, handle as previously
= (editor.value,{tst & activated = taskdone.value, html = html +|+ BT (editor.form ++ finbut.form), hst = hst})

editTaskPred :: !a !(a -> (Bool, [BodyTag]))-> (Task a) | iData a 
editTaskPred  a pred = mkTask "editTask" (editTaskPred` a)
where
	editTaskPred` a tst=:{tasknr,html,hst,userId}
	# taskId			= iTaskId userId tasknr "EdFin"
	# editId			= iTaskId userId tasknr "EdVal"
	# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) id hst  	// remember if the task has been done
	| taskdone.value																			// test if task has completed
		# (editor,hst) 	= (mkEditForm  (Init,cFormId tst.options editId a <@ Display) hst)		// yes, read out current value, make editor passive
		= (editor.value,{tst & activated = True, html = html +|+ BT editor.form, hst = hst})	// return result task
	# (editor,hst) 		= mkEditForm  (Init,cFormId tst.options editId a <@ Submit) hst			// no, read out current value from active editor
	| editor.changed
		| fst (pred editor.value)
			# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) (\_ -> True) hst 	// remember task status for next time
			= editTaskPred` a {tst & hst = hst, html = html}									// task is now completed, handle as previously
		= (editor.value,{tst & activated = taskdone.value, html = html +|+ BT (editor.form ++ snd (pred editor.value)), hst = hst})
	= (editor.value,{tst & activated = taskdone.value, html = html +|+ BT editor.form, hst = hst})

// monads for combining itasks

(=>>) infix 1 :: !(Task a) !(a -> Task b) -> Task b | iCreateAndPrint b
(=>>) taska taskb = mybind
where
	mybind tst=:{options}
	# (a,tst=:{activated}) = taska tst
//	= taskb a {tst & options = options}
	| activated	= taskb a {tst & options = options}
	= (createDefault,tst)

(#>>) infixl 1 :: !(Task a) !(Task b) -> Task b
(#>>) taska taskb = mybind
where
	mybind tst=:{options}
	# (a,tst) = taska tst
	= taskb {tst & options = options}

return_V :: !a -> (Task a) | iCreateAndPrint a
return_V a  = mkTask "return_V" (return a) 

// some simple variants

return_D :: !a -> (Task a) | gForm {|*|}, iCreateAndPrint a
return_D a = mkTask "return_D" return_Display`
where
	return_Display` tst
	= (a,{tst & html = tst.html +|+ BT [toHtml a ]})		// return result task

return_VF :: !a ![BodyTag] -> (Task a) | iCreateAndPrint a
return_VF a bodytag = mkTask "return_VF" return_VF`
where
	return_VF` tst
	= (a,{tst & html = tst.html +|+ BT bodytag})

(<|) infix 6 :: !(Task a) !(a -> (Bool, [BodyTag])) -> Task a | iCreate a
(<|) taska pred = doTask
where
	doTask tst=:{html = ohtml,activated}
	| not activated 					= (createDefault,tst)
	# (a,tst=:{activated,html= nhtml}) 	= taska {tst & html = BT []}
	| not activated || fst (pred a)		= (a,{tst & html = ohtml +|+ nhtml})
	= doTask {tst & html = ohtml +|+ BT (snd (pred a))}

(?>>) infix 5 :: ![BodyTag] !(Task a) -> (Task a) | iData a
//(?>>) infix 5 :: [BodyTag] (Task a) -> (Task a) | iCreate a
(?>>) prompt task = doTask
where
	doTask tst=:{html=ohtml,activated}
	| not activated						= (createDefault,tst)
//	# (a,tst=:{activated,html=nhtml}) 	= IF_Ajax (UseAjax @>> task) task {tst & html = BT []}
	# (a,tst=:{activated,html=nhtml}) 	= task {tst & html = BT []}
	| activated 						= (a,{tst & html = ohtml})
	= (a,{tst & html = ohtml +|+ BT prompt +|+ nhtml})

(!>>) infix 5 :: ![BodyTag] !(Task a) -> (Task a) | iCreate a
(!>>) prompt task = doTask
where
	doTask tst=:{html=ohtml,activated=myturn}
	| not myturn			= (createDefault,tst)
	# (a,tst=:{html=nhtml}) = task {tst & html = BT []}
	= (a,{tst & html = ohtml +|+ BT prompt +|+ nhtml})


/////////////////////////////////////

// the following definitions are the simple versions
// they work, but are NOT suited for big applications
// I will kick them out some day....

repeatTask :: !(a -> Task a) !(a -> Bool) -> a -> Task a | iData a
repeatTask task pred = dorepeatTask
where
	dorepeatTask a 
	= newTask "doReapeatTask" dorepeatTask`
	where
		dorepeatTask` tst
		| pred a	= (a,tst) 
		# (na,tst)	= task a tst	
		= dorepeatTask na tst

// same, but by remembering task results stack space can be saved
newTask :: !String !(Task a) -> (Task a) 	| iData a 
newTask taskname mytask = mkTask taskname newTask`
where
	newTask` tst=:{tasknr,userId,options}		
	# taskId					= iTaskId userId tasknr taskname
	# (taskval,tst) 			= LiftHst (mkStoreForm (Init,storageFormId options taskId (False,createDefault)) id) tst  // remember if the task has been done
	# (taskdone,taskvalue)		= taskval.value										// select values
	| taskdone					= (taskvalue,tst)									// if rewritten return stored value
	# (val,tst=:{activated})	= mytask {tst & tasknr = [-1:tasknr]} 				// do task, first shift tasknr
	| not activated				= (val,{tst & tasknr = tasknr, options = options})	// subtask not ready, return value of subtasks
	# tst						= deleteSubTasks tasknr tst							// task ready, garbage collect it
	# (_,tst) 					= LiftHst (mkStoreForm (Init,storageFormId options taskId (False,createDefault)) (\_ -> (True,val))) tst  // remember if the task has been done
	= (val,{tst & tasknr = tasknr, options = options})

// foreverTask is a loop
// when gc option set and task finished, it will throw away all subtasks and start all over
// otherwise, when task finshed it will remember the new tasknr to prevent checking of previously finished tasks

foreverTask :: !(Task a) -> Task a | iData a
foreverTask task = mkTask "foreverTask" foreverTask`
where
	foreverTask` tst=:{tasknr,activated,userId,options} 
	| options.gc == Collect																				// garbace collect everything when task finsihed
		# (val,tst=:{activated})= task {tst & tasknr = [-1:tasknr]}										// shift tasknr
		| activated 			= foreverTask` (deleteSubTasks tasknr {tst & tasknr = tasknr, options = options}) 			// loop
		= (val,tst)					
	# taskId					= iTaskId userId tasknr "ForSt"											// create store id
	# (currtasknr,tst)			= LiftHst (mkStoreForm (Init,storageFormId options taskId tasknr) id) tst		// fetch actual tasknr
	# (val,tst=:{activated})	= task {tst & tasknr = [-1:currtasknr.value]}
	| activated 																						// task is completed	
		# ntasknr				= incNr currtasknr.value												// incr tasknr
		# (currtasknr,tst)		= LiftHst (mkStoreForm (Init,storageFormId options taskId tasknr) (\_ -> ntasknr)) tst // store next task nr
		= foreverTask` {tst & tasknr = tasknr, options = options}										// initialize new task
	= (val,tst)					

(<!) infix 6 :: !(Task a) !(a -> .Bool) -> Task a | iCreateAndPrint a
(<!) taska pred = mkTask "less!" doTask
where
	doTask tst=:{activated, tasknr}
	# (a,tst=:{activated}) 	= taska {tst & tasknr = [-1:tasknr]}
	| not activated 		= (a,tst)
	| not (pred a)			
		# tst = deleteSubTasks tasknr tst
		= (a,{tst & activated = False})
	= (a,tst)

// parallel subtask creation utility

mkParSubTask :: !String !Int (Task a) -> (Task a)  | iCreateAndPrint a					// two shifts are needed
mkParSubTask name i task = mkParSubTask`
where
	mkParSubTask` tst=:{tasknr}
	# (v,tst) = mkTaskNoInc (name <+++ "." <+++ i) mysubtask {tst & tasknr = [i:tasknr],activated = True} // shift task
	= (v,{tst & tasknr = tasknr})
	where
		mysubtask tst=:{tasknr} = task {tst & tasknr = [-1:tasknr], activated = True}	// shift once again!

// assigning tasks to users, each user is identified by a number

(@:) infix 3 :: !(!String,!Int) !(Task a)	-> (Task a)			| iData a				// force thread if Ajax is used
(@:) (taskname,nuserId) taska = \tst=:{userId} -> assignTaskTo False taskname userId taska {tst & userId = nuserId}

(@::) infix 3 :: !Int !(Task a)	-> (Task a)			| iData  a							// force thread if Ajax is used							
(@::) nuserId taska = \tst=:{userId} -> assignTaskTo False ("Task for " <+++ userId) userId taska {tst & userId = nuserId}

assignTaskTo :: !Bool !String !Int !(Task a) !*TSt -> (a,!*TSt)			| iData a	
assignTaskTo verbose taskname userId taska tst=:{html=ohtml,activated,userId = nuserId}
| not activated						= (createDefault,tst)
# tst								= IF_Ajax (administrateNewThread userId tst) tst 
# (a,tst=:{html=nhtml,activated})	= IF_Ajax (UseAjax @>> taska) taska {tst & html = BT [],userId = nuserId}		// activate task of indicated user
| activated 						= (a,{tst & activated = True						// work is done	
											  ,	userId = userId							// restore previous user id						
											  ,	html = ohtml /*+|+ 						// show old code						
													   ((nuserId,taskname) @@: nhtml)*/})	// plus new one tagged
= (a,{tst & userId = userId																// restore user Id
		  , html = 	ohtml +|+ 															// show old code
					if verbose 
						( BT [Br, Txt ("Waiting for Task "), CTxt Yellow taskname, Txt " from ", showUser nuserId,Br] +|+  // show waiting for
						  ((nuserId,taskname) @@: BT [Txt "Requested by ", showUser userId,Br,Br] +|+ nhtml)) 
						((nuserId,taskname) @@: nhtml)
	 })												

administrateNewThread ouserId tst =: {tasknr,userId,options}
| ouserId == userId		= tst
# newTaskId				= iTaskId userId tasknr "_newthread"
# (chosen,tst=:{hst})	= LiftHst (mkStoreForm  (Init,storageFormId options newTaskId False) id) tst	// first time here ?
| not chosen.value
	# (_,hst) 			= setPUserNewThread userId hst													// yes, new thread created
	# (_,tst)			= LiftHst (mkStoreForm  (Init,storageFormId options newTaskId False) (\_ -> True)) {tst & hst = hst}
	= tst
= tst	


/*
(@::) infix 3 :: !Int !(Task a)	-> (Task a)			| iData  a							// force thread if Ajax is used							
(@::) nuserId taska = \tst=:{userId} -> assignTask` userId {tst & userId = nuserId}
where
	assignTask` userId tst=:{html,activated}
	| not activated						= (createDefault,tst)
	# (a,tst=:{html=nhtml,activated})	= IF_Ajax (UseAjax @>> taska) taska  {tst & html = BT [],userId = nuserId}		// activate task of indicated user
	| activated 						= (a,{tst & userId = userId							// work is done						
												  ,	html = html})	
	= (a,{tst & userId = userId																// restore user Id
			  , html = 	html +|+  
			  			BT [Br, Txt "Waiting for ", CTxt Yellow ("Task " <+++ userId), Txt " from ", showUser nuserId,Br] +|+ 
						((nuserId,"Task " <+++ userId) @@: 
							BT [Txt "Requested by ", showUser userId,Br,Br] +|+ nhtml)})				// combine html code, filter later					

*/

// sequential tasks

seqTasks :: ![(String,Task a)] -> (Task [a])| iCreateAndPrint a
seqTasks options = mkTask "seqTasks" seqTasks`
where
	seqTasks` tst=:{tasknr}
	# (val,tst)	 = doseqTasks options [] {tst & tasknr = [-1:tasknr]}
	= (val,{tst & tasknr = tasknr})

	doseqTasks [] accu tst 		= (reverse accu,{tst & activated = True})
	doseqTasks [(taskname,task):ts] accu tst=:{html,options} 
	# (a,tst=:{activated=adone,html=ahtml}) 
									= task {tst & activated = True, html = BT []}
	| not adone						= (reverse accu,{tst & html = html +|+ BT [CTxt Yellow taskname,Br,Br] +|+ ahtml})
	= doseqTasks ts [a:accu] {tst & html = html +|+ ahtml, options = options}

// choose one or more tasks out of a collection
buttonTask :: !String !(Task a) -> (Task a) | iCreateAndPrint a
buttonTask s task = iCTask_button "buttonTask" [(s,task)]

iCTask_button tracename options = mkTask tracename (dochooseTask True options)

chooseTask :: ![(String,Task a)] -> (Task a) | iCreateAndPrint a
chooseTask options = mkTask "chooseTask" (dochooseTask True options)

chooseTaskV :: ![(String,Task a)] -> (Task a) | iCreateAndPrint a
chooseTaskV options = mkTask "chooseTask" (dochooseTask False options)

dochooseTask _ [] tst			= return createDefault tst				
dochooseTask horizontal taskOptions tst=:{tasknr,html,options,userId}									// choose one subtask out of the list
# taskId						= iTaskId userId tasknr ("ChoSt" <+++ length taskOptions)
# buttonId						= iTaskId userId tasknr "ChoBut"
# (chosen,tst)					= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
| chosen.value == -1			// no choice made yet
	# allButtons				= if horizontal 
										[[(but txt,\_ -> n) \\ txt <- map fst taskOptions & n <- [0..]]]
										[[(but txt,\_ -> n)] \\ txt <- map fst taskOptions & n <- [0..]]
	# (choice,tst)				= LiftHst (TableFuncBut (Init,pageFormId options buttonId allButtons)) tst
	# (chosen,tst)				= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) choice.value) tst
	| chosen.value == -1		= (createDefault,{tst & activated =False,html = html +|+ BT choice.form})
	# chosenTask				= snd (taskOptions!!chosen.value)
	# (a,tst=:{activated=adone,html=ahtml}) = chosenTask {tst & tasknr = [-1:tasknr], activated = True, html = BT []}
	= (a,{tst & tasknr = tasknr, activated = adone, html = html +|+ ahtml})
# chosenTask					= snd (taskOptions!!chosen.value)
# (a,tst=:{activated=adone,html=ahtml}) = chosenTask {tst & tasknr = [-1:tasknr], activated = True, html = BT []}
= (a,{tst & tasknr = tasknr, activated = adone, html = html +|+ ahtml})

but i = LButton defpixel i

chooseTask_pdm :: ![(String,Task a)] -> (Task a) |iCreateAndPrint a
chooseTask_pdm taskOptions = mkTask "chooseTask_pdm" (dochooseTask_pdm taskOptions)
where
	dochooseTask_pdm [] tst			= (createDefault,{tst& activated = True})	
	dochooseTask_pdm taskOptions tst=:{tasknr,html,userId,options}								// choose one subtask out of the list
	# taskId						= iTaskId userId tasknr ("ChoPdm" <+++ length taskOptions)
	# (choice,tst)					= LiftHst (FuncMenu  (Init,sessionFormId options taskId (0,[(txt,id) \\ txt <- map fst taskOptions]))) tst
	# (_,tst=:{activated=adone,html=ahtml})	
									= internEditSTask "" "Done" Void {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 	
	| not adone						= (createDefault,{tst & activated = False, html = html +|+ BT choice.form +|+ ahtml, tasknr = tasknr})
	# chosenIdx						= snd choice.value
	# chosenTask					= snd (taskOptions!!chosenIdx)
	# (a,tst=:{activated=bdone,html=bhtml}) 
									= chosenTask {tst & activated = True, html = BT [], tasknr = [0:tasknr]}
	= (a,{tst & activated = adone&&bdone, html = html +|+ bhtml, tasknr = tasknr})

mchoiceTasks :: ![(String,Task a)] -> (Task [a]) | iCreateAndPrint a
mchoiceTasks taskOptions = mkTask "mchoiceTask" (domchoiceTasks taskOptions)
where
	domchoiceTasks [] tst	= ([],{tst& activated = True})
	domchoiceTasks taskOptions tst=:{tasknr,html,options,userId}									// choose one subtask out of the list
	# seltaskId				= iTaskId userId tasknr ("MtpChSel" <+++ length taskOptions)
	# donetaskId			= iTaskId userId tasknr "MtpChSt"
	# (cboxes,tst)			= LiftHst (ListFuncCheckBox (Init,sessionFormId options seltaskId initCheckboxes)) tst
	# (done,tst)			= LiftHst (mkStoreForm      (Init,storageFormId options donetaskId False) id) tst
	# optionsform			= cboxes.form <=|> [Txt text \\ (text,_) <- taskOptions]
	| done.value			= seqTasks [option \\ option <- taskOptions & True <- snd cboxes.value] {tst & tasknr = [0:tasknr]}
	# (_,tst=:{html=ahtml,activated = adone})
							= (internEditSTask "" "OK" Void) {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 
	| not adone				= ([],{tst & html = html +|+ BT [optionsform] +|+ ahtml})
	# (_,tst)				= LiftHst (mkStoreForm      (Init,storageFormId options donetaskId False) (\_ -> True)) tst
	= domchoiceTasks taskOptions {tst & tasknr = tasknr, html = html, options = options, userId =userId, activated = True}									// choose one subtask out of the list

	initCheckboxes  = 
		[(CBNotChecked  text,  \ b bs id -> id) \\ (text,_) <- taskOptions]

// tasks ending as soon as one of its subtasks completes

(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iCreateAndPrint a
(-||-) taska taskb = mkTask "-||-" (doOrTask (taska,taskb))

orTask :: !(Task a,Task a) -> (Task a) | iCreateAndPrint a
orTask (taska,taskb) = mkTask "orTask" (doOrTask (taska,taskb))

doOrTask (taska,taskb) tst=:{tasknr,options,html,userId}
# taskId								= iTaskId userId tasknr "orTaskSt"
# (chosen,tst)							= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
| chosen.value == 0						// task a was finished first in the past
	# (a,tst)							= mkParSubTask "orTask" 0 taska {tst & tasknr = tasknr, html = BT []}
	= (a,{tst & html = html})
| chosen.value == 1						// task b was finished first in the past
	# (b,tst)							= mkParSubTask "orTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
	= (b,{tst & html = html})
# (a,tst=:{activated=adone,html=ahtml})	= mkParSubTask "orTask" 0 taska {tst & tasknr = tasknr, html = BT [],options = options}
# (b,tst=:{activated=bdone,html=bhtml})	= mkParSubTask "orTask" 1 taskb {tst & tasknr = tasknr, html = BT [],options = options}
| adone
	# tst 								= deleteSubTasks [1:tasknr] {tst & tasknr = tasknr}
	# (chosen,tst)						= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) (\_ -> 0)) {tst & html = BT []}
	= (a,{tst & html = html, activated = True})
| bdone
	# tst 								= deleteSubTasks [0:tasknr] {tst & tasknr = tasknr}
	# (chosen,tst)						= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) (\_ -> 1)) {tst & html = BT []}
	= (b,{tst & html = html, activated = True})
= (a,{tst & activated = False, html = html +|+ ahtml +|+ bhtml})
 

orTask2 :: !(Task a,Task b) -> (Task (EITHER a b)) | iCreateAndPrint a & iCreateAndPrint b
orTask2 (taska,taskb) = mkTask "orTask2" (doorTask2 (taska,taskb))
where
	doorTask2 (taska,taskb) tst=:{tasknr,html,options,userId}
	# taskId								= iTaskId userId tasknr "orTask2St"
	# (chosen,tst)							= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
	| chosen.value == 0						// task a was finished first in the past
		# (a,tst=:{html=ahtml})				= mkParSubTask "orTask" 0 taska {tst & tasknr = tasknr, html = BT []}
		= (LEFT a,{tst & html = html})
	| chosen.value == 1						// task b was finished first in the past
		# (b,tst=:{html=bhtml})				= mkParSubTask "orTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
		= (RIGHT b,{tst & html = html})
	# (a,tst=:{activated=adone,html=ahtml})	= mkParSubTask "orTask" 0 taska {tst & tasknr = tasknr, html = BT []}
	# (b,tst=:{activated=bdone,html=bhtml})	= mkParSubTask "orTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
	| adone
		# tst 								= deleteSubTasks [1:tasknr] {tst & tasknr = tasknr}
		# (chosen,tst)						= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) (\_ -> 0)) {tst & html = BT []}
		= (LEFT a,{tst & html = html, activated = True})
	| bdone
		# tst 								= deleteSubTasks [0:tasknr] {tst & tasknr = tasknr}
		# (chosen,tst)						= LiftHst (mkStoreForm  (Init,storageFormId tst.options taskId -1) (\_ -> 1)) {tst & html = BT []}
		= (RIGHT b,{tst & html = html, activated = True})
	= (LEFT a,{tst & activated = False, html = html +|+ ahtml +|+ bhtml})

checkAnyTasks traceid taskoptions (ctasknr,skipnr) (bool,which) tst=:{tasknr}
| ctasknr == length taskoptions	= (bool,which,tst)
| ctasknr == skipnr				= checkAnyTasks traceid taskoptions (inc ctasknr,skipnr) (bool,which) tst
# task							= taskoptions!!ctasknr
# (a,tst=:{activated = adone})	= mkParSubTask traceid ctasknr task {tst & tasknr = tasknr, activated = True}
= checkAnyTasks traceid taskoptions (inc ctasknr,skipnr) (bool||adone,if adone ctasknr which) {tst & tasknr = tasknr, activated = True}

orTasks :: ![(String,Task a)] -> (Task a) | iData a
orTasks taskOptions = mkTask "orTasks" (doorTasks taskOptions)
where
	doorTasks [] tst	= return createDefault tst
	doorTasks tasks tst=:{tasknr,html,options,userId}
	# taskId			= iTaskId userId tasknr "orTasksChosen"
	# (chosenS,tst)		= LiftHst (mkStoreForm  (Init,storageFormId options taskId (-1,createDefault)) id) tst
	# (pchosen,avalue)	= chosenS.value
	| pchosen <> -1		// one of the tasks has been finished already
		= (avalue,{tst & activated = True, html = html}) 
	# ((chosen,buttons,chosenname),tst) 
						= LiftHst (mkTaskButtons "or Tasks:" "or" userId tasknr options (map fst taskOptions)) {tst & html = BT []}
	# (finished,which,tst=:{html=allhtml})= checkAnyTasks "orTasks" (map snd taskOptions) (0,chosen) (False,0) {tst & html = BT [], activated = True}
	# chosenvalue		= if finished which chosen			// it can be the case that someone else has finshed one of the tasks
	# chosenTaskName	= fst (taskOptions!!chosenvalue)
	# chosenTask		= snd (taskOptions!!chosenvalue)
	# (a,tst=:{activated=adone,html=ahtml}) 
						= mkParSubTask "orTasks" chosenvalue chosenTask {tst & tasknr = tasknr, activated = True, html = BT []}
	| not adone			= (a,{tst 	& activated = adone
									, html =	html +|+ 
												BT buttons +-+ 	(BT chosenname +|+ ahtml) +|+ 
												(userId -@: allhtml)
							})
	# tst 				= deleteSubTasks tasknr {tst & tasknr = tasknr}
	# (_,tst)			= LiftHst (mkStoreForm  (Init,storageFormId options taskId (-1,createDefault)) (\_ -> (chosenvalue,a)))  {tst & html = BT []} // remember finished task for next tim
	= (a,{tst & activated = adone, html = html, tasknr = tasknr}) 

// Parallel tasks ending if all complete

(-&&-) infixr 4 ::  !(Task a) !(Task b) -> (Task (a,b)) | iCreateAndPrint a & iCreateAndPrint b
(-&&-) taska taskb = mkTask "-&&-" (doAndTask (taska,taskb))

andTask :: !(Task a,Task b) -> (Task (a,b)) | iCreateAndPrint a & iCreateAndPrint b
andTask (taska,taskb) = mkTask "andTask" (doAndTask (taska,taskb))

doAndTask (taska,taskb) tst=:{tasknr,html}
# (a,tst=:{activated=adone,html=ahtml})	= mkParSubTask "andTask" 0 taska {tst & html = BT []}
# (b,tst=:{activated=bdone,html=bhtml})	= mkParSubTask "andTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
= ((a,b),{tst & activated = adone&&bdone, html = html +|+ ahtml +|+ bhtml})

andTasks :: ![(String,Task a)] -> (Task [a]) | iCreateAndPrint a
andTasks taskOptions = mkTask "andTasks" (doandTasks taskOptions)
where
	doandTasks [] tst	= return [] tst
	doandTasks taskOptions tst=:{tasknr,html,options,userId}
	# (alist,tst=:{activated=finished})		
						= checkAllTasks "andTasks" taskOptions (0,-1) True [] {tst & html = BT [], activated = True}
	| finished			= (map snd alist,{tst & html = html}) 		// all andTasks are finished
	# ((chosen,buttons,chosenname),tst) 							// user can select one of the tasks to work on
						= LiftHst (mkTaskButtons "and Tasks:" "and" userId tasknr options (map fst taskOptions)) tst
	# chosenTask		= snd (taskOptions!!chosen)
	# chosenTaskName	= fst (taskOptions!!chosen)
	# (a,tst=:{activated=adone,html=ahtml}) 						// enable the selected task (finished or not)
						= mkParSubTask "andTasks" chosen chosenTask {tst & tasknr = tasknr, activated = True, html = BT []}
	# (alist,tst=:{activated=finished,html=allhtml})				// check again whether all other tasks are now finished, collect their code		
						= checkAllTasks "andTasks" taskOptions (0,chosen) True [] {tst & tasknr = tasknr, html = BT [], activated = True}
	| not adone			= ([a],{tst &	activated = False 			// not done, since chosen task not finished
									, 	html = 	html +|+ 
												BT buttons +-+ 	(BT chosenname +|+ ahtml) +|+ 
												(userId -@: allhtml) // code for non selected alternatives are not shown for the owner of this task
							})
	# (alist,tst=:{activated=finished,html=allhtml})		
						= checkAllTasks "andTasks" taskOptions (0,-1) True [] {tst & html = BT [],activated = True} 
	| finished			= (map snd alist,{tst & activated = finished, html = html}) // since selected task and others are finished
	= (map snd alist,{tst 	& activated = finished
							, html = 	html +|+ 
										BT buttons +-+ 	(BT chosenname +|+ ahtml) +|+ 
										(userId -@: allhtml)
						})

checkAllTasks traceid options (ctasknr,skipnr) bool alist tst=:{tasknr}
| ctasknr == length options 	= (reverse alist,{tst & activated = bool})
| ctasknr == skipnr				= checkAllTasks traceid options (inc ctasknr,skipnr) bool alist tst
# (taskname,task)				= options!!ctasknr
# (a,tst=:{activated = adone})	= mkParSubTask traceid ctasknr task {tst & tasknr = tasknr, activated = True}
= checkAllTasks traceid options (inc ctasknr,skipnr) (bool&&adone) [(taskname,a):alist] {tst & tasknr = tasknr, activated = True}

/* Do use this one!!
andTasks_mstone :: do all iTasks in any order (interleaved), task completed when all done
					but continue with next task as soon as one of the tasks is completed
					string indicates which task delivered what
*/

andTasks_mstone :: [(String,Task a)] -> (Task [(String,a)]) | iCreateAndPrint a
andTasks_mstone taskOptions = mkTask "andTasks_mstone" (PMilestoneTasks` taskOptions)
where
	PMilestoneTasks` [] tst	= return [] tst
	PMilestoneTasks` taskOptions tst=:{tasknr,html,options,userId}
	# (alist,tst=:{activated=finished,html=allhtml})		
						= checkAllTasks "andTasks" taskOptions (0,-1) True [] {tst & html = BT [], activated = True}
	| finished			= (alist,{tst & html = html}) 
	# ((chosen,buttons,chosenname),tst) 
						= LiftHst (mkTaskButtons "and Tasks:" "and" userId tasknr options (map fst taskOptions)) tst
	# chosenTask		= snd (taskOptions!!chosen)
	# chosenTaskName	= fst (taskOptions!!chosen)
	# (a,tst=:{activated=adone,html=ahtml}) 
						= mkParSubTask "andTasks" chosen chosenTask {tst & tasknr = tasknr, activated = True, html = BT []}
	# (milestoneReached,_,tst)	
						= checkAnyTasks "andTasks_mstone" (map snd taskOptions) (0,-1) (False,-1) {tst & html = BT []}
	| not adone			= (alist,{tst & activated = adone || milestoneReached
									, 	html = 	html +|+ 
												BT buttons +-+ 	(BT chosenname +|+ ahtml) +|+ 
												(userId -@: allhtml) 
							})
	# (alist,tst=:{activated=finished,html=allhtml})		
						= checkAllTasks "andTasks" taskOptions (0,chosen) True [] {tst & html = BT []}
	| finished			= (alist,{tst & activated = finished, html = html })
	= (alist,{tst 	& 	activated = finished || milestoneReached
					, html = 	html +|+ 
								BT buttons +-+ 	(BT chosenname +|+ ahtml) +|+ 
								(userId -@: allhtml)
					})

andTasks_mu :: !String ![(Int,Task a)] -> (Task [a]) | iData a
andTasks_mu taskid tasks = newTask "andTasks_mu" (domu_andTasks tasks)
where
	domu_andTasks list = andTasks [(taskid <+++ " " <+++ i, i @:: task) \\ (i,task) <- list] 

// very experimental higher order lazy task stuf

(-!>) infix 4  :: (Task s) (Task a) -> (Task (Maybe s,TCl a)) | iCreateAndPrint s & iCreateAndPrint a
(-!>)  stoptask task =  mkTask "-!>" stop`
where
	stop` tst=:{tasknr,html,options,userId}
	# (val,tst=:{activated = taskdone,html = taskhtml}) = task     {tst & activated = True, html = BT [], tasknr = normalTaskId,options = options}
	# (s,  tst=:{activated = stopped, html = stophtml})	= stoptask {tst & activated = True, html = BT [], tasknr = stopTaskId,  options = options}
	| stopped	= return_V (Just s, TCl (close task))   {tst & html = html, activated = True}
	| taskdone	= return_V (Nothing,TCl (return_V val)) {tst & html = html +|+ taskhtml, activated = True}
	= return_V (Nothing,TCl (return_V val)) {tst & html = html +|+ taskhtml +|+ stophtml, activated = False}
	where
		close t = \tst -> t {tst & tasknr = normalTaskId, options = options, userId = userId} // reset userId because it influences the task id

		stopTaskId 		= [-1,0:tasknr]
		normalTaskId  	= [-1,1:tasknr]

channel  :: String (Task a) -> (Task (TCl a,TCl a)) | iCreateAndPrint a
channel name task =  mkTask "channel" (doSplit name task)

doSplit name task tst=:{tasknr,options,userId}
= return_V (TCl (sender myTask),TCl (receiver myTask)) tst
where
	myTask tst = task {tst & tasknr = [-1:tasknr], options = options, userId = userId}

	sender task tst=:{activated,tasknr}
	| not activated				= (createDefault,tst)
	# (val,tst) 				= task tst
	= (val,{tst & tasknr = tasknr})

	receiver task  tst=:{activated,tasknr,html}
	| not activated			 	= (createDefault,tst)
	# (val,tst=:{activated}) 	= task tst
	| activated	= (val,{tst & html = html, activated = True , tasknr = tasknr})
	= (val,{tst & html = html /*+|+ BT [Txt ("Waiting for completion of "<+++ name)]*/, tasknr = tasknr})

closureTask  :: String (Task a) -> (Task (TCl a)) | iCreateAndPrint a
closureTask name task = mkTask ("closure " +++ name) mkClosure
where
	mkClosure tst=:{tasknr,options,userId}
	# ((TCl sa,ra),tst) 	= doSplit name task tst
	# (_,tst)     				= sa tst
	= (ra, {tst & activated = True})

closureLzTask  :: String (Task a) -> (Task (TCl a)) | iCreateAndPrint a
closureLzTask name task = mkTask ("closure " +++ name) mkClosure
where
	mkClosure tst=:{tasknr,options,userId}
	# ((TCl sa,ra),tst) 	= doSplit name task tst
	# (_,tst)     				= sa tst
	= (ra, {tst & activated = True})

	doSplit name task tst=:{tasknr,options,userId}
	= return_V (TCl (sender myTask),TCl (receiver myTask)) tst
	where
		myTask tst = task {tst & tasknr = [-1:tasknr], options = options, userId = userId}
	
		sender task tst=:{activated,tasknr}
		| not activated				= (createDefault,tst)
		# (requested,tst)			= (sharedMem id) tst  // is this task demanded ?
		| not requested.value		= (createDefault,tst)
		# (val,tst) 				= task tst
		= (val,{tst & tasknr = tasknr})
	
		receiver task  tst=:{activated,tasknr,html}
		| not activated			 	= (createDefault,tst)
		# (requested,tst)			= (sharedMem (\_ -> True)) tst  // this task is now demanded !
		# (val,tst=:{activated}) 	= task tst
		| activated	= (val,{tst & html = html, activated = True , tasknr = tasknr})
		= (val,{tst & html = html /*+|+ BT [Txt ("Waiting for completion of "<+++ name)]*/, tasknr = tasknr})

		sharedStoreId	= iTaskId userId tasknr "Shared_Store"
		sharedMem fun	= LiftHst (mkStoreForm (Init,storageFormId options sharedStoreId False) fun)

waitForTimeTask:: !HtmlTime	-> (Task HtmlTime)
waitForTimeTask time = mkTask "waitForTimeTask" waitForTimeTask`
where
	waitForTimeTask` tst=:{tasknr,userId,hst}
	# taskId				= iTaskId userId tasknr "Time_"
	# (stime,hst) 			= mkStoreForm (Init,storageFormId tst.options taskId time) id hst  			// remember time
	# ((currtime,_),hst)	= getTimeAndDate hst
	| currtime < stime.value= (stime.value,{tst & activated = False,hst = hst})
	= (currtime - stime.value,{tst & hst = hst})

waitForTimerTask:: !HtmlTime	-> (Task HtmlTime)
waitForTimerTask time  = waitForTimerTask`
where
	waitForTimerTask` tst=:{hst}
	# ((ctime,_),hst)	= getTimeAndDate hst
	= waitForTimeTask (ctime + time) {tst & hst = hst}

waitForDateTask:: !HtmlDate	-> (Task HtmlDate)
waitForDateTask date = mkTask "waitForDateTask" waitForDateTask`
where
	waitForDateTask` tst=:{tasknr,userId,hst}
	# taskId				= iTaskId userId tasknr "Date_"
	# (taskdone,hst) 		= mkStoreForm (Init,storageFormId tst.options taskId (False,date)) id hst  			// remember date
	# ((_,currdate),hst) 	= getTimeAndDate hst
	| currdate < date		= (date,{tst & activated = False, hst = hst})
	= (date,{tst & hst = hst})

// functions on TSt

taskId :: TSt -> (Int,TSt)
taskId tst=:{userId} = (userId,tst)

userId :: TSt -> (Int,TSt)
userId tst=:{staticInfo} = (staticInfo.currentUserId,tst)

addHtml :: [BodyTag] TSt -> TSt
addHtml bodytag  tst=:{activated, html}  
| not activated = tst						// not active, return default value
= {tst & html = html +|+ BT bodytag}		// active, so perform task or get its result

// lifters to iTask state
(*>>) infix 4 :: (TSt -> (a,TSt)) (a -> Task b) -> (Task b)
(*>>) ftst b = doit
where
	doit tst
	# (a,tst) = ftst tst
	= b a tst

(*@>) infix 4 :: (TSt -> TSt) (Task a) -> Task a
(*@>) ftst b = doit
where
	doit tst
	# tst = ftst tst
	= b tst

appIData :: (IDataFun a) -> (Task a) | iData a 
appIData idatafun = \tst -> mkTask "appIData" (appIData` idatafun) tst
where
	appIData` idata tst=:{tasknr,html,hst}
	# (idata,hst) 										= idatafun hst
	# (_,{tasknr,activated,html=ahtml,hst}) 			= internEditSTask "appIDataDone" "Done" Void {tst & activated = True, html = BT [],hst = hst}	
	= (idata.value,{tst & tasknr = tasknr,activated = activated, html = html +|+ 
															(if activated (BT idata.form) (BT idata.form +|+ ahtml)), hst = hst})

appIData2 :: (String *HSt -> *(Form a,*HSt)) -> (Task a) | iData a 
appIData2 idatafun = \tst -> mkTask "appIData" (appIData` idatafun) tst
where
	appIData` idata tst=:{tasknr,html,hst,userId}
	# taskId											= iTaskId userId tasknr "iData"
	# (idata,hst) 										= idatafun taskId hst
	# (_,{tasknr,activated,html=ahtml,hst}) 			= internEditSTask "appIDataDone" "Done" Void {tst & activated = True, html = BT [],hst = hst}	
	= (idata.value,{tst & tasknr = tasknr,activated = activated, html = html +|+ 
															(if activated (BT idata.form) (BT idata.form +|+ ahtml)), hst = hst})

appHSt2 :: !String (HSt -> (a,HSt)) -> (Task a) | iData a
appHSt2 name fun = mkTask name doit
where
	doit tst=:{hst}
	# (value,hst)		= fun hst
	= (value,{tst & hst = hst, activated = True})													// task is now completed, handle as previously

appHSt :: !String (HSt -> (a,HSt)) -> (Task a) | iData a
appHSt name fun = mkTask name doit 
where
	doit tst=:{tasknr,hst,userId,options}
	# taskId			= iTaskId userId tasknr name 
	# (store,hst) 		= mkStoreForm (Init,storageFormId options taskId (False,createDefault )) id hst  			
	# (done,svalue)		= store.value
	| done				= (svalue,{tst & hst = hst, activated = True})		// if task has completed, don't do it again
	# (fvalue,hst)		= fun hst
	# (store,hst)  		= mkStoreForm (Init,storageFormId options taskId (False,createDefault)) (\_ -> (True,fvalue)) hst 	// remember task status for next time
	# (_,nvalue)		= store.value
	= (nvalue,{tst & activated = True, hst = hst})							// task is now completed, handle as previously
	
Once :: (Task a) -> (Task a) | iData a
Once fun = mkTask "Once" doit
where
	doit tst=:{activated,html,tasknr,hst,userId,options}
	# taskId			= iTaskId userId tasknr "Once_"
	# (store,hst) 		= mkStoreForm (Init,storageFormId options taskId (False,createDefault)) id hst  			
	# (done,value)		= store.value
	| done 				= (value,{tst & hst = hst})													// if task has completed, don't do it again
	# (value,tst=:{hst})= fun {tst & hst = hst}
	# (store,hst) 		= mkStoreForm (Init,storageFormId options taskId (False,createDefault)) (\_ -> (True,value)) hst 	// remember task status for next time
	# (done,value)		= store.value
	= (value,{tst & activated = done, hst = hst})													// task is now completed, handle as previously

// Notice that when combining tasks the context restrictions on certain types will get stronger
// It can vary from : no restriction on a -> iTrace a -> iData a
// In most cases the user can simply ask Clean to derive the corresponding generic functions
// For the type Task this will not work since it is a higher order type
// Therefore when yielding a task as result of a task,
// the type Task need to be wrapped into TCl for which the generic functions are defined below
// Tested for iTrace, will not work for iData

//:: TCl a 		= TCl (Task a)			

import DrupBasic

write{|TCl|} write_a (TCl task) wst
	= write{|*|} (serializeThread task) wst

read {|TCl|} read_a  wst 
	# (Read str i file) = read{|*|} wst
	= Read (TCl  (deserializethread str)) i file

gPrint{|TCl|} ga (TCl task) ps = ps <<- serializeThread task

gParse{|TCl|} ga expr
# mbstring = parseString expr
| isNothing mbstring = Nothing
= Just (TCl (fst(copy_from_string {s` \\ s` <-: fromJust mbstring})))
where
	parseString :: Expr -> Maybe String
	parseString expr = gParse{|*|} expr

gUpd{|TCl|} gc (UpdSearch _ 0)	  	 c		= (UpdDone, c)								
gUpd{|TCl|} gc (UpdSearch val cnt)  c		= (UpdSearch val (cnt - 2),c)						
gUpd{|TCl|} gc (UpdCreate l)        _		
# (mode,default)	= gc (UpdCreate l) undef
= (UpdCreate l, TCl (\tst -> (default,tst)))			
gUpd{|TCl|} gc mode                 b		= (mode, b)										

gForm{|TCl|} gfa (init,formid) hst
= ({value=formid.ival,changed=False,form=[]},hst)


// *** utility section ***

// simple html code generation utilities

showUser nr
= CTxt Yellow ("User " <+++ nr)

CTxt color message
= Font [Fnt_Color (`Colorname color)] [B [] (toString message)]

BCTxt color message
= Font [Fnt_Color (`Colorname color)] [Big [] (toString message)]

mkDiv :: String [BodyTag] -> [BodyTag]
mkDiv id bodytag = [normaldiv]
where
	normaldiv = Div [`Div_Std [Std_Id id, Std_Class	"thread"]] bodytag

// Printing and tracing stuf...


InsertTrace :: !Bool !TaskNr !Int String !String ![Trace] -> [Trace]
InsertTrace finished idx who taskname val trace = InsertTrace` ridx who val trace
where
	InsertTrace` :: !TaskNr !Int !String ![Trace] -> [Trace]
	InsertTrace` [i] 	who str traces
	| i < 0					= abort ("negative task numbers:" <+++ showTaskNr idx <+++ "," <+++ who <+++ "," <+++ taskname)
	# (Trace _ itraces)		= select i traces
	= updateAt` i (Trace (Just (finished,(who,show,taskname,str))) itraces)  traces
	InsertTrace` [i:is] who str traces
	| i < 0					= abort ("negative task numbers:" <+++ showTaskNr idx <+++ "," <+++ who <+++ "," <+++ taskname)
	# (Trace ni itraces)	= select i traces
	# nistraces				= InsertTrace` is who str itraces
	= updateAt` i (Trace ni nistraces) traces

	select :: !Int ![Trace] -> Trace
	select i list
	| i < length list = list!!i 
	=  Trace Nothing []

	show 	= idx //showTaskNr idx
	ridx	= reverse idx

	updateAt`:: !Int !Trace ![Trace] -> [Trace]
	updateAt` n x list
	| n < 0		= abort "negative numbers not allowed"
	= updateAt` n x list
	where
		updateAt`:: !Int !Trace ![Trace] -> [Trace]
		updateAt` 0 x []		= [x]
		updateAt` 0 x [y:ys]	= [x:ys]
		updateAt` n x []		= [Trace Nothing []	: updateAt` (n-1) x []]
		updateAt` n x [y:ys]	= [y      			: updateAt` (n-1) x ys]

printTrace2 Nothing 	= EmptyBody
printTrace2 (Just a)  	= BodyTag [Br, Br, B [] "Task Tree:", Br, STable emptyBackground (print False a)]
where
	print _ []		= []
	print b trace	= [pr b x ++ [STable emptyBackground (print (isDone x||b) xs)]\\ (Trace x xs) <- trace] 

	pr _ Nothing 			= []
	pr dprev (Just (dtask,(w,i,tn,s)))	
	| dprev && (not dtask)					= pr False Nothing	// subtask not important anymore (assume no milestone tasks)
	| not dtask	&& tn%(0,4) == "Ajax "		= showTask cellattr1b White Navy Aqua  Silver (w,i,tn,s)
	| not dtask	&& tn%(0,6) == "Client "	= showTask cellattr1b White Navy Aqua  Silver (w,i,tn,s)
	| not dtask								= showTask cellattr1b White Navy Maroon Silver (w,i,tn,s)
	= showTask cellattr1a White Yellow Red White (w,i,tn,s)
	
	showTask2 attr1 c1 c2 c3 c4 (w,i,tn,s)
	= [Table doneBackground 	[ Tr [] [Td attr1 [font c1 (toString (last (reverse i)))],	Td cellattr2 [font c2 tn]]
								, Tr [] [Td attr1 [font c3 (toString w)], 					Td cellattr2 [font c4 s]]
								]
	  ,Br]

	showTask att c1 c2 c3 c4 (w,i,tn,s)
	= [STable doneBackground 	
		[ [font c1 (toString w),font c2 ("T" <+++ showTaskNr i)]
		, [EmptyBody, font c3 tn]
		, [EmptyBody, font c4 s]
		]
		]
	isDone Nothing = False
	isDone (Just (b,(w,i,tn,s))) = b


	doneBackground = 	[ Tbl_CellPadding (Pixels 1), Tbl_CellSpacing (Pixels 0), cellwidth
						, Tbl_Rules Rul_None, Tbl_Frame Frm_Border 
						]
	doneBackground2 = 	[ Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0), cellwidth
						]
	emptyBackground = 	[Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)]
	cellattr1a		=	[Td_Bgcolor (`Colorname Green),  Td_Width (Pixels 10), Td_VAlign Alo_Absmiddle]
	cellattr1b		=	[Td_Bgcolor (`Colorname Silver), Td_Width (Pixels 10), Td_VAlign Alo_Absmiddle]
	cellattr2		=	[Td_VAlign Alo_Top]
	cellwidth		= 	Tbl_Width (Pixels 130)

	font color message
	= Font [Fnt_Color (`Colorname color), Fnt_Size -1] [B [] message]


// experimental and very very very dangerous !!!

// The following experimental fucntions are defined to support "Ajax technologie" and Client site evaluation of i-Tasks.
// To make this possible, a part of the iTask task tree must be assigened to be a thread such that it can be evaluated as a stand-alone i-Task.
// Currently, the programmer has to decide which iTask should become a thread.
// For each event (iData triplet), the system will search for the thread to handle it.
// If a thread task is finished, the parent thread task is activated, and so on.

serializeThread :: !.(Task .a) -> .String
serializeThread task = copy_to_string task

//deserializethread :: String -> Task a
deserializethread :: .String -> .(Task .a)
deserializethread thread = fst (copy_from_string {c \\ c <-: thread} )	

serializeThreadClient :: !(Task a) -> String
serializeThreadClient task =  IF_Ajax (IF_OnClient (graph_to_string_with_descriptors task) "") ""


// mkTaskThread creates a thread for an iTask

mkTaskThread :: !SubPage !(Task a) -> Task a 	| iData a										// execute a thread
mkTaskThread UseAjax taska = IF_Ajax 															// only if Ajax & threads enabled
									(newTask ("Ajax Thread") (mkTaskThread2 False taska))
									taska 

mkTaskThread OnClient taska = IF_Ajax 															// only if Ajax & threads enabled
									(IF_OnClient
										(newTask "Client Thread" (mkTaskThread2 True  taska))	// evaluate on client if possible
										(newTask "Ajax NoClient Thread" (mkTaskThread2 False taska)) 	// otherwise use Ajax
									)
									taska 

mkTaskThread2 :: !Bool !(Task a) -> Task a 										// execute a thread
mkTaskThread2 onclient task = evalTask
where
	evalTask tst=:{tasknr,activated,options,userId}									// thread - task is not yet finished
	# (mbthread,tst)	= findThreadInTable tasknr tst								// look if there is an entry for this task
	| isNothing mbthread															// not yet, insert new entry		
		# options = {options & tasklife = if onclient Client options.tasklife}
		# tst = insertNewThread 	{ thrTaskNr 		= tasknr
									, thrUserId 		= userId
									, thrOptions 		= options
									, thrCallback 		= serializeThread task
									, thrCallbackClient = serializeThreadClient task 
									, thrDone			= False
									} tst 
		= evalTask tst																// try it again, entry point should be there
	# (_,thread)		= fromJust mbthread											// entry point found
	= evalTaskThread thread tst														// and evaluate it

insertNewThread :: !TaskThread *TSt -> *TSt										// insert new thread in table
insertNewThread thread tst		
# (table,tst)	= ThreadTableStorage id tst										// read thread table
# (_,tst) 		= ThreadTableStorage (\_ -> [thread:table]) tst 				// insert the new thread
= tst

evalTaskThread :: !TaskThread -> Task a 											// execute the thread !!!!
evalTaskThread entry=:{thrTaskNr,thrUserId,thrOptions,thrCallback,thrDone} = evalTaskThread` 
where
	evalTaskThread` tst=:{tasknr,options,userId,html}									
	# (a,tst=:{activated,html=nhtml}) 	= (deserializethread thrCallback) {tst & tasknr = thrTaskNr, options = thrOptions, userId = thrUserId,html = BT []} 
	| not thrDone
		| activated																		// thread is finished, delete the entry...
			# tst =  deleteThreads thrTaskNr {tst & html = html +|+ nhtml}				// remove thread from administration
	//		# tst =  deleteSubTasks thrTaskNr {tst & html = html +|+ nhtml}				// thread and subtasks finished
			# tst = insertNewThread 	{ thrTaskNr 		= thrTaskNr					// administrate resulting value
										, thrUserId 		= thrUserId
										, thrOptions 		= thrOptions
										, thrCallback 		= serializeThread (\tst -> (a,{tst & activated = True}))
										, thrCallbackClient = serializeThreadClient (\tst -> (a,tst))
										, thrDone 			= True
										} tst 
			= (a,{tst & tasknr = tasknr, options = options, userId = userId})			// remove entry from table
		= (a,{tst & tasknr = tasknr, options = options, userId = userId,html = html +|+ DivCode (showTaskNr thrTaskNr) nhtml})
	= (a,{tst & tasknr = tasknr, options = options, userId = userId,html = html +|+ DivCode (showTaskNr thrTaskNr) nhtml})



startAjaxApplication :: !Int !VersionInfo !(Task a) !*TSt -> ((Maybe a,!Int,TaskNr,!String,!Bool,![TaskNr]),*TSt) 		// determines which threads to execute and calls them..
startAjaxApplication thisUser versioninfo taska tst=:{tasknr,options,html,trace,userId}
# (mbevent,tst)			= getTripletTaskNrs tst										// see if there are any events, i.e. triplets received
| isNothing mbevent 																// no events
	# (a,tst)	= taska tst															// evaluate main application from scratch
	= ((Just a,defaultUser,tasknr,"Page",True,[tasknr]), tst)

# event					= fromJust mbevent											// event found
# (table,tst)			= ThreadTableStorage id tst									// read thread table
| isNil table																		// events, but no threads, evaluate main application from scratch
	# (a,tst=:{activated}) 	= taska tst												// evaluate main application from scratch
	= ((Just a,defaultUser,event,if activated "iData application ended" "No Thread(s)",True,[tasknr]), {tst & activated = activated})

# (mbthread,tst)		= findParentThread event tst								// look for thread to evaluate
| isNil mbthread																	// no thread can be found, happens e.g. when one switches from tasks
	# (a,tst) 	= taska tst															// evaluate main application from scratch
	= ((Just a,defaultUser,event,"Page, no thread",True,[tasknr]), tst)

# thread 				= hd mbthread												// thread found
| isMember thread.thrTaskNr versioninfo.deletedThreads								// thread has been deleted is some past, version conflict
	= ((Nothing,defaultUser,event,"Task does not exist anymore, please refresh",True,[tasknr]), tst)

| versioninfo.newThread																// newthread added by someone
	# (a,tst)	= taska tst															// evaluate main application from scratch
	= ((Just a,defaultUser,event,"Page, new task has been added",True,[tasknr]), tst)

| not (isNil versioninfo.deletedThreads) 											// some thread has been deleted										
	# (a,tst) 	= taska tst															// administartion not up to date, evaluate main application from scratch
	= ((Just a,defaultUser,event,"Page, another task has been deleted",True,[tasknr]), tst)

| thread.thrUserId <> thisUser														// updating becomes too complicated
	# (a,tst) 			= taska tst													// evaluate main application from scratch
	= ((Just a,defaultUser,event,"Thread of " <+++ thread.thrUserId,True,[tasknr]), tst)

# (_,tst=:{activated}) 	= evalTaskThread thread {tst & html = BT []}				// yes, *finally*, we heave a thread ...
| not activated																		// thread not yet finished
	= ((Nothing,thisUser,event,"Thread",thread.thrOptions.tasklife <> Client,[thread.thrTaskNr]),tst)					// no further evaluation, aks user for more input

# (mbthread,tst)		= findParentThread (tl thread.thrTaskNr) tst						// look for thread to evaluate
= doParent mbthread taska event [thread.thrTaskNr] {tst & html = BT [], options = options}				// more to evaluate, call thread one level higher
where
	doParent [] taska event accu tst												// no more parents of current event, do main task
	# (a,tst=:{activated}) 	= taska {tst & html = BT []}							// start main task
	# message				= if activated "iData application ended" "iData Root"
	= ((Just a,defaultUser,event,message, True,[tasknr:accu]), {tst & activated = activated})

	doParent [parent:next] taska event accu tst										// do parent of current thread
	| parent.thrUserId <> thisUser													// updating becomes too complicated
		# (a,tst) 			= taska tst												// evaluate main application from scratch
		= ((Just a,defaultUser,event,"Parent of " <+++ parent.thrUserId,True,[tasknr]), tst)

	# (_,tst=:{activated}) 	= evalTaskThread parent {tst & html = BT []}			// start parent
	| not activated																	// parent thread not yet finished
		= ((Nothing,thisUser,event, "Thread(s)", parent.thrOptions.tasklife <> Client,[parent.thrTaskNr:accu]),tst)		// no further evaluation, aks user for more input
	# (mbthread,tst)		= findParentThread (tl parent.thrTaskNr) tst			// look for thread to evaluate
	= doParent mbthread taska event [parent.thrTaskNr:accu] {tst & options = options}// continue with grand parent ...
	
// Thread Table Manipulation functions


// Currently for testing an unordered table is used, should become a ordered tree someday...

showThreadTable :: *TSt -> ([BodyTag],*TSt)	// watch it: the actual threadnumber stored is one level deaper, so [-1:nr] instead of nr !!
showThreadTable tst
# (table,tst)	= ThreadTableStorage id tst											// read thread table
# body			= [Br, B [] "Threaded Tasks: "] ++ foldl (++) [] [[Br, showWhere entry.thrOptions.tasklife,Txt (showThreadNr entry.thrTaskNr)] \\ entry <- table]
= (body,tst)
where
	showWhere lifespan
	| lifespan == Client	= Txt "On Client: " 
	| otherwise				= Txt "On Server: " 

ThreadTableStorage :: (ThreadTable -> ThreadTable) -> (Task ThreadTable)			// used to store Tasknr of callbackfunctions / threads
ThreadTableStorage fun = handleTable
where
	handleTable tst=:{options,staticInfo}  
	# (table,tst) = LiftHst (mkStoreForm (Init,storageFormId 
						{ tasklife 		= staticInfo.threadTableLoc
						, taskstorage 	= PlainString 
						, taskmode		= NoForm
						, gc			= NoCollect} storageId_ThTa []) fun) tst
	= (table.value,tst)

	storageId_ThTa = ThisExe +++ "-ThreadTable"

findThreadInTable :: !TaskNr *TSt -> *(Maybe (!Int,!TaskThread),*TSt)				// find thread that belongs to given tasknr
findThreadInTable tasknr tst
# (table,tst)	= ThreadTableStorage id tst											// read thread table
# pos			= lookupThread tasknr 0 table										// look if there is an entry for this task
| pos < 0		= (Nothing, tst)
= (Just (pos,table!!pos),tst) 
where
	lookupThread :: !TaskNr !Int !ThreadTable -> Int
	lookupThread tableKey n []			
		= -1																		// no, cannot find thread
	lookupThread tasknrToFind n [entry:next]
		| showTaskNr tasknrToFind == showTaskNr entry.thrTaskNr	= n					// yes, thread is administrated
		= lookupThread tasknrToFind (inc n) next

deleteThreads :: !TaskNr !*TSt -> *TSt
deleteThreads tasknr tst															// delete a thread and all its children
# (mbthread,tst)		= findThreadInTable tasknr tst								// find the thread entry in the table
# mytasknr				= reverse tasknr
| isNothing mbthread	= deleteChildren mytasknr tst								// no entry, but delete children
# (pos,_)				= fromJust mbthread
# (_,tst)				= ThreadTableStorage (\table -> removeAt pos table) tst 	// remove entry
= deleteChildren mytasknr tst														// and all children
where
	deleteChildren mytasknr tst=:{staticInfo}
	# (table,tst)	 		= ThreadTableStorage id tst								// read thread table
	# allChildsPos			= [pos \\ entry <- table & pos <- [0..] | isChild mytasknr entry.thrTaskNr ]
	| isNil allChildsPos	= tst
	# otherUsersThreads		= [ ((table!!entry).thrUserId,(table!!entry).thrTaskNr) \\ entry <- allChildsPos | (table!!entry).thrUserId <> staticInfo.currentUserId]
	# tst					= administrateDeletedThreads otherUsersThreads tst 
	# table					= deleteChilds (reverse (sort allChildsPos)) table		// delete highest entries first !
	# (table,tst)	 		= ThreadTableStorage (\_ -> table) tst					// read thread table
	= tst

	deleteChilds [] table 			= table
	deleteChilds [pos:next] table 	= deleteChilds next (removeAt pos table)

	isChild mytasknr mbchild = take (length mytasknr) (reverse mbchild) == mytasknr

administrateDeletedThreads [] tst = tst
administrateDeletedThreads [(user,tasknr):users] tst=:{hst}
# (_,hst)	= addPUserDeletedThread user tasknr hst				// administrate deleted thread in user administration
= administrateDeletedThreads users {tst & hst = hst}				// such that they are forced to recalculate the whole page

/*
forceNewPage users tst=:{hst}
# (appVersion,hst)	= setAppversion id hst						// read out current version number of application
# hst				= forceUser users appVersion.value hst		// and store it in the version number of the users
= {tst & hst = hst}												// such that they are forced to recalculate the whole page
where
	forceUser []  _ hst = hst
	forceUser [i:is] versionnr hst
	# (_,hst)	= setPUserNr i (\_ -> versionnr) hst		// and store it in the version number of the user
	= forceUser is versionnr hst	
*/

getTripletTaskNrs :: !*TSt -> *(Maybe TaskNr,*TSt)									// get list of tasknr belonging to events received
getTripletTaskNrs tst=:{hst = hst=:{states}}
# (triplets,states) = getAllTriplets states
= (lowestTaskNr [mkTasknr (getDigits s) \\ ((s,_,_),_) <- triplets | s%(0,5) == "iTask_"],{tst & hst = {hst & states = states}})
where
	getDigits s = takeWhile ((<>) '-') (stl (dropWhile ((<>) '_') (mkList s)))

	mkTasknr list = reverse (map digitToInt [c \\ c <- list | isDigit c])

	lowestTaskNr [] 	= Nothing
	lowestTaskNr [x:xs] = Just (lowest x xs)										// lowest number gives highest position in tree

	lowest :: TaskNr [TaskNr] -> TaskNr
	lowest x [] 	= x
	lowest x [y:ys]
	| x < y = lowest x ys
	= lowest y ys

findParentThread ::  !TaskNr !*TSt -> *([TaskThread],*TSt)							// finds parent thread closest to given set of task numbers
findParentThread tasknr tst
# (table,tst)		= ThreadTableStorage id tst										// read thread table
| isNil table		= ([], tst)														// nothing in table, no parents
| length tasknr <= 1 = ([], tst)													// no tasks left up
# revtasknr			= reverse (tl tasknr)											// not relevant
# entries 			= filter (\entry -> revtasknr%(0,length entry.thrTaskNr - 2) == (reverse (tl entry.thrTaskNr))) table			// finds thread closest to this one
| isNil entries		= ([], tst)
= (sortBy compare entries,tst)
where
	compare :: !TaskThread !TaskThread -> Bool
	compare x y = length x.thrTaskNr > length y.thrTaskNr
	
