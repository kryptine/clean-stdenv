implementation module htmlTask

import StdEnv, StdHtml

derive gForm 	[], Void
derive gUpd 	[], Void
derive gParse 	Void
derive gPrint 	Void
derive gerda 	Void

derive gForm Trace
derive gForm Maybe

import dynamic_string, EncodeDecode

:: *TSt 		=	{ tasknr 		:: ![Int]			// for generating unique form-id's
					, activated		:: !Bool   			// if true activate task, if set as result task completed	
					, myId			:: !Int				// id of user to which task is assigned
					, userId		:: !Int				// id of application user 
					, html			:: !HtmlTree		// accumulator for html code
					, storageInfo	:: !Storage			// iData lifespan and storage format
					, trace			:: !Maybe [Trace]	// for displaying task trace
					, hst			:: !HSt				// iData state
					}

:: HtmlTree		=	BT [BodyTag]						// simple code
				|	(@@:) infix  0 (Int,String) HtmlTree// code with id of user attached to it
				|	(+-+) infixl 1 HtmlTree HtmlTree	// code to be placed next to each other				
				|	(+|+) infixl 1 HtmlTree HtmlTree	// code to be placed below each other				

:: Storage		=	{ tasklife		:: !Lifespan		
					, taskstorage	:: !StorageFormat
					, taskmode		:: !Mode
					}

:: Trace		=	Trace TraceInfo [Trace]				// traceinfo with possibly subprocess

:: TraceInfo	:== Maybe (Int,String,String)			// Who did it, task nr, value produced

// setting global iData options for tasks

instance setTaskAttr Lifespan
where setTaskAttr lifespan tst = {tst & storageInfo.tasklife = lifespan}

instance setTaskAttr StorageFormat
where setTaskAttr storageformat tst = {tst & storageInfo.taskstorage = storageformat}

instance setTaskAttr Mode
where setTaskAttr mode tst = {tst & storageInfo.taskmode = mode}

// wrappers

multiUserTask 	:: !Int!(Task a)  	!*HSt -> (Html,*HSt) 			| iData a 
multiUserTask nusers task  hst 
# (idform,hst) 	= FuncMenu (Init,nFormId "User_Selected" 
						(0,[("User " +++ toString i,\_ -> i) \\ i<-[0..nusers - 1] ])) hst
# currentWorker	= snd idform.value
# (_,html,hst) 	= startTask currentWorker task hst
= mkHtml "mtest" (idform.form ++ html) hst

startTask 		:: !Int !(Task a) 		!*HSt -> (a,[BodyTag],!*HSt) 	| iData a 
startTask thisUser taska hst
# userVersionNr			= "User" <+++ thisUser <+++ "_VersionPNr"
# sessionVersionNr		= "User" <+++ thisUser <+++ "_VersionSNr" 
# traceId				= "User" <+++ thisUser <+++ "_Trace" 
# (pversion,hst)	 	= mkStoreForm (Init, pFormId userVersionNr 0) id hst
# (refresh,hst) 		= simpleButton userVersionNr "Refresh" id hst
# (traceAsked,hst) 		= simpleButton traceId "ShowTrace" (\_ -> True) hst
# doTrace				= traceAsked.value False
# (sversion,hst)	 	= mkStoreForm (Init, nFormId sessionVersionNr pversion.value) (if refresh.changed (\_ -> pversion.value) id) hst
| sversion.value < pversion.value	= (createDefault,  refresh.form ++ [Br,Br, Hr [],Br] <|.|>
														[Font [Fnt_Color (`Colorname Yellow)]
													   [B [] "Sorry, cannot apply command.",Br, 
													    B [] "Your page is not up-to date!",Br]],hst)
# (a,{html,hst,trace}) = taska 	{ tasknr	= [0]
							, activated = True
							, userId	= thisUser 
							, myId		= defaultUser 
							, html 		= BT []
							, trace		= if doTrace (Just []) Nothing
							, hst 		= hst
							, storageInfo = {tasklife = Session, taskstorage = PlainString, taskmode = Edit }}
# (pversion,hst)	 	= mkStoreForm (Init, pFormId userVersionNr 0) inc hst
# (sversion,hst)	 	= mkStoreForm (Init, nFormId sessionVersionNr pversion.value) inc hst
# (selbuts,seltask,hst)	= Filter thisUser defaultUser ((defaultUser,"Main") @@: html) hst
= 	(a,	refresh.form ++ traceAsked.form ++
		[Br,Br, Hr [],Br] ++ 
		if doTrace
			[ printTrace trace ]
//			[ toHtml trace ]
			[ mkSTable2 [ [yellowUser thisUser,EmptyBody,EmptyBody]
						, [mkColForm selbuts, EmptyBody, BodyTag seltask]
						]
			]
	,hst)
where
	defaultUser	= 0

	mkSTable2 :: [[BodyTag]] -> BodyTag
	mkSTable2 table
	= Table []	(mktable table)
	where
		mktable table 	= [Tr [] (mkrow rows) \\ rows <- table]	
		mkrow rows 		= [Td [Td_VAlign Alo_Top] [row] \\ row <- rows] 
	
	Filter id user tree hst
	# (_,accu) 		= Collect id user [] tree
	| isNil accu	= ([],[],hst)
	# (names,tasks) = unzip accu
	# (fun,hst)		= ListFuncBut (Init,sFormId ("User" <+++ id <+++ "_Task" <+++ length accu) [(LButton defpixel name,dotask i) \\ name <- names & i <- [0..]]) hst
	# (selected,hst)= mkStoreForm (Init,sFormId ("User" <+++ id <+++ "_Task" <+++ length accu) 0) fun.value hst 
	= (fun.form,tasks!!if (selected.value >= length accu) 0 selected.value,hst)
	where
		dotask i _ = i
	
	Collect id user accu ((nuser,taskname) @@: tree)
	# (myhtml,accu)	= Collect id nuser accu tree
	| id == nuser && not (isNil myhtml)
					= ([],[(taskname,myhtml):accu])
	| otherwise		= ([],accu)
	Collect id user accu (BT bdtg)
					= (bdtg,accu)
	Collect id user accu  (tree1 +|+ tree2)
	# (lhtml,accu)	= Collect id user accu tree1
	# (rhtml,accu)	= Collect id user accu tree2
	= (lhtml <|.|> rhtml,accu)
	Collect id user accu  (tree1 +-+ tree2)
	# (lhtml,accu)	= Collect id user accu tree1
	# (rhtml,accu)	= Collect id user [] tree2
	= ([lhtml <=> rhtml],accu)

	isNil [] = True
	isNil _ = False

singleUserTask 	:: !(Task a) 		   	!*HSt -> (Html,*HSt) 			| iData a 
singleUserTask task hst 
# (_,html,hst) = startTask 0 task hst
= mkHtml "stest" html hst

// combinators and functions on Tasks
	
mkTask :: (*TSt -> *(a,*TSt)) -> (Task a) | iData a
mkTask mytask = \tst -> mkTask` tst
where
	mkTask` tst=:{activated,html,myId}		
	# tst			 						= incTask tst			// every task should first increment its tasknumber
	= mkTaskNoInc mytask tst

mkTaskNoInc :: (*TSt -> *(a,*TSt)) -> (Task a) | iData a			// same as mkTask, but no increment of task nr
mkTaskNoInc mytask = \tst -> mkTask` tst
where
	mkTask` tst=:{activated,html,tasknr,myId}		
	| not activated							= (createDefault,tst)	// not active, return default value
	# (val,tst=:{activated,trace})			=  mytask tst			// active, so perform task or get its result
	| not activated	|| isNothing trace		= (val,tst)				// no trace, just return value
	= (val,{tst & trace 					= Just (InsertTrace tasknr myId (printToString val) (fromJust trace))}) // adjust trace

repeatTask :: (Task a) -> Task a | iData a
repeatTask task
= task #>> mkTaskNoInc (repeatTask task)

// assigning tasks to users

(@:) infix 4 :: !(!Int,!String) (Task a)	-> (Task a)			| iData a
(@:) (userId,taskname) taska = \tst=:{myId} -> mkTask (assignTask` myId) {tst & myId = userId}
where
	assignTask` myId tst=:{html=ohtml}
	# (a,tst=:{html=nhtml,activated})	= taska {tst & html = BT [],myId = userId}		// activate task of indicated user
	| activated 						= (a,{tst & activated = True
												  ,	myId = myId							// work is done						
												  ,	html = ohtml +|+ 					// clear screen
													BT [yellowUser userId, Txt " finished task ",yellow taskname, Br,Br] +|+
													((userId,taskname) @@: nhtml)})	
	= (a,{tst & myId = myId																// restore user Id
			  , html = 	ohtml +|+ 
						BT [Br, Txt ("Waiting for Task "), yellow taskname, Txt " from ", yellowUser userId,Br] +|+ 
						((userId,taskname) @@: BT [Txt "Task ",yellow taskname, Txt " requested by ", yellowUser myId,Br,Br] +|+ nhtml)})				// combine html code, filter later					

(@::) infix 4 :: !Int (Task a)	-> (Task a)			| iData a
(@::) userId taska = \tst=:{myId} -> mkTask (assignTask` myId) {tst & myId = userId}
where
	assignTask` myId tst=:{html}
	# (a,tst=:{html=nhtml,activated})	= taska {tst & html = BT [],myId = userId}		// activate task of indicated user
	| activated 						= (a,{tst & myId = myId							// work is done						
												  ,	html = html})	
	= (a,{tst & myId = myId																// restore user Id
			  , html = 	html +|+  ((userId,"Task " <+++ myId) @@: nhtml)})				// combine html code, filter later					

// sequential tasks

STask :: String a -> (Task a) | iData a 
STask prompt a = \tst -> mkTask (STask` a) tst
where
	STask` a tst=:{tasknr,html,hst}
	# tasknr			= showTaskNr tasknr
	# taskId			= itaskId tasknr "_Seq"
	# editId			= itaskId tasknr "_Val"
	# buttonId			= itaskId tasknr "_But"
	# (taskdone,hst) 	= mkStoreForm (Init,cFormId tst.storageInfo taskId False) id hst  			// remember if the task has been done
	| taskdone.value																				// test if task has completed
		# (editor,hst) 	= (mkEditForm  (Init,cdFormId tst.storageInfo editId a <@ Display) hst)		// yes, read out current value, make editor passive
		= (editor.value,{tst & activated = True, html = html +|+ BT editor.form, hst = hst})		// return result task
	# (editor,hst) 		= mkEditForm  (Init,cFormId tst.storageInfo editId a) hst					// no, read out current value from active editor
	# (finbut,hst)  	= simpleButton buttonId prompt (\_ -> True) hst								// add button for marking task as done
	# (taskdone,hst) 	= mkStoreForm (Init,cFormId tst.storageInfo taskId False) finbut.value hst 	// remember task status for next time
	| taskdone.value	= STask` a {tst & hst = hst}												// task is now completed, handle as previously
	= (a,{tst & activated = taskdone.value, html = html +|+ BT (editor.form ++ finbut.form), hst = hst})

STask_button 		:: String (Task a) 			-> (Task a) 	| iData a
STask_button s task = CTask_button [(s,task)]

STasks :: [(String,Task a)] -> (Task [a])| iData a 
STasks options = \tst -> mkTaskNoInc (doSandTasks` options []) tst
where
	doSandTasks` [] accu tst 		= returnV (reverse accu) tst
	doSandTasks` [(txt,task):ts] accu tst=:{tasknr,html,hst} 
	# (a,{tasknr,activated=adone,html=ahtml,trace=trace,hst}) 
									= task {tst & activated = True, html = BT []}
	| not adone						= (reverse accu,{tst & trace = trace,tasknr = tasknr,activated = adone, html = html +|+ BT [Txt ("Task: " +++ txt),Br] +|+ ahtml,hst = hst})
	= doSandTasks` ts [a:accu] {tst & trace = trace, tasknr = tasknr,activated = adone, html = html +|+ ahtml,hst = hst}

// Choose one or more tasks out of a collection

CTask_button :: [(String,Task a)] -> (Task a) | iData a
CTask_button options = \tst -> mkTask (doCTask` options) tst
where
	doCTask` [] tst					= returnV createDefault tst				
	doCTask` options tst=:{tasknr,html,hst}									// choose one subtask out of the list
	# taskId						= itaskId (showTaskNr tasknr) ("_Or0." <+++ length options)
	# buttonId						= itaskId (showTaskNr tasknr) "_But"
	# (chosen,hst)					= mkStoreForm  (Init,cFormId tst.storageInfo taskId -1) id hst
	| chosen.value == -1
		# (choice,hst)				= TableFuncBut (Init,cFormId tst.storageInfo buttonId [[(but txt,\_ -> n) \\ txt <- map fst options & n <- [0..]]] <@ Page) hst
		# (chosen,hst)				= mkStoreForm  (Init,cFormId tst.storageInfo taskId -1) choice.value hst
		| chosen.value == -1		= (createDefault,{tst & activated =False,html = html +|+ BT choice.form, hst = hst})
		# chosenTask				= snd (options!!chosen.value)
		# (a,{activated=adone,html=ahtml,hst}) = chosenTask {tst & tasknr = [-1:tasknr], activated = True, html = BT [], hst = hst}
		= (a,{tst & tasknr = tasknr, activated = adone, html = html +|+ ahtml,hst = hst})
	# chosenTask					= snd (options!!chosen.value)
	# (a,{activated=adone,html=ahtml,hst}) = chosenTask {tst & tasknr = [-1:tasknr], activated = True, html = BT [], hst = hst}
	= (a,{tst & tasknr = tasknr, activated = adone, html = html +|+ ahtml,hst = hst})

	but i = LButton defpixel i

CTask_pdmenu :: [(String,Task a)] -> (Task a) | iData a
CTask_pdmenu options = \tst -> mkTask (doCTask` options) tst
where
	doCTask` [] tst					= returnV createDefault tst	
	doCTask` options tst=:{tasknr,html,hst}								// choose one subtask out of the list
	# taskId						= itaskId (showTaskNr tasknr) ("_Or0." <+++ length options)
	# (choice,hst)					= FuncMenu  (Init,cFormId tst.storageInfo taskId (0,[(txt,id) \\ txt <- map fst options]))	hst
	# (_,{tasknr,activated=adone,html=ahtml,hst})	
									= STask  "Done" Void {tst & activated = True, html = BT [], hst = hst} 	
	| not adone						= (createDefault,{tst & tasknr = tasknr,activated = False, html = html +|+ BT choice.form +|+ ahtml, hst = hst})
	# chosenIdx						= snd choice.value
	# chosenTask					= snd (options!!chosenIdx)
	# (a,{activated=bdone,html=bhtml,hst}) 
									= chosenTask {tst & tasknr = [-1:tasknr],activated = True, html = BT [], hst = hst}
	= (a,{tst & tasknr = tasknr, activated = adone&&bdone, html = html +|+ bhtml,hst = hst})
	
MCTask_ckbox :: [(String,Task a)] -> (Task [a]) | iData a
MCTask_ckbox options = \tst -> mkTaskNoInc (MCTask_ckbox` options) tst
where
	MCTask_ckbox` [] tst			= returnV [] tst	
	MCTask_ckbox` options tst=:{tasknr,html,hst}									// choose one subtask out of the list
	# (cboxes,hst)					= ListFuncCheckBox (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) ("_MLC." <+++ length options)) initCheckboxes) hst
	# optionsform					= cboxes.form <=|> [Txt text \\ (text,_) <- options]
	# (_,{tasknr,activated=adone,html=ahtml,hst}) = STask "OK" Void {tst & activated = True, html = BT [], hst = hst}	
	| not adone						= (createDefault,{tst & tasknr=tasknr,activated=False,html=html +|+ BT [optionsform] +|+ ahtml, hst = hst})
	# mytasks						= [option \\ option <- options & True <- snd cboxes.value]
	= STasks mytasks {tst & tasknr=tasknr,activated=True, hst = hst}

	initCheckboxes  = 
		[(CBNotChecked  text,  \ b bs id -> id) \\ (text,_) <- options]

// Parallel tasks ending as soon as one completes

PCTask2 :: (Task a,Task a) -> (Task a) | iData a 
PCTask2 (taska,taskb) = \tst -> mkTask (PCTask2` (taska,taskb)) tst
where
	PCTask2` (taska,taskb) tst=:{tasknr,html,hst}
	# (a,{activated=adone,html=ahtml,hst}) 	= taska {tst & tasknr = [-1,0:tasknr],activated = True, html = BT [], hst = hst}
	# (b,{activated=bdone,html=bhtml,hst}) 	= taskb {tst & tasknr = [-1,1:tasknr],activated = True, html = BT [], hst = hst}
	# (aorb,aorbdone,myhtml)				= if adone (a,adone,ahtml) (if bdone (b,bdone,bhtml) (a,False,ahtml +|+ bhtml))
	= (aorb,{tst & activated = aorbdone, html = html +|+ myhtml, hst =  hst})

PCTasks :: [(String,Task a)] -> (Task a) | iData a 
PCTasks options = \tst -> mkTask (PCTasks` options) tst
where
	PCTasks` [] tst 				= returnV createDefault tst
	PCTasks` tasks tst=:{tasknr,html,hst}
	# (chosen,hst)					= mkStoreForm  (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) ("_One0." <+++ length options) ) 0) id hst
	# (choice,hst)					= TableFuncBut2 (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) "_But" ) [[(mode chosen.value n, but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]] <@ Page) hst
	# (chosen,hst)					= mkStoreForm  (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) ("_One0." <+++ length options) ) 0) choice.value hst
	# (choice,hst)					= TableFuncBut2 (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) "_But" ) [[(mode chosen.value n, but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]] <@ Page) hst
	# chosenTask					= snd (options!!chosen.value)
	# (a,{tasknr,activated=adone,html=ahtml,hst})
									= chosenTask {tst & tasknr = [-1,chosen.value:tasknr], activated = True, html = BT [], hst = hst}
	| not adone						= (a,{tst & activated = adone, html = html +|+ BT choice.form +-+ ahtml, hst = hst})
	= (a,{tst & activated = adone, html = html +|+ ahtml, hst = hst})

	but i = LButton defpixel (i <+++ ":Or")
	mode i j
	| i==j = Display
	= Edit

// Parallel tasks ending if all complete

PTask2 :: (Task a,Task b) -> (Task (a,b)) | iData a & iData b
PTask2 (taska,taskb) = \tst -> mkTask (PTask2` (taska,taskb)) tst
where
	PTask2` (taska,taskb) tst=:{tasknr,html,hst}
	# (a,{activated=adone,html=ahtml,hst})	= taska {tst & tasknr = [-1,0:tasknr],activated = True, html = BT [], hst = hst}	
	# (b,{activated=bdone,html=bhtml,hst})	= taskb {tst & tasknr = [-1,1:tasknr],activated = True, html = BT [], hst = hst}
	= ((a,b),{tst & activated = adone&&bdone, html = html +|+ ahtml +|+ bhtml,hst = hst})

PTasks :: [(String,Task a)] -> (Task [a]) | iData a 
PTasks options = \tst -> mkTask (doPTasks` options) tst
where
	doPTasks` [] tst	= returnV [] tst
	doPTasks` options tst=:{tasknr,html,hst,trace}
	# (chosen,hst)		= mkStoreForm   (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) ("_All" <+++ length options) ) 0) id hst
	# (choice,hst)		= TableFuncBut2 (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) "_But" ) [[(mode chosen.value n,but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]] <@ Page) hst
	# (chosen,hst)		= mkStoreForm   (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) ("_All" <+++ length options) ) 0) choice.value hst
	# (choice,hst)		= TableFuncBut2 (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) "_But" ) [[(mode chosen.value n,but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]] <@ Page) hst
	# chosenTask		= snd (options!!chosen.value)
	# chosenTaskName	= fst (options!!chosen.value)
	# (alist,{activated=finished,hst,trace})		
						= checkAllTasks (map snd options) 0 [] {tst & html = BT [], hst = hst,trace = trace}
	| finished			= (alist,{tst & activated = finished, hst = hst,trace = trace})
	# (a,{activated=adone,html=ahtml,hst,trace}) = chosenTask {tst & tasknr = [-1,chosen.value:tasknr], activated = True, html = BT [], hst = hst, trace = trace}
	| not adone			= ([a],{tst & 	trace = trace,
										activated = adone, 
										html = html +|+ BT choice.form +-+ 
												(BT [Txt ("Task: " +++ chosenTaskName +++ "."),Br] +|+ ahtml), 
										hst = hst})
	# (alist,{activated=finished,hst,trace})		
						= checkAllTasks (map snd options) 0 [] {tst & html = BT [], hst = hst, trace = trace}
	| finished			= (alist,{tst & activated = finished, hst = hst,trace =trace})
	= ([a],{tst & trace = trace,
				  activated = finished, html = 	html +|+ 
												BT choice.form +-+ (BT [Txt ("Task: " +++ chosenTaskName+++ "."),Br] +|+ 
																	ahtml), hst = hst})

	but i = LButton defpixel (i <+++ ":And")
	mode i j
	| i==j = Display
	= Edit

checkAllTasks taskoptions ctasknr alist tst=:{tasknr,html,hst,trace}
| ctasknr == length taskoptions	= (reverse alist,{tst & activated = True, html = BT [], hst = hst, trace = trace})
# task							= taskoptions!!ctasknr
# (a,{activated = adone,hst,trace})	= task {tst & tasknr = [-1,ctasknr:tasknr], activated = True, html = BT [], hst = hst,trace = trace}
| adone							= checkAllTasks taskoptions (inc ctasknr) [a:alist] {tst & tasknr = tasknr, html = BT [], hst = hst, trace = trace}
= ([],{tst & tasknr = tasknr, activated = False, html = BT [], hst = hst, trace = trace})

checkAnyTasks taskoptions ctasknr bool tst=:{tasknr,html,hst,trace}
| ctasknr == length taskoptions	= (bool,tst)
# task							= taskoptions!!ctasknr
# (a,{activated = adone,hst,trace})	= task {tst & tasknr = [-1,ctasknr:tasknr], activated = True, html = BT [], hst = hst,trace = trace}
= checkAnyTasks taskoptions (inc ctasknr) (bool||adone) {tst & tasknr = tasknr, html = BT [], hst = hst, trace = trace}

PMilestoneTasks :: [(String,Task a)] -> (Task [a]) | iData a 
PMilestoneTasks options = \tst -> mkTask (PMilestoneTasks` options) tst
where
	PMilestoneTasks` [] tst		= returnV [] tst
	PMilestoneTasks` options tst=:{tasknr,html,hst}
	# (chosen,hst)				= mkStoreForm  (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) "_PMile_" ) 0) id hst
	# (choice,hst)				= TableFuncBut2 (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) "_But_" ) [[(mode chosen.value n,but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]] <@ Page) hst
	# (chosen,hst)				= mkStoreForm  (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) "_PMile_" ) 0) choice.value hst
	# (choice,hst)				= TableFuncBut2 (Init,cFormId tst.storageInfo (itaskId (showTaskNr tasknr) "_But_" ) [[(mode chosen.value n,but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]] <@ Page) hst
	# chosenTask				= snd (options!!chosen.value)
	# chosenTaskName			= fst (options!!chosen.value)
	# taskoptions				= map snd options
	# (milestoneReached,{hst})	= checkAnyTasks taskoptions 0 False {tst & html = BT [], hst = hst}
	# (a,{activated=adone,html=ahtml,hst}) 
								= chosenTask {tst & tasknr = [-1:addTasknr tasknr chosen.value], activated = True, html = BT [], hst = hst}
	# tasknr					= addTasknr tasknr (length options)
	| not adone					= ([a],{tst & tasknr = tasknr, activated = milestoneReached, 
										html = 	html +|+ 
												BT choice.form +-+  (BT [Txt ("Task: " +++ chosenTaskName +++ "."),Br] +|+ 
																		ahtml), hst = hst})
	# (alist,{activated=finished,hst})		
								= checkAllTasks taskoptions 0 [] {tst & html = BT [], hst = hst}
	| finished					= (alist,{tst & tasknr = tasknr, activated = True, hst = hst})
	= ([a],{tst & tasknr = tasknr, activated = milestoneReached, html =	html +|+ 
														BT choice.form +-+  (BT [Txt ("Task: " +++ chosenTaskName +++ "."),Br] +|+ 
																			ahtml), hst = hst})

	but i = LButton defpixel (i <+++ ":MileSt")
	mode i j
	| i==j = Display
	= Edit

PmuTasks :: [(Int,Task a)] -> (Task [a]) | iData a 
PmuTasks tasks = \tst-> mkTask (PmuTasks` tasks) tst
where
	PmuTasks` [] tst								= returnV [] tst
	PmuTasks` [(ida,taska):tasks] tst=:{html}
	# (a, tst=:{html=htmla,activated=adone})		= (ida @:: taska) {tst & html = (ida,"Task") @@: BT [], activated = True}
	# (ax,tst=:{html=htmlstasks,activated=alldone})	= PmuTasks` tasks (incTask {tst & html = (ida,"Task") @@: BT []})
	= ([a:ax],{tst & html = html +|+ htmla +|+ htmlstasks,activated=adone&&alldone})	

returnV :: a -> (Task a) | iData a 
returnV a  = \tst  -> mkTask returnV` tst
where
	returnV` tst = (a,{tst & activated = True})				// return result task

returnTask :: a -> (Task a) | iData a 
returnTask a = \tst -> mkTask (returnTask` a) tst
where
	returnTask` a  tst=:{tasknr,activated,html,hst}
	# editId			= "edit_" <+++ showTaskNr tasknr
	# (editor,hst) 		= (mkEditForm  (Set,cdFormId tst.storageInfo editId a <@ Display) hst)			// yes, read out current value, make editor passive
	= (editor.value,{tst & html = html +|+ BT editor.form, activated = True, hst = hst})		// return result task

returnVF :: a [BodyTag] -> (Task a) | iData a 
returnVF a bodytag = \tst = mkTask returnVF` tst
where
	returnVF` tst =:{html} 
	= (a,{tst & html = html +|+ BT bodytag, activated = True})

returnF :: [BodyTag] -> TSt -> TSt
returnF bodytag = \tst = returnVF` tst
where
	returnVF` tst=:{activated, html}  
	| not activated				= tst		// not active, return default value
	= {tst & html = html +|+ BT bodytag}	// active, so perform task or get its result

mkRTask :: String (Task a) *TSt -> ((Task a,Task a),*TSt) | iData a
mkRTask s task tst = let (a,b,c) = mkRTask` s task (incTask tst) in ((a,b),c)
where
	mkRTask` s task tst=:{tasknr = maintasknr,storageInfo} = (bossTask, workerTask s task,tst)
	where
		workerTask s task tst = mkTask (workerTask` s task) tst
		where
			workerTask` s task tst=:{tasknr,html,hst} 
			# (todo,hst)	= checkBossSignal id hst	// check whether lazy task evaluation has to be done
			| todo.value								// yes	
				# (a,{activated=adone,html=ahtml,hst}) = task {tst & tasknr = maintasknr++[0], activated = True, html = BT [], hst = hst}			// do task
				# (_,hst) 					= lazyTaskStore (\_ -> (adone,a)) hst	// store task and status
				= (a,{tst & html = html +|+ BT (if adone [] [Txt ("lazy task \"" +++ s +++ "\" activated:"),Br]) +|+ ahtml, hst = hst})
			= (createDefault,{tst & hst = hst})	// no
	
		bossTask tst = mkTask (bossTask`) tst
		where
			bossTask` tst=:{tasknr,html,hst} 
			# buttonId		= "getlt" <+++ showTaskNr tasknr
			# (finbut,hst)  = simpleButton buttonId s (\_ -> True) hst	// button press will trigger related lazy task	
			# (todo,hst)	= checkBossSignal finbut.value hst			// set store True if button pressed
			# (result,hst)	= lazyTaskStore id hst						// inspect status task
			# (done,value)	= result.value
			| not done 		= (createDefault,{tst & activated = False, html = html +|+ BT (if todo.value [Txt ("Waiting for task \"" +++ s +++ "\"..")] finbut.form), hst = hst})
			= (value,{tst & html = html +|+  BT [Txt ("Result of lazy task \"" +++ s +++ "\" :")], hst = hst})
	
		lazyTaskStore   fun = mkStoreForm (Init,cFormId storageInfo ("getLT" <+++ showTaskNr maintasknr) (False,createDefault)) fun 
		checkBossSignal fun = mkStoreForm (Init,cFormId storageInfo ("setLT" <+++ showTaskNr maintasknr) (fun False)) fun 
		
mkRTaskCall :: String b (b -> Task a) *TSt -> ((b -> Task a,Task a),*TSt) | iData a
												& iData b
mkRTaskCall  s initb batask tst = let (a,b,c) = mkRTaskCall` s (incTask tst) in ((a,b),c)
where
	mkRTaskCall` s tst=:{tasknr = maintasknr,storageInfo} = (bossTask, workerTask s,tst)
	where
		workerTask s tst = mkTask (workerTask` s) tst
		where
			workerTask` s tst=:{tasknr,html,hst}
			# (boss,hst)		= bossStore id hst		// check input from boss
			# (worker,hst)		= workerStore id hst	// check result from worker
			# (bdone,binput)	= boss.value
			# (wdone,wresult)	= worker.value
			| wdone				= (wresult,{tst & activated = True, html = html +|+ BT [Txt ("Lazy task \"" +++ s +++ "\" completed:")], hst = hst})
			| bdone
				# (wresult,{activated=wdone,html=whtml,hst}) = batask binput {tst & tasknr = maintasknr++[0], activated = True, html = BT [], hst = hst}	// apply task to input from boss
				| wdone															// worker task finshed
					# (_,hst)	= workerStore (\_ -> (wdone,wresult)) hst		// store task and status
					= workerTask` s {tst &  hst = hst}				// complete as before
				= (createDefault,{tst & activated = False, html = html +|+ BT (if wdone [] [Txt ("lazy task \"" +++ s +++ "\" activated:"),Br]) +|+ whtml, hst = hst})
			= (createDefault,{tst & activated = False, html = html +|+ BT [Txt ("Waiting for task \"" +++ s +++ "\"..")], hst = hst})
	
		bossTask b tst = mkTask bossTask` tst
		where
			bossTask` tst=:{tasknr,html,hst} 
			# (boss,hst)		= bossStore id hst		// check input from boss
			# (worker,hst)		= workerStore id hst	// check result from worker
			# (bdone,binput)	= boss.value
			# (wdone,wresult)	= worker.value
			| bdone && wdone	= (wresult,{tst & activated = True, html = html +|+ BT [Txt ("Result of lazy task \"" +++ s +++ "\" :")], hst = hst})	// finished
			| not bdone
				# (_, hst)		= bossStore (\_ -> (True,b)) hst	// store b information to communicate to worker	
				= (createDefault,{tst & activated = False, html = html +|+ BT [Txt ("Waiting for task \"" +++ s +++ "\"..")], hst = hst})
			= (createDefault,{tst & activated = False, html = html +|+ BT [Txt ("Waiting for task \"" +++ s +++ "\"..")], hst = hst})	
	
		workerStore   fun = mkStoreForm (Init,cFormId storageInfo ("workerStore" <+++ showTaskNr maintasknr) (False,createDefault)) fun 
		bossStore     fun = mkStoreForm (Init,cFormId storageInfo ("bossStore"   <+++ showTaskNr maintasknr) (False,initb)) fun 
		
mkRDynTaskCall :: String a *TSt -> (((Task a) -> (Task a),Task a),*TSt) | iData a
mkRDynTaskCall s a tst = mkRDynTaskCall` (incTask tst)
where
	mkRDynTaskCall` tst=:{tasknr = maintasknr,storageInfo} = ((bossTask, workerTask),tst)
	where
		workerTask tst = mkTask workerTask` tst
		where
			workerTask` tst=:{tasknr,html,hst} 
			# (boss,hst)		= bossStore (False,defaulttask) hst		// check input from boss
			# (worker,hst)		= workerStore id hst					// check result from worker
			# (bdone,btask)		= boss.value
			# (wdone,wresult)	= worker.value
			| wdone				= (wresult,{tst & activated = True, html = html +|+ BT [Txt ("Lazy task \"" +++ s +++ "\" completed:")], hst = hst})	
			| bdone
				# (wresult,{activated=wdone,html=whtml,hst}) = btask {tst & tasknr = maintasknr++[0], activated = True, html = BT [], hst = hst}	// apply task stored in memory
				| wdone															// worker task finshed
					# (_,hst)	= workerStore (\_ -> (wdone,wresult)) hst		// store task and status
					= workerTask` {tst & hst = hst} 							// complete as before
				= (createDefault,{tst & activated = False, html = html +|+ BT [Txt ("lazy task \"" +++ s +++ "\" activated:"),Br] +|+ whtml, hst = hst})
			= (createDefault,{tst & activated = False, html = html +|+ BT [Txt ("Waiting for task \"" +++ s +++ "\"..")], hst = hst})		// no
	
		bossTask taska tst = mkTask bossTask` tst
		where
			bossTask` tst=:{tasknr,html,hst} 
			# (boss,hst)		= bossStore (False,defaulttask) hst		// check input from boss
			# (worker,hst)		= workerStore id hst					// check result from worker
			# (bdone,btask)		= boss.value
			# (wdone,wresult)	= worker.value
			| bdone && wdone	= (wresult,{tst & activated = True, html = html +|+ BT [Txt ("Result of lazy task \"" +++ s +++ "\" :")], hst = hst})	// finished
			| not bdone
				# (_, hst)		= bossStore (True,taska) hst			// store b information to communicate to worker	
				= (createDefault,{tst & activated = False, html = html +|+ BT [Txt ("Task commited.\nWaiting for task \"" +++ s +++ "\"..")], hst = hst})
			= (createDefault,{tst & activated = False, html = html +|+ BT [Txt ("Waiting for task \"" +++ s +++ "\"..")], hst = hst})	
	
		workerStore   fun = mkStoreForm (Init,cFormId storageInfo ("workerStore" <+++ showTaskNr maintasknr) (False,createDefault)) fun 

		bossStore (set,task) hst
		# (boss,hst) 			= mkStoreForm (Init,cFormId storageInfo ("bossStore" <+++ showTaskNr maintasknr) initBoss) settask hst
		# (bdone,encbtask)		= boss.value
		# btask					= case string_to_dynamic` encbtask of
									(mytask:: *TSt -> *(a^,*TSt)) -> mytask
									_ -> 	defaulttask
		= ({boss & value = (bdone,btask)},hst)
		where
			initBoss			= (False,convertTask defaulttask)
			settask				= if set (\_ -> (True,convertTask task)) id
			convertTask task 	= dynamic_to_string (dynamic task::*TSt -> *(a^,*TSt))

			string_to_dynamic` s = string_to_dynamic ( {s` \\ s` <-: s})

		defaulttask 		 	= STask "DefaultTask" a
		
// time and date related tasks

waitForTimeTask:: HtmlTime	-> (Task HtmlTime)
waitForTimeTask time = \tst ->  mkTask waitForTimeTask` tst
where
	waitForTimeTask` tst=:{tasknr,hst}
	# taskId				= itaskId (showTaskNr tasknr) "_Time_"
	# (taskdone,hst) 		= mkStoreForm (Init,cFormId tst.storageInfo taskId (False,time)) id hst  			// remember time
	# ((currtime,_),hst)	= getTimeAndDate hst
	| currtime < time		= (time,{tst & activated = False,hst = hst})
	= (time,{tst & hst = hst})

waitForDateTask:: HtmlDate	-> (Task HtmlDate)
waitForDateTask date = \tst ->  mkTask waitForDateTask` tst
where
	waitForDateTask` tst=:{tasknr,hst}
	# taskId				= itaskId (showTaskNr tasknr) "_Date_"
	# (taskdone,hst) 		= mkStoreForm (Init,cFormId tst.storageInfo taskId (False,date)) id hst  			// remember date
	# ((_,currdate),hst) 	= getTimeAndDate hst
	| currdate < date		= (date,{tst & activated = False, hst = hst})
	= (date,{tst & hst = hst})

// lifting section

appIData :: (IDataFun a) -> (Task a) | iData a
appIData idatafun = \tst -> mkTask (appIData` idatafun) tst
where
	appIData` idata tst=:{tasknr,html,hst}
	# (idata,hst) 										= idatafun hst
	# (_,{tasknr,activated,html=ahtml,hst}) 			= STask  "Done" Void {tst & activated = True, html = BT [],hst = hst}	
	= (idata.value,{tst & tasknr = tasknr,activated = activated, html = html +|+ 
															(if activated (BT idata.form) (BT idata.form +|+ ahtml)), hst = hst})

appHSt :: (HSt -> (a,HSt)) -> (Task a) | iData a
appHSt fun = mkTask doit
where
	doit tst=:{activated,html,tasknr,hst,storageInfo}
	# ntasknr			= showTaskNr tasknr
	# taskId			= "iTask_" 	<+++ ntasknr
	# (store,hst) 		= mkStoreForm (Init,cFormId storageInfo taskId (False,createDefault)) id hst  			
	# (done,value)		= store.value
	| done 				= (value,{tst & hst = hst})	// if task has completed, don't do it again
	# (value,hst)		= fun hst
	# (store,hst) 		= mkStoreForm (Init,cFormId storageInfo taskId (False,createDefault)) (\_ -> (True,value)) hst 	// remember task status for next time
	# (done,value)		= store.value
	= (value,{tst & activated = done, hst = hst})													// task is now completed, handle as previously
	


// monadic shorthands
(*>>) infix 4 :: w:(St .s .a)  v:(.a -> .(St .s .b)) -> u:(St .s .b), [u <= v, u <= w]
(*>>) ftst b = doit
where
	doit tst
	# (a,tst) = ftst tst
	= b a tst

(@>>) infix 4 :: w:(.s -> .s)  v:(St .s .b) -> u:(St .s .b), [u <= v, u <= w]
(@>>) ftst b = doit
where
	doit tst
	# tst = ftst tst
	= b tst

Once :: (St TSt a) -> (St TSt a) | iData a
Once fun = mkTask doit
where
	doit tst=:{activated,html,tasknr,hst,storageInfo}
	# ntasknr			= showTaskNr tasknr
	# taskId			= itaskId (showTaskNr tasknr) "_Once_"
	# (store,hst) 		= mkStoreForm (Init,cFormId storageInfo taskId (False,createDefault)) id hst  			
	# (done,value)		= store.value
	| done 				= (value,{tst & hst = hst})	// if task has completed, don't do it again
	# (value,tst=:{hst})= fun {tst & hst = hst}
	# (store,hst) 		= mkStoreForm (Init,cFormId storageInfo taskId (False,createDefault)) (\_ -> (True,value)) hst 	// remember task status for next time
	# (done,value)		= store.value
	= (value,{tst & activated = done, hst = hst})													// task is now completed, handle as previously

(=>>) infix 1 :: w:(St .s .a) v:(.a -> .(St .s .b)) -> u:(St .s .b), [u <= v, u <= w]
(=>>) a b = a `bind` b

(#>>) infixl 1 :: w:(St .s .a) v:(St .s .b) -> u:(St .s .b), [u <= v, u <= w]
(#>>) a b = a `bind` (\_ -> b)

(<|) infix 3 :: (St TSt a) (a -> .Bool, a -> String) -> (St TSt a) | iData a
(<|) taska (pred,message) = \tst -> mkTask doTask tst
where
	doTask tst=:{html = ohtml}
	# (a,tst=:{activated}) 	= taska tst
	| not activated 		= (a,tst)
	| pred a 				= (a,tst)
	# (a,tst=:{html = nhtml})= mkTask doTask {tst & html = BT []}
	| pred a 				 = (a,{tst & html = ohtml +|+ nhtml})
	= (a,{tst & html = ohtml +|+ BT [Txt (message a)] +|+ nhtml})

(<<@) infix 3 ::  v:(St TSt .a) b  -> u:(St TSt .a) | setTaskAttr b, [u <= v]
(<<@) task attr 
= \tst -> doit tst
where
	doit tst=:{storageInfo}
	# tst = setTaskAttr attr tst
	# (a,tst) = task (setTaskAttr attr tst)
	= (a,{tst & storageInfo = storageInfo})

(?>>) infix 2 :: [BodyTag] v:(St TSt .a) -> v:(St TSt .a)
(?>>) prompt task = \tst -> doit tst
where
	doit tst=:{html=ohtml,activated=myturn,myId}
	# (a,tst=:{activated,html=nhtml}) = task {tst & html = BT []}
	| activated || not myturn= (a,{tst & html = ohtml})
	= (a,{tst & html = ohtml +|+ BT prompt +|+ nhtml})

(!>>) infix 2 :: [BodyTag] v:(St TSt .a) -> v:(St TSt .a)
(!>>) prompt task = \tst -> doit tst
where
	doit tst=:{html=ohtml,activated=myturn,myId}
	# (a,tst=:{activated,html=nhtml}) = task {tst & html = BT []}
	| not myturn	= (a,{tst & html = ohtml})
	= (a,{tst & html = ohtml +|+ BT prompt +|+ nhtml})

myId :: TSt -> (Int,TSt)
myId tst=:{myId} = (myId,tst)

userId :: TSt -> (Int,TSt)
userId tst=:{userId} = (userId,tst)

// *** utility section ***

// editors

cFormId  {tasklife,taskstorage,taskmode} s d = {sFormId  s d & lifespan = tasklife, storage = taskstorage, mode = taskmode}
cdFormId {tasklife,taskstorage,taskmode} s d = {sdFormId s d & lifespan = tasklife, storage = taskstorage, mode = taskmode}

// simple html code generation utilities

yellowUser nr
= yellow ("User " <+++ nr <+++ " :")

yellow message
= Font [Fnt_Color (`Colorname Yellow)] [B [] message]

green message
= Font [Fnt_Color (`Colorname Green)] [B [] message]

// task number generation

incTask tst = {tst & tasknr = incTasknr tst.tasknr}

incTasknr [] = [0]
incTasknr [i:is] = [i+1:is]

addTasknr [] j = [j]
addTasknr [i:is] j = [i+j:is]

showTaskNr [] 		= ""
showTaskNr [i] 		= toString i
showTaskNr [i:is] 	= showTaskNr is <+++ "." <+++ toString i 

itaskId nr postfix = "iTask_" <+++ nr <+++ postfix

// Trace handling

Start
# t = InsertTrace [0,0] 22 "bla0.1" []
# t = InsertTrace [1,0] 22 "bla0.2" t
# t = InsertTrace [0] 22 "bla0" t
# t = InsertTrace [2] 22 "bla2" t
# t = InsertTrace [1] 22 "bla1" t
= printTrace (Just t)

InsertTrace :: ![Int] !Int !String ![Trace] -> [Trace]
InsertTrace idx who val trace = InsertTrace` ridx who val trace
where
	InsertTrace` :: ![Int] !Int !String ![Trace] -> [Trace]
	InsertTrace` [i] 	who str traces
	# (Trace _ itraces)		= select i traces
	= updateAt` i (Trace (Just (who,show,str)) itraces)  traces
	InsertTrace` [i:is] who str traces
	# (Trace ni itraces)	= select i traces
	# nistraces				= InsertTrace` is who str itraces
	= updateAt` i (Trace ni nistraces) traces

	select :: !Int ![Trace] -> Trace
	select i list
	| i < length list = list!!i 
	=  Trace Nothing []

	show 	= showTaskNr idx
	ridx	= reverse idx

	updateAt`:: !Int !Trace ![Trace] -> [Trace]
	updateAt` 0 x []		= [x]
	updateAt` 0 x [y:ys]	= [x:ys]
	updateAt` n x []		= [Trace Nothing []	: updateAt` (n-1) x []]
	updateAt` n x [y:ys]	= [y      			: updateAt` (n-1) x ys]

printTrace Nothing 		= EmptyBody
printTrace (Just a)  	= STable [] [[print x] \\ x <- a]
where
	print (Trace Nothing rest) 	
		= STable [] [[EmptyBody,			EmptyBody,					EmptyBody, 	STable [] [[print x] \\ x <- rest]]
					]
	print (Trace (Just (w,i,s)) rest) 	
		= STable [] [[EmptyBody,			EmptyBody,					EmptyBody, 	STable [] [[print x] \\ x <- rest]] 
					,[yellow (toString w),	green ("T" <+++ toString i),Txt s, 		EmptyBody]
					]
					

	
// debugging code 

print_graph :: !a -> Bool;
print_graph a = code {
.d 1 0
jsr _print_graph
.o 0 0
pushB TRUE
}

my_dynamic_to_string :: !Dynamic -> {#Char};
my_dynamic_to_string d
| not (print_graph d)
= abort ""
#! s=dynamic_to_string d;
| not (print_graph (tohexstring s))
= abort "" 
# d2 = string_to_dynamic {c \\ c <-: s};
| not (print_graph d2)
= abort ""
= s;

tohexstring :: {#Char} -> {#Char};
tohexstring s = {tohexchar s i \\ i<-[0..2*size s-1]};

tohexchar :: {#Char} Int -> Char;
tohexchar s i
# c=((toInt s.[i>>1]) >> ((1-(i bitand 1))<<2)) bitand 15;
| c<10
= toChar (48+c);
= toChar (55+c);

	