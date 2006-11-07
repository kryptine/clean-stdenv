implementation module htmlTask

import StdEnv, StdHtml

derive gForm 	[], Void
derive gUpd 	[], Void
derive gParse 	Void
derive gPrint 	Void
derive gerda 	Void

import dynamic_string, EncodeDecode

:: *TSt 		=	{ tasknr 		:: ![Int]
					, myturn		:: !Bool   	
					, html			:: ![BodyTag]
					, storageinfo	:: !Storage
					, hst			:: !HSt
					}
:: Storage		=	{ tasklife		:: !Lifespan
					, taskstorage	:: !StorageFormat
					}


startTask :: (Task a) *HSt -> ([BodyTag],HSt) | iData a 
startTask taska hst
# (_,{html,hst}) = taska {tasknr = [], myturn = True, html = [], hst = hst, storageinfo = {tasklife = Session, taskstorage = PlainString }}
= (html,hst)

instance setTaskAttribute Lifespan
where setTaskAttribute lifespan tst = {tst & storageinfo.tasklife = lifespan}

instance setTaskAttribute StorageFormat
where setTaskAttribute storageformat tst = {tst & storageinfo.taskstorage = storageformat}

mkTask :: (*TSt -> *(a,*TSt)) -> (Task a) | iData a
mkTask mytask = \tst -> mkTask` tst
where
	mkTask` tst=:{myturn}		
	# tst 			= incTask tst				// every task should first increment its tasknumber
	| not myturn	= (createDefault,tst)		// not active, return default value
	= mytask tst

STask :: String a -> (Task a) | iData a 
STask prompt a = \tst -> mkTask (STask` a) tst
where
	STask` a tst=:{tasknr,html,hst}
	# taskId			= "iTask_" <+++ mkTaskNr tasknr
	# editId			= "iEdit_" <+++ mkTaskNr tasknr
	# buttonId			= mkTaskNr tasknr
	# (taskdone,hst) 	= mkStoreForm (Init,cFormId tst.storageinfo taskId False) id hst  	// remember if the task has been done
	| taskdone.value																		// test if task has completed
		# (editor,hst) 	= (mkEditForm  (Init,cdFormId tst.storageinfo editId a) hst)		// yes, read out current value, make editor passive
		= (editor.value,{tst & myturn = True, html = html <|.|> editor.form, hst = hst})	// return result task
	# (editor,hst) 		= mkEditForm  (Init,cFormId tst.storageinfo editId a) hst			// no, read out current value from active editor
	# (finbut,hst)  	= simpleButton buttonId prompt (\_ -> True) hst						// add button for marking task as done
	# (taskdone,hst) 	= mkStoreForm (Init,cFormId tst.storageinfo taskId False) finbut.value hst 	// remember task status for next time
	| taskdone.value	= STask` a {tst & hst = hst}										// task is now completed, handle as previously
	= (a,{tst & myturn = taskdone.value, html = html <|.|> (editor.form ++ finbut.form), hst = hst})

STask_button 		:: String (Task a) 			-> (Task a) 	| iData a
STask_button s task = CTask_button [(s,task)]

STasks :: [(String,Task a)] -> (Task [a])| iData a 
STasks options = \tst -> mkTask (doSandTasks` options []) tst
where
	doSandTasks` [] accu tst 		= returnV (reverse accu) tst
	doSandTasks` [(txt,task):ts] accu tst=:{tasknr,html,hst} 
	# (a,{tasknr,myturn=adone,html=ahtml,hst}) 
									= task {tst & myturn = True, html = []}
	| not adone						= (reverse accu,{tst & tasknr = tasknr,myturn = adone, html = html <|.|> [Txt ("Task: " +++ txt),Br] <|.|> ahtml,hst = hst})
	= mkTask (doSandTasks` ts [a:accu]) {tst & tasknr = tasknr,myturn = adone, html = html <|.|> ahtml,hst = hst}

CTask_button :: [(String,Task a)] -> (Task a) | iData a
CTask_button options = \tst -> mkTask (doCTask` options) tst
where
	doCTask` [] tst					= returnV createDefault tst				
	doCTask` options tst=:{tasknr,html,hst}									// choose one subtask out of the list
	# (choice,hst)					= TableFuncBut (Init,cFormId tst.storageinfo ("Cbt_task_" <+++ mkTaskNr tasknr) [[(but txt,\_ -> n) \\ txt <- map fst options & n <- [0..]]]) hst
	# (chosen,hst)					= mkStoreForm  (Init,cFormId tst.storageinfo ("Cbt_chosen_" <+++ mkTaskNr tasknr) -1) choice.value hst
	| chosen.value == -1			= (createDefault,{tst & myturn =False,html = html <|.|> choice.form, hst = hst})
	# chosenTask					= snd (options!!chosen.value)
	# (a,{tasknr,myturn=adone,html=ahtml,hst}) = chosenTask {tst & tasknr = tasknr ++ [1], myturn = True, html = [], hst = hst}
	= (a,{tst & tasknr = tasknr, myturn = adone, html = html <|.|> ahtml,hst = hst})

	but i = LButton defpixel i

CTask_pdmenu :: [(String,Task a)] -> (Task a) | iData a
CTask_pdmenu options = \tst -> mkTask (doCTask` options) tst
where
	doCTask` [] tst					= returnV createDefault tst	
	doCTask` options tst=:{tasknr,html,hst}								// choose one subtask out of the list
	# (choice,hst)					= FuncMenu  (Init,cFormId tst.storageinfo ("Cpd_task_" <+++ mkTaskNr tasknr) (0,[(txt,id) \\ txt <- map fst options]))	hst
	# (_,{tasknr,myturn=adone,html=ahtml,hst})	
									= STask  "Done" Void {tst & tasknr = tasknr ++ [0],myturn = True, html = [], hst = hst}	
	| not adone						= (createDefault,{tst & tasknr = tasknr,myturn = False, html = html <|.|> choice.form <|.|> ahtml, hst = hst})
	# chosenIdx						= snd choice.value
	# chosenTask					= snd (options!!chosenIdx)
	# (a,{tasknr,myturn=bdone,html=bhtml,hst}) 
									= chosenTask {tst & tasknr = tasknr ++ [1],myturn = True, html = [], hst = hst}
	= (a,{tst & tasknr = tasknr, myturn = adone&&bdone, html = html <|.|> bhtml,hst = hst})
	
MCTask_ckbox :: [(String,Task a)] -> (Task [a]) | iData a
MCTask_ckbox options = \tst -> mkTask (MCTask_ckbox` options) tst
where
	MCTask_ckbox` [] tst			= returnV [] tst	
	MCTask_ckbox` options tst=:{tasknr,html,hst}									// choose one subtask out of the list
	# (cboxes,hst)					= ListFuncCheckBox (Init,cFormId tst.storageinfo ("MC_check" <+++ mkTaskNr tasknr) initCheckboxes) hst
	# optionsform					= cboxes.form <=|> [Txt text \\ (text,_) <- options]
	# (_,{tasknr,myturn=adone,html=ahtml,hst}) = STask  "OK" Void {tst & myturn = True, html = [], hst = hst}	
	| not adone						= (createDefault,{tst & tasknr=tasknr,myturn=False,html=html <|.|> [optionsform] <|.|> ahtml, hst = hst})
	# mytasks						= [option \\ option <- options & True <- snd cboxes.value]
	= STasks mytasks {tst & tasknr=tasknr,myturn=True, hst = hst}

	initCheckboxes  = 
		[(CBNotChecked  text,  \ b bs id -> id) \\ (text,_) <- options]

PCTask2 :: (Task a,Task a) -> (Task a) | iData a 
PCTask2 (taska,taskb) = \tst -> mkTask (PCTask2` (taska,taskb)) tst
where
	PCTask2` (taska,taskb) tst=:{tasknr,html,hst}
	# (a,{myturn=adone,html=ahtml,hst}) = taska {tst & tasknr = tasknr ++ [0],myturn = True, html = [], hst = hst}	
	# (b,{myturn=bdone,html=bhtml,hst}) = taskb {tst & tasknr = tasknr ++ [1],myturn = True, html = [], hst = hst}
	# (aorb,aorbdone,myhtml)			= if adone (a,adone,ahtml) (if bdone (b,bdone,bhtml) (a,False,ahtml <|.|> bhtml))
	= (aorb,{tst & myturn = aorbdone, html = html <|.|> myhtml, hst =  hst})

PCTasks :: [(String,Task a)] -> (Task a) | iData a 
PCTasks options = \tst -> mkTask (PCTasks` options) tst
where
	PCTasks` [] tst 				= returnV createDefault tst
	PCTasks` tasks tst=:{tasknr,html,hst}
	# (choice,hst)					= TableFuncBut (Init,cFormId tst.storageinfo ("Cbt_task_" <+++ mkTaskNr tasknr) [[(but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]]) hst
	# (chosen,hst)					= mkStoreForm  (Init,cFormId tst.storageinfo ("Cbt_chosen_" <+++ mkTaskNr tasknr) 0) choice.value hst
	# chosenTask					= snd (options!!chosen.value)
	# (a,{tasknr,myturn=adone,html=ahtml,hst})
									= chosenTask {tst & tasknr = tasknr ++ [chosen.value + 1], myturn = True, html = [], hst = hst}
	| not adone						= (a,{tst & tasknr = tasknr, myturn = adone, html = html <|.|> [choice.form <=> ahtml], hst = hst})
	= (a,{tst & tasknr = tasknr, myturn = adone, html = html <|.|> ahtml, hst = hst})

	but i = LButton defpixel i

PTask2 :: (Task a,Task b) -> (Task (a,b)) | iData a & iData b
PTask2 (taska,taskb) = \tst -> mkTask (PTask2` (taska,taskb)) tst
where
	PTask2` (taska,taskb) tst=:{tasknr,html,hst}
	# (a,{myturn=adone,html=ahtml,hst})	= taska {tst & tasknr = tasknr ++ [0],myturn = True, html = [], hst = hst}	
	# (b,{myturn=bdone,html=bhtml,hst})	= taskb {tst & tasknr = tasknr ++ [1],myturn = True, html = [], hst = hst}
	= ((a,b),{tst & myturn = adone&&bdone, html = html <|.|> ahtml <|.|> bhtml,hst = hst})

PTasks :: [(String,Task a)] -> (Task [a]) | iData a 
PTasks options = \tst -> mkTask (doPTasks` options) tst
where
	doPTasks` [] tst			= returnV [] tst
	doPTasks` options tst=:{tasknr,html,hst}
	# (choice,hst)				= TableFuncBut (Init,cFormId tst.storageinfo ("Cbt_task_" <+++ mkTaskNr tasknr) [[(but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]]) hst
	# (chosen,hst)				= mkStoreForm  (Init,cFormId tst.storageinfo ("Cbt_chosen_" <+++ mkTaskNr tasknr) 0) choice.value hst
	# chosenTask				= snd (options!!chosen.value)
	# chosenTaskName			= fst (options!!chosen.value)
	# (a,{myturn=adone,html=ahtml,hst}) = chosenTask {tst & tasknr = tasknr ++ [chosen.value + 1], myturn = True, html = [], hst = hst}
	| not adone					= ([a],{tst & myturn = adone, html = html <|.|> [choice.form <=> ( [Txt ("Task: " +++ chosenTaskName)] <|.|> ahtml)], hst = hst})
	# (alist,{myturn=finished,hst})		
								= checkAllTasks 0 [] {tst & html = [], hst = hst}
	| finished					= (alist,{tst & myturn = finished, hst = hst})
	= ([a],{tst & myturn = finished, html = html <|.|> [choice.form <=> ([Txt ("Task: " +++ chosenTaskName)] <|.|> ahtml)], hst = hst})

	but i = LButton defpixel i

	checkAllTasks ctasknr alist tst=:{tasknr,html,hst}
	| ctasknr == length options	= (reverse alist,{tst & myturn = True, html = [], hst = hst})
	# task						= snd (options!!ctasknr)
	# (a,{myturn = adone,hst})	= task {tst & tasknr = tasknr ++ [ctasknr + 1], myturn = True, html = [], hst = hst}
	| adone						= checkAllTasks (inc ctasknr) [a:alist] {tst & tasknr = tasknr, html = [], hst = hst}
	= ([],{tst & myturn = False, html = [], hst = hst})

PMilestoneTasks :: [(String,Task a)] -> (Task [a]) | iData a 
PMilestoneTasks options = \tst -> mkTask (PMilestoneTasks` options) tst
where
	PMilestoneTasks` [] tst		= returnV [] tst
	PMilestoneTasks` options tst=:{tasknr,html,hst}
	# (choice,hst)				= TableFuncBut (Init,cFormId tst.storageinfo ("Cbt_task_" <+++ mkTaskNr tasknr) [[(but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]]) hst
	# (chosen,hst)				= mkStoreForm  (Init,cFormId tst.storageinfo ("Cbt_chosen_" <+++ mkTaskNr tasknr) 0) choice.value hst
	# chosenTask				= snd (options!!chosen.value)
	# chosenTaskName			= fst (options!!chosen.value)
	# (milestoneReached,{hst})	= checkAnyTasks 0 {tst & html = [], hst = hst}
	# (a,{myturn=adone,html=ahtml,hst}) 
								= chosenTask {tst & tasknr = tasknr ++ [chosen.value + 1], myturn = True, html = [], hst = hst}
	| not adone					= ([a],{tst & tasknr = tasknr, myturn = milestoneReached, html = html <|.|> [choice.form <=> ( [Txt ("Task: " +++ chosenTaskName)] <|.|> ahtml)], hst = hst})
	# (alist,{myturn=finished,hst})		
								= checkAllTasks 0 [] {tst & html = [], hst = hst}
	| finished					= (alist,{tst & myturn = True, hst = hst})
	= ([a],{tst & myturn = milestoneReached, html = html <|.|> [choice.form <=> ([Txt ("Task: " +++ chosenTaskName)] <|.|> ahtml)], hst = hst})

	but i = LButton defpixel i

	checkAllTasks ctasknr alist tst=:{tasknr,html,hst}
	| ctasknr == length options	= (reverse alist,{tst & myturn = True, html = [], hst = hst})
	# task						= snd (options!!ctasknr)
	# (a,{myturn=adone,html,hst})
								= task {tst & tasknr = tasknr ++ [ctasknr + 1], myturn = True, html = [], hst = hst}
	| adone						= checkAllTasks (inc ctasknr) [a:alist] {tst & html = [], hst = hst}
	= ([],{tst & myturn = False, html = [], hst = hst})

	checkAnyTasks ctasknr tst=:{tasknr,myturn,html,hst}
	| ctasknr == length options	= (False,tst)
	# task						= snd (options!!ctasknr)
	# (a,{myturn=adone,html,hst})
								= task {tst & tasknr = tasknr ++ [ctasknr + 1], myturn = True, html = [], hst = hst}
	| adone						= (True,{tst & myturn = adone, html = html, hst = hst})
	= checkAnyTasks (inc ctasknr) {tst & html = [], hst = hst}
	
returnV :: a -> (Task a) | iData a 
returnV a  = \tst  -> (a,tst)				// return result task

returnTask :: a -> (Task a) | iData a 
returnTask a = \tst -> mkTask (returnTask` a) tst
where
	returnTask` a  tst=:{tasknr,myturn,html,hst}
	# editId			= "edit_" <+++ mkTaskNr tasknr
	# (editor,hst) 		= (mkEditForm  (Set,cdFormId tst.storageinfo editId a) hst)			// yes, read out current value, make editor passive
	= (editor.value,{tst & html = html <|.|> editor.form, hst = hst})		// return result task

returnVF :: a [BodyTag] -> (Task a) | iData a 
returnVF a bodytag = \tst=:{html} -> (a,{tst & html = html <|.|> bodytag})	// return result task

returnF :: [BodyTag] -> TSt -> TSt
returnF bodytag = \tst=:{html} -> {tst & html = html <|.|> bodytag}			// return result task

mkRTask :: String (Task a) *TSt -> ((Task a,Task a),*TSt) | iData a
mkRTask s task tst = let (a,b,c) = mkRTask` s task (incTask tst) in ((a,b),c)
where
	mkRTask` s task tst=:{tasknr = maintasknr,storageinfo} = (bossTask, workerTask s task,tst)
	where
		workerTask s task tst = mkTask (workerTask` s task) tst
		where
			workerTask` s task tst=:{tasknr,html,hst} 
			# (todo,hst)	= checkBossSignal id hst	// check whether lazy task evaluation has to be done
			| todo.value								// yes	
				# (a,{myturn=adone,html=ahtml,hst}) = task {tst & tasknr = maintasknr++[0], myturn = True, html = [], hst = hst}			// do task
				# (_,hst) 					= lazyTaskStore (\_ -> (adone,a)) hst	// store task and status
				= (a,{tst & html = html <|.|> if adone [] [Txt ("lazy task \"" +++ s +++ "\" activated:"),Br] <|.|> ahtml, hst = hst})
			= (createDefault,{tst & hst = hst})	// no
	
		bossTask tst = mkTask (bossTask`) tst
		where
			bossTask` tst=:{tasknr,html,hst} 
			# buttonId		= "getlt" <+++ mkTaskNr tasknr
			# (finbut,hst)  = simpleButton buttonId s (\_ -> True) hst	// button press will trigger related lazy task	
			# (todo,hst)	= checkBossSignal finbut.value hst			// set store True if button pressed
			# (result,hst)	= lazyTaskStore id hst						// inspect status task
			# (done,value)	= result.value
			| not done 		= (createDefault,{tst & myturn = False, html = html <|.|> if todo.value [Txt ("Waiting for task \"" +++ s +++ "\"..")] finbut.form, hst = hst})
			= (value,{tst & html = html <|.|>  [Txt ("Result of lazy task \"" +++ s +++ "\" :")], hst = hst})
	
		lazyTaskStore   fun = mkStoreForm (Init,cFormId storageinfo ("getLT" <+++ mkTaskNr maintasknr) (False,createDefault)) fun 
		checkBossSignal fun = mkStoreForm (Init,cFormId storageinfo ("setLT" <+++ mkTaskNr maintasknr) (fun False)) fun 
		
mkRTaskCall :: String b (b -> Task a) *TSt -> ((b -> Task a,Task a),*TSt) | iData a
												& iData b
mkRTaskCall  s initb batask tst = let (a,b,c) = mkRTaskCall` s (incTask tst) in ((a,b),c)
where
	mkRTaskCall` s tst=:{tasknr = maintasknr,storageinfo} = (bossTask, workerTask s,tst)
	where
		workerTask s tst = mkTask (workerTask` s) tst
		where
			workerTask` s tst=:{tasknr,html,hst}
			# (boss,hst)		= bossStore id hst		// check input from boss
			# (worker,hst)		= workerStore id hst	// check result from worker
			# (bdone,binput)	= boss.value
			# (wdone,wresult)	= worker.value
			| wdone				= (wresult,{tst & myturn = True, html = html <|.|>  [Txt ("Lazy task \"" +++ s +++ "\" completed:")], hst = hst})
			| bdone
				# (wresult,{myturn=wdone,html=whtml,hst}) = batask binput {tst & tasknr = maintasknr++[0], myturn = True, html = [], hst = hst}	// apply task to input from boss
				| wdone															// worker task finshed
					# (_,hst)	= workerStore (\_ -> (wdone,wresult)) hst		// store task and status
					= workerTask` s {tst &  hst = hst}				// complete as before
				= (createDefault,{tst & myturn = False, html = html <|.|> if wdone [] [Txt ("lazy task \"" +++ s +++ "\" activated:"),Br] <|.|> whtml, hst = hst})
			= (createDefault,{tst & myturn = False, html = html <|.|>[Txt ("Waiting for task \"" +++ s +++ "\"..")], hst = hst})
	
		bossTask b tst = mkTask bossTask` tst
		where
			bossTask` tst=:{tasknr,html,hst} 
			# (boss,hst)		= bossStore id hst		// check input from boss
			# (worker,hst)		= workerStore id hst	// check result from worker
			# (bdone,binput)	= boss.value
			# (wdone,wresult)	= worker.value
			| bdone && wdone	= (wresult,{tst & myturn = True, html = html <|.|>  [Txt ("Result of lazy task \"" +++ s +++ "\" :")], hst = hst})	// finished
			| not bdone
				# (_, hst)		= bossStore (\_ -> (True,b)) hst	// store b information to communicate to worker	
				= (createDefault,{tst & myturn = False, html = html <|.|>[Txt ("Waiting for task \"" +++ s +++ "\"..")], hst = hst})
			= (createDefault,{tst & myturn = False, html = html <|.|>[Txt ("Waiting for task \"" +++ s +++ "\"..")], hst = hst})	
	
		workerStore   fun = mkStoreForm (Init,cFormId storageinfo ("workerStore" <+++ mkTaskNr maintasknr) (False,createDefault)) fun 
		bossStore     fun = mkStoreForm (Init,cFormId storageinfo ("bossStore"   <+++ mkTaskNr maintasknr) (False,initb)) fun 
		
mkRDynTaskCall :: String a *TSt -> (((Task a) -> (Task a),Task a),*TSt) | iData a
mkRDynTaskCall s a tst = mkRDynTaskCall` (incTask tst)
where
	mkRDynTaskCall` tst=:{tasknr = maintasknr,storageinfo} = ((bossTask, workerTask),tst)
	where
		workerTask tst = mkTask workerTask` tst
		where
			workerTask` tst=:{tasknr,html,hst} 
			# (boss,hst)		= bossStore (False,defaulttask) hst		// check input from boss
			# (worker,hst)		= workerStore id hst					// check result from worker
			# (bdone,btask)		= boss.value
			# (wdone,wresult)	= worker.value
			| wdone				= (wresult,{tst & myturn = True, html = html <|.|>  [Txt ("Lazy task \"" +++ s +++ "\" completed:")], hst = hst})	
			| bdone
				# (wresult,{myturn=wdone,html=whtml,hst}) = btask {tst & tasknr = maintasknr++[0], myturn = True, html = [], hst = hst}	// apply task stored in memory
				| wdone															// worker task finshed
					# (_,hst)	= workerStore (\_ -> (wdone,wresult)) hst		// store task and status
					= workerTask` {tst & hst = hst} 							// complete as before
				= (createDefault,{tst & myturn = False, html = html <|.|> [Txt ("lazy task \"" +++ s +++ "\" activated:"),Br] <|.|> whtml, hst = hst})
			= (createDefault,{tst & myturn = False, html = html <|.|> [Txt ("Waiting for task \"" +++ s +++ "\"..")], hst = hst})		// no
	
		bossTask taska tst = mkTask bossTask` tst
		where
			bossTask` tst=:{tasknr,html,hst} 
			# (boss,hst)		= bossStore (False,defaulttask) hst		// check input from boss
			# (worker,hst)		= workerStore id hst					// check result from worker
			# (bdone,btask)		= boss.value
			# (wdone,wresult)	= worker.value
			| bdone && wdone	= (wresult,{tst & myturn = True, html = html <|.|>  [Txt ("Result of lazy task \"" +++ s +++ "\" :")], hst = hst})	// finished
			| not bdone
				# (_, hst)		= bossStore (True,taska) hst			// store b information to communicate to worker	
				= (createDefault,{tst & myturn = False, html = html <|.|>[Txt ("Task commited.\nWaiting for task \"" +++ s +++ "\"..")], hst = hst})
			= (createDefault,{tst & myturn = False, html = html <|.|>[Txt ("Waiting for task \"" +++ s +++ "\"..")], hst = hst})	
	
		workerStore   fun = mkStoreForm (Init,cFormId storageinfo ("workerStore" <+++ mkTaskNr maintasknr) (False,createDefault)) fun 

		bossStore (set,task) hst
		# (boss,hst) 			= mkStoreForm (Init,cFormId storageinfo ("bossStore" <+++ mkTaskNr maintasknr) initBoss) settask hst
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
	waitForTimeTask` tst=:{tasknr,html,hst}
	# taskId				= "iTask_timer_" <+++ mkTaskNr tasknr
	# (taskdone,hst) 		= mkStoreForm (Init,cFormId tst.storageinfo taskId (False,time)) id hst  			// remember time
	# ((currtime,_),hst)	= getTimeAndDate hst
	| currtime < time		= (time,{tst & myturn = True, html = html <|.|> [Txt ("Waiting for time " ):[toHtml time]], hst = hst})
	= (time,{tst & hst = hst})

waitForDateTask:: HtmlDate	-> (Task HtmlDate)
waitForDateTask date = \tst ->  mkTask waitForDateTask` tst
where
	waitForDateTask` tst=:{tasknr,html,hst}
	# taskId				= "iTask_date_" <+++ mkTaskNr tasknr
	# (taskdone,hst) 		= mkStoreForm (Init,cFormId tst.storageinfo taskId (False,date)) id hst  			// remember date
	# ((_,currdate),hst) 	= getTimeAndDate hst
	| currdate < date		= (date,{tst & myturn = True, html = html <|.|> [Txt ("Waiting for date " ):[toHtml date]], hst = hst})
	= (date,{tst & hst = hst})

// lifting section

appIData :: (IDataFun a) -> (Task a) | iData a
appIData idatafun = \tst -> mkTask (appIData` idatafun) tst
where
	appIData` idata tst=:{tasknr,html,hst}
	# (idata,hst) 							= idatafun hst
	# (_,{tasknr,myturn,html=ahtml,hst}) 	= STask  "Done" Void {tst & myturn = True, html = [],hst = hst}	
	= (idata.value,{tst & tasknr = tasknr,myturn 	= myturn, html = html <|.|> if myturn idata.form (idata.form <|.|> ahtml), hst = hst})

appHSt :: (HSt -> (a,HSt)) TSt -> (a,TSt)
appHSt hstfun tst=:{tasknr,myturn,html,hst}
# (a,hst) = hstfun hst
= (a,{tst & hst = hst})
	
// utility section

mkTaskNr [] = ""
mkTaskNr [i:is] = toString i <+++ "." <+++ mkTaskNr is

incTask tst = {tst & tasknr = incTasknr tst.tasknr}
where
	incTasknr [] = [0]
	incTasknr [i:is] = [i+1:is]

cFormId  {tasklife,taskstorage} s d = {nFormId  s d & lifespan = tasklife, storage = taskstorage}
cdFormId {tasklife,taskstorage} s d = {ndFormId s d & lifespan = tasklife, storage = taskstorage}

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

	