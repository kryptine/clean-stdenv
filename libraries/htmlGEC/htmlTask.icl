implementation module htmlTask

import StdEnv, StdHtml

derive gForm 	[], Niks
derive gUpd 	[], Niks
derive gParse 	Niks
derive gPrint 	Niks
derive gerda 	Niks


import dynamic_string, EncodeDecode





:: *TSt 		:== (([Int],Bool,[BodyTag]),HSt)   	// Task State: task nr, has this task to be done?, html code accumulator

:: Niks 		= Niks								// to make an empty task

startTask :: (Task a) *HSt -> ([BodyTag],HSt) | iData a 
startTask taska hst
# (_,((_,_,html),hst)) = taska (newTask,hst) 
= (html,hst)
where
	newTask = ([],True,[])

mkTask :: (*TSt -> *(a,*TSt)) -> (Task a) | iData a
mkTask mytask = \tst -> mkTask` tst
where
	mkTask` tst=:((i,myturn,html),hst)			
	# tst 			= incTask tst				// every task should first increment its tasknumber
	| not myturn	= (createDefault,tst)		// not active, return default value
	= mytask tst

incTask ((i,b,html),hst) = ((incTasknr i,b,html),hst)
where
	incTasknr [] = [0]
	incTasknr [i:is] = [i+1:is]



returnTask :: a -> (Task a) | iData a 
returnTask a = \tst -> mkTask (returnTask` a) tst
where
	returnTask` a ((i,myturn,html),hst)
	# editId			= "edit_" <+++ mkTaskNr i
	# (editor,hst) 		= (mkEditForm  (Set,ndFormId editId a) hst)			// yes, read out current value, make editor passive
	= (editor.value,((i,myturn,html <|.|> editor.form),hst))				// return result task

returnVF :: a [BodyTag] -> (Task a) | iData a 
returnVF a bodytag =
	\tst=:((i,myturn,html),hst) -> (a,((i,myturn,html <|.|> bodytag),hst))				// return result task

returnV :: a -> (Task a) | iData a 
returnV a  = 
	\tst  -> (a,tst)				// return result task

returnF :: [BodyTag] -> TSt -> TSt
returnF bodytag =
	\tst=:((i,myturn,html),hst) -> ((i,myturn,html <|.|> bodytag),hst)				// return result task

STask :: String a -> (Task a) | iData a 
STask prompt a = \tst -> mkTask (STask` a) tst
where
	STask` a ((i,myturn,html),hst)
	# taskId			= "Stask_" <+++ mkTaskNr i
	# editId			= "Sedit_" <+++ mkTaskNr i
	# buttonId			= mkTaskNr i
	# (taskdone,hst) 	= mkStoreForm (Init,sFormId taskId False) id hst  			// remember if the task done
	| taskdone.value																// test if task completed
		# (editor,hst) 	= (mkEditForm  (Init,ndFormId editId a) hst)				// yes, read out current value, make editor passive
		= (editor.value,((i,True,html <|.|> editor.form),hst))						// return result task
	# (editor,hst) 		= mkEditForm  (Init,sFormId editId a) hst					// no, read out current value from active editor
	# (finbut,hst)  	= simpleButton buttonId prompt (\_ -> True) hst				// add button for marking task as done
	# (taskdone,hst) 	= mkStoreForm (Init,sFormId taskId False) finbut.value hst 	// remember task status for next time
	| taskdone.value	= STask` a ((i,myturn,html),hst)							// task is now completed, handle as previously
	= (a,((i,taskdone.value,html <|.|> (editor.form ++ finbut.form)),hst))

CTask_pdmenu :: [(String,Task a)] -> (Task a) | iData a
CTask_pdmenu options = \tst -> mkTask (doCTask` options) tst
where
	doCTask` [] tst					= returnV createDefault tst	
	doCTask` options tst=:((i,myturn,html),hst)									// choose one subtask out of the list
	# (choice,hst)					= FuncMenu  (Init,sFormId ("Cpd_task_" <+++ mkTaskNr i) (0,[(txt,id) \\ txt <- map fst options]))	hst
	# (_,((i,adone,ahtml),hst)) 	= STask  "Cpd_Done" Niks ((i ++ [0],True,[]),hst)	
	| not adone						= (createDefault,((i,False,html <|.|> choice.form <|.|> ahtml),hst))
	# chosenIdx						= snd choice.value
	# chosenTask					= snd (options!!chosenIdx)
	# (a,((i,bdone,bhtml),hst)) 	= chosenTask ((i ++ [1],True,[]),hst)
	= (a,((i,adone&&bdone,html <|.|> bhtml),hst))

CTask_button :: [(String,Task a)] -> (Task a) | iData a
CTask_button options = \tst -> mkTask (doCTask` options) tst
where
	doCTask` [] tst					= returnV createDefault tst				
	doCTask` options tst=:((i,myturn,html),hst)									// choose one subtask out of the list
	# (choice,hst)					= TableFuncBut (Init,sFormId ("Cbt_task_" <+++ mkTaskNr i) [[(but txt,\_ -> n) \\ txt <- map fst options & n <- [0..]]]) hst
	# (chosen,hst)					= mkStoreForm  (Init,sFormId ("Cbt_chosen_" <+++ mkTaskNr i) -1) choice.value hst
	| chosen.value == -1			= (createDefault,((i,False,html <|.|> choice.form),hst))
	# chosenTask					= snd (options!!chosen.value)
	# (a,((i,adone,ahtml),hst)) 	= chosenTask ((i ++ [1],True,[]),hst)
	= (a,((i,adone,html <|.|> ahtml),hst))

	but i = LButton defpixel i

MCTask_ckbox :: [(String,Task a)] -> (Task [a]) | iData a
MCTask_ckbox options = \tst -> mkTask (MCTask_ckbox` options) tst
where
	MCTask_ckbox` [] tst			= returnV [] tst	
	MCTask_ckbox` options tst=:((i,myturn,html),hst)									// choose one subtask out of the list
	# (cboxes,hst)					= ListFuncCheckBox (Init,sFormId ("MC_check" <+++ mkTaskNr i) initCheckboxes) hst
	# optionsform					= cboxes.form <=|> [Txt text \\ (text,_) <- options]
	# (_,((i,adone,ahtml),hst)) 	= STask  "OK" Niks ((i,True,[]),hst)	
	| not adone						= (createDefault,((i,False,html <|.|> [optionsform] <|.|> ahtml),hst))
	# mytasks						= [option \\ option <- options & True <- snd cboxes.value]
	= STasks mytasks ((i,True,html),hst)

	initCheckboxes  = 
		[(CBNotChecked  text,  \ b bs id -> id) \\ (text,_) <- options]

STasks :: [(String,Task a)] -> (Task [a])| iData a 
STasks options = \tst -> mkTask (doSandTasks` options []) tst
where
	doSandTasks` [] accu tst 		= returnV (reverse accu) tst
	doSandTasks` [(txt,task):ts] accu tst=:((i,myturn,html),hst)	 
	# (a,((i,adone,ahtml),hst)) 	= task ((i,True,[]),hst)
	| not adone						= (reverse accu,((i,adone,html <|.|> [Txt ("Task: " +++ txt),Br] <|.|> ahtml),hst))
	= mkTask (doSandTasks` ts [a:accu]) ((i,adone,html <|.|> ahtml),hst)


PTask2 :: (Task a,Task b) -> (Task (a,b)) | iData a & iData b
PTask2 (taska,taskb) = \tst -> mkTask (PTask2` (taska,taskb)) tst
where
	PTask2` (taska,taskb) tst=:((i,myturn,html),hst)
	# (a,((_,adone,ahtml),hst)) 	= taska ((i ++ [0],True,[]),hst)	
	# (b,((_,bdone,bhtml),hst)) 	= taskb ((i ++ [1],True,[]),hst)
	= ((a,b),((i,adone&&bdone,html <|.|> ahtml <|.|> bhtml),hst))

PCTask2 :: (Task a,Task a) -> (Task a) | iData a 
PCTask2 (taska,taskb) = \tst -> mkTask (PCTask2` (taska,taskb)) tst
where
	PCTask2` (taska,taskb) tst=:((i,myturn,html),hst)
	# (a,((_,adone,ahtml),hst)) 	= taska ((i ++ [0],True,[]),hst)	
	# (b,((_,bdone,bhtml),hst)) 	= taskb ((i ++ [1],True,[]),hst)
	# (aorb,aorbdone,myhtml)		= if adone (a,adone,ahtml) (if bdone (b,bdone,bhtml) (a,False,ahtml <|.|> bhtml))
	= (aorb,((i,aorbdone,html <|.|> myhtml),hst))

PCTasks :: [(String,Task a)] -> (Task a) | iData a 
PCTasks options = \tst -> mkTask (PCTasks` options) tst
where
	PCTasks` [] tst 				= returnV createDefault tst
	PCTasks` tasks tst=:((i,myturn,html),hst)
	# (choice,hst)					= TableFuncBut (Init,sFormId ("Cbt_task_" <+++ mkTaskNr i) [[(but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]]) hst
	# (chosen,hst)					= mkStoreForm  (Init,sFormId ("Cbt_chosen_" <+++ mkTaskNr i) 0) choice.value hst
	# chosenTask					= snd (options!!chosen.value)
	# (a,((i,adone,ahtml),hst)) 	= chosenTask ((i ++ [chosen.value + 1],True,[]),hst)
	| not adone						= (a,((i,adone,html <|.|> [choice.form <=> ahtml]),hst))
	= (a,((i,adone,html <|.|> ahtml),hst))

	but i = LButton defpixel i

PTasks :: [(String,Task a)] -> (Task [a]) | iData a 
PTasks options = \tst -> mkTask (doPTasks` options) tst
where
	doPTasks` [] tst			= returnV [] tst
	doPTasks` options tst=:((i,myturn,html),hst)
	# (choice,hst)				= TableFuncBut (Init,sFormId ("Cbt_task_" <+++ mkTaskNr i) [[(but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]]) hst
	# (chosen,hst)				= mkStoreForm  (Init,sFormId ("Cbt_chosen_" <+++ mkTaskNr i) 0) choice.value hst
	# chosenTask				= snd (options!!chosen.value)
	# chosenTaskName			= fst (options!!chosen.value)
	# (a,((_,adone,ahtml),hst)) = chosenTask ((i ++ [chosen.value + 1],True,[]),hst)
	| not adone					= ([a],((i,adone,html <|.|> [choice.form <=> ( [Txt ("Task: " +++ chosenTaskName)] <|.|> ahtml)]),hst))
	# (alist,((_,finished,_),hst))		
								= checkAllTasks 0 [] ((i,myturn,[]),hst)
	| finished					= (alist,((i,finished,html),hst))
	= ([a],((i,finished,html <|.|> [choice.form <=> ([Txt ("Task: " +++ chosenTaskName)] <|.|> ahtml)]),hst))

	but i = LButton defpixel i

	checkAllTasks tasknr alist tst=:((i,myturn,_),hst)
	| tasknr == length options	= (reverse alist,((i,True,[]),hst))	
	# task						= snd (options!!tasknr)
	# (a,((_,adone,html),hst))	= task ((i ++ [tasknr + 1],True,[]),hst)
	| adone						= checkAllTasks (inc tasknr) [a:alist] ((i,myturn,[]),hst)
	= ([],((i,False,[]),hst))

PMilestoneTasks :: [(String,Task a)] -> (Task [a]) | iData a 
PMilestoneTasks options = \tst -> mkTask (PMilestoneTasks` options) tst
where
	PMilestoneTasks` [] tst			= returnV [] tst
	PMilestoneTasks` options tst=:((i,myturn,html),hst)
	# (choice,hst)				= TableFuncBut (Init,sFormId ("Cbt_task_" <+++ mkTaskNr i) [[(but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]]) hst
	# (chosen,hst)				= mkStoreForm  (Init,sFormId ("Cbt_chosen_" <+++ mkTaskNr i) 0) choice.value hst
	# chosenTask				= snd (options!!chosen.value)
	# chosenTaskName			= fst (options!!chosen.value)
	# (milestoneReached,(_,hst))= checkAnyTasks 0 ((i,myturn,[]),hst)
	# (a,((_,adone,ahtml),hst)) = chosenTask ((i ++ [chosen.value + 1],True,[]),hst)
	| not adone					= ([a],((i,milestoneReached,html <|.|> [choice.form <=> ( [Txt ("Task: " +++ chosenTaskName)] <|.|> ahtml)]),hst))
	# (alist,((_,finished,_),hst))		
								= checkAllTasks 0 [] ((i,myturn,[]),hst)
	| finished					= (alist,((i,finished,html),hst))
	= ([a],((i,milestoneReached,html <|.|> [choice.form <=> ([Txt ("Task: " +++ chosenTaskName)] <|.|> ahtml)]),hst))

	but i = LButton defpixel i

	checkAllTasks tasknr alist tst=:((i,myturn,_),hst)
	| tasknr == length options	= (reverse alist,((i,True,[]),hst))	
	# task						= snd (options!!tasknr)
	# (a,((_,adone,html),hst))	= task ((i ++ [tasknr + 1],True,[]),hst)
	| adone						= checkAllTasks (inc tasknr) [a:alist] ((i,myturn,[]),hst)
	= ([],((i,False,[]),hst))

	checkAnyTasks tasknr tst=:((i,myturn,_),hst)
	| tasknr == length options	= (False,tst)
	# task						= snd (options!!tasknr)
	# (a,((_,adone,html),hst))	= task ((i ++ [tasknr + 1],True,[]),hst)
	| adone						= (True,((i,adone,html),hst))
	= checkAnyTasks (inc tasknr) ((i,myturn,[]),hst)

STask_button 		:: String (Task a) 			-> (Task a) 	| iData a
STask_button s task = CTask_button [(s,task)]


mkRTask :: String (Task a) *TSt -> ((Task a,Task a),*TSt) | iData a
mkRTask s task tst = let (a,b,c) = mkRTask` s task (incTask tst) in ((a,b),c)
where
	mkRTask` s task tst=:((j,myturn,html),hst) = (bossTask, workerTask s task,tst)
	where
		workerTask s task tst = mkTask (workerTask` s task) tst
		where
			workerTask` s task tst=:((i,myturn,html),hst) 
			# (todo,hst)	= checkBossSignal id hst	// check whether lazy task evaluation has to be done
			| todo.value								// yes	
				# (a,((_,adone,ahtml),hst)) = task ((j++[0],True,[]),hst)			// do task
				# (_,hst) 					= lazyTaskStore (\_ -> (adone,a)) hst	// store task and status
				= (a,((i,myturn,html <|.|> if adone [] [Txt ("lazy task \"" +++ s +++ "\" activated:"),Br] <|.|> ahtml),hst))
			= (createDefault,((i,myturn,html),hst))		// no
	
		bossTask tst = mkTask (bossTask`) tst
		where
			bossTask` tst=:((i,myturn,html),hst) 
			# buttonId		= "getlt" <+++ mkTaskNr i
			# (finbut,hst)  = simpleButton buttonId s (\_ -> True) hst	// button press will trigger related lazy task	
			# (todo,hst)	= checkBossSignal finbut.value hst			// set store True if button pressed
			# (result,hst)	= lazyTaskStore id hst						// inspect status task
			# (done,value)	= result.value
			| not done 		= (createDefault,((i,False,html<|.|>if todo.value [Txt ("Waiting for task \"" +++ s +++ "\"..")] finbut.form),hst))
			= (value,((i,myturn,html <|.|>  [Txt ("Result of lazy task \"" +++ s +++ "\" :")]),hst))	
	
		lazyTaskStore   fun = mkStoreForm (Init,sFormId ("getLT" <+++ mkTaskNr j) (False,createDefault)) fun 
		checkBossSignal fun = mkStoreForm (Init,sFormId ("setLT" <+++ mkTaskNr j) (fun False)) fun 
		
mkRTaskCall :: String b (b -> Task a) *TSt -> ((b -> Task a,Task a),*TSt) | iData a
												& iData b
mkRTaskCall  s initb batask tst = let (a,b,c) = mkRTaskCall` s (incTask tst) in ((a,b),c)
where
	mkRTaskCall` s tst=:((j,myturn,html),hst) = (bossTask, workerTask s,tst)
	where
		workerTask s tst = mkTask (workerTask` s) tst
		where
			workerTask` s tst=:((i,myturn,html),hst) 
			# (boss,hst)	= bossStore id hst		// check input from boss
			# (worker,hst)	= workerStore id hst	// check result from worker
			# (bdone,binput)= boss.value
			# (wdone,wresult)= worker.value
			| wdone			= (wresult,((i,True,html<|.|>  [Txt ("Lazy task \"" +++ s +++ "\" completed:")]),hst))	
			| bdone
				# (wresult,((_,wdone,whtml),hst)) = batask binput ((j++[0],True,[]),hst)	// apply task to input from boss
				| wdone															// worker task finshed
					# (_,hst)	= workerStore (\_ -> (wdone,wresult)) hst		// store task and status
					= workerTask` s ((i,myturn,html),hst) 				// complete as before
				= (createDefault,((i,False,html <|.|> if wdone [] [Txt ("lazy task \"" +++ s +++ "\" activated:"),Br] <|.|> whtml),hst))
			= (createDefault,((i,False,html<|.|>[Txt ("Waiting for task \"" +++ s +++ "\"..")]),hst))		// no
	
		bossTask b tst = mkTask bossTask` tst
		where
			bossTask` tst=:((i,myturn,html),hst) 
			# (boss,hst)				= bossStore id hst		// check input from boss
			# (worker,hst)				= workerStore id hst	// check result from worker
			# (bdone,binput)= boss.value
			# (wdone,wresult)= worker.value
			| bdone && wdone			= (wresult,((i,True,html<|.|>  [Txt ("Result of lazy task \"" +++ s +++ "\" :")]),hst))	// finished
			| not bdone
				# (_, hst)		= bossStore (\_ -> (True,b)) hst	// store b information to communicate to worker	
				= (createDefault,((i,False,html<|.|>[Txt ("Waiting for task \"" +++ s +++ "\"..")]),hst))
			= (createDefault,((i,False,html<|.|>[Txt ("Waiting for task \"" +++ s +++ "\"..")]),hst))	
	
		workerStore   fun = mkStoreForm (Init,sFormId ("workerStore" <+++ mkTaskNr j) (False,createDefault)) fun 
		bossStore     fun = mkStoreForm (Init,sFormId ("bossStore"   <+++ mkTaskNr j) (False,initb)) fun 


		
mkRDynTaskCall :: String a *TSt -> (((Task a) -> (Task a),Task a),*TSt) | iData a
mkRDynTaskCall s a tst = mkRDynTaskCall` (incTask tst)
where
	mkRDynTaskCall` tst=:((j,myturn,html),hst) = ((bossTask, workerTask),tst)
	where
		workerTask tst = mkTask workerTask` tst
		where
			workerTask` tst=:((i,myturn,html),hst) 
			# (boss,hst)	= bossStore (False,defaulttask) hst		// check input from boss
			# (worker,hst)	= workerStore id hst				// check result from worker
			# (bdone,btask)	= boss.value
			# (wdone,wresult)= worker.value
			| wdone			= (wresult,((i,True,html<|.|>  [Txt ("Lazy task \"" +++ s +++ "\" completed:")]),hst))	
			| bdone
				# (wresult,((_,wdone,whtml),hst)) = btask ((j++[0],True,[]),hst)	// apply task stored in memory
				| wdone																// worker task finshed
					# (_,hst)	= workerStore (\_ -> (wdone,wresult)) hst			// store task and status
					= workerTask` ((i,myturn,whtml),hst) 							// complete as before
				= (createDefault,((i,False,html <|.|> [Txt ("lazy task \"" +++ s +++ "\" activated:"),Br] <|.|> whtml),hst))
			= (createDefault,((i,False,html<|.|>[Txt ("Waiting for task \"" +++ s +++ "\"..")]),hst))		// no
	
		bossTask taska tst = mkTask bossTask` tst
		where
			bossTask` tst=:((i,myturn,html),hst) 
			# (boss,hst)		= bossStore (False,defaulttask) hst		// check input from boss
			# (worker,hst)		= workerStore id hst			// check result from worker
			# (bdone,btask)		= boss.value
			# (wdone,wresult)	= worker.value
			| bdone && wdone	= (wresult,((i,True,html<|.|>  [Txt ("Result of lazy task \"" +++ s +++ "\" :")]),hst))	// finished
			| not bdone
				# (_, hst)		= bossStore (True,taska) hst	// store b information to communicate to worker	
				= (createDefault,((i,False,html<|.|>[Txt ("Task commited.\nWaiting for task \"" +++ s +++ "\"..")]),hst))
			= (createDefault,((i,False,html<|.|>[Txt ("Waiting for task \"" +++ s +++ "\"..")]),hst))	
	
		workerStore   fun = mkStoreForm (Init,sFormId ("workerStore" <+++ mkTaskNr j) (False,createDefault)) fun 

		bossStore (set,task) hst
		# (boss,hst) 		= mkStoreForm (Init,sFormId ("bossStore" <+++ mkTaskNr j) initBoss) settask hst
		# (bdone,encbtask)	= boss.value
		# btask				= case string_to_dynamic` encbtask of
									(mytask:: *TSt -> *(a^,*TSt)) -> mytask
									_ -> 	defaulttask
		= ({boss & value = (bdone,btask)},hst)
		where
			initBoss			= (False,convertTask defaulttask)
			settask				= if set (\_ -> (True,convertTask task)) id
			convertTask task 	= dynamic_to_string (dynamic task::*TSt -> *(a^,*TSt))

			string_to_dynamic` s = string_to_dynamic ( {s` \\ s` <-: s})

		defaulttask 		 = STask "DefaultTask" a
		
// time and date related tasks

waitForTimeTask:: HtmlTime	-> (Task HtmlTime)
waitForTimeTask time = \tst ->  mkTask waitForTimeTask` tst
where
	waitForTimeTask` tst=:((i,myturn,html),hst)
	# taskId			= "Stask_" <+++ mkTaskNr i
	# (taskdone,hst) 	= mkStoreForm (Init,sFormId taskId (False,time)) id hst  			// remember time
	# ((currtime,_),hst) = getTimeAndDate hst
	| currtime < time	= (time,((i,True,html <|.|> [Txt ("Waiting for time " ):[toHtml time]]),hst))
	= (time,((i,myturn,html),hst))

waitForDateTask:: HtmlDate	-> (Task HtmlDate)
waitForDateTask date = \tst ->  mkTask waitForDateTask` tst
where
	waitForDateTask` tst=:((i,myturn,html),hst)
	# taskId			= "Stask_" <+++ mkTaskNr i
	# (taskdone,hst) 	= mkStoreForm (Init,sFormId taskId (False,date)) id hst  			// remember date
	# ((_,currdate),hst) = getTimeAndDate hst
	| currdate < date	= (date,((i,True,html <|.|> [Txt ("Waiting for date " ):[toHtml date]]),hst))
	= (date,((i,myturn,html),hst))


// utility section

mkTaskNr [] = ""
mkTaskNr [i:is] = toString i <+++ "." <+++ mkTaskNr is


appIData :: (IDataFun a) -> (Task a) | iData a
appIData idatafun = \tst -> mkTask (appIData` idatafun) tst
where
	appIData` idata tst=:((i,myturn,html),hst)
	# (idata,hst) 				= idatafun hst
	# (_,((i,adone,ahtml),hst)) = STask  "Done" Niks ((i,True,[]),hst)	
	= (idata.value,((i,adone,html <|.|> if adone idata.form (idata.form <|.|> ahtml)),hst))

appHSt :: (HSt -> (a,HSt)) TSt -> (a,TSt)
appHSt hstfun tst=:((i,myturn,html),hst)
# (a,hst) = hstfun hst
= (a,((i,myturn,html),hst))
	
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

//K:: !x y -> y |  iData y
K x y = y
	