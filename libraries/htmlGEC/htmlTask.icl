implementation module htmlTask

import StdEnv, StdHtml

derive gForm [], Niks
derive gUpd [], Niks
derive gParse Niks
derive gPrint Niks

:: *TSt 		:== (([Int],Bool,[BodyTag]),HSt)   	// Task State: task nr, has this task to be done?, html code accumulator

:: Niks 		= Niks								// to make an empty task


startTask :: (Task a) *HSt -> ([BodyTag],HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
startTask taska hst
# (_,((_,_,html),hst)) = taska (newTask,hst) 
= (html,hst)
where
	newTask = ([],True,[])

mkTask :: (*TSt -> *(a,*TSt)) -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
mkTask mytask = \tst -> mkTask` tst
where
	mkTask` tst=:((i,myturn,html),hst)										// choose one subtask out of the list
	# tst 			= incTask tst				// every task should first increment its tasknumber
	| not myturn	= (createDefault,tst)					// not active, return value
	= mytask tst
	where
		incTask ((i,b,html),hst) = ((incTasknr i,b,html),hst)
		where
			incTasknr [] = [0]
			incTasknr [i:is] = [i+1:is]

returnTask :: a -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
returnTask a = \tst -> mkTask (returnTask` a) tst
where
	returnTask` a ((i,myturn,html),hst)
	# editId			= "edit_" <+++ mkTaskNr i
	# (editor,hst) 		= (mkEditForm  (Set,ndFormId editId a) hst)			// yes, read out current value, make editor passive
	= (editor.value,((i,myturn,html <|.|> editor.form),hst))				// return result task

returnVF :: a [BodyTag] -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
returnVF a bodytag =
	\tst=:((i,myturn,html),hst) -> (a,((i,myturn,html <|.|> bodytag),hst))				// return result task

returnV :: a -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
returnV a  = 
	\tst  -> (a,tst)				// return result task

returnF :: [BodyTag] -> TSt -> TSt
returnF bodytag =
	\tst=:((i,myturn,html),hst) -> ((i,myturn,html <|.|> bodytag),hst)				// return result task

STask :: String a -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
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

CTask_pdmenu :: [(String,Task a)] -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
CTask_pdmenu options = \tst -> mkTask (doCTask` options) tst
where
	doCTask` options tst=:((i,myturn,html),hst)									// choose one subtask out of the list
	# (choice,hst)					= FuncMenu  (Init,sFormId ("Cpd_task_" <+++ mkTaskNr i) (0,[(txt,id) \\ txt <- map fst options]))	hst
	# (_,((i,adone,ahtml),hst)) 	= STask  "Cpd_Done" Niks ((i ++ [0],True,[]),hst)	
	| not adone						= (createDefault,((i,False,html <|.|> choice.form <|.|> ahtml),hst))
	# chosenIdx						= snd choice.value
	# chosenTask					= snd (options!!chosenIdx)
	# (a,((i,bdone,bhtml),hst)) 	= chosenTask ((i ++ [1],True,[]),hst)
	= (a,((i,adone&&bdone,html <|.|> bhtml),hst))

CTask_button :: [(String,Task a)] -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
CTask_button options = \tst -> mkTask (doCTask` options) tst
where
	doCTask` options tst=:((i,myturn,html),hst)									// choose one subtask out of the list
	# (choice,hst)					= TableFuncBut (Init,sFormId ("Cbt_task_" <+++ mkTaskNr i) [[(but txt,\_ -> n) \\ txt <- map fst options & n <- [0..]]]) hst
	# (chosen,hst)					= mkStoreForm  (Init,sFormId ("Cbt_chosen_" <+++ mkTaskNr i) -1) choice.value hst
	| chosen.value == -1			= (createDefault,((i,False,html <|.|> choice.form),hst))
	# chosenTask					= snd (options!!chosen.value)
	# (a,((i,adone,ahtml),hst)) 	= chosenTask ((i ++ [1],True,[]),hst)
	= (a,((i,adone,html <|.|> ahtml),hst))

	but i = LButton defpixel i

MCTask_ckbox :: [(String,Task a)] -> (Task [a]) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
MCTask_ckbox options = \tst -> mkTask (MCTask_ckbox options) tst
where
	MCTask_ckbox`` options tst=:((i,myturn,html),hst)									// choose one subtask out of the list
	# (cboxes,hst)					= ListFuncCheckBox (Init,sFormId ("MC_check" <+++ mkTaskNr i) initCheckboxes) hst
	# optionsform					= cboxes.form <=|> [Txt text \\ (text,_) <- options]
	# (_,((i,adone,ahtml),hst)) 	= STask  "OK" Niks ((i,True,[]),hst)	
	| not adone						= (createDefault,((i,False,html <|.|> [optionsform] <|.|> ahtml),hst))
	# mytasks						= [option \\ option <- options & True <- snd cboxes.value]
	= STasks mytasks ((i,True,html),hst)

	initCheckboxes  = 
		[(CBNotChecked  text,  \ b bs id -> id) \\ (text,_) <- options]

STasks :: [(String,Task a)] -> (Task [a])| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
STasks options = \tst -> mkTask (doSandTasks` options []) tst
where
	doSandTasks` [] accu tst 		= returnV (reverse accu) tst
	doSandTasks` [(txt,task):ts] accu tst=:((i,myturn,html),hst)	 
	# (a,((i,adone,ahtml),hst)) 	= task ((i,True,[]),hst)
	| not adone						= (reverse accu,((i,adone,html <|.|> [Txt ("Task: " +++ txt),Br] <|.|> ahtml),hst))
	= mkTask (doSandTasks` ts [a:accu]) ((i,adone,html <|.|> ahtml),hst)


PTask2 :: (Task a,Task b) -> (Task (a,b)) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a & gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
PTask2 (taska,taskb) = \tst -> mkTask (PTask2` (taska,taskb)) tst
where
	PTask2` (taska,taskb) tst=:((i,myturn,html),hst)
	# (a,((_,adone,ahtml),hst)) 	= taska ((i ++ [0],True,[]),hst)	
	# (b,((_,bdone,bhtml),hst)) 	= taskb ((i ++ [1],True,[]),hst)
	= ((a,b),((i,adone&&bdone,html <|.|> ahtml <|.|> bhtml),hst))

PCTask2 :: (Task a,Task a) -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
PCTask2 (taska,taskb) = \tst -> mkTask (doPorTask` (taska,taskb)) tst
where
	doPorTask` (taska,taskb) tst=:((i,myturn,html),hst)
	# (a,((_,adone,ahtml),hst)) 	= taska ((i ++ [0],True,[]),hst)	
	# (b,((_,bdone,bhtml),hst)) 	= taskb ((i ++ [1],True,[]),hst)
	# (aorb,aorbdone,myhtml)		= if adone (a,adone,ahtml) (if bdone (b,bdone,bhtml) (a,False,ahtml <|.|> bhtml))
	= (aorb,((i,aorbdone,html <|.|> myhtml),hst))


PCTasks :: [(String,Task a)] -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
PCTasks options = \tst -> mkTask (doPorTasks` options) tst
where
	doPorTasks` tasks tst=:((i,myturn,html),hst)
	# (choice,hst)					= TableFuncBut (Init,sFormId ("Cbt_task_" <+++ mkTaskNr i) [[(but txt,\_ -> n)] \\ txt <- map fst options & n <- [0..]]) hst
	# (chosen,hst)					= mkStoreForm  (Init,sFormId ("Cbt_chosen_" <+++ mkTaskNr i) 0) choice.value hst
	# chosenTask					= snd (options!!chosen.value)
	# (a,((i,adone,ahtml),hst)) 	= chosenTask ((i ++ [chosen.value + 1],True,[]),hst)
	| not adone						= (a,((i,adone,html <|.|> [choice.form <=> ahtml]),hst))
	= (a,((i,adone,html <|.|> ahtml),hst))

	but i = LButton defpixel i

STask_button 		:: String (Task a) 			-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
STask_button s task = CTask_button [(s,task)]
// utility section

mkTaskNr [] = ""
mkTaskNr [i:is] = toString i <+++ "." <+++ mkTaskNr is


appIData :: (IDataFun a) -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
appIData idatafun = \tst -> mkTask (appIData` idatafun) tst
where
	appIData` idata tst=:((i,myturn,html),hst)
	# (idata,hst) 				= idatafun hst
	# (_,((i,adone,ahtml),hst)) = STask  "Done" Niks ((i,True,[]),hst)	
	= (idata.value,((i,adone,html <|.|> if adone idata.form (idata.form <|.|> ahtml)),hst))
	
	