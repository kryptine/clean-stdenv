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

doSTask :: String a -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
doSTask prompt a = \tst -> mkTask (doTask` a) tst
where
	doTask` a ((i,myturn,html),hst)
	# taskId			= "task_" <+++ mkTaskNr i
	# editId			= "edit_" <+++ mkTaskNr i
	# buttonId			= mkTaskNr i
	# (taskdone,hst) 	= mkStoreForm (Init,nFormId taskId False) id hst  			// remember if the task done
	| taskdone.value																// test if task completed
		# (editor,hst) 	= (mkEditForm  (Init,ndFormId editId a) hst)				// yes, read out current value, make editor passive
		= (editor.value,((i,True,html <|.|> editor.form),hst))						// return result task
	# (editor,hst) 		= mkEditForm  (Init,nFormId editId a) hst					// no, read out current value from active editor
	# (finbut,hst)  	= simpleButton buttonId prompt (\_ -> True) hst				// add button for marking task as done
	# (taskdone,hst) 	= mkStoreForm (Init,nFormId taskId False) finbut.value hst 	// remember task status for next time
	| taskdone.value	= doTask` a ((i,myturn,html),hst)							// task is now completed, handle as previously
	= (a,((i,taskdone.value,html <|.|> (editor.form ++ finbut.form)),hst))

doCpdmenuTask :: [(String,Task a)] -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
doCpdmenuTask options = \tst -> mkTask (doCTask` options) tst
where
	doCTask` options tst=:((i,myturn,html),hst)									// choose one subtask out of the list
	# taskId						= "task_" <+++ mkTaskNr i
	# (choice,hst)					= FuncMenu  (Init,nFormId taskId (0,[(txt,id) \\ txt <- map fst options]))	hst
	# (_,((i,adone,ahtml),hst)) 	= doSTask  "Done" Niks ((i ++ [0],True,[]),hst)	
	| not adone						= (createDefault,((i,False,html <|.|> choice.form <|.|> ahtml),hst))
	# chosenIdx						= snd choice.value
	# chosenTask					= snd (options!!chosenIdx)
	# (a,((i,bdone,bhtml),hst)) 	= chosenTask ((i ++ [1],True,[]),hst)
	= (a,((i,adone&&bdone,html <|.|> bhtml),hst))


doCbuttonTask :: [(String,Task a)] -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
doCbuttonTask options = \tst -> mkTask (doCTask` options) tst
where
	doCTask` options tst=:((i,myturn,html),hst)									// choose one subtask out of the list
	# taskId						= "task_" <+++ mkTaskNr i
	# (choice,hst)					= TableFuncBut  (Init,nFormId taskId [[(but txt,\_ -> n) \\ txt <- map fst options & n <- [0..]]]) hst
	# (chosen,hst)					= mkStoreForm (Init,nFormId ("chosen_" <+++ mkTaskNr i) -1) choice.value hst
	| chosen.value == -1			= (createDefault,((i,False,html <|.|> choice.form),hst))
	# chosenTask					= snd (options!!chosen.value)
	# (a,((i,adone,ahtml),hst)) 	= chosenTask ((i ++ [1],True,[]),hst)
	= (a,((i,adone,html <|.|> ahtml),hst))

	but i = LButton defpixel i

doPTask :: (Task a,Task b) -> (Task (a,b)) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a & gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
doPTask (taska,taskb) = \tst -> mkTask (doPTask` (taska,taskb)) tst
where
	doPTask` (taska,taskb) tst=:((i,myturn,html),hst)
	# (a,((_,adone,ahtml),hst)) 	= taska ((i ++ [0],True,[]),hst)	
	# (b,((_,bdone,bhtml),hst)) 	= taskb ((i ++ [1],True,[]),hst)
	= ((a,b),((i,adone&&bdone,html <|.|> ahtml <|.|> bhtml),hst))


// utility section

mkTaskNr [] = ""
mkTaskNr [i:is] = toString i <+++ "." <+++ mkTaskNr is


appIData :: (IDataFun a) -> (Task a) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
appIData idatafun = \tst -> mkTask (appIData` idatafun) tst
where
	appIData` idata tst=:((i,myturn,html),hst)
	# (idata,hst) 				= idatafun hst
	# (_,((i,adone,ahtml),hst)) = doSTask  "Done" Niks ((i,True,[]),hst)	
	= (idata.value,((i,adone,html <|.|> if adone idata.form (idata.form <|.|> ahtml)),hst))
	
	