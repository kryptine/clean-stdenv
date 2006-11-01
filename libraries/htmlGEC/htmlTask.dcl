definition module htmlTask

// *experimental* library for controlling interactive Tasks (iTask) based on iData 
// (c) 2006 MJP

import StdHtml

:: *TSt										// task state
:: Task a		:== St *TSt a				// an interactive task
:: IDataFun a	:== St *HSt (Form a)		// an iData Form



/*
startTask		:: start function for iTasks

STask			:: a Sequential iTask
STask_button	:: do corresponding iTask when button pressed
STasks			:: do all iTasks one after another, task completed when all done

CTask_button	:: Choose one iTask from list, depending on button pressed
CTask_pdmenu	:: Choose one iTask from list, depending on pulldownmenu item selected

MCTask_ckbox	:: Multiple Choice of iTasks, depending on marked checkboxes

PCTask2			:: do both iTasks in any order (paralel), task completed as soon as first one done
PCTasks			:: do all  iTasks in any order (paralel), task completed as soon as first one done

PTask2			:: do both iTasks in any order (paralel), task completed when both done
PTask			:: do all  iTasks in any order (paralel), task completed when all  done

returnTask		:: return the value and show it, no IO action from the user required
returnVF		:: return the value and show the code, no IO action from the user required
returnV			:: return the value, no IO action from the user required
returnF			:: add html code

appIData		:: lift iData editors to iTask domain

mkTask			:: promote TSt state function to an iTask, i.e. task will only be called when it is its turn to be activated
					Needed for defining recursive tasks
mkRTask			:: Remote Task: split indicated task in two tasks: a calling task and a receiving task
					the caller will wait until the receiver has completed the task
mkRTaskCall 	:: as mkRTask, but the caller will provide input for the remote task
mkRDynTaskCall 	:: a remote task is set up, but the task that is created will be determined dynamically !
					BE CAREFUL: static dynamics are used here, will work only for one exectable.
*/

startTask 		:: (Task a) *HSt 		-> ([BodyTag],HSt) 				| iData, TC a 
mkTask 			:: (*TSt -> *(a,*TSt)) 	-> (Task a) 					| iData, TC a 


STask 			:: String a 			-> (Task a)						| iData, TC a 
STask_button	:: String (Task a)		-> (Task a) 					| iData, TC a
STasks			:: [(String,Task a)] 	-> (Task [a])					| iData, TC a 

CTask_button 	:: [(String,Task a)] 	-> (Task a) 					| iData, TC a
CTask_pdmenu 	:: [(String,Task a)] 	-> (Task a)	 					| iData, TC a

MCTask_ckbox 	:: [(String,Task a)] 	-> (Task [a]) 					| iData, TC a

PCTask2			:: (Task a,Task a) 		-> (Task a) 					| iData, TC a 
PCTasks			:: [(String,Task a)] 	-> (Task a)						| iData, TC a 

PTask2 			:: (Task a,Task b) 		-> (Task (a,b)) 				| iData, TC a & iData, TC b
PTasks 			:: [(String,Task a)]	-> (Task [a])					| iData, TC a 


returnTask 		:: a 					-> (Task a) 					| iData, TC a 
returnVF 		:: a [BodyTag] 			-> (Task a) 					| iData, TC a 
returnV 		:: a 					-> (Task a) 					| iData, TC a 
returnF 		:: [BodyTag] 			-> TSt -> TSt

appIData 		:: (IDataFun a) 		-> (Task a) 					| iData, TC a

mkRTask 		:: String (Task a) *TSt -> ((Task a,Task a),*TSt) 		| iData, TC a 
mkRTaskCall		:: String b (b -> Task a) *TSt 
										-> ((b -> Task a,Task a),*TSt)	| iData, TC a & iData, TC b
mkRDynTaskCall 	:: String a *TSt -> (((Task a) -> (Task a),Task a),*TSt) | iData, TC a
