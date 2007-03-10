definition module htmlTask

// library for controlling interactive Tasks (iTask) based on iData
// (c) 2006,2007 MJP

import StdHtml

:: *TSt										// task state
:: Task a		:== St *TSt a				// an interactive task
:: Void 		= Void						// for tasks returning non interesting results, won't show up in editors either

class setTaskAttr a :: !a *TSt -> *TSt

derive gForm 	Void						
derive gUpd 	Void
derive gParse 	Void
derive gPrint 	Void
derive gerda 	Void

/* Initiating the iTask library:
startTask		:: start function for iTasks for user with indicated id		
singleUserTask 	:: start wrapper function for single user 
multiUserTask 	:: start wrapper function for user with indicated id with option to switch between [0..users - 1]  
*/

startTask 		:: !Int !(Task a) 	!*HSt -> (a,[BodyTag],!*HSt) 	| iData a 
singleUserTask 	:: !(Task a) 		!*HSt -> (Html,*HSt) 			| iData a 
multiUserTask 	:: !Int !(Task a)  	!*HSt -> (Html,*HSt) 			| iData a 

/* promote iData editor
editTask		:: create an editor with button to finish task
(<<@)			:: set iData attribute globally for indicated (composition of) iTask(s) 
*/
editTask 		:: String a 	-> Task a							| iData a 
(<<@) infix  3 	:: (Task a) b  	-> Task a 							| setTaskAttr b

instance setTaskAttr Lifespan, StorageFormat, Mode

/* monadic operators on iTasks
(=>>)			:: bind
(#>>)			:: bind, no argument passed
returnV			:: return the value
*/

(=>>) infix  1 	:: (Task a) (a -> Task b) 	-> Task b
(#>>) infixl 1 	:: (Task a) (Task b) 		-> Task b
returnV 		:: a 						-> Task a 				| iData a 

/* prompting variants
(?>>)			:: prompt as long as task is active but not finished
(!>>)			:: prompt when task is activated
(<|)			:: repeat task (from scratch) as long as predicate does not hold, and give error message otherwise
returnVF		:: return the value and show the Html code specified
returnDisplay	:: return the value and show it in iData display format
*/

(?>>) infix  2 	:: [BodyTag] (Task a) 		-> Task a
(!>>) infix  2 	:: [BodyTag] (Task a) 		-> Task a				| iData a
(<|)  infix  3 	:: (Task a) (a -> .Bool, a -> String) -> Task a 	| iData a
returnVF 		:: a [BodyTag] 		  		-> Task a				| iData a 
returnDisplay	:: a 						-> Task a				| iData a 

/* Assign tasks to user with indicated id
(@:)			:: will prompt who is waiting for task with give name
(@::)			:: no prompting
*/
(@:)  infix 4 	:: !(!Int,!String) (Task a)	-> (Task a)			| iData a
(@::) infix 4 	:: !Int (Task a)		    -> (Task a)			| iData a

/* Promote any TSt state transition function to an iTask:
newTask			:: to promote a user defined function to as task which is (possibly recursively) called when activated
newTaskGC		:: same, and garbage collect *all* (persistent) subtasks
newTaskStd		:: same, non optimized version will increase stack
repeatTask		:: infinitely repeating Task
repeatTaskGC	:: same, and garbage collect *all* (persistent) subtasks
repeatTaskStd	:: same, non optimized version will increase stack
*/

newTask 		:: !String (Task a) 		-> (Task a) 		| iData a 
newTaskGC 		:: !String (Task a) 		-> (Task a) 		| iData a 
newTaskStd 		:: !String (Task a) 		-> (Task a) 		| iData a 
repeatTask		:: (Task a) 				-> Task a 			| iData a
repeatTaskGC	:: (Task a) 				-> Task a 			| iData a
repeatTaskStd 	:: (Task a) 				-> Task a 			| iData a

/*	Sequential Tasks:
SeqTask			:: do corresponding iTask when button pressed
SeqTasks		:: do all iTasks one after another, task completed when all done
*/
SeqTask			:: String (Task a)		-> (Task a) 			| iData a
SeqTasks		:: [(String,Task a)] 	-> (Task [a])			| iData a 

/* Choose Tasks
ChooseTask		:: Choose one iTask from list, depending on button pressed
ChooseTask_pdm	:: Choose one iTask from list, depending on pulldownmenu item selected
MChoiceTask		:: Multiple Choice of iTasks, depending on marked checkboxes
*/
ChooseTask		:: [(String,Task a)] 	-> (Task a) 			| iData a
ChooseTask_pdm 	:: [(String,Task a)] 	-> (Task a)	 			| iData a
MChoiceTasks 	:: [(String,Task a)] 	-> (Task [a]) 			| iData a

/* Do m Tasks parallel / interleaved and FINISH as soon as SOME Task completes:
OrTask			:: do both iTasks in any order, task completed and ends as soon as first one done
OrTasks			:: do all  iTasks in any order, task completed and ends as soon as first one done
*/
OrTask 			:: (Task a,Task a) 		-> (Task a) 			| iData a
OrTask2			:: (Task a,Task b) 		-> (Task (EITHER a b)) 	| iData a & iData b
OrTasks			:: [(String,Task a)] 	-> (Task a)				| iData a 

/* Do Tasks parallel / interleaved and FINISH when ALL Tasks done:
PTask2			:: do both iTasks in any order (paralel), task completed when both done
PTasks			:: do all  iTasks in any order (paralel), task completed when all  done
PMilestoneTasks :: do all  iTasks in any order (paralel), task completed when all  done
					but continue with next task as soon as SOME Task completes
					string indicates which tasks have completed
PmuTasks		:: assign task to indicated users, task completed when all done
*/
AndTask			:: (Task a,Task b) 		-> (Task (a,b)) 		| iData a & iData b
AndTasks		:: [(String,Task a)]	-> (Task [a])			| iData a 
muAndTasks 		:: String [(Int,Task a)]-> (Task [a]) 			| iData a 

/* Time and Date management:
waitForTimeTask	:: Task is done when time has come
waitForTimerTask:: Task is done when specified amount of time has passed 
waitForDateTask	:: Task is done when date has come
*/
waitForTimeTask	:: HtmlTime				-> (Task HtmlTime)
waitForTimerTask:: HtmlTime				-> (Task HtmlTime)
waitForDateTask	:: HtmlDate				-> (Task HtmlDate)

/* Operations on Task state
taskId			:: id assigned to task
userId			:: id of application user
addHtml			:: add html code
*/

taskId			:: TSt -> (Int,TSt)
userId 			:: TSt -> (Int,TSt)
addHtml 		:: [BodyTag] TSt -> TSt

/* Lifting to iTask domain
(*>>)			:: lift functions of type (TSt -> (a,TSt)) to iTask domain 
(@>>)			:: lift functions of (TSt -> TSt) to iTask domain 
appIData		:: lift iData editors to iTask domain
appHSt			:: lift HSt domain to TSt domain
*/
(*>>) infix 4 	:: (TSt -> (a,TSt)) (a -> Task b) 	-> Task b
(@>>) infix 4 	:: (TSt -> TSt) (Task a) 			-> Task a
appIData 		:: (IDataFun a) 					-> Task a 			| iData a
appHSt 			:: (HSt -> (a,HSt)) 				-> Task a			| iData a




/* Experimental!! DONT USE NOT FINISHED

Setting up communication channels between users:
mkRTask			:: Remote Task: split indicated task in two tasks: a calling task and a receiving task
					the caller will wait until the receiver has completed the task
mkRTaskCall 	:: as mkRTask, but the caller will provide input for the remote task
mkRDynTaskCall 	:: a remote task is set up, but the task that is created will be determined dynamically !
					BE CAREFUL: static dynamics are used here, will work only for one exectable.
*/
mkRTask 		:: String (Task a) *TSt -> ((Task a,Task a),*TSt) 		| iData a 
mkRTaskCall		:: String b (b -> Task a) *TSt 
										-> ((b -> Task a,Task a),*TSt)	| iData a & iData b
mkRDynTaskCall 	:: String a *TSt -> (((Task a) -> (Task a),Task a),*TSt)| iData a

PMilestoneTasks :: [(String,Task a)] 	-> (Task [(String,a)]) 	| iData a 

