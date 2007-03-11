definition module htmlTask

// library for controlling interactive Tasks (iTask) based on iData
// (c) 2006,2007 MJP

import StdHtml

class iTrace a
		| gPrint{|*|}
		, default a
class default a
		| gUpd {|*|} a

:: *TSt										// task state
:: Task a		:== St *TSt a				// an interactive task
:: Void 		= Void						// for tasks returning non interesting results, won't show up in editors either
:: TClosure a 	= TClosure (Task a)			// to allow a task to deliver a task as result, so higher order tasks are possible

class setTaskAttr a :: !a *TSt -> *TSt

derive gForm 	Void						
derive gUpd 	Void, TClosure
derive gPrint 	Void, TClosure
derive gParse 	Void
derive gerda 	Void

/* Initiating the iTask library:
startTask		:: start function for iTasks for user with indicated id		
singleUserTask 	:: start wrapper function for single user 
multiUserTask 	:: start wrapper function for user with indicated id with option to switch between [0..users - 1]  
*/

startTask 		:: !Int !(Task a) 	!*HSt -> (a,[BodyTag],!*HSt) 	| default a
singleUserTask 	:: !(Task a) 		!*HSt -> (Html,*HSt) 			| default a
multiUserTask 	:: !Int !(Task a)  	!*HSt -> (Html,*HSt) 			| default a

/* promote iData editor
editTask		:: create an editor with button to finish task
(<<@)			:: set iData attribute globally for indicated (composition of) iTask(s) 
*/
editTask 		:: String a 	-> Task a							| iTrace, iData a 
(<<@) infix  3 	:: (Task a) b  	-> Task a 							| setTaskAttr b

instance setTaskAttr Lifespan, StorageFormat, Mode

/* monadic operators on iTasks
(=>>)			:: bind
(#>>)			:: bind, no argument passed
returnV			:: return the value
*/

(=>>) infix  1 	:: (Task a) (a -> Task b) 	-> Task b
(#>>) infixl 1 	:: (Task a) (Task b) 		-> Task b
returnV 		:: a 						-> Task a 				| iTrace a

/* prompting variants
(?>>)			:: prompt as long as task is active but not finished
(!>>)			:: prompt when task is activated
(<|)			:: repeat task (from scratch) as long as predicate does not hold, and give error message otherwise
returnVF		:: return the value and show the Html code specified
returnDisplay	:: return the value and show it in iData display format
*/

(?>>) infix  2 	:: [BodyTag] (Task a) 		-> Task a
(!>>) infix  2 	:: [BodyTag] (Task a) 		-> Task a			| default a
(<|)  infix  3 	:: (Task a) (a -> .Bool, a -> String) -> Task a | default a
returnVF 		:: a [BodyTag] 		  		-> Task a			| iTrace a
returnDisplay	:: a 						-> Task a			| gForm {|*|}, iTrace a

/* Assign tasks to user with indicated id
(@:)			:: will prompt who is waiting for task with give name
(@::)			:: no prompting
*/
(@:)  infix 4 	:: !(!Int,!String) (Task a)	-> (Task a)			| default a
(@::) infix 4 	:: !Int (Task a)		    -> (Task a)			| default a

/* Promote any TSt state transition function to an iTask:
newTask			:: to promote a user defined function to as task which is (possibly recursively) called when activated
newTaskGC		:: same, and garbage collect *all* (persistent) subtasks
newTaskStd		:: same, non optimized version will increase stack
repeatTask		:: infinitely repeating Task
repeatTaskGC	:: same, and garbage collect *all* (persistent) subtasks
repeatTaskStd	:: same, non optimized version will increase stack
*/

newTask 		:: !String (Task a) 		-> (Task a) 		| iTrace, iData a 
newTaskGC 		:: !String (Task a) 		-> (Task a) 		| iTrace, iData a 
newTaskStd 		:: !String (Task a) 		-> (Task a) 		| iTrace a
repeatTask		:: (Task a) 				-> Task a 			| iTrace, iData a
repeatTaskGC	:: (Task a) 				-> Task a 			| iTrace a
repeatTaskStd 	:: (Task a) 				-> Task a 			| iTrace a

/*	Sequential Tasks:
seqTask			:: do corresponding iTask when button pressed
seqTasks		:: do all iTasks one after another, task completed when all done
*/
seqTask			:: String (Task a)		-> (Task a) 			| iTrace a
seqTasks		:: [(String,Task a)] 	-> (Task [a])			| iTrace a

/* Choose Tasks
chooseTask		:: Choose one iTask from list, depending on button pressed
chooseTask_pdm	:: Choose one iTask from list, depending on pulldownmenu item selected
mchoiceTask		:: Multiple Choice of iTasks, depending on marked checkboxes
*/
chooseTask		:: [(String,Task a)] 	-> (Task a) 			| iTrace a
chooseTask_pdm 	:: [(String,Task a)] 	-> (Task a)	 			| iTrace a
mchoiceTasks 	:: [(String,Task a)] 	-> (Task [a]) 			| iTrace a

/* Do m Tasks parallel / interleaved and FINISH as soon as SOME Task completes:
orTask			:: do both iTasks in any order, task completed and ends as soon as first one done
orTask2			:: do both iTasks in any order, task completed and ends as soon as first one done
orTasks			:: do all  iTasks in any order, task completed and ends as soon as first one done
*/
orTask 			:: (Task a,Task a) 		-> (Task a) 			| iTrace a
orTask2			:: (Task a,Task b) 		-> (Task (EITHER a b)) 	| iTrace a & iTrace b
orTasks			:: [(String,Task a)] 	-> (Task a)				| iTrace a 

/* Do Tasks parallel / interleaved and FINISH when ALL Tasks done:
andTask			:: do both iTasks in any order (paralel), task completed when both done
andTasks		:: do all  iTasks in any order (paralel), task completed when all  done
mu_andTasks		:: assign task to indicated users, task completed when all done
*/
andTask			:: (Task a,Task b) 		-> (Task (a,b)) 		| iTrace a & iTrace b
andTasks		:: [(String,Task a)]	-> (Task [a])			| iTrace a
mu_andTasks 	:: String [(Int,Task a)]-> (Task [a]) 			| iTrace, iData a

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
appIData 		:: (IDataFun a) 					-> Task a 			| iTrace, iData a
appHSt 			:: (HSt -> (a,HSt)) 				-> Task a			| iTrace, iData a

/* Experimental!! DONT USE NOT FINISHED

Setting up communication channels between users:
mkRTask			:: Remote Task: split indicated task in two tasks: a calling task and a receiving task
					the caller will wait until the receiver has completed the task
mkRTaskCall 	:: as mkRTask, but the caller will provide input for the remote task
mkRDynTaskCall 	:: a remote task is set up, but the task that is created will be determined dynamically !
					BE CAREFUL: static dynamics are used here, will work only for one exectable.
PMilestoneTasks :: do all  iTasks in any order (paralel), task completed when all  done
					but continue with next task as soon as SOME Task completes
					string indicates which tasks have completed
*/
mkRTask 		:: String (Task a) *TSt -> ((Task a,Task a),*TSt) 		| iTrace, iData a 
mkRTaskCall		:: String b (b -> Task a) *TSt 
										-> ((b -> Task a,Task a),*TSt)	| iTrace, iData a & iData b
mkRDynTaskCall 	:: String a *TSt -> (((Task a) -> (Task a),Task a),*TSt)| iTrace, iData a

PMilestoneTasks :: [(String,Task a)] 	-> (Task [(String,a)]) 			| iTrace a

stopTask :: (Task a) -> (Task (Bool,TClosure a)) | iTrace a

