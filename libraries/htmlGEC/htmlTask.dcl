definition module htmlTask

// library for controlling interactive Tasks (iTask) based on iData
// (c) 2006,2007 MJP

import htmlSettings, htmlButtons

:: *TSt										// task state
:: Task a		:== St *TSt a				// an interactive task
:: Void 		= Void						// for tasks returning non interesting results, won't show up in editors either

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

startTask 		:: !Int !(Task a) 	!*HSt -> (a,[BodyTag],!*HSt) 	| iCreate a
singleUserTask 	:: !(Task a) 		!*HSt -> (Html,*HSt) 			| iCreate a
multiUserTask 	:: !Int !(Task a)  	!*HSt -> (Html,*HSt) 			| iCreate a

/* promote iData editor
editTask		:: create an editor with button to finish task
(<<@)			:: set iData attribute globally for indicated (composition of) iTask(s) 
*/
editTask 		:: String a 	-> Task a							| iData a 
(<<@) infix  3 	:: (Task a) b  	-> Task a 							| setTaskAttr b

class 	 setTaskAttr a :: !a *TSt -> *TSt
instance setTaskAttr Lifespan, StorageFormat, Mode

/* monadic operators on iTasks
(=>>)			:: bind
(#>>)			:: bind, no argument passed
return_V		:: return the value
*/

(=>>) infix  1 	:: (Task a) (a -> Task b) 	-> Task b
(#>>) infixl 1 	:: (Task a) (Task b) 		-> Task b
return_V 		:: a 						-> Task a 				| iCreateAndPrint a

/* prompting variants
(?>>)			:: prompt as long as task is active but not finished
(!>>)			:: prompt when task is activated
(<|)			:: repeat task (from scratch) as long as predicate does not hold, and give error message otherwise
return_VF		:: return the value and show the Html code specified
return_D		:: return the value and show it in iData display format
*/

(?>>) infix  2 	:: [BodyTag] (Task a) 		-> Task a
(!>>) infix  2 	:: [BodyTag] (Task a) 		-> Task a			| iCreate a
(<|)  infix  3 	:: (Task a) (a -> .Bool, a -> String) -> Task a | iCreate a
return_VF 		:: a [BodyTag] 		  		-> Task a			| iCreateAndPrint a
return_D		:: a 						-> Task a			| gForm {|*|}, iCreateAndPrint a

/* Assign tasks to user with indicated id
(@:)			:: will prompt who is waiting for task with give name
(@::)			:: same, default task name given
*/
(@:)  infix 4 	:: !(!String,!Int) (Task a)	-> (Task a)			| iCreate a
(@::) infix 4 	:: !Int (Task a)		    -> (Task a)			| iCreate a

/* Promote any TSt state transition function to an iTask:
newTask			:: to promote a user defined function to as task which is (possibly recursively) called when activated
newTask_GC		:: same, and garbage collect *all* (persistent) subtasks
newTask_Std		:: same, non optimized version will increase stack
repeatTask		:: infinitely repeating Task
repeatTask_GC	:: same, and garbage collect *all* (persistent) subtasks
repeatTask_Std	:: same, non optimized version will increase stack
*/

newTask 		:: !String (Task a) 		-> (Task a) 		| iData a 
newTask_GC 		:: !String (Task a) 		-> (Task a) 		| iData a 
newTask_Std 	:: !String (Task a) 		-> (Task a) 		| iCreateAndPrint a
repeatTask		:: (Task a) 				-> Task a 			| iData a
repeatTask_GC	:: (Task a) 				-> Task a 			| iCreateAndPrint a
repeatTask_Std 	:: (Task a) 				-> Task a 			| iCreateAndPrint a

/*	Sequencing Tasks:
seqTasks		:: do all iTasks one after another, task completed when all done
*/
seqTasks		:: [(String,Task a)] 	-> (Task [a])			| iCreateAndPrint a

/* Choose Tasks
buttonTask		:: Choose the iTask when button pressed
chooseTask		:: Choose one iTask from list, depending on button pressed
chooseTask_pdm	:: Choose one iTask from list, depending on pulldownmenu item selected
mchoiceTask		:: Multiple Choice of iTasks, depending on marked checkboxes
*/
buttonTask		:: String (Task a)		-> (Task a) 			| iCreateAndPrint a
chooseTask		:: [(String,Task a)] 	-> (Task a) 			| iCreateAndPrint a
chooseTask_pdm 	:: [(String,Task a)] 	-> (Task a)	 			| iCreateAndPrint a
mchoiceTasks 	:: [(String,Task a)] 	-> (Task [a]) 			| iCreateAndPrint a

/* Do m Tasks parallel / interleaved and FINISH as soon as SOME Task completes:
orTask			:: do both iTasks in any order, task completed and ends as soon as first one done
orTask2			:: do both iTasks in any order, task completed and ends as soon as first one done
orTasks			:: do all  iTasks in any order, task completed and ends as soon as first one done
*/
orTask 			:: (Task a,Task a) 		-> (Task a) 			| iCreateAndPrint a
orTask2			:: (Task a,Task b) 		-> (Task (EITHER a b)) 	| iCreateAndPrint a & iCreateAndPrint b
orTasks			:: [(String,Task a)] 	-> (Task a)				| iCreateAndPrint a 

/* Do Tasks parallel / interleaved and FINISH when ALL Tasks done:
andTask			:: do both iTasks in any order (interleaved), task completed when both done
andTasks		:: do all  iTasks in any order (interleaved), task completed when all  done
andTasks_mu		:: assign task to indicated users, task completed when all done
*/
andTask			:: (Task a,Task b) 		-> (Task (a,b)) 		| iCreateAndPrint a & iCreateAndPrint b
andTasks		:: [(String,Task a)]	-> (Task [a])			| iCreateAndPrint a
andTasks_mu 	:: String [(Int,Task a)]-> (Task [a]) 			| iData a

/* Do not yet use these tasks when you garbage collect tasks !!
andTasks_mstone :: do all iTasks in any order (interleaved), task completed when all done
					but continue with next task as soon as one of the tasks is completed
					string indicates which task delivered what
*/
andTasks_mstone :: [(String,Task a)] 	-> (Task [(String,a)]) 		| iCreateAndPrint a

/* Time and Date management:
waitForTimeTask	:: Task is done when time has come
waitForTimerTask:: Task is done when specified amount of time has passed 
waitForDateTask	:: Task is done when date has come
*/
waitForTimeTask	:: HtmlTime				-> (Task HtmlTime)
waitForTimerTask:: HtmlTime				-> (Task HtmlTime)
waitForDateTask	:: HtmlDate				-> (Task HtmlDate)

/* Do not yet use these tasks when you garbage collect tasks !!
sharedTask		:: either a finished task or an interrupted Task (when boolean Task yields True) is returned
				   the work done so far it returned and can be can be continued somewhere else
*/
:: TClosure a 	= TClosure (Task a)			

sharedTask 		:: (Task Bool) (Task a) -> (Task (Bool,TClosure a)) | iCreateAndPrint a

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


