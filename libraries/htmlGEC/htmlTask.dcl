definition module htmlTask

// library for controlling interactive Tasks (iTask) based on iData
// (c) 2006,2007 MJP

import StdHtml

:: *TSt										// task state
:: Task a		:== St *TSt a				// an interactive task
:: Void 		= Void						// for tasks returning non interesting results

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
STask			:: create an editor with button to finish task
*/
STask 			:: String a -> (Task a)								| iData a 

/* monadic shorthands
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
(<|)			:: repeat task as long as predicate does not hold, and give error message otherwise
returnTask		:: return the value and show it 
returnVF		:: return the value and show the Html code specified
returnF			:: add html code
*/

(?>>) infix  2 	:: [BodyTag] (Task a) 		-> Task a
(!>>) infix  2 	:: [BodyTag] (Task a) 		-> Task a
(<|)  infix  3 	:: (Task a) (a -> .Bool, a -> String) -> Task a 	| iData a
returnTask 		:: a 						-> Task a				| iData a 
returnVF 		:: a [BodyTag] 		  		-> Task a				| iData a 
returnF 		:: [BodyTag] TSt -> TSt

/* Promote any TSt state transition function to an iTask:
recTask			:: to create a function which can recursively be called as a task
repeatTask		:: infinitely repeating Task
recTaskGC		:: same, and garbage collect *all* (persistent) subtasks
repeatTaskGC	:: same, and garbage collect *all* (persistent) subtasks
recTask2		:: same, non optimized version will increase stack
repeatTask2		:: same, non optimized version will increase stack
*/

recTask 		:: !String (Task a) 		-> (Task a) 		| iData a 
repeatTask		:: (Task a) 				-> Task a 			| iData a

recTaskGC 		:: !String (Task a) 		-> (Task a) 		| iData a 
repeatTaskGC	:: (Task a) 				-> Task a 			| iData a

recTask2 		:: !String (Task a) 		-> (Task a) 		| iData a 
repeatTask2 	:: (Task a) 				-> Task a 			| iData a



/*	Sequential Tasks:
STask			:: a Sequential iTask
STask_button	:: do corresponding iTask when button pressed
STasks			:: do all iTasks one after another, task completed when all done
*/
STask_button	:: String (Task a)		-> (Task a) 			| iData a
STasks			:: [(String,Task a)] 	-> (Task [a])			| iData a 

/* Choose one Task out of n:
CTask			:: Choose one iTask from list, depending on button pressed
CTask_pdmenu	:: Choose one iTask from list, depending on pulldownmenu item selected
*/
CTask		 	:: [(String,Task a)] 	-> (Task a) 			| iData a
CTask_pdmenu 	:: [(String,Task a)] 	-> (Task a)	 			| iData a

/* Choose m Tasks out of n:
MCTask_ckbox	:: Multiple Choice of iTasks, depending on marked checkboxes
*/
MCTask_ckbox 	:: [(String,Task a)] 	-> (Task [a]) 			| iData a

/* Do m Tasks parallel / interleaved and FINISH as soon as SOME Task completes:
PCTask2			:: do both iTasks in any order, task completed and ends as soon as first one done
PCTasks			:: do all  iTasks in any order, task completed and ends as soon as first one done
*/
PCTask2			:: (Task a,Task a) 		-> (Task a) 			| iData a 
PCTasks			:: [(String,Task a)] 	-> (Task a)				| iData a 

/* Do Tasks parallel / interleaved and FINISH when ALL Tasks done:
PTask2			:: do both iTasks in any order (paralel), task completed when both done
PTasks			:: do all  iTasks in any order (paralel), task completed when all  done
PMilestoneTasks :: do all  iTasks in any order (paralel), task completed when all  done
					but continue with next task as soon as SOME Task completes
					string indicates which tasks have completed
PmuTasks		:: assign task to indicated users, task completed when all done
*/
PTask2 			:: (Task a,Task b) 		-> (Task (a,b)) 		| iData a & iData b
PTasks 			:: [(String,Task a)]	-> (Task [a])			| iData a 
PMilestoneTasks :: [(String,Task a)] 	-> (Task [(String,a)]) 	| iData a 
PmuTasks 		:: String [(Int,Task a)]-> (Task [a]) 			| iData a 


/* Assign tasks with informative name to user with indicated id
(@:)			:: will prompt who is waiting for what
(@::)			:: no prompting
*/
(@:)  infix 4 	:: !(!Int,!String) (Task a)	-> (Task a)			| iData a
(@::) infix 4 	:: !Int (Task a)		    -> (Task a)			| iData a


/*
(<<@)			:: set attribute for indicated task
*/
(<<@) infix  3 	:: (Task a) b  -> (Task a) | setTaskAttr b

/* Global Attribute settings: iTask are by default Lifespan = Session, StorageFormt = PlainString
*/
class setTaskAttr a :: !a *TSt -> *TSt

instance setTaskAttr Lifespan, StorageFormat, Mode
/* Time and Date management:
waitForTimeTask	:: Task is done when time has come
waitForTimerTask:: Task is done when specified amount of time has passed 
waitForDateTask	:: Task is done when date has come
*/
waitForTimeTask	:: HtmlTime				-> (Task HtmlTime)
waitForTimerTask:: HtmlTime				-> (Task HtmlTime)
waitForDateTask	:: HtmlDate				-> (Task HtmlDate)

/*
(*>>)			:: applying function of type: TSt -> (a,TSt)
(@>>)			:: applying function of type: TSt -> TSt
*/

(*>>) infix  4	:: w:(St .s .a)  v:(.a -> .(St .s .b)) -> u:(St .s .b), [u <= v, u <= w]
(@>>) infix  4	:: w:(.s -> .s)  v:(St .s .b) -> u:(St .s .b), [u <= v, u <= w]
/* Operations on Task state
taskId			:: id assigned to task
userId			:: id of application user
*/

taskId			:: TSt -> (Int,TSt)
userId 			:: TSt -> (Int,TSt)

/* Lifting iData domain to iTask domain
appIData		:: lift iData editors to iTask domain
appHSt			:: lift HSt domain to TSt domain
*/
appIData 		:: (IDataFun a) 		-> (Task a) 			| iData a
appHSt 			:: (HSt -> (a,HSt)) 	-> (Task a)				| iData a

/* Experimental!! DONT USE Setting up communication channels between users:
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

