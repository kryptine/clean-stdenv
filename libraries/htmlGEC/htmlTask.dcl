definition module htmlTask

// *experimental* library for controlling interactive Tasks (iTask) based on iData
// (c) 2006 MJP

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
startTask		:: general start function for iTasks for user with indicated id		
singleUserTask 	:: wrapper for single user 
multiUserTask 	:: wrapper for [0..users - 1], optional set of global Task attributes can be given  
*/
startTask 		:: !Int !(Task a) HSt -> (a,[BodyTag],HSt) 			  | iData a 
singleUserTask 	:: !(Task a) 					 !*HSt -> (Html,*HSt) | iData a 
multiUserTask 	:: !Int [*TSt -> *TSt] !(Task a)  !*HSt -> (Html,*HSt) | iData a 
/* Global Attribute settings: iTask are by default Lifespan = Session, StorageFormt = PlainString
For multi user systems 
*/
class setTaskAttribute a :: !a *TSt -> *TSt

instance setTaskAttribute Lifespan, StorageFormat, Mode

/* Assign tasks with informative name to user with indicated id
(@:)			:: will prompt who is waiting for what
(@::)			:: no prompting
*/
(@:)  infix 4 	:: !(!Int,!String) (Task a)	-> (Task a)			| iData a
(@::) infix 4 	:: !Int (Task a)		    -> (Task a)			| iData a

/* Promote any TSt state transition function to an iTask:
mkTask			:: function will only be called when it is its turn to be activated
					Also needed for defining recursive tasks
clearTask			:: same, but clear output of all finished recursive calls
*/
mkTask 			:: (*TSt -> *(a,*TSt)) 	-> (Task a) 			| iData a 

/*	Sequential Tasks:
STask			:: a Sequential iTask
STask_button	:: do corresponding iTask when button pressed
STasks			:: do all iTasks one after another, task completed when all done
*/
STask 			:: String a 			-> (Task a)				| iData a 
STask_button	:: String (Task a)		-> (Task a) 			| iData a
STasks			:: [(String,Task a)] 	-> (Task [a])			| iData a 

/* Choose one Task out of n:
CTask_button	:: Choose one iTask from list, depending on button pressed
CTask_pdmenu	:: Choose one iTask from list, depending on pulldownmenu item selected
*/
CTask_button 	:: [(String,Task a)] 	-> (Task a) 			| iData a
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
*/
PTask2 			:: (Task a,Task b) 		-> (Task (a,b)) 		| iData a & iData b
PTasks 			:: [(String,Task a)]	-> (Task [a])			| iData a 
PMilestoneTasks :: [(String,Task a)] 	-> (Task [a]) 			| iData a 

/* Tasks that do not require IO actions from the user:
returnV			:: return the value
returnTask		:: return the value and show it 
returnVF		:: return the value and show the Html code specified
returnF			:: add html code
*/
returnV 		:: a 					-> (Task a) 			| iData a 
returnTask 		:: a 					-> (Task a) 			| iData a 
returnVF 		:: a [BodyTag] 			-> (Task a) 			| iData a 
returnF 		:: [BodyTag] 			-> TSt -> TSt
myId			:: TSt -> (Int,TSt)

/* Setting up communication channels between users:
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


/* Time and Date management:
waitForTimeTask	:: Task is done when time has come
waitForDateTask	:: Task is done when date has come
*/
waitForTimeTask	:: HtmlTime				-> (Task HtmlTime)
waitForDateTask	:: HtmlDate				-> (Task HtmlDate)


/* Lifting iData domain to iTask domain
appIData		:: lift iData editors to iTask domain
appHSt			:: lift HSt domain to TSt domain
*/
appIData 		:: (IDataFun a) 		-> (Task a) 					| iData a
appHSt 			:: (HSt -> (a,HSt)) TSt -> (a,TSt)

/* monadic shorthands
(?>>)			:: only prompt as long as task is active but not finished
(!>>)			:: prompt
(=>>)			:: bind
(#>>)			:: bind, no argument passed
(#>>)			:: conditional added
*/

(?>>) infix 2 	:: [BodyTag] v:(St TSt .a) -> v:(St TSt .a)
(!>>) infix 2 	:: [BodyTag] v:(St TSt .a) -> v:(St TSt .a)
(=>>) infix 1 	:: w:(St .s .a) v:(.a -> .(St .s .b)) -> u:(St .s .b), [u <= v, u <= w]	// `bind`
(#>>) infixl 1 	:: w:(St .s .a) v:(St .s .b) -> u:(St .s .b), [u <= v, u <= w]			// `bind` ignoring argument
(<|) infix 3 	:: (*TSt -> *(a,*TSt)) (a -> .Bool,a -> String) -> .(*TSt -> *(a,*TSt)) | iData a		// repeat as long as predicate does not hold
