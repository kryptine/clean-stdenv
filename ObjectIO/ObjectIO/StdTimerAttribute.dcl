definition module StdTimerAttribute


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	StdTimerAttribute specifies which TimerAttributes are valid for each of the
//	standard timers.
//	Basic comparison operations and retrieval functions are also included.
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


import StdTimerDef


/*	The following functions specify the valid attributes for each standard timer.
*/

isValidTimerAttribute :: !(TimerAttribute .st) -> Bool
/*	Timer			(y = valid, . = invalid)
	TimerFunction	y | TimerInit			y |
	TimerId			y | TimerSelectState	y |
*/


/*	The following functions return True only iff the attribute equals the 
	indicated name.
*/
isTimerFunction			:: !(TimerAttribute .st) -> Bool
isTimerId				:: !(TimerAttribute .st) -> Bool
isTimerInit				:: !(TimerAttribute .st) -> Bool
isTimerSelectState		:: !(TimerAttribute .st) -> Bool


/*	The following functions return the attribute value if appropriate. 
	THESE ARE PARTIAL FUNCTIONS! They are only defined on the corresponding
	attribute.
*/
getTimerFun				:: !(TimerAttribute .st) -> TimerFunction .st
getTimerIdAtt			:: !(TimerAttribute .st) -> Id
getTimerInitFun			:: !(TimerAttribute .st) -> IdFun .st
getTimerSelectStateAtt	:: !(TimerAttribute .st) -> SelectState
