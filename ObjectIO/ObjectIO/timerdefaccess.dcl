definition module timerdefaccess


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Access function to timerDefinitions.
//	Author: Peter Achten
//	Modified: 8 October 2001 for Clean 2.0
//	********************************************************************************

import	StdTimerDef


timerDefGetAttributes	:: !(Timer t .ls .pst) -> [TimerAttribute *(.ls,.pst)]
timerDefSetAbility		:: !SelectState					!(Timer t .ls .pst) -> Timer t .ls .pst
timerDefSetInterval		:: !TimerInterval				!(Timer t .ls .pst) -> Timer t .ls .pst
timerDefSetFunction		:: !(TimerFunction *(.ls,.pst))	!(Timer t .ls .pst) -> Timer t .ls .pst
