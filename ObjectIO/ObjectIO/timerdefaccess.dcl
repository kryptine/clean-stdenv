definition module timerdefaccess


//	Object I/O library, version 1.2

//	Access function to timerDefinitions.

import	StdTimerAttribute, StdMaybe


timerDefGetAttributes	:: !(Timer t .ls .pst) -> [TimerAttribute *(.ls,.pst)]
timerDefGetElements		:: !(Timer t .ls .pst) -> t .ls .pst
timerDefSetAbility		:: !SelectState					!(Timer t .ls .pst) -> Timer t .ls .pst
timerDefSetInterval		:: !TimerInterval				!(Timer t .ls .pst) -> Timer t .ls .pst
timerDefSetFunction		:: !(TimerFunction *(.ls,.pst))	!(Timer t .ls .pst) -> Timer t .ls .pst
