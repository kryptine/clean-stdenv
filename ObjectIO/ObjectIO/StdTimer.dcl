definition module StdTimer


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdTimer specifies all timer operations.
//	********************************************************************************


import	StdTimerDef, StdTimerElementClass, StdMaybe
from	StdSystem	import ticksPerSecond
from	iostate		import PSt, IOSt


class Timers tdef where
	openTimer	:: .ls !(tdef .ls (PSt .l .p)) !(PSt .l .p)
							   -> (!ErrorReport,!PSt .l .p)
	getTimerType::      (tdef .ls .pst) -> TimerType
/*	Open a new timer.
	This function has no effect in case the interactive process already contains a 
	timer with the same Id. In case TimerElements are opened with duplicate Ids, the
	timer will not be opened. Negative TimerIntervals are set to zero.
	In case the timer does not have an Id, it will obtain an Id which is fresh with 
	respect to the current set of timers. The Id can be reused after closing this 
	timer.
*/

instance Timers (Timer t)	| TimerElements	t


closeTimer :: !Id !(IOSt .l .p) -> IOSt .l .p
/*	closeTimer closes the timer with the indicated Id.
*/


getTimers  ::     !(IOSt .l .p) -> (![(Id,TimerType)],!IOSt .l .p)
/*	getTimers returns the Ids and TimerTypes of all currently open timers.
*/


enableTimer			:: !Id !(IOSt .l .p) -> IOSt .l .p
disableTimer		:: !Id !(IOSt .l .p) -> IOSt .l .p
getTimerSelectState	:: !Id !(IOSt .l .p) -> (!Maybe SelectState,!IOSt .l .p)
/*	(en/dis)ableTimer (en/dis)ables the indicated timer.
	getTimerSelectState yields the SelectState of the indicated timer. If the timer 
	does not exist, then Nothing is yielded.
*/


setTimerInterval	:: !Id !TimerInterval	!(IOSt .l .p) -> IOSt .l .p
getTimerInterval	:: !Id					!(IOSt .l .p)
					-> (!Maybe TimerInterval,!IOSt .l .p)
/*	setTimerInterval
		sets the TimerInterval of the indicated timer. 
		Negative TimerIntervals are set to zero.
	getTimerInterval
		yields the TimerInterval of the indicated timer. 
		If the timer does not exist, then Nothing is yielded.
*/
