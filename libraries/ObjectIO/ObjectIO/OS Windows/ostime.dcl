definition module ostime

//	Clean Object I/O library, version 1.2

import StdOverloaded
import ostoolbox

::	OSTime

OSMaxTime :: OSTime

OSGetTime			::			!*OSToolbox -> (!OSTime,!*OSToolbox)
//	OSGetTime returns the current OS time

OSWait				:: !Int	.x	!*OSToolbox -> (.x,		!*OSToolbox)
//	OSWait waits atleast the given time (in milliseconds).

OSGetBlinkInterval	::			!*OSToolbox -> (!Int,	!*OSToolbox)
//	OSGetBlinkInterval returns the recommended blink interval time of a cursor (in milliseconds).

OSGetCurrentTime	::			!*OSToolbox -> (!(!Int,!Int,!Int),!*OSToolbox)
//	OSGetCurrentTime returns current (hours,minutes,seconds).

OSGetCurrentDate	::			!*OSToolbox -> (!(!Int,!Int,!Int,!Int),!*OSToolbox)
//	OSGetCurrentTime returns current (year,month,day,day_of_week).

instance -       OSTime		// Calculate difference between arg 1 and arg 2
instance toInt   OSTime		// Coerce OSTime to Integer (always positive or zero)
instance fromInt OSTime		// Coerce Int to OSTime (Integer will be made zero if negative)
