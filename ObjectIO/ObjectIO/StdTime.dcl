definition module StdTime


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.1
//	
//	StdTime contains time related operations.
//	********************************************************************************

from	StdOverloaded import <
from	ostick import Tick

::	Time
	=	{	hours	:: !Int		// hours		(0-23)
		,	minutes	:: !Int		// minutes		(0-59)
		,	seconds	:: !Int		// seconds		(0-59)
		}
::	Date
	=	{	year	:: !Int		// year
		,	month	:: !Int		// month		(1-12)
		,	day		:: !Int		// day			(1-31)
		,	dayNr	:: !Int		// day of week	(1-7, Sunday=1, Saturday=7)
		}

wait				:: !Int .x -> .x

/*	wait n x suspends the evaluation of x modally for n ticks.
	If n<=0, then x is evaluated immediately.
*/

instance < Tick

intPlusTick			::	!Int  !Tick	-> Tick
tickDifference		::	!Tick !Tick	-> Int

class TimeEnv env where
	getBlinkInterval:: !*env -> (!Int,	!*env)
	getCurrentTime	:: !*env -> (!Time,	!*env)
	getCurrentDate	:: !*env -> (!Date,	!*env)
	getCurrentTick	:: !*env -> (!Tick,	!*env)
/*	getBlinkInterval
		returns the time interval in ticks that should elapse between blinks of 
		e.g. a cursor. This interval may be changed by the user while the 
		interactive process is running!
	getCurrentTime
		returns the current Time.
	getCurrentDate
		returns the current Date.
	getCurrentTick
		returns the current Tick.
*/

instance TimeEnv World
