implementation module StdTime

//	Clean Object I/O library, version 1.2

//	Time related operations

import	StdClass, StdFunc, StdInt, StdReal
import	ostime, ossystem, /*MW11++*/ ostick

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

//	Suspend evaluation of the second argument modally for a number of ticks (see StdSystem).

wait :: !Int .x -> .x
wait nrTicks x
	| nrTicks<=0
	= x
	#! (x,_)	= OSWait nrTicks x OSNewToolbox
	= x
/* Macintosh version of OSWait:
OSWait :: !Int .x !*OSToolbox -> (.x,!*OSToolbox)
OSWait delay x tb
	# (now,tb)	= TickCount tb
	= waitticks now delay tb x
where
	waitticks :: !Int !Int .x !*OSToolbox -> (.x,!*OSToolbox)
	waitticks then delay x tb
		# (now,tb) = TickCount tb
		| now-then>=delay
		= (x,tb)
		= waitticks then delay x tb
*/

// MW11..
instance < Tick
	where
		(<) t1 t2	// tried to solve millenium bug like bugs
			# t1 = unpack_tick t1
			  t2 = unpack_tick t2
			= t2-t1>0	// this is better than "t1<t2" (plays a role if Mac is running longer than about 1.2 years)

intPlusTick	::	!Int !Tick -> Tick
intPlusTick i t
	= pack_tick (i+ (unpack_tick t))
		
tickDifference	::	!Tick !Tick	-> Int
tickDifference t1 t2
	= (unpack_tick t1) - (unpack_tick t2)

class TimeEnv env where
	getBlinkInterval:: !*env -> (!Int,	!*env)
	getCurrentTime	:: !*env -> (!Time,	!*env)
	getCurrentDate	:: !*env -> (!Date,	!*env)
	getCurrentTick	:: !*env -> (!Tick,	!*env)
// ..MW11

instance TimeEnv World where
	getBlinkInterval :: !*World -> (!Int,!*World)
	getBlinkInterval world
		# (msec,_)	= OSGetBlinkInterval OSNewToolbox
		| OStickspersecond==1000
		= (msec,world)
		# tsec		= (toInt ((toReal msec)/1000.0))*OStickspersecond
		= (tsec,world)
	
	getCurrentTime :: !*World -> (!Time,!*World)
	getCurrentTime world
		# ((hours,minutes,seconds),_)	= OSGetCurrentTime OSNewToolbox
		= ({hours=hours,minutes=minutes,seconds=seconds},world)
	/*	Macintosh version:
	OSGetCurrentTime :: !*OSToolbox -> (!(!Int,!Int,!Int),!*OSToolbox)
	OSGetCurrentTime tb
		# (time,tb)					= LoadLong TimeLoc tb
		# (hours,minutes,seconds,tb)= Secs2Time time tb
		= ((hours,minutes,seconds),tb)
	*/
	
	getCurrentDate :: !*World -> (!Date,!*World)
	getCurrentDate world
		# ((year,month,day,dayOfWeek),_)	= OSGetCurrentDate OSNewToolbox
		= ({year=year,month=month,day=day,dayNr=dayOfWeek},world)
	/*	Macintosh version:
	OSGetCurrentDate :: !*OSToolbox -> (!(!Int,!Int,!Int,!Int),!*OSToolbox)
	OSGetCurrentDate tb
		# (time,tb)					= LoadLong TimeLoc tb
		# (year,month,day,dayNr,tb)	= Secs2Date time tb
		= ((year,month,day,dayNr),tb)
	*/
	getCurrentTick	:: !*World -> (!Tick,	!*World) // MW11
	getCurrentTick world
		= os_getcurrenttick world

/*	Macintosh constants:
CaretTime	:== 756		// the address which contains the LongInt of the caret-time.
TimeLoc		:== 524		// the address which contains the time since 1-1-1904 (midnight).
*/