implementation module StdTime

//	Clean Object I/O library, version 1.2.1

//	Time related operations

import	StdBool, StdClass, StdFunc, StdInt, StdReal
import	ostime, ossystem, /*MW11++*/ ostick
from	StdLibMisc import Time, Date

//	Suspend evaluation of the second argument modally for a number of ticks (see StdSystem).

wait :: !Int .x -> .x
wait nrTicks x
	| nrTicks<=0
		= x
	| otherwise
		#! (x,_)	= osWait nrTicks x OSNewToolbox
		= x

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
		# (msec,_)	= osGetBlinkInterval OSNewToolbox
		| OStickspersecond==1000
		= (msec,world)
		# tsec		= (toInt ((toReal msec)/1000.0))*OStickspersecond
		= (tsec,world)
	
	getCurrentTime :: !*World -> (!Time,!*World)
	getCurrentTime world
		# ((hours,minutes,seconds),_)	= osGetCurrentTime OSNewToolbox
		= ({hours=hours,minutes=minutes,seconds=seconds},world)
	/*	Macintosh version:
	osGetCurrentTime :: !*OSToolbox -> (!(!Int,!Int,!Int),!*OSToolbox)
	osGetCurrentTime tb
		# (time,tb)					= LoadLong TimeLoc tb
		# (hours,minutes,seconds,tb)= Secs2Time time tb
		= ((hours,minutes,seconds),tb)
	*/
	
	getCurrentDate :: !*World -> (!Date,!*World)
	getCurrentDate world
		# ((year,month,day,dayOfWeek),_)	= osGetCurrentDate OSNewToolbox
		= ({year=year,month=month,day=day,dayNr=dayOfWeek},world)
	/*	Macintosh version:
	osGetCurrentDate :: !*OSToolbox -> (!(!Int,!Int,!Int,!Int),!*OSToolbox)
	osGetCurrentDate tb
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