implementation module timerevent


//	Clean Object I/O library, version 1.2


import	StdBool, StdClass
import	deviceevents, timeraccess
from	commondef	import FatalError, UContains, UCond
from	iostate		import PSt, IOSt, IOStHasDevice, IOStGetDevice, IOStSetDevice, IOStGetIOId
from	StdPSt		import accPIO


timereventFatalError :: String String -> .x
timereventFatalError function error
	= FatalError function "timerevent" error


/*	The timerEvent function determines whether the given SchedulerEvent can be applied
	to a timer of this process. These are the following cases:
	*	ScheduleTimerEvent: the timer event belongs to this process and device
	*	ScheduleMsgEvent:   the message event belongs to this process and device
	timerEvent assumes that it is not applied to an empty IOSt.
*/
timerEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
timerEvent schedulerEvent pState
	# (hasDevice,pState)	= accPIO (IOStHasDevice TimerDevice) pState
	| not hasDevice			// This condition should never occur: TimerDevice must have been 'installed'
		= timereventFatalError "TimerFunctions.dEvent" "could not retrieve TimerSystemState from IOSt"
	| otherwise
		= timerEvent schedulerEvent pState
where
	timerEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
	timerEvent schedulerEvent=:(ScheduleTimerEvent te=:{teLoc}) pState=:{io=ioState}
		# (ioid,ioState)	= IOStGetIOId ioState
		| teLoc.tlIOId<>ioid || teLoc.tlDevice<>TimerDevice
			= (False,Nothing,schedulerEvent,{pState & io=ioState})
		# (_,timer,ioState)	= IOStGetDevice TimerDevice ioState
		# timers			= TimerSystemStateGetTimerHandles timer
		  (found,timers)	= lookForTimer teLoc.tlParentId timers
		# ioState			= IOStSetDevice (TimerSystemState timers) ioState
		# pState			= {pState & io=ioState}
		| found
			#! deviceEvent	= TimerEvent te
			= (True,Just deviceEvent,schedulerEvent,pState)
		| otherwise
			= (False,Nothing,schedulerEvent,pState)
	where
		lookForTimer :: !Id !(TimerHandles .pst) -> (!Bool,!TimerHandles .pst)
		lookForTimer parent timers=:{tTimers=tHs}
			# (found,tHs)	= UContains (identifyTimerStateHandle parent) tHs
			= (found,{timers & tTimers=tHs})
	
	timerEvent schedulerEvent=:(ScheduleMsgEvent msgEvent) pState
		# (ioid,pState)		= accPIO IOStGetIOId pState
		  recloc			= case msgEvent of
							  	(QASyncMessage {qasmRecLoc}) -> qasmRecLoc
							  	(ASyncMessage  { asmRecLoc}) -> asmRecLoc
							  	(SyncMessage   {  smRecLoc}) -> smRecLoc
		| ioid==recloc.rlIOId && TimerDevice==recloc.rlDevice
			= (True,Just (ReceiverEvent msgEvent),schedulerEvent,pState)
		| otherwise
			= (False,Nothing,schedulerEvent,pState)
	
	timerEvent schedulerEvent pState
		= (False,Nothing,schedulerEvent,pState)
