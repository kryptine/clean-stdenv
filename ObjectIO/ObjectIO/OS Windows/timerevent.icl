implementation module timerevent


//	Clean Object I/O library, version 1.2


import	StdBool, StdClass
import	deviceevents, timeraccess
from	commondef	import UContains, UCond
from	iostate		import PSt, IOSt, IOStGetDevice, IOStSetDevice, IOStGetIOId
from	StdPSt		import accPIO


/*	The timerEvent function determines whether the given SchedulerEvent can be applied
	to a timer of this process. These are the following cases:
	*	ScheduleTimerEvent: the timer event belongs to this process and device
	*	ScheduleMsgEvent:   the message event belongs to this process and device
	timerEvent assumes that it is not applied to an empty IOSt.
*/
timerEvent :: !SchedulerEvent !(PSt .l .p) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l .p)
timerEvent schedulerEvent=:(ScheduleTimerEvent te=:{teLoc}) pState=:{io=ioState}
	# (ioid,ioState)	= IOStGetIOId ioState
	| teLoc.tlIOId<>ioid || teLoc.tlDevice<>TimerDevice
		= (False,Nothing,schedulerEvent,{pState & io=ioState})
	# (timer,ioState)	= IOStGetDevice TimerDevice ioState
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
