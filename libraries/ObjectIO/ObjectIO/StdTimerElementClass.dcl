definition module StdTimerElementClass


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdTimerElementClass define the standard set of timer element instances.
//	********************************************************************************


import	StdIOCommon, StdTimerDef
from	iostate		import PSt, IOSt
from	timerhandle	import TimerElementState


class TimerElements t where
	timerElementToHandles	:: !(t  .ls (PSt .l)) !(PSt .l)
			-> (![TimerElementState .ls (PSt .l)], !PSt .l)
	getTimerElementType		::	(t  .ls .pst)
			-> TimerElementType

instance TimerElements (NewLS	t)   | TimerElements t	// getTimerElementType==""
instance TimerElements (AddLS	t)   | TimerElements t	// getTimerElementType==""
instance TimerElements (ListLS	t)   | TimerElements t	// getTimerElementType==""
instance TimerElements NilLS							// getTimerElementType==""
instance TimerElements ((:+:) t1 t2) | TimerElements t1
									 & TimerElements t2	// getTimerElementType==""
