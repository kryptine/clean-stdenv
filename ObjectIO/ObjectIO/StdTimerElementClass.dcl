definition module StdTimerElementClass


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	StdTimerElementClass define the standard set of timer element instances.
//	Author: Peter Achten
//	Modified: 8 October 2001 for Clean 2.0
//	********************************************************************************


import	StdIOCommon, StdTimerDef
from	iostate		import :: PSt
from	timerhandle	import :: TimerElementState


class TimerElements t where
	timerElementToHandles	:: !.(t  .ls (PSt .l)) !(PSt .l)
			 -> (![TimerElementState .ls (PSt .l)], !PSt .l)
	getTimerElementType		::	.(t  .ls .pst)
			 -> TimerElementType

instance TimerElements (NewLS	t)   | TimerElements t
instance TimerElements (AddLS	t)   | TimerElements t
instance TimerElements (ListLS	t)   | TimerElements t
instance TimerElements NilLS
instance TimerElements ((:+:) t1 t2) | TimerElements t1
									 & TimerElements t2
