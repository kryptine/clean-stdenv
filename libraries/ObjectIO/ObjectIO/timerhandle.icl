implementation module timerhandle


//	Clean Object I/O library, version 1.2


import	StdBool, StdList
import	commondef, receiverhandle, receivertable
import	StdTimerDef


::	TimerElementState ls ps									// The internal implementation of a timer
	:==	TimerElementHandle ls ps							// is a TimerElementHandle

::	TimerHandles ps
	=	{	tTimers	:: ![TimerStateHandle ps]				// The timers of a process
		}
::	TimerStateHandle ps
	=	E..ls: TimerLSHandle !(TimerLSHandle ls ps)			// A timer with local state
::	TimerLSHandle ls ps
	=	{	tState	:: ls									// The local state of this timer
		,	tHandle	:: !TimerHandle ls ps					// The timer implementation
		}
::	TimerHandle ls ps
	=	{	tId		:: !Id									// The Id attribute or generated system Id of the timer
		,	tSelect	:: !Bool								// The TimerSelect==Able (by default True)
		,	tPeriod	:: !Int									// The interval time in ticks
		,	tFun	:: !TimerFunction *(ls,ps)				// The TimerFunction, optionally with local state
		,	tItems	:: [TimerElementHandle ls ps]			// The elements of the timer
		}
::	TimerElementHandle ls ps
	=	TimerReceiverHandle	(TimerReceiverHandle	ls ps)
	|	TimerListLSHandle	[TimerElementHandle		ls ps]
	|	TimerElimLSHandle	[TimerElementHandle		ls ps]
	|	TimerIntroLSHandle	(TimerIntroLSHandle		ls ps)
	|	TimerExtendLSHandle	(TimerExtendLSHandle	ls ps)
	|	TimerChangeLSHandle	(TimerChangeLSHandle	ls ps)
::	TimerReceiverHandle ls ps
	=	{	tReceiverHandle	:: ReceiverHandle ls ps
		,	tReceiverAtts	:: [TimerAttribute *(ls,ps)]
		}
::	TimerIntroLSHandle	ls ps
	=	E..ls1:
		{	tIntroLS		:: ls1
		,	tIntroItems		:: [TimerElementHandle ls1 ps]
		}
::	TimerExtendLSHandle	ls ps
	=	E..ls1:
		{	tExtendLS		:: ls1
		,	tExtendItems	:: [TimerElementHandle *(ls1,ls) ps]
		}
::	TimerChangeLSHandle	ls ps
	=	E..ls1:
		{	tChangeLS		:: ls1
		,	tChangeItems	:: [TimerElementHandle ls1 ps]
		}


//	Conversion functions from TimerElementState to TimerElementHandle, and vice versa:

TimerElementHandleToTimerElementState :: !(TimerElementHandle .ls .ps) -> TimerElementState .ls .ps
TimerElementHandleToTimerElementState tHs = tHs

TimerElementStateToTimerElementHandle :: !(TimerElementState .ls .ps) -> TimerElementHandle .ls .ps
TimerElementStateToTimerElementHandle tHs = tHs
