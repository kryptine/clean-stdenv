definition module StdTimerDef


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	StdTimerDef contains the types to define the standard set of timers.
//	Author: Peter Achten
//	Modified: 14 September 2001 for Clean 2.0
//	********************************************************************************


import	StdIOCommon


::	Timer t ls pst	= Timer TimerInterval (t ls pst) [TimerAttribute *(ls,pst)]

::	TimerInterval
	:==	Int

::	TimerAttribute	st							// Default:
	=	TimerFunction		(TimerFunction st)	// \_ x->x
	|	TimerId				Id					// no Id
	|	TimerInit			(IdFun st)			// no actions after opening timer
	|	TimerSelectState	SelectState			// timer Able

::	TimerFunction	st	:==	NrOfIntervals -> st -> st
::	NrOfIntervals		:== Int

::	TimerType			:==	String
::	TimerElementType	:==	String
