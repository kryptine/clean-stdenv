definition module processstack


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


from	StdOverloaded	import ==
from	systemid		import	SystemId


::	ProcessShowState
	=	{	psId	:: !SystemId		// the id of the process
		,	psShow	:: !ShowFlag		// flag: True iff the process is visible
		,	psKind	:: !ProcessKind		// the kind of the process
		}
::	ProcessKind		=	InteractiveProcess | VirtualProcess
::	ShowFlag		:==	Bool
::	ProcessStack	:==	[ProcessShowState]

instance == ProcessKind

emptyProcessStack		:: ProcessStack
pushProcessShowState	:: !ProcessShowState	!ProcessStack -> ProcessStack
setProcessShowState		:: !SystemId !Bool		!ProcessStack -> ProcessStack
selectProcessShowState	:: !SystemId			!ProcessStack -> ProcessStack
removeProcessShowState	:: !SystemId			!ProcessStack -> (!ProcessShowState,!ProcessStack)
topShowProcessShowState	::						!ProcessStack -> (!Bool,!SystemId)
