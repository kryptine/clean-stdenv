definition module receiverhandle


//	Clean Object I/O library, version 1.2


import	id, receivermessage
from	ostoolbox	import OSToolbox


::	*ReceiverHandles pst
	=	{	rReceivers	:: *[*ReceiverStateHandle pst]
		}
::	*ReceiverStateHandle pst
	=	E. .ls:
		{	rState		:: ls							// If has local state then [_], otherwise []
		,	rHandle		:: ReceiverHandle ls pst		// The receiver handle
		}
::	ReceiverHandle ls pst
	=	E. m r:
		{	rId			:: Id							// The id of the receiver
		,	rASMQ		:: [m]							// The asynchronous message queue
		,	rSelect		:: SelectState					// The attribute ReceiverSelect==Able (default True)
		,	rOneWay		:: Bool							// The receiver definition was Receiver (not Receiver2)
		,	rFun		:: RHandleFunction ls m r pst	// If rOneWay then [r]==[], otherwise [r]==[_]
// MW11..
		,	rInetInfo	:: !Maybe (!EndpointRef`,!InetReceiverCategory`,!Int,!IdFun !*OSToolbox)
														// For internet receivers
		,	rConnected	:: ![Id]						// storing the argument of the ReceiverCloseAlsoReceivers attribute
// ..MW11
		}
::	RHandleFunction ls m r pst
	:==	m -> *(ls,pst) -> *(ls,[r],pst)

// MW11..
::	InetReceiverASMQType	:== (!InetEvent`,!EndpointRef`,!Int)

::	InetEvent`				:== Int
::	EndpointRef`			:==	Int
::	InetReceiverCategory`	:==	Int
// ..MW11
