definition module receiverhandle


//	Clean Object I/O library, version 1.2


import	id, receivermessage


::	ReceiverHandles pst
	=	{	rReceivers	:: [ReceiverStateHandle pst]
		}
::	ReceiverStateHandle pst
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
		}
::	RHandleFunction ls m r pst
	:==	m -> (ls,pst) -> (ls,[r],pst)

receiverIdentified			:: !Id				!(ReceiverHandle .ls .pst)	-> Bool
receiverSetSelectState		:: !SelectState		!(ReceiverStateHandle .pst)	-> ReceiverStateHandle .pst
receiverHandleSyncMessage	:: !SyncMessage		!(ReceiverHandle .ls .pst) (.ls,.pst)
								-> ([SemiDynamic],ReceiverHandle .ls .pst, (.ls,.pst))
receiverAddASyncMessage		:: !Id !SemiDynamic	!(ReceiverHandle .ls .pst)	-> ReceiverHandle .ls .pst
