implementation module receiverhandle


//	Clean Object I/O library, version 1.2


import	StdBool, StdInt, StdList
import	receivermessage, semidynamic


::	ReceiverHandles pst
	=	{	rReceivers	:: [ReceiverStateHandle pst]
		}
::	ReceiverStateHandle pst
	=	E. .ls:
		{	rState		:: ls							// The local state of the receiver
		,	rHandle		:: ReceiverHandle ls pst		// The receiver handle
		}
::	ReceiverHandle ls pst
	=	E. m r:
		{	rId			:: Id							// The id of the receiver
		,	rASMQ		:: [m]							// The asynchronous message queue of the receiver
		,	rSelect		:: SelectState					// The current SelectState of the receiver
		,	rOneWay		:: Bool							// Flag: True iff receiver is uni-directional
		,	rFun		:: RHandleFunction ls m r pst	// If rOneWay then [r]==[], otherwise [r]==[_]
		}
::	RHandleFunction ls m r pst
	:==	m -> (ls,pst) -> (ls,[r],pst)


/*	Conversion functions:
	Cast contains abc code because it can't be typed conventionally.
	The function Cast is required to break the Existential Type abstraction needed
	for message passing. (JVG/RWS)
*/
Cast :: !a -> b
Cast a
	= code
		{
			pop_a 0
		}

receiverIdentified :: !Id !(ReceiverHandle .ls .pst) -> Bool
receiverIdentified id {rId}
	= id==rId

receiverSetSelectState :: !SelectState !(ReceiverStateHandle .pst) -> ReceiverStateHandle .pst
receiverSetSelectState select rsH=:{rHandle=rH}
	= {rsH & rHandle={rH & rSelect=select}}

receiverHandleSyncMessage :: !SyncMessage !(ReceiverHandle .ls .pst) (.ls,.pst) -> ([SemiDynamic],ReceiverHandle .ls .pst,(.ls,.pst))
receiverHandleSyncMessage {smRecLoc={rlReceiverId},smMsg} rH=:{rFun} context
	| not (receiverIdentified rlReceiverId rH)
		= ([],rH,context)
	| otherwise
		# (ls,resp,pst)	= rFun (Cast (getDynamic rlReceiverId smMsg)) context
		= ([setDynamic rlReceiverId resp smMsg],rH,(ls,pst))

receiverAddASyncMessage :: !Id !SemiDynamic !(ReceiverHandle .ls .pst) -> ReceiverHandle .ls .pst
receiverAddASyncMessage id sd rH=:{rASMQ}
	| receiverIdentified id rH
		= {rH & rASMQ=rASMQ++[Cast (getDynamic id sd)]}
	| otherwise
		= rH
