implementation module receiverhandle


//	Clean Object I/O library, version 1.2


import	StdBool, StdInt, StdList
import	receivermessage, semidynamic
from	ostoolbox	import OSToolbox // MW11++


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
		,	rInetInfo	:: !Maybe (!EndpointRef`,!InetReceiverCategory`,!Int,!IdFun !*OSToolbox)
														// For internet receivers
		,	rConnected	:: ![Id]						// storing the argument of the ReceiverCloseAlsoReceivers attribute
		}
::	RHandleFunction ls m r pst
	:==	m -> (ls,pst) -> (ls,[r],pst)

// MW11..
::	InetReceiverASMQType	:== (!InetEvent`,!EndpointRef`,!Int)

::	InetEvent`				:== Int
::	EndpointRef`			:==	Int
::	InetReceiverCategory`	:==	Int
// ..MW11

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

// MW11..
inetReceiverIdentified		::	!(!EndpointRef`, !InetReceiverCategory`)
								!(ReceiverHandle .ls .ps)	-> Bool
inetReceiverIdentified _ {rInetInfo=Nothing}
	= False
inetReceiverIdentified (epR1,type1) {rInetInfo=Just (epR2,type2,_,_)}
	= epR1==epR2 && type1==type2

inetReceiverIdentifiedWithId	::	!(!Id, !InetReceiverCategory`)
									!(ReceiverHandle .ls .ps)	-> Bool
inetReceiverIdentifiedWithId _ {rInetInfo=Nothing}
	= False
inetReceiverIdentifiedWithId (id,category) {rId, rInetInfo=Just (_,rCategory,_,_)}
	= id==rId && category==rCategory
// ..MW11

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

// MW11..
receiverApplyInetEvent		::	!InetReceiverASMQType !(ReceiverHandle .ls .ps) (.ls,.ps)
							->	(.ls,.ps)
receiverApplyInetEvent eventInfo rH=:{rFun,rInetInfo=Just _} context
	# (ls,_,ps)	= rFun (Cast eventInfo) context
	= (ls,ps)

getInetReceiverRId			::	!(ReceiverHandle .ls .ps)	-> (RId InetReceiverASMQType)
// converts an Id into an RId
getInetReceiverRId {rId}
	= toRId (fromId rId)
// ..MW11
