definition module StdTCPDef

//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdTCPDef provides basic definitions for using TCP
//	********************************************************************************

from	StdMaybe		import	Maybe
from	StdReceiverDef	import	Id, ReceiverFunction, ReceiverAttribute
from	StdOverloaded	import	toString, ==
from	StdChannels		import	DuplexChannel, ReceiveMsg, SendEvent
from	tcp 			import	TCP_SChannel_,TCP_RChannel_,TCP_Listener_,IPAddress

::	*TCP_SChannel		:==	TCP_SChannel_ ByteSeq
::	*TCP_RChannel		:==	TCP_RChannel_ ByteSeq
::	*TCP_Listener		:== TCP_Listener_ (IPAddress, TCP_DuplexChannel)

::	Port				:== Int

::	*TCP_DuplexChannel	:== DuplexChannel *TCP_SChannel_ *TCP_RChannel_ ByteSeq

::	ByteSeq
//	a sequence of bytes

instance toString	ByteSeq
instance ==			ByteSeq
toByteSeq		::	!x			-> ByteSeq	| toString x
byteSeqSize		::	!ByteSeq	-> Int
// byteSeqSize returns the size in bytes

instance toString IPAddress	
//	returns ip address in dotted decimal form

/////////////////////////// for event driven processing ////////////////////////////

//	to receive byte sequences

::	*TCP_Receiver ls ps 	
	=	TCP_Receiver
			Id TCP_RChannel	
			(ReceiverFunction (ReceiveMsg ByteSeq)	*(ls,ps))
			[ReceiverAttribute						*(ls,ps)]

::	SendNotifier sChannel ls ps
	=	SendNotifier
			sChannel
			(ReceiverFunction SendEvent		*(ls,ps))
			[ReceiverAttribute				*(ls,ps)]

//	to accept new connections

::	*TCP_ListenerReceiver ls ps 	
	=	TCP_ListenerReceiver
			Id TCP_Listener	
			((ReceiveMsg (IPAddress,TCP_DuplexChannel)) -> *(*(ls,ps) -> *(ls,ps)))
			[ReceiverAttribute								 *(ls,ps)]

//	to receive characters

::	*TCP_CharReceiver ls ps 	
	=	TCP_CharReceiver
			Id TCP_RChannel (Maybe NrOfIterations)
			(ReceiverFunction (ReceiveMsg Char)		*(ls,ps))
			[ReceiverAttribute						*(ls,ps)]

/*	For efficency the receiver function of a TCP_CharReceiver will be called from
	a loop. Within this loop no other events can be handled. The NrOfIterations
	parameter limits the maximum number of iterations.
*/

::	NrOfIterations			:== Int
::	InetLookupFunction ps	:== (Maybe IPAddress) -> *(ps -> ps)
::	InetConnectFunction ps 	:== (Maybe TCP_DuplexChannel) -> *(ps -> ps)

//////////////////////////////// for multiplexing //////////////////////////////////

:: *TCP_RChannels		=	TCP_RChannels [TCP_RChannel]	
:: *TCP_SChannels		=	TCP_SChannels [TCP_SChannel]
:: *TCP_Listeners		=	TCP_Listeners [TCP_Listener]

::	*PrimitiveRChannel
	=	TCP_RCHANNEL TCP_RChannel
	|	TCP_LISTENER TCP_Listener

::	SelectResult
	=	SR_Available
	|	SR_EOM
	|	SR_Sendable
	|	SR_Disconnected

