definition module StdStringChannels

//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdStringChannels provides channel instanciates to send and receive Strings
//	These channels use their own protocol above TCP
//	********************************************************************************

from	StdString	import String
import	StdTCPDef, StdChannels, StdEventTCP
from	StdReceiver		import Receivers, ReceiverType, RId
from	StdTCPChannels	import SelectSend, SelectReceive, getNrOfChannels

/*	If a string via a StringChannel is sent, then first the length of the string is
	sent, and then the string itself, e.g. sending the string "abc" will result in
	"3 abc\xD"
*/

////////////////////// StringChannels to receive ///////////////////////////////////

::	*StringRChannel_ a
::	*StringRChannel		:== StringRChannel_ String
::	*StringRChannels	=	StringRChannels [StringRChannel]
::	*StringChannelReceiver ls ps 	
	=	StringChannelReceiver
			(RId (ReceiveMsg String)) StringRChannel	
			(ReceiverFunction (ReceiveMsg String)		*(ls,ps))
			[ReceiverAttribute							*(ls,ps)]

toStringRChannel	::	TCP_RChannel -> StringRChannel

instance Receivers 			StringChannelReceiver
instance Receive			StringRChannel_
instance closeRChannel		StringRChannel_
instance MaxSize			StringRChannel_

////////////////////// StringChannels to send ////////////////////////////////////

::	*StringSChannel_ a
::	*StringSChannel		:== StringSChannel_ String
::	*StringSChannels	=	StringSChannels [StringSChannel]

toStringSChannel	::	TCP_SChannel -> StringSChannel

instance Send				StringSChannel_

/////////////////////////////////////////////////////////////////////////////////

// for openSendNotifier, closeSendNotifier
instance accSChannel 		StringSChannel_

// for selectChannel
instance SelectSend			StringSChannels
instance SelectReceive		StringRChannels
instance getNrOfChannels 	StringRChannels
