definition module StdStringChannels

//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.1
//	
//	StdStringChannels provides channel instances to send and receive Strings.
//	These channels use their own protocol above TCP.
//	********************************************************************************

import	StdString
import	StdTCPDef, StdChannels, StdEventTCP
from	StdReceiver		import Receivers, ReceiverType, RId
from	StdTCPChannels	import SelectSend, SelectReceive, getNrOfChannels, 
								TCP_SCharStreams, TCP_SCharStream, 
								TCP_RCharStreams, TCP_RCharStream, TCP_RCharStream_

/*	If a string via a StringChannel is sent, then first the length of the string is
	sent, and then the string itself, e.g. sending the string "abc" will result in
	"3 abc\xD"
*/

//	********************************************************************************
//	StringChannels to receive
//	********************************************************************************

::	*StringRChannel_ a
::	*StringRChannel		:== StringRChannel_ String
::	*StringRChannels	=	StringRChannels [StringRChannel]
::	*StringChannelReceiver ls pst 	
 =	StringChannelReceiver
		(RId (ReceiveMsg String)) StringRChannel	
		(ReceiverFunction (ReceiveMsg String) *(ls,pst))
		[ReceiverAttribute                    *(ls,pst)]

toStringRChannel		:: TCP_RChannel -> StringRChannel

instance Receivers 		StringChannelReceiver
instance Receive		StringRChannel_
instance closeRChannel	StringRChannel_
instance MaxSize		StringRChannel_


//	********************************************************************************
//	StringChannels to send
//	********************************************************************************

::	*StringSChannel_ a
::	*StringSChannel		:== StringSChannel_ String
::	*StringSChannels	=	StringSChannels [StringSChannel]

toStringSChannel		::	TCP_SChannel -> StringSChannel

instance Send StringSChannel_

//	For openSendNotifier, closeSendNotifier
instance accSChannel 		StringSChannel_

//	For selectChannel
instance SelectSend			StringSChannels
instance SelectReceive		StringRChannels
instance getNrOfChannels 	StringRChannels
