definition module StdChannels

//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdChannels defines operations on channels
//	********************************************************************************

from	StdMaybe		import	Maybe
from	StdOverloaded	import	==, toString
from	channelenv		import	ChannelEnv
from	iostate import PSt, IOSt
import StdPStClass

instance ChannelEnv World, (PSt  .l), (IOSt .l)

///////////////////////////////// receive channels /////////////////////////////////

class Receive ch
where
	receive_MT		::	!(Maybe !Timeout)			!*(*ch .a)  !*env	
					->	(!TimeoutReport, !Maybe !.a,!*(*ch .a), !*env)	
													| ChannelEnv  env
	receiveUpTo		::	!Int						!*(*ch .a)  !*env	
					->	(![.a],						!*(*ch .a), !*env)	
													| ChannelEnv  env
	available		:: 								!*(*ch .a)  !*env
					->	(!Bool,						!*(*ch .a), !*env)
													| ChannelEnv  env
	eom				:: 								!*(*ch .a)  !*env
					->	(!Bool,						!*(*ch .a), !*env)
													| ChannelEnv  env
/*	receive_MT
		tries to receive on a channel. This function will block, until data can be
		received, eom becomes true or the timeout expires.
	receiveUpTo max ch env
		receives messages on a channel until available becomes False or max
		messages have been received.
	available
		polls on a channel, whether some data is ready to be received. If the
		returned Boolean is True, then a following receive_MT will not block and
		return TR_Success.
	eom ("end of messages")
		polls on a channel, whether data can't be received anymore.
*/

class closeRChannel	ch	:: !*(*ch .a) !*env -> !*env	| ChannelEnv  env
// closes the channel

//////////////////////////////// send channels /////////////////////////////////////
	
class Send ch
where
	send_MT			::	!(Maybe !Timeout) !.a		!*(*ch .a)  !*env
					->	(!TimeoutReport, !Int,		!*(*ch .a), !*env)
													| ChannelEnv  env
	nsend_MT		:: 	!(Maybe !Timeout) ![.a]		!*(*ch .a)	!*env
					->	(!TimeoutReport, !Int,		!*(*ch .a), !*env)
													| ChannelEnv  env
	flushBuffer_MT	::	!(Maybe !Timeout) 			!*(*ch .a)  !*env
					->	(!TimeoutReport, !Int,		!*(*ch .a), !*env)
													| ChannelEnv  env
	closeChannel_MT	::	!(Maybe !Timeout) 			!*(*ch .a)  !*env
					->	(!TimeoutReport, !Int,					!*env)
													| ChannelEnv  env
	abortConnection	::								!*(*ch .a)	!*env
					->											!*env
													| ChannelEnv  env
	disconnected	::								!*(*ch .a)	!*env
					->	(!Bool,						!*(*ch .a), !*env)
													| ChannelEnv  env
	bufferSize		::								!*(*ch .a)
					->	(!Int, 						!*(*ch .a))	
/*	send_MT mbTimeout a ch env
		adds the data a to the channels internal buffer and tries to send this buffer
	nsend_MT mbTimeout l ch env
		adds the data l to the channels internal buffer and tries to send this buffer
	flushBuffer_MT
		tries to send the channels internal buffer
	closeSChannel_MT
		first tries to send the channels internal buffer and then closes the channel
	abortConnection
		will cause an abortive disconnect (sent data can be lost)
	disconnected
		polls on a channel, whether data can't be sent anymore. If the returned
		Boolean is True, then a following send_MT will not block and return
		TR_NoSuccess
	bufferSize returns the size of the channels internal buffer in bytes
	
	The integer value that is returned by send_MT, nsend_MT, flushBuffer_MT &
	closeSChannel_MT is the number of sent bytes.
*/
	
////////////////////////////////// miscellaneous ///////////////////////////////////

class MaxSize ch
  where
	setMaxSize		::	!Int !*(*ch .a)	-> *(*ch .a)
	getMaxSize		::		 !*(*ch .a)	-> (!Int, !*(*ch .a))
	clearMaxSize	::		 !*(*ch .a)	-> *(*ch .a)
//	to set,get or clear the maximum size of the data that can be received

:: DuplexChannel sChannel rChannel a
	=	{	sChannel	::	sChannel a
		,	rChannel	::	rChannel a
		}

::	TimeoutReport
	= 	TR_Expired
	|	TR_Success
	|	TR_NoSuccess

::	Timeout		:==	Int		// timeout in ticks

::	ReceiveMsg m		=	Received m
						|	EOM				
						//	receiving "EOM" will automatically close the receiver
::	SendEvent			=	Sendable
						|	Disconnected
						//	receiving "Disconnected" will automatically close the
						//	receiver

instance == TimeoutReport
instance toString TimeoutReport

//////////////////////////////// derived functions /////////////////////////////////

nreceive_MT			::	!(Maybe !Timeout) !Int		!*(*ch .a)  !*env	
					->	(!TimeoutReport, ![.a],		!*(*ch .a), !*env)	
													| Receive ch & ChannelEnv  env
/*	nreceive_MT mbTimeout n ch env
		tries to call receive_MT n times. If the result is (tReport, l, ch2, env2),
		then the following holds:
			tReport==TR_Succes		<=>	length l==n
			tReport==TR_NoSuccess	 =>	length l<n
*/

//	the following two receive functions call their "_MT" counterpart with no
//	timeout. If the data can't be received because eom became True the function will
//	abort.

receive			::			!*(*ch .a)  !*env
				-> (!.a,	!*(*ch .a), !*env)	
				| 	ChannelEnv env & Receive ch
nreceive		::	!Int 	!*(*ch .a)  !*env
				->	(![.a], !*(*ch .a), !*env)
				|	ChannelEnv env & Receive ch

//	the following three send functions call their "_MT" counterpart with no timeout. 

send		 	:: !.a			!*(*ch .a)  !*env
				->			   (!*(*ch .a), !*env)
				| 	ChannelEnv env & Send ch
nsend	 		:: ![.a]		!*(*ch .a)  !*env
				->			   (!*(*ch .a), !*env)
				| 	ChannelEnv env & nsend_MT ch
closeChannel	:: 				!*(*ch .a)  !*env
				->			   				!*env
				| 	ChannelEnv env & Send ch

//	the following two send functions call their "_MT" counterpart with timeout == 0.
//	"NB" is a shorthand for "non blocking"

send_NB 	 	:: !.a			!*(*ch .a)  !*env
				->			   (!*(*ch .a), !*env)
				| 	ChannelEnv env & Send ch
flushBuffer_NB	::				!*(*ch .a)  !*env
				->			   (!*(*ch .a), !*env)
				| 	ChannelEnv env & Send ch
