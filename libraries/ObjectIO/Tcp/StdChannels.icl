implementation module StdChannels

//	Clean Standard Object I/O library, version 1.2


import	StdEnv
import  StdMaybe
import	StdIOCommon, StdTime
import	channelenv
import	id
import commondef, iostate, receiverid, StdPStClass, StdReceiver

instance ChannelEnv World
  where
	channelEnvKind env = (WORLD, env)
	mb_close_inet_receiver_without_id _ _ world
		= world

/*	PSt is also an environment instance of the class ChannelEnv	*/

instance ChannelEnv (PSt .l)
  where
	channelEnvKind env
		= (PST, env)
	mb_close_inet_receiver_without_id reallyDoIt id_pair pSt=:{io}
		= { pSt & io = mb_close_inet_receiver_without_id True id_pair io }

	
/*	IOSt is also an environment instance of the class ChannelEnv	*/

instance ChannelEnv (IOSt .l)
  where
	channelEnvKind env
		= (IOST, env)
	mb_close_inet_receiver_without_id False _ ioState
		= ioState
	mb_close_inet_receiver_without_id True id_pair ioState
	#! (closed,ioState)			= IOStClosed ioState
	| closed
		= ioState
	# (found,receivers,ioState)	= IOStGetDevice ReceiverDevice ioState
	| not found			// PA: guard added
		= ioState
	# rsHs						= (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
	  (found,rsH,rsHs)			= Remove (inetReceiverStateIdentified1 id_pair) undef rsHs
	# ioState					= IOStSetDevice (ReceiverSystemState {rReceivers=rsHs}) ioState
	| not found
		= ioState
	| otherwise
		# id					= rsH.rHandle.rId
		  (idtable,ioState)		= IOStGetIdTable ioState
		  ioState				= IOStSetIdTable (snd (removeIdFromIdTable id idtable)) ioState
		  ioState				= unbindRId id ioState
		  ioState				= IOStSetRcvDisabled True ioState // MW11++
		  connectedIds			= rsH.rHandle.rConnected
		  ioState				= seq (map closeReceiver connectedIds) ioState
		  inetInfo				= rsH.rHandle.rInetInfo
		  (_,_,_,closeFun)		= fromJust inetInfo
		  ioState				= appIOToolbox closeFun ioState
		= ioState

inetReceiverStateIdentified1 :: !(!EndpointRef`, !InetReceiverCategory`) !(ReceiverStateHandle .ps) -> Bool
inetReceiverStateIdentified1 x {rHandle} = inetReceiverIdentified x rHandle

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
        
class closeRChannel	ch	:: !*(*ch .a) !*env -> !*env	| ChannelEnv  env

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

class MaxSize ch
  where
	setMaxSize		::	!Int !*(*ch .a)	-> *(*ch .a)
	getMaxSize		::		 !*(*ch .a)	-> (!Int, !*(*ch .a))
	clearMaxSize	::		 !*(*ch .a)	-> *(*ch .a)

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
						|	EOM				// receiving "EOM" will automatically close the receiver
::	SendEvent			=	Sendable
						|	Disconnected	// receiving "Disconnected" will automatically close the receiver
instance == TimeoutReport
  where
	(==) TR_Expired	x	= case x of TR_Expired	 		-> True
										_				-> False
	(==) TR_Success	x	= case x of TR_Success			-> True
										_				-> False
	(==) TR_NoSuccess	x	= case x of TR_NoSuccess	-> True
										_				-> False

instance toString TimeoutReport
  where
	toString TR_Expired = "TR_Expired"
	toString TR_Success = "TR_Success"
	toString TR_NoSuccess = "TR_NoSuccess"
	
/////////////////// derived functions ////////////////////////////////////////////////////

nreceive_MT			::	!(Maybe !Timeout) !Int		!*(*ch .a)  !*env	
					->	(!TimeoutReport, ![.a],		!*(*ch .a), !*env)	
													| Receive ch & ChannelEnv  env
nreceive_MT mbTimeout n ch env
	#!	(before, env)	= getCurrentTick env
		(l, ch, env)	= receiveUpTo n ch env
		(length, l)		= u_length l
	|	length==n
		= (TR_Success, l, ch, env)
	#!	(tReport, mbData, ch, env)	= receive_MT mbTimeout ch env
	|	tReport<>TR_Success
		= (tReport, l, ch, env)
	|	n-length==1
		= (tReport, l++[fromJust mbData], ch, env)
	#!	(after, env)	= getCurrentTick env
	= nreceive_MT (mbSubtract mbTimeout (tickDifference after before)) (n-length-1) ch env
  where
	mbSubtract Nothing _ 		= Nothing
	mbSubtract (Just timeout) i	= Just (timeout-i)

u_reverse::![.a] -> [.a]
u_reverse list = reverse_ list []
where 
	reverse_ [hd:tl] list	= reverse_ tl [hd:list]
	reverse_ [] list		= list

u_length l
	= u_length_ l [] 0
  where
	u_length_ [] akku n
		= (n, u_reverse akku)
	u_length_ [h:t] akku n
		= u_length_ t [h:akku] (inc n)
	


receive	:: 	!*(*ch .a)  !*env -> (!.a, !*(*ch .a), !*env)
			| 	ChannelEnv env & Receive ch
receive ch env
	#!	(timeoutReport, mbMessage, ch, env)	= receive_MT Nothing ch env
	| timeoutReport==TR_NoSuccess
		#!	(isEom, ch, env)	= eom ch env
		| isEom
			= abort "\nStdChannels: receive failed"
		= receive ch env
	= (fromJust mbMessage, ch, env)

nreceive		::	!Int !*(*ch .a)  !*env -> (![.a], !*(*ch .a), !*env)
				|	ChannelEnv env & Receive ch
nreceive n ch env
	#!	(timeoutReport, l, ch, env)	= nreceive_MT Nothing n ch env
	| timeoutReport==TR_NoSuccess
		#!	(isEom, ch, env)	= eom ch env
		| isEom
			= abort "\nStdChannels: nreceive failed"
		= nreceive n ch env
	= (l, ch, env)

send	 	:: !.a	!*(*ch .a) !*env -> (!*(*ch .a), !*env)
			| 	ChannelEnv env & Send ch
send msg ch env
	#!	(_,_,ch,env)	= send_MT Nothing msg ch env
	= (ch, env)

closeChannel:: 			!*(*ch .a) !*env
			->			  			   !*env	| 	ChannelEnv env & Send ch
closeChannel ch env
	#!	(_,_,env)	= closeChannel_MT Nothing ch env
	= env

nsend 	:: ![.a]		!*(*ch .a) !*env
			->			   (!*(*ch .a), !*env)	| 	ChannelEnv env & nsend_MT ch
nsend msg ch env
	#!	(_,_,ch,env)	= nsend_MT Nothing msg ch env
	= (ch, env)

send_NB  	:: !.a	!*(*ch .a) !*env -> (!*(*ch .a), !*env)
			| 	ChannelEnv env & Send ch
send_NB msg ch env
	#!	(_,_,ch,env)	= send_MT (Just 0) msg ch env
	= (ch, env)

flushBuffer_NB	::				!*(*ch .a)  !*env
				->			   (!*(*ch .a), !*env)
				| 	ChannelEnv env & Send ch
flushBuffer_NB ch env
	#!	(_,_,ch,env)	= flushBuffer_MT (Just 0) ch env
	= (ch, env)

