implementation module StdChannelEnv


import StdBool, StdFunc, StdList, StdMisc, StdTuple
import channelenv
import commondef, iostate, receiverid, StdPStClass, StdReceiver


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
