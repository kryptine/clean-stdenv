module echoServer

//  ********************************************************************************
//  Clean tutorial example program.
//  
//  This program demonstrates the usage of functions for event driven TCP.
//	It listens on port 7, accepts a connection and echoes the input
//
//  ********************************************************************************

import  StdEnv, StdTCP, StdIO

echoPort	:== 7

::	*PState	:== PSt TCP_DuplexChannel Bool
//	The Boolean value stores, whether EOM happened on the receive channel.

Start world
	# (ok, mbListener, world)	= openTCP_Listener echoPort world
	| not ok
		= abort ("chatServer: can't listen on port "+++toString echoPort)
	#!(console, world)		= stdio world
	  console	= fwrites "This server program waits until a client program " console
	  console	= fwrites "tries to connect.\n" console
	  (_,world)	= fclose console world
	  ((_,duplexChan), listener, world)
	  							= receive (fromJust mbListener) world
	  world						= closeRChannel listener world
	= startIO NDI duplexChan False initialize [] world

///////////////////////////////////////////////////////////
////	initialize - the function to initialze the PSt ////
///////////////////////////////////////////////////////////

initialize	:: PState -> PState
initialize pSt=:{ ls={rChannel,sChannel}, io }
	# (tcpRcvId, io)	= openId io
	  pSt 				= { pSt & ls = { rChannel=undef, sChannel=undef }, io=io }

  // open a receiver for the receive channel

	  (errReport1, pSt)	= openReceiver tcpRcvId 
	  								  (TCP_Receiver tcpRcvId rChannel rcvFun []) pSt

  // open a receiver for the send channel

	  (errReport2, sChannel, pSt)
						= openSendNotifier tcpRcvId 
										   (SendNotifier sChannel sndFun []) pSt
	| errReport1<>NoError || errReport2<>NoError
		= abort "error: can't open receiver"
	= { pSt & ls={ rChannel=undef, sChannel=sChannel } }

/////////////////////////////////////////////////////////////////////////////
////	rcvFun - the callback function for the receive channels receiver ////
/////////////////////////////////////////////////////////////////////////////

rcvFun :: (ReceiveMsg ByteSeq) (Id,PState) -> (Id,PState)
rcvFun (Received byteSeq) (tcpRcvId, pSt=:{ ls=ls=:{sChannel}, io})
	# (sChannel, io)		= send_NB byteSeq sChannel io
	  (buffSize,sChannel)	= bufferSize sChannel

  //  disable this receiver, if the send channel is full

	  io = case buffSize of
	  		0 -> io
	  		_ -> disableReceivers [tcpRcvId] io
	= (tcpRcvId, { pSt & ls={ ls & sChannel=sChannel}, io=io })
rcvFun EOM (tcpRcvId, pSt=:{ ls=ls=:{sChannel}, io})
	# (buffSize,sChannel)	= bufferSize sChannel
	  pSt	= { pSt & ls  = { ls & sChannel=sChannel}, ps=True, io=io }

  //  close program only, if all data in the send channels ineternal buffer has been
  //  sent

	  pSt = case buffSize of
	  			0 	-> closeProcess (close pSt)
				_	-> pSt
	= (tcpRcvId, pSt)

//////////////////////////////////////////////////////////////////////////
////	sndFun - the callback function for the send channels receiver ////
//////////////////////////////////////////////////////////////////////////

sndFun :: SendEvent (Id,PState) -> (Id,PState)
sndFun Sendable (tcpRcvId, pSt=:{ ls=ls=:{sChannel}, ps=eomHappened ,io})
	# (sChannel, io)		= flushBuffer_NB sChannel io
	  (buffSize,sChannel)	= bufferSize sChannel
	  pSt	= { pSt & ls  = { ls & sChannel=sChannel}, io=io }

  //  enable the receive channel's receiver again, if the send channel is still
  //  sendable

	  pSt = case (buffSize,eomHappened) of
	  			(0, False)	-> { pSt & io = enableReceivers [tcpRcvId] pSt.io }
	  			(0, True )	-> close pSt
				_			-> pSt
	= (tcpRcvId, pSt)
sndFun Disconnected (ls, pSt)
	= (ls, closeProcess pSt)

close	::	PState -> PState
close pSt=:{ls=ls=:{sChannel}, io}
	#!	io				= closeChannel sChannel io
	= { pSt & ls={ ls & sChannel=undef}, io=io }
