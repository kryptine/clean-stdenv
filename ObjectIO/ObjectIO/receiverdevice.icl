implementation module receiverdevice


//	Clean Object I/O library, version 1.2


import	StdEnv
import	StdReceiver
import	devicefunctions, iostate, receiverevent, receiverid
from	commondef	import FatalError, URemove, UCond
from	StdPSt		import	appPIO, accPIO


receiverdeviceFatalError :: String String -> .x
receiverdeviceFatalError rule error
	= FatalError rule "receiverdevice" error

ReceiverFunctions :: DeviceFunctions (PSt .l .p)
ReceiverFunctions
	= {	dShow	= id
	  ,	dHide	= id
	  ,	dEvent	= receiverEvent
	  ,	dDoIO	= receiverIO
	  ,	dOpen	= receiverOpen
	  ,	dClose	= receiverClose
	  }

receiverOpen :: !(PSt .l .p) -> PSt .l .p
receiverOpen pState=:{io=ioState}
	# (hasReceiver,ioState)	= IOStHasDevice ReceiverDevice ioState
	| hasReceiver
		= {pState & io=ioState}
	| otherwise
		# ioState			= IOStSetDevice (ReceiverSystemState {rReceivers=[]}) ioState
		# (deviceFunctions,ioState)
							= IOStGetDeviceFunctions ioState
		# ioState			= IOStSetDeviceFunctions [ReceiverFunctions:deviceFunctions] ioState
		= {pState & io=ioState}

receiverClose :: !(PSt .l .p) -> PSt .l .p
receiverClose pState=:{io=ioState}
// MW11...
	# ioState				= callReceiverCloseFunctions ioState
	  ioState				= IOStSetRcvDisabled True ioState
// ... MW11
	# (receivers,ioState)	= IOStGetDevice ReceiverDevice ioState
	  rHs					= (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
	  rIds					= map (\{rHandle={rId}}->rId) rHs
	# (idtable,ioState)		= IOStGetIdTable ioState
	# ioState				= IOStSetIdTable (snd (removeIdsFromIdTable rIds idtable)) ioState
	# ioState				= unbindRIds rIds ioState
	# ioState				= IOStRemoveDevice ReceiverDevice ioState
	= {pState & io=ioState}
// MW11..
  where
	callReceiverCloseFunctions :: !(IOSt .l .p) -> (IOSt .l .p)
	callReceiverCloseFunctions ioState
		# (receivers,ioState)	= IOStGetDevice ReceiverDevice ioState
		  rHs					= (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
		= seq (map callCloseFunc rHs) ioState
	  where
		callCloseFunc {rHandle={rInetInfo=Nothing, rConnected}} ioState
			= seq (map closeReceiver rConnected) ioState
		callCloseFunc {rHandle={rInetInfo=Just (_,_,_,closeFun), rConnected}} ioState
			= appIOToolbox closeFun (seq (map closeReceiver rConnected) ioState)
// .. MW11

/*	The receiver handles three cases of message events (for the time being timers are not included in receivers):
	- QASyncMessage:
		this is a request to add the given asynchronous message to the indicated 
		receiver. Globally the size of the asynchronous message queue has already been
		increased. 
	- ASyncMessage:
		this is a request to handle the first asynchronous message available in the
		asynchronous message queue. Globally the size of the asynchronous message queue
		has already been decreased.
	- SyncMessage:
		this is a request to handle the given synchronous message.
*/
receiverIO :: !DeviceEvent !(PSt .l .p) -> (!DeviceEvent,!PSt .l .p)
receiverIO deviceEvent=:(ReceiverEvent (QASyncMessage event)) pState
	= (deviceEvent,receiverASyncIO event pState)
where
	receiverASyncIO :: !QASyncMessage !(PSt .l .p) -> PSt .l .p
	receiverASyncIO event=:{qasmRecLoc={rlReceiverId},qasmMsg} pState
		#	(receivers,ioState)	= IOStGetDevice ReceiverDevice pState.io
			rsHs				= (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
		#!	rsHs				= qMessage rlReceiverId qasmMsg rsHs
		#	ioState				= IOStSetDevice (ReceiverSystemState {rReceivers=rsHs}) ioState
		#	pState				= {pState & io=ioState}
		=	pState
	where
		qMessage :: !Id !SemiDynamic ![ReceiverStateHandle .pst] -> [ReceiverStateHandle .pst]
		qMessage rid msg [rsH=:{rHandle=rH}:rsHs]
			|	receiverIdentified rid rH
			#!	rH		= receiverAddASyncMessage rid msg rH
			=	[{rsH & rHandle=rH}:rsHs]
			|	otherwise
			#	rsHs	= qMessage rid msg rsHs
			=	[rsH:rsHs]
		qMessage _ _ rsHs
			=	rsHs
receiverIO deviceEvent=:(ReceiverEvent (ASyncMessage event)) pState
	# (receivers,pState)= accPIO (IOStGetDevice ReceiverDevice) pState
	  rHs				= ReceiverSystemStateGetReceiverHandles receivers
	# pState			= letOneReceiverDoIO rl rHs pState
	= (deviceEvent,pState)
where
	rl	= event.asmRecLoc
	
	letOneReceiverDoIO :: !RecLoc !(ReceiverHandles (PSt .l .p)) !(PSt .l .p) -> PSt .l .p
	letOneReceiverDoIO {rlParentId} {rReceivers=rsHs} pState
		= pState2
	where
		dummy			= receiverdeviceFatalError "receiverIO (ReceiverEvent (ASyncMessage _))" "receiver could not be found"
		(_,rsH1,rsHs1)	= URemove (identifyReceiverStateHandle rlParentId) dummy rsHs
		pState1			= appPIO (IOStSetDevice receivers) pState
		(rsH2,pState2)	= letReceiverDoIO rsH1 pState1
		receivers		= ReceiverSystemState {rReceivers=rsHs1++[rsH2]}
		
		letReceiverDoIO :: !(ReceiverStateHandle .pst) .pst -> (!ReceiverStateHandle .pst,.pst)
		letReceiverDoIO rsH=:{rState=ls,rHandle=rH=:{rASMQ=[msg:tailQ],rFun}} pState
			# (ls,_,pState)	= rFun msg (ls,pState)
			= ({rState=ls,rHandle={rH & rASMQ=tailQ}},pState)
		letReceiverDoIO _ _
			= receiverdeviceFatalError "letReceiverDoIO" "message queue of target receiver is empty"
receiverIO (ReceiverEvent (SyncMessage event)) pState
	# (lastProcess,pState)	= accPIO IOStLastInteraction pState
	# (event,pState)		= receiverSyncIO lastProcess event pState
	= (ReceiverEvent (SyncMessage event),pState)
where
	receiverSyncIO :: !Bool !SyncMessage !(PSt .l .p) -> (!SyncMessage,!PSt .l .p)
	receiverSyncIO lastProcess event pState
		| not found
		= (event1,pState2)
		with
			event1	= if lastProcess {event & smError=[ReceiverUnknown]} event
		| isEmpty error
		= ({event & smResp=resp},  pState2)
		= ({event & smError=error},pState2)
	where
		(receivers,ioState)	= IOStGetDevice ReceiverDevice pState.io
		rHs					= (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
		pState1				= {pState & io=IOStSetDevice (ReceiverSystemState {rReceivers=rHs1}) ioState}
		(found,error,resp,rHs1,pState2)
							= applyReceiverFunction event rHs pState1
		
		applyReceiverFunction :: !SyncMessage ![ReceiverStateHandle .pst] .pst
			-> (!Bool,[MessageError],[SemiDynamic],[ReceiverStateHandle .pst],.pst)
		applyReceiverFunction event=:{smRecLoc={rlReceiverId}} [rsH=:{rState=ls,rHandle=rH}:rsHs] ps
			| not (receiverIdentified rlReceiverId rH)
			= (found,error,resp,[rsH:rsHs1],ps1)
			with
				(found,error,resp,rsHs1,ps1) = applyReceiverFunction event rsHs ps
			| enabled rH.rSelect
			= (True,[],resp,[{rState=ls1,rHandle=rH1}:rsHs],ps1)
			with
				(resp,rH1,(ls1,ps1))	= receiverHandleSyncMessage event rH (ls,ps)
			= (True,[ReceiverUnable],[],[rsH:rsHs],ps)
		applyReceiverFunction _ rsHs ps
			= (False,[],[],rsHs,ps)
//	MW11..
receiverIO deviceEvent=:(InternetEvent event) pState
	#	(receivers,pState)	= accPIO (IOStGetDevice ReceiverDevice) pState
		rsHs				= ReceiverSystemStateGetReceiverHandles receivers
		pState				= letOneReceiverDoInetEvent event rsHs pState
	= (deviceEvent,pState)
// 	..MW11
receiverIO _ _
	= receiverdeviceFatalError "receiverIO" "device event passed receiver event filter without handling"

// MW11..
letOneReceiverDoInetEvent (eventCode,endpointRef,inetReceiverCategory,misc) {rReceivers=rsHs} pState
	# (opt_rsH,rsHs)		= selectReceiver (endpointRef,inetReceiverCategory) rsHs
	| isNothing opt_rsH
		= pState			// geen IOSetReceiverDevice nodig, omdat er geen veranderingen gebeurd zijn
	# eventInfo				= (eventCode,endpointRef,misc)
	# rsH = fromJust opt_rsH
	| rsH.rHandle.rSelect==Able && isEmpty rsH.rHandle.rASMQ
		 = applyInetEvent eventInfo rsH rsHs pState	// apply the event immediately
	// receiver is unable, so queue the event via asyncSend to handle it later
	# receivers			= ReceiverSystemState {rReceivers=[rsH:rsHs]} // left at the beginnig
	  pState			= appPIO (IOStSetDevice receivers) pState
	# (sR,pState)		= asyncSend (getInetReceiverRId rsH.rHandle) eventInfo pState
	| sR<>SendOk
		= abort "receiverdevice: I have a bug (78)"	
	= pState
  where

	selectReceiver 	:: !(!EndpointRef`,!InetReceiverCategory`) ![ReceiverStateHandle .ps]
					-> (Maybe (ReceiverStateHandle .ps),![ReceiverStateHandle .ps])
	selectReceiver receiverId=:(endpointRef,type) [rsH=:{rHandle={rInetInfo=Just (epr,tp,_,_)}}:rsHs]
		| endpointRef==epr && type==tp
			= (Just rsH,rsHs)
		# (opt_rsH, rsHs) = selectReceiver receiverId rsHs
		= (opt_rsH,[rsH:rsHs])
	selectReceiver receiverId [rsH:rsHs]
		# (opt_rsH, rsHs) = selectReceiver receiverId rsHs
		= (opt_rsH,[rsH:rsHs])
	selectReceiver _ []
		= (Nothing,[])
	

applyInetEvent	::	!InetReceiverASMQType !.(ReceiverStateHandle *(PSt .ls .ps))
					[ReceiverStateHandle *(PSt .ls .ps)] !*(PSt .ls .ps)
				->	(PSt .ls .ps)
applyInetEvent eventInfo rsH=:{rState,rHandle} rsHs pState
	= pState2
	with
		pState1				= appPIO (IOStSetDevice receivers) pState
		(rState2,pState2)	= receiverApplyInetEvent eventInfo rHandle (rState,pState1)
		receivers			= ReceiverSystemState {rReceivers=[{rsH & rState=rState2}:rsHs]} // left at the beginnig
// ..MW11

identifyReceiverStateHandle :: !Id !(ReceiverStateHandle .pst) -> (!Bool,!ReceiverStateHandle .pst)
identifyReceiverStateHandle id rsH=:{rHandle={rId}}
	= (id==rId,rsH)
