implementation module StdReceiver


//	Clean Object I/O library, version 1.2


import	StdInt, StdBool, StdList, StdTuple, StdOverloaded
import	commondef, id, receiveraccess, receiverdefaccess, receiverdevice, receiverid, scheduler
from	StdPSt	import	accPIO, appPIO, St


StdReceiverFatalError :: String String -> .x
StdReceiverFatalError rule error
	= FatalError rule "StdReceiver" error


//	Open one-way receiver:

receiverStateIdentified :: !Id !(ReceiverStateHandle .ps) -> Bool
receiverStateIdentified id {rHandle}
	= receiverIdentified id rHandle

class Receivers rdef where
	openReceiver	:: .ls !(rdef .ls (PSt .l .p)) !(PSt .l .p)	-> (!ErrorReport,!PSt .l .p)
	reopenReceiver	:: .ls !(rdef .ls (PSt .l .p)) !(PSt .l .p)	-> (!ErrorReport,!PSt .l .p)
	getReceiverType	::     .(rdef .ls .pst)						-> ReceiverType

instance Receivers (Receiver m) where
	openReceiver :: .ls !(Receiver m .ls (PSt .l .p)) !(PSt .l .p) -> (!ErrorReport,!PSt .l .p)
	openReceiver ls rDef pState
		# pState				= ReceiverFunctions.dOpen pState
		# (idtable,pState)		= accPIO IOStGetIdTable pState
		| memberIdTable id idtable
			= (ErrorIdsInUse,pState)
		# (rt,pState)			= accPIO IOStGetReceiverTable pState
		  maybe_parent			= getReceiverTableEntry id rt
		| isJust maybe_parent	// This condition should not occur: IdTable didn't contain Id while ReceiverTable does.
			= StdReceiverFatalError "openReceiver (Receiver)" "inconsistency detected between IdTable and ReceiverTable"
		| otherwise
			= (NoError,pState2)
		with
			(receivers,ioState1)= IOStGetDevice ReceiverDevice pState.io
			rsHs				= (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
			rsH					= newReceiverStateHandle id ls1 select f
			ioState2			= IOStSetDevice (ReceiverSystemState {rReceivers=[rsH:rsHs]}) ioState1
			ioState3			= bindRId id select id ReceiverDevice ioState2
			(ioId,ioState4)		= IOStGetIOId ioState3
			ioState5			= IOStSetIdTable (snd (addIdToIdTable id {idpIOId=ioId,idpDevice=ReceiverDevice,idpId=id} idtable)) ioState4
			pState1				= {pState & io=ioState5}
			(ls1,pState2)		= receiverInit (ls,pState1)
	where
		rid						= receiverDefRId         rDef
		select					= receiverDefSelectState rDef
		f						= receiverDefFunction    rDef
		receiverInit			= getReceiverInitFun (snd (Select isReceiverInit (ReceiverInit (\x->x)) (receiverDefAttributes rDef)))
		id						= RIdtoId rid
	
	reopenReceiver :: .ls !(Receiver m .ls (PSt .l .p)) !(PSt .l .p) -> (!ErrorReport,!PSt .l .p)
	reopenReceiver ls rDef pState
		= openReceiver ls rDef (appPIO (closeReceiver (RIdtoId (receiverDefRId rDef))) pState)
	
	getReceiverType :: .(Receiver m .ls .pst) -> ReceiverType
	getReceiverType _			= "Receiver"

instance Receivers (Receiver2 m r) where
	openReceiver :: .ls !(Receiver2 m r .ls (PSt .l .p)) !(PSt .l .p) -> (!ErrorReport,!PSt .l .p)
	openReceiver ls rDef pState
		# pState				= ReceiverFunctions.dOpen pState
		# (idtable,pState)		= accPIO IOStGetIdTable pState
		| memberIdTable id idtable
			= (ErrorIdsInUse,pState)
		# (rt,pState)			= accPIO IOStGetReceiverTable pState
		  maybe_parent			= getReceiverTableEntry id rt
		| isJust maybe_parent	// This condition should not occur: IdTable didn't contain Id while ReceiverTable does.
			= StdReceiverFatalError "openReceiver (Receiver2)" "inconsistency detected between IdTable and ReceiverTable"
		| otherwise
			= (NoError,pState2)
		with
			(receivers,ioState1)= IOStGetDevice ReceiverDevice pState.io
			rsHs				= (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
			rsH					= newReceiverStateHandle2 id ls1 select f
			ioState2			= IOStSetDevice (ReceiverSystemState {rReceivers=[rsH:rsHs]}) ioState1
			ioState3			= bindRId id select id ReceiverDevice ioState2
			(ioId,ioState4)		= IOStGetIOId ioState3
			ioState5			= IOStSetIdTable (snd (addIdToIdTable id {idpIOId=ioId,idpDevice=ReceiverDevice,idpId=id} idtable)) ioState4
			pState1				= {pState & io=ioState5}
			(ls1,pState2)		= receiverInit (ls,pState1)
	where
		r2id					= receiver2DefR2Id        rDef
		select					= receiver2DefSelectState rDef
		f						= receiver2DefFunction    rDef
		receiverInit			= getReceiverInitFun (snd (Select isReceiverInit (ReceiverInit (\x->x)) (receiver2DefAttributes rDef)))
		id						= R2IdtoId r2id
	
	reopenReceiver :: .ls !(Receiver2 m r .ls (PSt .l .p)) !(PSt .l .p) -> (!ErrorReport,!PSt .l .p)
	reopenReceiver ls rDef pState
		= openReceiver ls rDef (appPIO (closeReceiver (R2IdtoId (receiver2DefR2Id rDef))) pState)
	
	getReceiverType :: .(Receiver2 m r .ls .pst) -> ReceiverType
	getReceiverType _			= "Receiver2"


//	Closing receivers.

closeReceiver :: !Id !(IOSt .l .p) -> IOSt .l .p
closeReceiver id ioState
	| not (isCustomRId id || isCustomR2Id id)
		= ioState
	# (receivers,ioState)	= IOStGetDevice ReceiverDevice ioState
	  rsHs					= (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
	  (found,_,rsHs)		= Remove (receiverStateIdentified id) (dummy "closeReceiver") rsHs
	# ioState				= IOStSetDevice (ReceiverSystemState {rReceivers=rsHs}) ioState
	| not found
		= ioState
	| otherwise
		# (idtable,ioState)	= IOStGetIdTable ioState
		# ioState			= IOStSetIdTable (snd (removeIdFromIdTable id idtable)) ioState
		= unbindRId id ioState


//	Get the Ids and ReceiverTypes of all current receivers:

getReceivers :: !(IOSt .l .p) -> (![(Id,ReceiverType)],!IOSt .l .p)
getReceivers ioState
	# (receivers,ioState)	= IOStGetDevice ReceiverDevice ioState
	  (idstypes,rsHs)		= getreceivers (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
	# ioState				= IOStSetDevice (ReceiverSystemState {rReceivers=rsHs}) ioState
	= (idstypes,ioState)
where
	getreceivers :: ![ReceiverStateHandle .ps] -> ([(Id,ReceiverType)],![ReceiverStateHandle .ps])
	getreceivers [rsH:rsHs]
		# (idtype, rsH)		= getreceiver  rsH
		  (idtypes,rsHs)	= getreceivers rsHs
		= ([idtype:idtypes],[rsH:rsHs])
	where
		getreceiver :: !(ReceiverStateHandle .ps) -> ((Id,ReceiverType),!ReceiverStateHandle .ps)
		getreceiver rsH=:{rHandle={rId,rOneWay}}
			= ((rId,rType),rsH)
		where
			rType	= if rOneWay "Receiver" "Receiver2"
	getreceivers _
		= ([],[])


//	Changing attributes:

enableReceivers :: ![Id] !(IOSt .l .p) -> IOSt .l .p
enableReceivers ids ioState
	= changeReceivers (receiverSetSelectState Able) (receiverEntrySetSelectState Able) ids ioState

disableReceivers :: ![Id] !(IOSt .l .p) -> IOSt .l .p
disableReceivers ids ioState
	= changeReceivers (receiverSetSelectState Unable) (receiverEntrySetSelectState Unable) ids ioState

receiverEntrySetSelectState :: !SelectState !ReceiverTableEntry -> ReceiverTableEntry
receiverEntrySetSelectState select rte
	= {rte & rteSelectState=select}

changeReceivers :: (IdFun (ReceiverStateHandle (PSt .l .p))) (IdFun ReceiverTableEntry) ![Id] !(IOSt .l .p)
																							 -> IOSt .l .p
changeReceivers changeReceiverState changeReceiverEntry ids ioState
	| isEmpty okids			// There aren't any receiver ids in the list
		= ioState
	# (receivers,ioState)	= IOStGetDevice ReceiverDevice ioState
	  rsHs					= (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
	  (myids,rsHs)			= changereceiverstates changeReceiverState okids rsHs
	# ioState				= IOStSetDevice (ReceiverSystemState {rReceivers=rsHs}) ioState
	| isEmpty myids			// No receivers were changed
		= ioState
	| otherwise
		# (rt,ioState)		= IOStGetReceiverTable ioState
		  rt				= changereceiverentries changeReceiverEntry myids rt
		# ioState			= IOStSetReceiverTable rt ioState
		= ioState
where
	okids					= filter (\id->isCustomRId id || isCustomR2Id id) ids
	
	changereceiverstates :: !(IdFun (ReceiverStateHandle .ps)) ![Id] ![ReceiverStateHandle .ps]
														   -> (![Id],![ReceiverStateHandle .ps])
	changereceiverstates f ids rsHs
		| isEmpty ids || isEmpty rsHs	= ([],[])
	changereceiverstates f ids [rsH=:{rHandle={rId}}:rsHs]
		| hadId							= ([rId:rIds],[f rsH:rsHs1])
		| otherwise						= (     rIds, [  rsH:rsHs1])
	where
		(hadId,_,ids1)					= Remove ((==) rId) (dummy "changereceiverstates") ids
		(rIds,rsHs1)					= changereceiverstates f ids1 rsHs
	
	changereceiverentries :: !(IdFun ReceiverTableEntry) ![Id] !ReceiverTable -> ReceiverTable
	changereceiverentries f [id:ids] rt
		# maybe_rte					= getReceiverTableEntry id rt
		| isNothing maybe_rte		= changereceiverentries f ids rt
		| otherwise					= changereceiverentries f ids (setReceiverTableEntry (f (fromJust maybe_rte)) rt)
	changereceiverentries _ _ rt	= rt


//	Get the SelectState of a receiver:

getReceiverSelectState :: !Id !(IOSt .l .p) -> (!Maybe SelectState,!IOSt .l .p)
getReceiverSelectState id ioState
	| not (isCustomRId id || isCustomR2Id id)
		= (Nothing,ioState)
	| otherwise
		# (receivers,ioState)	= IOStGetDevice ReceiverDevice ioState
		  rsHs					= (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
		  (select,rsHs1)		= getselectstate id rsHs
		# ioState				= IOStSetDevice (ReceiverSystemState {rReceivers=rsHs1}) ioState
		= (select,ioState)
where
	getselectstate :: !Id ![ReceiverStateHandle .ps] -> (!Maybe SelectState,![ReceiverStateHandle .ps])
	getselectstate id [rsH=:{rHandle=rH=:{rSelect}}:rsHs]
		| receiverIdentified id rH	= (Just rSelect,[rsH:rsHs])
		| otherwise					= (select,[rsH:rsHs1])
		with
			(select,rsHs1)			= getselectstate id rsHs
	getselectstate _ _
		= (Nothing,[])


//	Message passing:

::	SendReport
	=	SendOk
	|	SendUnknownReceiver
	|	SendUnableReceiver
	|	SendDeadlock

instance == SendReport where
	(==) :: !SendReport !SendReport -> Bool
	(==) SendOk					report	= case report of
											SendOk	-> True
											_		-> False
	(==) SendUnknownReceiver	report	= case report of
											SendUnknownReceiver	-> True
											_		-> False
	(==) SendUnableReceiver		report	= case report of
											SendUnableReceiver	-> True
											_		-> False
	(==) SendDeadlock			report	= case report of
											SendDeadlock	-> True
											_		-> False

instance toString SendReport where
	toString :: !SendReport -> {#Char}
	toString SendOk				= "SendOk"
	toString SendUnknownReceiver= "SendUnknownReceiver"
	toString SendUnableReceiver	= "SendUnableReceiver"
	toString SendDeadlock		= "SendDeadlock"


/*	Asynchronous, uni-directional, message passing.
	If the receiver could not be found in the global ReceiverTable, 
		then return SendUnknownReceiver and do nothing.
	If the receiver could be found, then increase the length of the 
		asynchronous message queue of the receiver in the global 
		ReceiverTable.
		Add the message to the asynchronous message queue of the 
		receiver using the scheduler.
*/
asyncSend :: !(RId msg) msg !(PSt .l .p) -> (!SendReport,!PSt .l .p)
asyncSend rid msg pState
	# id					= RIdtoId rid
	# (rt,ioState)			= IOStGetReceiverTable pState.io
	  maybe_rte				= getReceiverTableEntry id rt
	| isNothing maybe_rte
		= (SendUnknownReceiver,{pState & io=ioState})
	# rte					= fromJust maybe_rte
	  rteLoc				= rte.rteLoc
	  pid					= rteLoc.rlIOId
	  qEvent				= {qasmRecLoc=rteLoc,qasmMsg=openDynamic (RIdtoDId rid) msg}
	# (ioid,ioState)		= IOStGetIOId ioState
	  rte					= {rte & rteASMCount=rte.rteASMCount+1}
	  rt					= setReceiverTableEntry rte rt
	# ioState				= IOStSetReceiverTable rt ioState
	# pState				= {pState & io=ioState}
	| pid==ioid
		= (SendOk,pState1)
	with
		(_,_,pState1)		= handleOneEventForDevices (ScheduleMsgEvent (QASyncMessage qEvent)) pState
	# (maybe_error,_,pState)= cswitchProcess pid (ScheduleMsgEvent (QASyncMessage qEvent)) pState
	| (isJust maybe_error)
		= (toSendError (fromJust maybe_error),pState)
	| otherwise
		= (SendOk,pState)


/*	Synchronous, uni-directional, message passing.
	If the receiver could not be found in the global ReceiverTable,
		then return SendUnknownReceiver and do nothing.
	If the receiver could be found, then let the receiver handle the
		synchronous message using the scheduler.
*/
syncSend :: !(RId msg) msg !(PSt .l .p) -> (!SendReport, !PSt .l .p)
syncSend rid msg pState
	# id						= RIdtoId rid
	# (rt,ioState)				= IOStGetReceiverTable pState.io
	  maybe_parent				= getReceiverTableEntry id rt
	| isNothing maybe_parent
		= (SendUnknownReceiver,{pState & io=ioState})
	# parent					= fromJust maybe_parent
	  rteLoc					= parent.rteLoc
	  pid						= rteLoc.rlIOId
	  sEvent					= {	smRecLoc	= rteLoc
		  						  ,	smMsg		= openDynamic (RIdtoDId rid) msg
		  						  ,	smResp		= []
		  						  ,	smError		= []
		  						  }
	# (ioid,ioState)			= IOStGetIOId ioState
	# pState					= {pState & io=ioState}
	| pid==ioid
		= PStHandleSyncMessage sEvent pState
	| otherwise
		# (opt_error,_,pState)	= cswitchProcess pid (ScheduleMsgEvent (SyncMessage sEvent)) pState
		  report				= if (isJust opt_error) (toSendError (fromJust opt_error)) SendOk
		= (report,pState)
where
	PStHandleSyncMessage :: !SyncMessage !(PSt .l .p) -> (!SendReport, !PSt .l .p)
	PStHandleSyncMessage sm pState
		# (_,schedulerEvent,pState)
								= handleOneEventForDevices (ScheduleMsgEvent (SyncMessage sm)) pState
		  sm					= case schedulerEvent of
						  			(ScheduleMsgEvent (SyncMessage sm))	-> sm
				  					_									-> StdReceiverFatalError "syncSend" "unexpected scheduler event"
		  errors				= sm.smError
		  report				= if (isEmpty errors)
				  					SendOk
				  					(case (hd errors) of
										ReceiverUnable	-> SendUnableReceiver
										ReceiverUnknown	-> SendUnknownReceiver
									)
		= (report,pState)


/*	Synchronous, bi-directional, message passing.
	If the receiver could not be found in the global ReceiverTable,
		then return SendUnknownReceiver and do nothing.
	If the receiver could be found, then let the receiver handle the
		synchronous message using the scheduler.
*/
syncSend2 :: !(R2Id msg resp) msg !(PSt .l .p) -> (!(!SendReport,!Maybe resp), !PSt .l .p)
syncSend2 r2id msg pState
	# id						= R2IdtoId r2id
	# (rt,pState)				= accPIO IOStGetReceiverTable pState
	  maybe_parent				= getReceiverTableEntry id rt
	| isNothing maybe_parent
		= ((SendUnknownReceiver,Nothing),pState)
	# parent					= fromJust maybe_parent
	  rteLoc					= parent.rteLoc
	  pid						= rteLoc.rlIOId
	  sEvent					= {	smRecLoc		= rteLoc
	  							  ,	smMsg			= openDynamic (R2IdtoDId r2id) msg
	  							  ,	smResp			= []
	  							  ,	smError			= []
	  							  }
	# (ioid,pState)				= accPIO IOStGetIOId pState
	| pid==ioid
		= PStHandleSync2Message (R2IdtoDId` r2id) sEvent pState
	# (opt_error,resp,pState)	= cswitchProcess pid (ScheduleMsgEvent (SyncMessage sEvent)) pState
	| isJust opt_error
		= ((toSendError (fromJust opt_error),Nothing),pState)
	| isEmpty resp
		= StdReceiverFatalError "syncSend2" "no response received from bi-directional receiver"
	| otherwise
		= ((SendOk,Just (readDynamic (R2IdtoDId` r2id) (hd resp))),pState)
where
	PStHandleSync2Message :: !(DId resp) !SyncMessage !(PSt .l .p) -> (!(!SendReport,!Maybe resp), !PSt .l .p)
	PStHandleSync2Message did sm pState
		# (_,schedulerEvent,pState)
								= handleOneEventForDevices (ScheduleMsgEvent (SyncMessage sm)) pState
		  sm					= case schedulerEvent of
		  							(ScheduleMsgEvent (SyncMessage sm))	-> sm
		  							_									-> StdReceiverFatalError "syncSend2" "unexpected scheduler event"
		  errors				= sm.smError
		  resps					= sm.smResp
		  resp					= if (isEmpty resps ) Nothing (Just (readDynamic did (hd resps)))
		  report				= if (isEmpty errors) SendOk (case (hd errors) of
																ReceiverUnable	-> SendUnableReceiver
																ReceiverUnknown	-> SendUnknownReceiver
															 )
		= ((report,resp),pState)

toSendError :: !SwitchError -> SendReport
toSendError SwitchToYourself				= SendUnknownReceiver
toSendError SwitchToDoesNotExist			= SendUnknownReceiver
toSendError SwitchToReceiverDoesNotExist	= SendUnknownReceiver
toSendError SwitchReceiverUnable			= SendUnableReceiver
toSendError SwitchEndsUpInDeadlock			= SendDeadlock
