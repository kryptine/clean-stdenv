implementation module StdReceiver


//	Clean Object I/O library, version 1.2.1


import	StdInt, StdBool, StdList, StdTuple, StdOverloaded, StdFunc
import	commondef, id, receiveraccess, receiverdefaccess, receiverdevice, receiverid, scheduler
from	StdPSt	import	accPIO, appPIO, St


StdReceiverFatalError :: String String -> .x
StdReceiverFatalError rule error
	= FatalError rule "StdReceiver" error


//	Open one-way receiver:

class Receivers rdef where
	openReceiver	:: .ls !*(*rdef .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)
	getReceiverType	::      *(*rdef .ls .pst)				-> ReceiverType

instance Receivers (Receiver m) where
// MW11 was	openReceiver :: .ls !(Receiver m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)
	openReceiver :: .ls !*(*Receiver m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)
	openReceiver ls rDef pState
		# pState					= ReceiverFunctions.dOpen pState
		# (idtable,ioState)			= IOStGetIdTable pState.io
		| memberIdTable id idtable
			= (ErrorIdsInUse,{pState & io=IOStSetIdTable idtable ioState})
		# (rt,ioState)				= IOStGetReceiverTable ioState
		# (maybe_parent,rt)			= getReceiverTableEntry id rt
		# ioState					= IOStSetReceiverTable rt ioState
		| isJust maybe_parent		// This condition should not occur: IdTable didn't contain Id while ReceiverTable does.
			= StdReceiverFatalError "openReceiver (Receiver)" "inconsistency detected between IdTable and ReceiverTable"
		# (found,rDevice,ioState)	= IOStGetDevice ReceiverDevice ioState
		| not found					// This condition should not occur: ReceiverDevice has just been 'installed'
			= StdReceiverFatalError "openReceiver (Receiver)" "could not retrieve ReceiverSystemState from IOSt"
		| otherwise
			= (NoError,pState2)
		with
			rsHs				= (ReceiverSystemStateGetReceiverHandles rDevice).rReceivers
// MW11 was			rsH					= newReceiverStateHandle id ls1 select f
			rsH					= newReceiverStateHandle id ls1 select connectedIds f
			ioState2			= IOStSetDevice (ReceiverSystemState {rReceivers=[rsH:rsHs]}) ioState
			ioState3			= bindRId id select id ReceiverDevice ioState2
			(ioId,ioState4)		= IOStGetIOId ioState3
			ioState5			= IOStSetIdTable (snd (addIdToIdTable id {idpIOId=ioId,idpDevice=ReceiverDevice,idpId=id} idtable)) ioState4
			pState1				= {pState & io=ioState5}
			(ls1,pState2)		= receiverInit (ls,pState1)
	where
		rid						= receiverDefRId         rDef
		select					= receiverDefSelectState rDef
		f						= receiverDefFunction    rDef
// MW11 was		receiverInit			= getReceiverInitFun (snd (Select isReceiverInit (ReceiverInit (\x->x)) (receiverDefAttributes rDef)))
		receiverInit			= getReceiverInitFun (snd (Select isReceiverInit (ReceiverInit (\x->x)) rDefAttributes))
		connectedIds			= getReceiverConnectedReceivers (snd (Select isReceiverConnectedReceivers (ReceiverConnectedReceivers []) rDefAttributes)) // MW11++
		id						= RIdtoId rid
		rDefAttributes			= receiverDefAttributes rDef // MW11++
	
/* MW11
	reopenReceiver :: .ls !(Receiver m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)
	reopenReceiver ls rDef pState
		= openReceiver ls rDef (appPIO (closeReceiver (RIdtoId (receiverDefRId rDef))) pState)
*/	
	getReceiverType :: *(*Receiver m .ls .pst) -> ReceiverType
	getReceiverType _			= "Receiver"

instance Receivers (Receiver2 m r) where
// MW11 was	openReceiver :: .ls !(Receiver2 m r .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)
	openReceiver :: .ls !*(*Receiver2 m r .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)
	openReceiver ls rDef pState
		# pState					= ReceiverFunctions.dOpen pState
		# (idtable,ioState)			= IOStGetIdTable pState.io
		| memberIdTable id idtable
			= (ErrorIdsInUse,{pState & io=IOStSetIdTable idtable ioState})
		# (rt,ioState)				= IOStGetReceiverTable ioState
		# (maybe_parent,rt)			= getReceiverTableEntry id rt
		# ioState					= IOStSetReceiverTable rt ioState
		| isJust maybe_parent		// This condition should not occur: IdTable didn't contain Id while ReceiverTable does.
			= StdReceiverFatalError "openReceiver (Receiver2)" "inconsistency detected between IdTable and ReceiverTable"
		# (found,rDevice,ioState)	= IOStGetDevice ReceiverDevice ioState
		| not found
			= StdReceiverFatalError "openReceiver (Receiver2)" "could not retrieve ReceiverSystemState from IOSt"
		| otherwise
			= (NoError,pState2)
		with
			rsHs				= (ReceiverSystemStateGetReceiverHandles rDevice).rReceivers
// MW11 was			rsH					= newReceiverStateHandle2 id ls1 select f
			rsH					= newReceiverStateHandle2 id ls1 select connectedIds f
			ioState2			= IOStSetDevice (ReceiverSystemState {rReceivers=[rsH:rsHs]}) ioState
			ioState3			= bindRId id select id ReceiverDevice ioState2
			(ioId,ioState4)		= IOStGetIOId ioState3
			ioState5			= IOStSetIdTable (snd (addIdToIdTable id {idpIOId=ioId,idpDevice=ReceiverDevice,idpId=id} idtable)) ioState4
			pState1				= {pState & io=ioState5}
			(ls1,pState2)		= receiverInit (ls,pState1)
	where
		r2id					= receiver2DefR2Id        rDef
		select					= receiver2DefSelectState rDef
		f						= receiver2DefFunction    rDef
// MW11 was		receiverInit			= getReceiverInitFun (snd (Select isReceiverInit (ReceiverInit (\x->x)) (receiver2DefAttributes rDef)))
		receiverInit			= getReceiverInitFun (snd (Select isReceiverInit (ReceiverInit (\x->x)) rDefAttributes))
		connectedIds			= getReceiverConnectedReceivers (snd (Select isReceiverConnectedReceivers (ReceiverConnectedReceivers []) rDefAttributes)) // MW11++
		id						= R2IdtoId r2id
		rDefAttributes			= receiver2DefAttributes rDef // MW11++
	
/* MW11
	reopenReceiver :: .ls !(Receiver2 m r .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)
	reopenReceiver ls rDef pState
		= openReceiver ls rDef (appPIO (closeReceiver (R2IdtoId (receiver2DefR2Id rDef))) pState)
*/
	
	getReceiverType :: *(*Receiver2 m r .ls .pst) -> ReceiverType
	getReceiverType _			= "Receiver2"


//	Closing receivers.

closeReceiver :: !Id !(IOSt .l) -> IOSt .l
closeReceiver id ioState
// MW11..
	#! (closed,ioState)			= IOStClosed ioState
	| closed
		= ioState
// ...MW11
// MW11 was	| not (isCustomRId id || isCustomR2Id id)
	| not (isCustomRId id || isCustomR2Id id || isCustomId id)
		= ioState
	# (found,rDevice,ioState)	= IOStGetDevice ReceiverDevice ioState
	| not found
		= ioState
	# rsHs						= (ReceiverSystemStateGetReceiverHandles rDevice).rReceivers
	  (found,rsH,rsHs)			= URemove (receiverStateIdentified id) (dummy "closeReceiver") rsHs
	# ioState					= IOStSetDevice (ReceiverSystemState {rReceivers=rsHs}) ioState
	| not found
		= ioState
	| otherwise
		# (idtable,ioState)		= IOStGetIdTable ioState
		  ioState				= IOStSetIdTable (snd (removeIdFromIdTable id idtable)) ioState
		  ioState				= unbindRId id ioState
// MW11..
		  ioState				= IOStSetRcvDisabled True ioState
		  connectedIds			= rsH.rHandle.rConnected
		  ioState				= seq (map closeReceiver connectedIds) ioState
		  inetInfo				= rsH.rHandle.rInetInfo
		| isNothing inetInfo
			= ioState
		# (_,_,_,closeFun)		= fromJust inetInfo
		  ioState				= appIOToolbox closeFun ioState
// ..MW11
		= ioState
where
	receiverStateIdentified :: !Id !(ReceiverStateHandle .pst) -> *(!Bool,!ReceiverStateHandle .pst)
	receiverStateIdentified id rsH=:{rHandle}
		= (receiverIdentified id rHandle,rsH)

//	Get the Ids and ReceiverTypes of all current receivers:

getReceivers :: !(IOSt .l) -> (![(Id,ReceiverType)],!IOSt .l)
getReceivers ioState
	# (found,rDevice,ioState)	= IOStGetDevice ReceiverDevice ioState
	| not found
		= ([],ioState)
	# (idstypes,rsHs)			= getreceivers (ReceiverSystemStateGetReceiverHandles rDevice).rReceivers
	# ioState					= IOStSetDevice (ReceiverSystemState {rReceivers=rsHs}) ioState
	= (idstypes,ioState)
where
	getreceivers :: ![ReceiverStateHandle .pst] -> ([(Id,ReceiverType)],![ReceiverStateHandle .pst])
	getreceivers [rsH:rsHs]
		# (idtype, rsH)		= getreceiver  rsH
		  (idtypes,rsHs)	= getreceivers rsHs
		= ([idtype:idtypes],[rsH:rsHs])
	where
		getreceiver :: !(ReceiverStateHandle .pst) -> ((Id,ReceiverType),!ReceiverStateHandle .pst)
		getreceiver rsH=:{rHandle={rId,rOneWay}}
			= ((rId,rType),rsH)
		where
			rType	= if rOneWay "Receiver" "Receiver2"
	getreceivers _
		= ([],[])


//	Changing attributes:

enableReceivers :: ![Id] !(IOSt .l) -> IOSt .l
enableReceivers ids ioState
	= changeReceivers (receiverSetSelectState Able) (receiverEntrySetSelectState Able) ids ioState

disableReceivers :: ![Id] !(IOSt .l) -> IOSt .l
disableReceivers ids ioState
	# ioState = IOStSetRcvDisabled True ioState
	= changeReceivers (receiverSetSelectState Unable) (receiverEntrySetSelectState Unable) ids ioState

receiverEntrySetSelectState :: !SelectState !ReceiverTableEntry -> ReceiverTableEntry
receiverEntrySetSelectState select rte
	= {rte & rteSelectState=select}

changeReceivers :: (IdFun (ReceiverStateHandle (PSt .l))) (IdFun ReceiverTableEntry) ![Id] !(IOSt .l) -> IOSt .l
changeReceivers changeReceiverState changeReceiverEntry ids ioState
	| isEmpty okids			// There aren't any receiver ids in the list
		= ioState
	# (found,rDevice,ioState)	= IOStGetDevice ReceiverDevice ioState
	| not found
		= ioState
	# rsHs						= (ReceiverSystemStateGetReceiverHandles rDevice).rReceivers
// MW11..
//	  allIds					= getConnectedIds okids [] rsHs rsHs
// PA...
	  (allIds,rsHs)				= getConnectedIds okids rsHs
// ...PA
// ..MW11
// MW11 was	  (myids,rsHs)			= changereceiverstates changeReceiverState okids rsHs
	  (myids,rsHs)				= changereceiverstates changeReceiverState allIds rsHs
	# ioState					= IOStSetDevice (ReceiverSystemState {rReceivers=rsHs}) ioState
	| isEmpty myids			// No receivers were changed
		= ioState
	| otherwise
		# (rt,ioState)			= IOStGetReceiverTable ioState
		  rt					= changereceiverentries changeReceiverEntry myids rt
		# ioState				= IOStSetReceiverTable rt ioState
		= ioState
where
// MW11 was	okids					= filter (\id->isCustomRId id || isCustomR2Id id) ids
	okids						= filter (\id->isCustomRId id || isCustomR2Id id || isCustomId id) ids
	
/* MW11..
	getConnectedIds	:: ![Id] ![Id] ![ReceiverStateHandle .pst] ![ReceiverStateHandle .pst] -> [Id]
	getConnectedIds ids _ [] _
		= ids
	getConnectedIds ids alreadyHandled [rsH=:{rHandle={rId,rConnected}}:rsHs] allStateHandles
		| not (isMember rId ids) || isEmpty rConnected || isMember rId alreadyHandled
			= getConnectedIds ids alreadyHandled rsHs allStateHandles
		| otherwise
			= getConnectedIds (removeDup (rConnected++ids)) [rId:alreadyHandled] allStateHandles allStateHandles
		// search again in the whole set of receivers
..MW11 */
// PA...: replaced by the following because of uniqueness
	getConnectedIds :: ![Id] !*[ReceiverStateHandle .pst] -> (![Id],!*[ReceiverStateHandle .pst])
	getConnectedIds ids rsHs
		# (rshIds,rsHs)	= AccessList getReceiverIds rsHs
		= (getConnectedIds` ids [] rshIds rshIds,rsHs)
	where
		getReceiverIds :: !*(ReceiverStateHandle .pst) -> ((Id,[Id]),!*ReceiverStateHandle .pst)
		getReceiverIds rsH=:{rHandle={rId,rConnected}} = ((rId,rConnected),rsH)
		
		getConnectedIds`	:: ![Id] ![Id] ![(Id,[Id])] ![(Id,[Id])] -> [Id]
		getConnectedIds` ids _ [] _
			= ids
		getConnectedIds` ids alreadyHandled [(rId,rConnected):rsHs] allStateHandles
			| not (isMember rId ids) || isEmpty rConnected || isMember rId alreadyHandled
				= getConnectedIds` ids alreadyHandled rsHs allStateHandles
			| otherwise	// search again in the whole set of receivers
				= getConnectedIds` (removeDup (rConnected++ids)) [rId:alreadyHandled] allStateHandles allStateHandles
// ...PA

	changereceiverstates :: !(IdFun (ReceiverStateHandle .pst)) ![Id] ![ReceiverStateHandle .pst]
															-> (![Id],![ReceiverStateHandle .pst])
	changereceiverstates _ [] rsHs
		= ([],rsHs)
	changereceiverstates _ ids []
		= ([],[])
	changereceiverstates f ids [rsH=:{rHandle={rId}}:rsHs]
		| hadId
			= ([rId:rIds],[f rsH:rsHs1])
		| otherwise
			= (     rIds, [  rsH:rsHs1])
	where
		(hadId,_,ids1)	= Remove ((==) rId) (dummy "changereceiverstates") ids
		(rIds,rsHs1)	= changereceiverstates f ids1 rsHs
	
	changereceiverentries :: !(IdFun ReceiverTableEntry) ![Id] !*ReceiverTable -> *ReceiverTable
	changereceiverentries f [id:ids] rt
		# (maybe_rte,rt)			= getReceiverTableEntry id rt
		| isNothing maybe_rte		= changereceiverentries f ids rt
		| otherwise					= changereceiverentries f ids (setReceiverTableEntry (f (fromJust maybe_rte)) rt)
	changereceiverentries _ _ rt
		= rt


//	Get the SelectState of a receiver:

getReceiverSelectState :: !Id !(IOSt .l) -> (!Maybe SelectState,!IOSt .l)
getReceiverSelectState id ioState
// MW11 was	| not (isCustomRId id || isCustomR2Id id)
	| not (isCustomRId id || isCustomR2Id id || isCustomId id)
		= (Nothing,ioState)
	# (found,rDevice,ioState)	= IOStGetDevice ReceiverDevice ioState
	| not found
		= (Nothing,ioState)
	| otherwise
		# rsHs					= (ReceiverSystemStateGetReceiverHandles rDevice).rReceivers
		  (select,rsHs1)		= getselectstate id rsHs
		# ioState				= IOStSetDevice (ReceiverSystemState {rReceivers=rsHs1}) ioState
		= (select,ioState)
where
	getselectstate :: !Id ![ReceiverStateHandle .pst] -> (!Maybe SelectState,![ReceiverStateHandle .pst])
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
	|	OtherSendReport !String

instance == SendReport where
	(==) :: !SendReport !SendReport -> Bool
	(==) SendOk					report	= case report of
											SendOk				-> True
											_					-> False
	(==) SendUnknownReceiver	report	= case report of
											SendUnknownReceiver	-> True
											_					-> False
	(==) SendUnableReceiver		report	= case report of
											SendUnableReceiver	-> True
											_					-> False
	(==) SendDeadlock			report	= case report of
											SendDeadlock		-> True
											_					-> False
	(==) (OtherSendReport s1)	report	= case report of
											OtherSendReport s2	-> s1==s2
											_					-> False

instance toString SendReport where
	toString :: !SendReport -> {#Char}
	toString SendOk					= "SendOk"
	toString SendUnknownReceiver	= "SendUnknownReceiver"
	toString SendUnableReceiver		= "SendUnableReceiver"
	toString SendDeadlock			= "SendDeadlock"
	toString (OtherSendReport s)	= "(OtherSendReport "+++s+++")"


/*	Asynchronous, uni-directional, message passing.
	If the receiver could not be found in the global ReceiverTable, 
		then return SendUnknownReceiver and do nothing.
	If the receiver could be found, then increase the length of the 
		asynchronous message queue of the receiver in the global 
		ReceiverTable.
		Add the message to the asynchronous message queue of the 
		receiver using the scheduler.
*/
asyncSend :: !(RId msg) msg !(PSt .l) -> (!SendReport,!PSt .l)
asyncSend rid msg pState
	# id					= RIdtoId rid
	# (rt,ioState)			= IOStGetReceiverTable pState.io
	# (maybe_rte,rt)		= getReceiverTableEntry id rt
	| isNothing maybe_rte
		# ioState			= IOStSetReceiverTable rt ioState
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
syncSend :: !(RId msg) msg !(PSt .l) -> (!SendReport, !PSt .l)
syncSend rid msg pState
	# id						= RIdtoId rid
	# (rt,ioState)				= IOStGetReceiverTable pState.io
	# (maybe_parent,rt)			= getReceiverTableEntry id rt
	# ioState					= IOStSetReceiverTable rt ioState
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
	PStHandleSyncMessage :: !SyncMessage !(PSt .l) -> (!SendReport, !PSt .l)
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
syncSend2 :: !(R2Id msg resp) msg !(PSt .l) -> (!(!SendReport,!Maybe resp), !PSt .l)
syncSend2 r2id msg pState=:{io=ioState}
	# id						= R2IdtoId r2id
	# (rt,ioState)				= IOStGetReceiverTable ioState
	# (maybe_parent,rt)			= getReceiverTableEntry id rt
	# ioState					= IOStSetReceiverTable rt ioState
	| isNothing maybe_parent
		= ((SendUnknownReceiver,Nothing),{pState & io=ioState})
	# parent					= fromJust maybe_parent
	  rteLoc					= parent.rteLoc
	  pid						= rteLoc.rlIOId
	  sEvent					= {	smRecLoc		= rteLoc
	  							  ,	smMsg			= openDynamic (R2IdtoDId r2id) msg
	  							  ,	smResp			= []
	  							  ,	smError			= []
	  							  }
	# (ioid,ioState)			= IOStGetIOId ioState
	# pState					= {pState & io=ioState}
	| pid==ioid
		= PStHandleSync2Message (R2IdtoDId` r2id) sEvent pState
	# (opt_error,resp,pState)	= cswitchProcess pid (ScheduleMsgEvent (SyncMessage sEvent)) pState
	| isJust opt_error
		= ((toSendError (fromJust opt_error),Nothing),pState)
	| isEmpty resp
		= StdReceiverFatalError "syncSend2" "no response received from bi-directional receiver"
	# maybe_response			= readDynamic (R2IdtoDId` r2id) (hd resp)
	| isNothing maybe_response
		= ((OtherSendReport "incorrect response received from bi-directional receiver",Nothing),pState)
	| otherwise
		= ((SendOk,maybe_response),pState)
where
	PStHandleSync2Message :: !(DId resp) !SyncMessage !(PSt .l) -> (!(!SendReport,!Maybe resp), !PSt .l)
	PStHandleSync2Message did sm pState
		# (_,schedulerEvent,pState)
								= handleOneEventForDevices (ScheduleMsgEvent (SyncMessage sm)) pState
		  sm					= case schedulerEvent of
		  							(ScheduleMsgEvent (SyncMessage sm))	-> sm
		  							_									-> StdReceiverFatalError "syncSend2" "unexpected scheduler event"
		  errors				= sm.smError
		  resps					= sm.smResp
		| not (isEmpty errors)
			# sendReport		= case (hd errors) of
									ReceiverUnable	-> SendUnableReceiver
									ReceiverUnknown	-> SendUnknownReceiver
			= ((sendReport,Nothing),pState)
		| isEmpty resps
			= StdReceiverFatalError "syncSend2" "no response received from bi-directional receiver"
		# maybe_response		= readDynamic did (hd resps)
		| isNothing maybe_response
			= ((OtherSendReport "incorrect response received from bi-directional receiver",Nothing),pState)
		| otherwise
			= ((SendOk,maybe_response),pState)

toSendError :: !SwitchError -> SendReport
toSendError SwitchToYourself				= SendUnknownReceiver
toSendError SwitchToDoesNotExist			= SendUnknownReceiver
toSendError SwitchToReceiverDoesNotExist	= SendUnknownReceiver
toSendError SwitchReceiverUnable			= SendUnableReceiver
toSendError SwitchEndsUpInDeadlock			= SendDeadlock
