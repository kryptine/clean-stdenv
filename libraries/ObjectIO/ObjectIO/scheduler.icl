implementation module scheduler


//	Clean Object I/O library, version 1.2


import	StdBool, StdList, StdTuple
import	osevent
from	ostime				import OSGetTime, OSTime
from	ostoolbox			import OSNewToolbox, OSInitToolbox
import	commondef, devicefunctions, iostate, processstack, roundrobin, timertable, world
from	documentinterface	import closeOSDInfo
from	StdProcessDef		import ProcessInit
from	StdPSt				import accPIO, appPIO
from	StdProcessAttribute	import isProcessKindAttribute


::	*Environs
	=	{	envsEvents		:: !*OSEvents
		,	envsWorld		:: !*World
		}
::	*GContext p
	=	{	groupPublic		:: p
		,	groupLocals		:: !*Locals p
		}
::	*Context
	=	{	cEnvs			:: !*Environs			// The global environments
		,	cProcessStack	:: ProcessStack			// The global process stack
		,	cMaxIONr		:: SystemId				// The global maximum system number
		,	cGroups			:: *Groups				// All process groups
		,	cModalProcess	:: Maybe SystemId		// The SystemId of the interactive process that has a modal window
		,	cReceiverTable	:: ReceiverTable		// The global receiver-process table
		,	cTimerTable		:: TimerTable			// The table of all currently active timers
		,	cIdTable		:: IdTable				// The table of all bound Ids
		,	cOSTime			:: OSTime				// The current OSTime
		,	cIdSeed			:: Int					// The global id generating number (actually the World)
		,	cOSToolbox		:: !*OSToolbox			// The toolbox environment
		}

schedulerFatalError :: String String -> .x
schedulerFatalError rule error
	= Error rule "scheduler" error


/*	Retrieval operations:			*/

//	On Context:

ContextGetProcessStack :: !Context -> (!ProcessStack,!Context)
ContextGetProcessStack context=:{cProcessStack}
	= (cProcessStack,context)

ContextGetGroups :: !Context -> (!Groups,!Context)
ContextGetGroups context=:{cGroups}
	= (cGroups,{context & cGroups=emptyRR})

ContextGetSleepTime :: !Context -> (!Int,!Context)
ContextGetSleepTime context=:{cTimerTable,cReceiverTable}
	# maybe_sleep	= getTimeIntervalFromTimerTable cTimerTable
	# maybe_receiver= getActiveReceiverTableEntry cReceiverTable
	  sleep			= if (isJust maybe_receiver) 0						// a receiver with a non-empty message queue exists
	  				 (if (isJust maybe_sleep)	(fromJust maybe_sleep)	// a timer with given interval is waiting
	  				 							OSLongSleep)			// neither a receiver nor timer
	= (sleep,context)

ContextGetOSEvents :: !Context -> (!OSEvents,!Context)
ContextGetOSEvents context=:{cEnvs=envs=:{envsEvents}}
	= (envsEvents,{context & cEnvs={envs & envsEvents=OSnewEvents}})

ContextSetOSEvents :: !(!OSEvents,!Context) -> Context
ContextSetOSEvents (osEvents,context=:{cEnvs=envs})
	= {context & cEnvs={envs & envsEvents=osEvents}}


//	On GContext:

splitGContext :: !(GContext .p) -> (!LocalIO .p,!GContext .p)
splitGContext gContext=:{groupLocals=locals}
	# (local,locals) = getcurrentRR locals
	= (local,{gContext & groupLocals=locals})

adddoneLocal :: !(LocalIO .p) !(GContext .p) -> GContext .p
adddoneLocal localIO gContext=:{groupLocals}
	= {gContext & groupLocals=adddoneRR localIO groupLocals}

GContextToGroupIO :: !(GContext .p) -> GroupIO
GContextToGroupIO {groupPublic,groupLocals}
	= {groupState=groupPublic,groupIO=groupLocals}


//	On RuntimeState:

rsIsBlocked :: !RuntimeState -> (!Bool,SystemId)
rsIsBlocked (Blocked ioid)	= (True, ioid)
rsIsBlocked _				= (False,NullSystemId)

rsIsClosed :: !RuntimeState -> Bool
rsIsClosed Closed			= True
rsIsClosed _				= False


//	Starting an interactive process.

initContext :: !(ProcessInit (PSt .l .p)) !String !(!.l,!.p) !DocumentInterface !ProcessKind !*World -> (!Context,!*OSToolbox)
initContext ioDefInit ioDefAbout (local,public) documentInterface ioKind world
	# w						= loadWorld world
	# world					= storeWorld w world
	# initEnvs				= {envsEvents=OSnewEvents,envsWorld=world}
	= (	{	cEnvs			= initEnvs
		,	cProcessStack	= ioStack
		,	cMaxIONr		= InitSystemId
		,	cGroups			= initGroups
		,	cModalProcess	= initModalId
		,	cReceiverTable	= initialReceiverTable
		,	cTimerTable		= initialTimerTable
		,	cIdTable		= initialIdTable
		,	cOSTime			= ostime
		,	cIdSeed			= w
		,	cOSToolbox		= OSNewToolbox
		}
	  ,	tb1
	  )
where
	initModalId				= Nothing
	show					= ioKind==InteractiveProcess
	ioStack					= [{psId=InitSystemId,psShow=show,psKind=ioKind}]
	ioState					= createNewIOSt [] ioDefInit ioDefAbout InitSystemId Nothing Nothing ShareGUI documentInterface ioKind
	groupIO					= openGroupIO ioState local public
	initGroups				= toRR [] [groupIO]
	tb						= OSInitToolbox OSNewToolbox
	(ostime,tb1)			= OSGetTime tb

initContext` :: !*World -> (!Context,!*OSToolbox)
initContext` world
	# w						= loadWorld world
	# world					= storeWorld w world
	# initEnvs				= {envsEvents=OSnewEvents,envsWorld=world}
	= (	{	cEnvs			= initEnvs
		,	cProcessStack	= ioStack
		,	cMaxIONr		= InitSystemId
		,	cGroups			= initGroups
		,	cModalProcess	= initModalId
		,	cReceiverTable	= initialReceiverTable
		,	cTimerTable		= initialTimerTable
		,	cIdTable		= initialIdTable
		,	cOSTime			= ostime
		,	cIdSeed			= w
		,	cOSToolbox		= OSNewToolbox
		}
	  ,	tb1
	  )
where
	initModalId				= Nothing
	ioStack					= []
	initGroups				= toRR [] []
	tb						= OSInitToolbox OSNewToolbox
	(ostime,tb1)			= OSGetTime tb

createNewIOSt :: ![ProcessAttribute (PSt .l .p)] !(ProcessInit (PSt .l .p)) String !SystemId !(Maybe SystemId) 
					!(Maybe GUIShare) !Bool !DocumentInterface !ProcessKind
	-> IOSt .l .p
createNewIOSt pAtts ioDefInit ioDefAbout nr parentId guishare isSubProcess documentInterface ioKind
	= emptyIOSt nr parentId guishare documentInterface ioKind pAtts ioDefInit Nothing


//	Handling events until termination of all interactive processes.

handleEvents :: !Context !*OSToolbox -> (!Context,!*OSToolbox)
handleEvents context tb
	# (_,context)	= handleContextOSEvent OSNullEvent context
	= OShandleEvents terminate ContextGetOSEvents ContextSetOSEvents ContextGetSleepTime handleContextOSEvent (context,tb)
where
	terminate :: !Context -> (!Bool,!Context)
	terminate context=:{cProcessStack}
		= (isEmpty cProcessStack,context)


//	Closing a final context. 

closeContext :: !Context !*OSToolbox -> *World
closeContext {cProcessStack,cEnvs={envsWorld}} tb
	| isEmpty cProcessStack	= storeWorld 42 envsWorld
	| otherwise				= schedulerFatalError "closeContext" "not a final Context"


//	Handling events while condition holds.

CondHandleEvents :: !(St Context Bool) !*OSToolbox !Context -> (!*OSToolbox,!Context)
CondHandleEvents cond tb context
	# (context,tb)	= OShandleEvents terminate ContextGetOSEvents ContextSetOSEvents ContextGetSleepTime handleContextOSEvent (context,tb)
	= (tb,context)
where
	terminate :: !Context -> (!Bool,!Context)
	terminate context
		# (continue,context)	= cond context
		= (not continue,context)

handleContextOSEvent :: !OSEvent !Context -> (![Int],!Context)
handleContextOSEvent osEvent context=:{cProcessStack,cGroups,cReceiverTable,cTimerTable,cOSTime,cOSToolbox}
//	PA: shift the time in the timertable.
	# (ostime,tb)				= OSGetTime cOSToolbox
	  timeshift					= toInt (ostime-cOSTime)
	  timertable				= shiftTimeInTimerTable timeshift cTimerTable
//	PA: determine whether a TimerEvent or ASyncMessage can be generated
	  (schedulerEvent,receivertable,timertable)
	  							= toSchedulerEvent osEvent cReceiverTable timertable cOSTime
	  groups					= resetRR cGroups
	# context					= {context & cGroups=groups,cReceiverTable=receivertable,cTimerTable=timertable,cOSTime=ostime,cOSToolbox=tb}
	# (schedulerEvent,context)	= handleEventForContext False schedulerEvent context
	  replyToOS					= case schedulerEvent of
	  								(ScheduleOSEvent _ reply)	-> reply
	  								_							-> []
	# (ioStack,context)			= ContextGetProcessStack context
	  (_,oldTopIO)				= topShowProcessShowState cProcessStack
	  (newTopIOVis,newTopIO)	= topShowProcessShowState ioStack
	| oldTopIO==newTopIO || not newTopIOVis
		= (replyToOS,context)
	| otherwise
		# (groups,context)		= ContextGetGroups context
		# (ioStack,groups)		= activateTopOfGroups newTopIO ioStack (resetRR groups)
		= (replyToOS,{context & cProcessStack=ioStack,cGroups=groups})


/*	PA: new function:
	in case a non-urgent, removable system event has been caught, check if a TimerEvent or ASyncMessage 
	can be generated instead. If both a TimerEvent and an ASyncMessage are available, use the OSTime to 
	decide which one to choose.
*/
toSchedulerEvent :: !OSEvent !ReceiverTable !TimerTable !OSTime -> (!SchedulerEvent,!ReceiverTable,!TimerTable)
toSchedulerEvent osevent receivertable timertable osTime
	| OSEventIsUrgent osevent
		= (schedulerEvent,receivertable,timertable)
	| not sure_timer && not sure_receiver
		= (schedulerEvent,receivertable,timertable)
	| sure_timer && sure_receiver
		| isEven (toInt osTime)
			= (timerEvent,receivertable,timertable`)
		// otherwise
			= (asyncEvent,receivertable`,timertable)
	| sure_timer
		= (timerEvent,receivertable,timertable`)
	| otherwise
		= (asyncEvent,receivertable`,timertable)
where
	maybe_timer					= getTimeIntervalFromTimerTable timertable
	maybe_receiver				= getActiveReceiverTableEntry receivertable
	sure_timer					= isJust maybe_timer && fromJust maybe_timer<=0
	sure_receiver				= isJust maybe_receiver
	schedulerEvent				= ScheduleOSEvent osevent []
	(asyncEvent,receivertable`)	= toASyncEvent (fromJust maybe_receiver) receivertable
	(timerEvent,timertable`)	= toTimerEvent timertable
	
	//	The receiver for which an ASyncMessage is generated is placed behind all other receivers, 
	//	creating a round-robin order. Its asynchronous message queue length field is decreased.
	toASyncEvent :: !Id !ReceiverTable -> (!SchedulerEvent,!ReceiverTable)
	toASyncEvent rid receivertable
		#! rte				= fromJust (getReceiverTableEntry rid receivertable)
		#! rte				= {rte & rteASMCount=rte.rteASMCount-1}
		#! receivertable	= setReceiverTableEntry rte (snd (removeReceiverFromReceiverTable rid receivertable))
		= (ScheduleMsgEvent (ASyncMessage {asmRecLoc=rte.rteLoc}),receivertable)
	
	//	The timer for which a TimerEvent is generated is determined by getActiveTimerInTable.
	//	This function already takes care of fairness using a round robin scheme.
	toTimerEvent :: !TimerTable -> (!SchedulerEvent,!TimerTable)
	toTimerEvent timertable
		# (maybeTimerEvent,timertable)	= getActiveTimerInTimerTable timertable
		= (ScheduleTimerEvent (fromJust maybeTimerEvent),timertable)

handleEventForContext :: !Bool !SchedulerEvent !Context -> (!SchedulerEvent,!Context)
handleEventForContext eventDone schedulerEvent context=:{cGroups=groups}
	# (notodo,groups)	= notodoRR groups
	| notodo
		= (schedulerEvent,{context & cGroups=groups})
	# (groupIO,groups)	= getcurrentRR groups
	# (quitted,groupIO)	= groupQuitted groupIO
	# context			= {context & cGroups=groups}
	| quitted
		= handleEventForContext eventDone schedulerEvent context
	| otherwise
		# (eventDone,schedulerEvent,groupIO,context)
						= handleEventForGroup eventDone schedulerEvent groupIO context
		# context		= {context & cGroups=adddoneRR groupIO context.cGroups}
		= handleEventForContext eventDone schedulerEvent context
	with
		handleEventForGroup :: !Bool !SchedulerEvent !GroupIO !Context
						   -> (!Bool,!SchedulerEvent,!GroupIO,!Context)
		handleEventForGroup eventDone schedulerEvent {groupState=share,groupIO=locals} context
			# locals	= resetRR locals
			# gcontext	= {groupPublic=share,groupLocals=locals}
			# (eventDone,schedulerEvent,gcontext,context)= handleEventForLocals eventDone schedulerEvent gcontext context
			= (eventDone,schedulerEvent,GContextToGroupIO gcontext,context)
where
	groupQuitted :: !GroupIO -> (!Bool,!GroupIO)
	groupQuitted group=:{groupIO}
		# (isEmpty,groupIO) = isEmptyRR groupIO
		= (isEmpty,{group & groupIO=groupIO})

handleEventForLocals :: !Bool !SchedulerEvent !(GContext .p) !Context -> (!Bool,!SchedulerEvent,!GContext .p,!Context)
handleEventForLocals eventDone schedulerEvent gContext=:{groupLocals=locals} context
	# (notodo,locals)	= notodoRR locals
	| notodo
		= (eventDone,schedulerEvent,{gContext & groupLocals=locals},context)
	# (localIO,locals)	= getcurrentRR locals
	# (quitted,localIO)	= localIOQuitted localIO
	# (isModal,localIO)	= localIOModal   localIO
	| quitted && not isModal
		= handleEventForLocals eventDone schedulerEvent {gContext & groupLocals=locals} context
	| quitted
		= handleEventForLocals eventDone schedulerEvent {gContext & groupLocals=adddoneRR localIO locals} context
	| otherwise
		# gContext		= {gContext & groupLocals=locals}
		# (eventDone,schedulerEvent,localIO,gContext,context)
						= handleEventForLocalIO eventDone schedulerEvent localIO gContext context
		# gContext		= adddoneLocal localIO gContext
		= handleEventForLocals eventDone schedulerEvent gContext context
where
	localIOQuitted :: !(LocalIO .p) -> (!Bool,!LocalIO .p)
	localIOQuitted localIO
		# (closed,ioState) = IOStClosed localIO.localIOSt
		= (closed,{localIO & localIOSt=ioState})
	
	localIOModal :: !(LocalIO .p) -> (!Bool,!LocalIO .p)
	localIOModal localIO
		# (optModal,ioState)= IOStGetIOIsModal localIO.localIOSt
		# (myId,ioState)	= IOStGetIOId ioState
		= (isJust optModal && myId==fromJust optModal,{localIO & localIOSt=ioState})

handleEventForLocalIO :: !Bool !SchedulerEvent !(LocalIO .p) !(GContext .p) !Context
					 -> (!Bool,!SchedulerEvent,! LocalIO .p, ! GContext .p, !Context)
handleEventForLocalIO eventDone schedulerEvent {localState=opt_local,localIOSt=ioState} gContext context
	# (runtime,ioState)						= IOStGetRuntimeState ioState
	| fst (rsIsBlocked runtime)
		= (eventDone, schedulerEvent, {localState=opt_local, localIOSt=ioState},gContext, context)
	# (initIO,ioState)						= IOStGetInitIO ioState
	# (dummies,pState)						= cSwitchIn gContext (fromJust opt_local) context ioState
	# pState								= initIO pState
	# (ioKind,pState)						= accPIO IOStGetProcessKind pState
	| ioKind==VirtualProcess
		= (eventDone, schedulerEvent, {localState=Just local1,localIOSt=ioState1},gContext1,context1)
	with
		pState1								= closeVirtualProcess pState
		(gContext1,local1,context1,ioState1)= cSwitchOut dummies pState1
	# (closed,pState)						= accPIO IOStClosed pState
	| closed
		= (eventDone, schedulerEvent, {localState=Just local1,localIOSt=ioState1},gContext1,context1)
	with
		(gContext1,local1,context1,ioState1)= cSwitchOut dummies pState
	| otherwise
		= (eventDone1,schedulerEvent1,{localState=Just local2,localIOSt=ioState2},gContext2,context2)
	with
		(deviceFunctions,pState0)			= accPIO IOStGetDeviceFunctions pState
		ioFunctions							= [(df.dEvent,df.dDoIO) \\ df<-deviceFunctions]
		(eventDone1,schedulerEvent1,pState1)= handleEventForDevices ioFunctions eventDone schedulerEvent pState0
		(gContext2,local2,context2,ioState2)= cSwitchOut dummies pState1
where
	closeVirtualProcess :: !(PSt .l .p) -> PSt .l .p
	closeVirtualProcess pState=:{io}
		# (subids,ioState)		= IOStGetSubProcessIds io
		| not (isEmpty subids)
			= {pState & io=ioState}
		| otherwise
			# (ioStack,ioState)	= IOStGetProcessStack ioState
			# (nr,ioState)		= IOStGetIOId ioState
			  (_, ioStack)		= removeProcessShowState nr ioStack
			# ioState			= IOStSetProcessStack ioStack ioState
			# ioState			= removeIOIdFromParentProcess nr ioState
			# ioState			= IOStSetRuntimeState Closed ioState
			= {pState & io=ioState}

cSwitchIn :: !(GContext .p) !.l !Context !(IOSt .l .p) -> (!(![*World],!Locals .p,!Groups),!PSt .l .p)
cSwitchIn {groupPublic,groupLocals} local
	{cEnvs={envsEvents,envsWorld},cProcessStack,cMaxIONr,cGroups,cModalProcess,cReceiverTable,cTimerTable,cIdTable,cOSTime,cIdSeed} ioState
	# ioState				= IOStSetProcessStack cProcessStack ioState
	# ioState				= IOStSetEvents envsEvents ioState
	# ioState				= IOStSetMaxIONr cMaxIONr ioState
	# (ioContext,ioState)	= IOStSwapIO ([envsWorld],groupLocals,cGroups) ioState
	# ioState				= IOStSetIOIsModal cModalProcess ioState
	# ioState				= IOStSetIdTable cIdTable ioState
	# ioState				= IOStSetReceiverTable cReceiverTable ioState
	# ioState				= IOStSetTimerTable cTimerTable ioState
	# ioState				= IOStSetOSTime cOSTime ioState
	# ioState				= IOStSetIdSeed cIdSeed ioState
	# pState				= {ls=local,ps=groupPublic,io=ioState}
	= (ioContext,pState)

cSwitchOut :: !(![*World],!Locals .p,!Groups) !(PSt .l .p) -> (!GContext .p,!.l,!Context,!IOSt .l .p)
cSwitchOut ioContext {ls,ps,io}
	# (ostime,   ioState)		= IOStGetOSTime			io
	# (tt,       ioState)		= IOStGetTimerTable		ioState
	# (idseed,   ioState)		= IOStGetIdSeed			ioState
	# (ridlocs,  ioState)		= IOStGetReceiverTable	ioState
	# (idtable,  ioState)		= IOStGetIdTable		ioState
	# (modalId,  ioState)		= IOStGetIOIsModal		ioState
	# (ioContext,ioState)		= IOStSwapIO ioContext	ioState
	# (worlds,localIO,groups)	= ioContext
	# (maxIONr,  ioState)		= IOStGetMaxIONr		ioState
	# (ioStack,  ioState)		= IOStGetProcessStack	ioState
	# (es,	     ioState)		= IOStGetEvents			ioState
	# envs						= {envsEvents=es,envsWorld=hd worlds}
	# context					= {	cEnvs			= envs
								  ,	cProcessStack	= ioStack
								  ,	cMaxIONr		= maxIONr
								  ,	cGroups			= groups
								  ,	cModalProcess	= modalId
								  ,	cReceiverTable	= ridlocs
								  ,	cTimerTable		= tt
								  ,	cIdTable		= idtable
								  ,	cOSTime			= ostime
								  ,	cIdSeed			= idseed
								  ,	cOSToolbox		= OSNewToolbox
								  }
	# gContext					= {groupPublic=ps,groupLocals=localIO}
	= (gContext,ls,context,ioState)

/*	handleEventForDevices in sequence lets the devices handle the scheduler event until it is handled
	or the process is terminated (IOStClosed returns True).
	Before handing over the event to the device DoIOFunction, the device first maps the event to a
	device event if possible using its EventFunction. 
*/	
handleEventForDevices :: ![!(!EventFunction (PSt .l .p),!DoIOFunction (PSt .l .p))] !Bool !SchedulerEvent (PSt .l .p)
																				-> (!Bool,!SchedulerEvent, PSt .l .p)
handleEventForDevices [(mapDeviceEvent,doDeviceIO):doIOs] eventDone schedulerEvent pState
	| eventDone
		= (eventDone,schedulerEvent,pState)
	# (closed,pState)				= accPIO IOStClosed pState
	| closed
		= (True,schedulerEvent,pState)
	# (forThisDevice,okDeviceEvent,schedulerEvent,pState)
									= mapDeviceEvent schedulerEvent pState
	| not forThisDevice
		= handleEventForDevices doIOs eventDone schedulerEvent pState
	| isNothing okDeviceEvent
		= handleEventForDevices doIOs True schedulerEvent pState
	| otherwise
		# (deviceEvent,pState)		= doDeviceIO (fromJust okDeviceEvent) pState
		# schedulerEvent			= mergeMsgEventIntoSchedulerEvent deviceEvent schedulerEvent
		= handleEventForDevices doIOs True schedulerEvent pState
where
	mergeMsgEventIntoSchedulerEvent :: !DeviceEvent !SchedulerEvent -> SchedulerEvent
	mergeMsgEventIntoSchedulerEvent (ReceiverEvent msgEvent) _	= ScheduleMsgEvent msgEvent
	mergeMsgEventIntoSchedulerEvent _ schedulerEvent			= schedulerEvent
handleEventForDevices _ eventDone schedulerEvent pState
	= (eventDone,schedulerEvent,pState)

handleOneEventForDevices :: !SchedulerEvent !(PSt .l .p) -> (!Bool,!SchedulerEvent,!PSt .l .p)
handleOneEventForDevices schedulerEvent pState
	# (deviceFunctions,pState)	= accPIO IOStGetDeviceFunctions pState
	  ioFunctions				= [(df.dEvent,df.dDoIO) \\ df<-deviceFunctions]
	= handleEventForDevices ioFunctions False schedulerEvent pState


/*	Creating interactive processes other than the initial interactive process.
	Throughout the I/O library it is assumed that all new process groups and all new data sharing
	processes are created AFTER the current interactive process. This is done for two reasons:
	-	locating offspring processes becomes more efficient (when you need to quit them). 
	-	it provides a simple additional means of communication between parent and child processes
		(piggybacking the Event` data item).
	Virtual processes (created with addVirtualProcess) evaluate only the initial actions. It is
	assumed by the system that the initial actions do not create any devices instances or set the 
	AppleMenuTitle. This MUST be taken care of by the system programmer (the system will abort if so). 
	Interactive processes (created with addInteractiveProcess) do create devices. 
*/

ShareGUI			:==	True
NotShareGUI			:==	False

//	Create a virtual process that will create other interactive processes.

addVirtualProcess :: !(ProcessInit (PSt .l .p)) String (.l,.p) !(PSt .l` .p`) -> PSt .l` .p`
addVirtualProcess ioDefInit ioDefAbout (local,public) pState
	# (nr,ioState)		= IOStNewMaxIONr pState.io
	# (parentId,ioState)= IOStGetIOId					ioState
	# (guishare,ioState)= getGUIShare ShareGUI			ioState
	# ioState			= addSubProcessId ShareGUI nr	ioState
	# (ioStack, ioState)= IOStGetProcessStack			ioState
	  ioStack			= pushProcessShowState {psId=nr,psShow=False,psKind=VirtualProcess} ioStack
	# ioState			= IOStSetProcessStack ioStack	ioState
	# (groups,ioState)	= IOStGetGroups					ioState
	# newIOSt			= createNewIOSt [] ioDefInit ioDefAbout nr (Just parentId) guishare ShareGUI NDI VirtualProcess
	# groupIO			= openGroupIO newIOSt local public
	# ioState			= IOStSetGroups (inserttodoRR groupIO groups) ioState
	= {pState & io=ioState}


//	Create a data sharing interactive process.

addInteractiveProcess :: ![ProcessAttribute (PSt .l .p)] !(ProcessInit (PSt .l .p)) String .l !Bool !DocumentInterface 
	!(PSt .l` .p) -> PSt .l` .p
addInteractiveProcess pAtts ioDefInit ioDefAbout local isSubProcess documentInterface pState
	# (nr,ioState)		= IOStNewMaxIONr pState.io
	# (parentId,ioState)= IOStGetIOId						ioState
	# (guishare,ioState)= getGUIShare isSubProcess			ioState
	# ioState			= addSubProcessId isSubProcess nr	ioState
	# (ioStack, ioState)= IOStGetProcessStack				ioState
	  ioStack			= pushProcessShowState {psId=nr,psShow=True,psKind=InteractiveProcess} ioStack
	# ioState			= IOStSetProcessStack ioStack		ioState
	# (locals,ioState)	= IOStGetLocals						ioState
	  parent			= if isSubProcess (Just parentId) Nothing
	  pAtts				= filter (isProcessKindAttribute documentInterface) pAtts
	# newIOSt			= createNewIOSt pAtts ioDefInit ioDefAbout nr parent guishare isSubProcess documentInterface InteractiveProcess
	# localIO			= openLocalIO newIOSt local
	# ioState			= IOStSetLocals (inserttodoRR localIO locals) ioState
	= {pState & io=ioState}

openGroupIO :: !(IOSt .l .p) !.l !.p -> GroupIO
openGroupIO ioState local public
	= {	groupState	= public
	  ,	groupIO		= toRR [] [openLocalIO ioState local]
	  }

openLocalIO :: !(IOSt .l .p) !.l -> LocalIO .p
openLocalIO ioState local
	= {	localState	= Just local
	  ,	localIOSt	= ioState
	  }

getGUIShare :: !Bool !(IOSt .l .p) -> (!Maybe GUIShare,!IOSt .l .p)
getGUIShare isSubProcess ioState
	| not isSubProcess
		= (Nothing,ioState)
	# (guishare,ioState)	= IOStGetGUIShare ioState
	| isJust guishare
		= (guishare,ioState)
	# (ioKind,ioState)		= IOStGetProcessKind ioState
	| ioKind==VirtualProcess
		= (guishare,ioState)
	| otherwise
		= (guishare,ioState)

addSubProcessId :: !Bool !SystemId !(IOSt .l .p) -> IOSt .l .p
addSubProcessId isSubProcess nr ioState
	| not isSubProcess
		= ioState
	| otherwise
		# (subids,ioState)	= IOStGetSubProcessIds ioState
		  ioState			= IOStSetSubProcessIds [nr:subids] ioState
		= ioState


//	Make the proper interactive process active.

activateTopOfGroups :: !SystemId !ProcessStack !Groups -> (!ProcessStack,!Groups)
activateTopOfGroups topIONr ioStack groups
	# (emptytodo,groups)			= notodoRR groups
	| emptytodo
		= (ioStack,groups)
	# (groupIO,groups)				= getcurrentRR groups
	  (activated,ioStack,groupIO)	= activateTopOfGroupIO topIONr ioStack groupIO
	| activated
		= (ioStack,inserttodoRR groupIO groups)
	| otherwise
		# (ioStack,groups)			= activateTopOfGroups  topIONr ioStack groups
		= (ioStack,inserttodoRR groupIO groups)
where
	activateTopOfGroupIO :: !SystemId !ProcessStack !GroupIO -> (!Bool,!ProcessStack,!GroupIO)
	activateTopOfGroupIO topIONr ioStack group
		# (activated,ioStack,locals)	= activateTopOfLocals topIONr ioStack (resetRR group.groupIO)
		= (activated,ioStack,{group & groupIO=locals})
	where
		activateTopOfLocals :: !SystemId !ProcessStack !(Locals .p) -> (!Bool,!ProcessStack,!Locals .p)
		activateTopOfLocals topIONr ioStack locals
			# (emptytodo,locals)			= notodoRR locals
			| emptytodo
				= (False,ioStack,locals)
			# (localIO,locals)				= getcurrentRR locals
			  (activated,ioStack,localIO)	= activateTopOfLocalIO topIONr ioStack localIO
			| activated
				= (activated,ioStack,inserttodoRR localIO locals)
			| otherwise
				# (activated,ioStack,locals)= activateTopOfLocals  topIONr ioStack locals
				= (activated,ioStack,inserttodoRR localIO locals)
		where
			activateTopOfLocalIO :: !SystemId !ProcessStack !(LocalIO .p) -> (!Bool,!ProcessStack,!LocalIO .p)
			activateTopOfLocalIO topIONr ioStack localIO
				# (nr,ioState)			= IOStGetIOId localIO.localIOSt
				| nr<>topIONr
					= (False,ioStack ,{localIO & localIOSt=ioState})
				| otherwise
					# ioState			= IOStSetProcessStack ioStack ioState
					# (ioStack,ioState)	= IOStGetProcessStack ioState
					= (True,ioStack,{localIO & localIOSt=ioState})


/*	Quit this interactive or virtual process.
	It should be an impossible situation for the system to have to quit a blocked process (the guard should never hold).
	Quitting a process involves the following:
	-	Set the RuntimeState to Closed (quitProcess is the only function that does this)
	-	Force all sub processes to quit
	-	Inform the parent process about its termination
	-	Remove the process from the ProcessStack
	-	Close all devices
*/
quitProcess :: !(PSt .l .p) -> PSt .l .p
quitProcess pState
	# (rs,pState)					= accPIO IOStGetRuntimeState pState
	| fst (rsIsBlocked rs)
		= schedulerFatalError "quitProcess" "closeProcess applied to blocked process"
	| rsIsClosed rs
		= pState
	| otherwise
		# (deviceFunctions,pState)	= accPIO IOStGetDeviceFunctions pState
		# pState					= StrictSeq [df.dClose \\ df<-deviceFunctions] pState
		# ioState					= IOStSetRuntimeState Closed	 pState.io
		# (nr,ioState)				= IOStGetIOId					 ioState
		# (subids,ioState)			= IOStGetSubProcessIds			 ioState
		# ioState					= quitSubProcesses subids		 ioState
		# ioState					= removeIOIdFromParentProcess nr ioState
		# (ioStack,ioState)			= IOStGetProcessStack			 ioState
		  (_,ioStack)				= removeProcessShowState nr		 ioStack
		# ioState					= IOStSetProcessStack ioStack	 ioState
		# ioState					= closeOSDInfo ioState
		= {pState & io=ioState}


/*	quitSubProcesses searches for all processes in the current process administration
	with the given ids. Each of these processes is forced to quit by setting the initialisation
	functions to [appPIO quitProcess]. In this way all recursive descendent processes will
	be quitted as well.
*/
quitSubProcesses :: ![SystemId] !(IOSt .l .p) -> IOSt .l .p
quitSubProcesses ids ioState
	# (locals,ioState)	= IOStGetLocals ioState
	  (ids,locals)		= quitLocalSubProcesses ids locals
	# ioState			= IOStSetLocals locals ioState
	# (groups,ioState)	= IOStGetGroups ioState
	  (_,groups)		= quitGroupSubProcesses ids groups
	# ioState			= IOStSetGroups groups ioState
	= ioState
where
	quitLocalSubProcesses :: ![SystemId] !(Locals .p) -> (![SystemId],!Locals .p)
	quitLocalSubProcesses ids locals
		| isEmpty ids
			= (ids,locals)
		| otherwise
			# (done,todo)	= fromRR locals
			  (ids,done)	= quitLocalSubProcesses` ids done
			  (ids,todo)	= quitLocalSubProcesses` ids todo
			= (ids,toRR done todo)
	where
		quitLocalSubProcesses` :: ![SystemId] ![LocalIO .p] -> (![SystemId],![LocalIO .p])
		quitLocalSubProcesses` ids=:[] locals
			= (ids,locals)
		quitLocalSubProcesses` ids locals=:[]
			= (ids,locals)
		quitLocalSubProcesses` ids [local=:{localState,localIOSt=ioState}:locals]
			| hadId
				= (ids2,[local1:locals1])
			with
				(subids,ioState2)	= IOStGetSubProcessIds ioState1
				ioState3			= IOStSetInitIO quitProcess ioState2
				local1				= {localState=localState,localIOSt=ioState3}
				(ids2,locals1)		= quitLocalSubProcesses` (ids1++subids) locals
			| otherwise
				= (ids2,[local1:locals1])
			with
				local1				= {localState=localState,localIOSt=ioState1}
				(ids2,locals1)		= quitLocalSubProcesses` ids1 locals
		where
			(ioid,ioState1)			= IOStGetIOId ioState
			(hadId,_,ids1)			= Remove ((==) ioid) NullSystemId ids
	
	quitGroupSubProcesses :: ![SystemId] !Groups -> (![SystemId],!Groups)
	quitGroupSubProcesses ids groups
		| isEmpty ids
			= (ids,groups)
		| otherwise
			# (done,todo)	= fromRR groups
			  (ids,done)	= quitGroupSubProcesses` ids done
			  (ids,todo)	= quitGroupSubProcesses` ids todo
			= (ids,toRR done todo)
	where
		quitGroupSubProcesses` :: ![SystemId] ![GroupIO] -> (![SystemId],![GroupIO])
		quitGroupSubProcesses` ids=:[] groups
			= (ids,groups)
		quitGroupSubProcesses` ids groups=:[]
			= (ids,groups)
		quitGroupSubProcesses` ids  [group=:{groupState,groupIO}:groups]
			# (ids,locals)	= quitLocalSubProcesses  ids groupIO
			  (ids,groups)	= quitGroupSubProcesses` ids groups
			= (ids,[{groupState=groupState,groupIO=locals}:groups])


/*	removeIOIdFromParentProcess searches for the parent process in the current process
	administration. It is a fatal error not to find this process. In the administration
	of the parent process the child process id is removed.
*/
removeIOIdFromParentProcess :: !SystemId !(IOSt .l .p) -> IOSt .l .p
removeIOIdFromParentProcess me ioState
	# (opt_parent,ioState)	= IOStGetParentId ioState
	| isNothing opt_parent
		= ioState
	# parent				= fromJust opt_parent
	# (locals,ioState)		= IOStGetLocals ioState
	# (done,locals)			= removeIOIdFromLocals me parent locals
	# ioState				= IOStSetLocals locals ioState
	| done
		= ioState
	# (groups,ioState)		= IOStGetGroups ioState
	# (done,groups)			= removeIOIdFromGroups me parent groups
	# ioState				= IOStSetGroups groups ioState
	| done
		= ioState
	| otherwise
		= schedulerFatalError "CloseProcess" "parent process could not be located"
where
	removeIOIdFromLocals :: !SystemId !SystemId !(Locals .p) -> (!Bool,!Locals .p)
	removeIOIdFromLocals me parent locals
		# (done,todo)			= fromRR locals
		  (removed,done)		= removeIOIdFromLocals` me parent done
		| removed
			= (removed,toRR done todo)
		| otherwise
			# (removed, todo)	= removeIOIdFromLocals` me parent todo
			= (removed,toRR done todo)
	where
		removeIOIdFromLocals` :: !SystemId !SystemId ![LocalIO .p] -> (!Bool,![LocalIO .p])
		removeIOIdFromLocals` me parent [local=:{localState,localIOSt=ioState}:locals]
			# (ioid,ioState)			= IOStGetIOId ioState
			| parent==ioid
				= (True,[local1:locals])
			with
				(subids,ioState1)	= IOStGetSubProcessIds ioState
				(_,_,subids1)		= Remove ((==) me) (dummy "removeIOIdFromLocals") subids
				ioState2			= IOStSetSubProcessIds subids1 ioState1
				local1				= {localState=localState,localIOSt=ioState2}
			| otherwise
				= (removed,[local1:locals1])
			with
				local1				= {localState=localState,localIOSt=ioState}
				(removed,locals1)	= removeIOIdFromLocals` me parent locals
		removeIOIdFromLocals` _ _ _
			= (False,[])
	
	removeIOIdFromGroups :: !SystemId !SystemId !Groups -> (!Bool,!Groups)
	removeIOIdFromGroups me parent groups
		# (done,todo)			= fromRR groups
		  (removed,done)		= removeIOIdFromGroups` me parent done
		| removed
			= (removed,toRR done todo)
		| otherwise
			# (removed, todo)	= removeIOIdFromGroups` me parent todo
			= (removed,toRR done todo)
	where
		removeIOIdFromGroups` :: !SystemId !SystemId ![GroupIO] -> (!Bool,![GroupIO])
		removeIOIdFromGroups` me parent [group=:{groupState,groupIO}:groups]
			# (removed,locals)		= removeIOIdFromLocals me parent groupIO
			  group					= {group & groupIO=locals}
			| removed
				= (removed,[group:groups])
			| otherwise
				# (removed,groups)	= removeIOIdFromGroups` me parent groups
				= (removed,[group:groups])
		removeIOIdFromGroups` _ _ _
			= (False,[])


//	Function threading operations:

::	SwitchError
	=	SwitchToYourself
	|	SwitchToDoesNotExist
	|	SwitchToReceiverDoesNotExist
	|	SwitchReceiverUnable
	|	SwitchEndsUpInDeadlock

/*	cswitchProcess processId msgEvent pstate switches to PSt identified by processId, lets that process 
	handle the msgEvent, and switches back to pstate.
	Nothing SwitchError is returned if no exceptions were detected. 
	Exceptions are:
	-	SwitchToYourself:				processId is the current PSt. 
	-	SwitchToDoesNotExist:			no current process can be identified by processId. 
	-	SwitchToReceiverDoesNotExist:	process  exists, but receiver doesn't.
	-	SwitchReceiverUnable:			receiver exists, but is not Able.
	-	SwitchEndsUpInDeadlock:			if this process would block for the process with 
										id processId to get unblocked, a deadlock would occur.
	In all exceptional cases the PSt remains unchanged.
	Observe that in case Nothing is returned, the ps PSt component may have changed value.
*/
cswitchProcess :: !SystemId !SchedulerEvent !(PSt .l .p) -> (!Maybe SwitchError,![SemiDynamic],!PSt .l .p)
cswitchProcess processId message pState
	| processId==returnId
		= (Just SwitchToYourself,    [],pState1)
	| not switchToExists
		= (Just SwitchToDoesNotExist,[],pState2)
	with
		context2						= {context1 & cGroups=groups2}
		pState2							= switchToPSt typeGContext typeIOSt returnId context2 local
	| inDeadlock	
		= (Just SwitchEndsUpInDeadlock,[],pState2)
	with
		context2						= {context1 & cGroups=groups3}
		pState2							= switchToPSt typeGContext typeIOSt returnId context2 local
	| otherwise
		= (checkSyncMessageError message1,getSyncMessageResponse message1,pState2)
	with
		context2						= {context1 & cGroups=groups3}
		(_,context3)					= CondHandleEvents (processIsBlocked processId) OSNewToolbox context2
		(groups4,context4)				= ContextGetGroups context3
		context5						= {context4 & cGroups=resetRR groups4}
		(message1,context6)				= handleEventForContext False message context5
		pState2							= switchToPSt typeGContext typeIOSt returnId context6 local
where
	(returnId,pState1)					= accPIO IOStGetIOId pState
	(gcontext,local,context,ioState)	= switchFromPSt pState1
	(groups,context1)					= ContextGetGroups context
	ioState1							= IOStSetRuntimeState (Blocked processId) ioState
	(typeGContext,ioState2)				= typeIsGContext ioState1
	(typeIOSt, ioState3)				= typeIsIOSt  ioState2
	blockedLocalIO						= {localState=Nothing,localIOSt=ioState3}
	group								= GContextToGroupIO (adddoneLocal blockedLocalIO gcontext)
	groups1								= adddoneRR group groups
	(switchToExists,groups2)			= turnRRToProcessInGroups processId groups1
	(inDeadlock,groups3)				= checkDeadlock  returnId processId groups2
	
	switchToPSt :: !(UnguardType (GContext .p)) !(UnguardType (IOSt .l .p)) !SystemId !Context .l -> PSt .l .p
	switchToPSt typeGContext typeIOSt returnId context=:{cGroups} local
		# (_,groups)					= turnRRToProcessInGroups returnId cGroups
		  (group,groups)				= getcurrentRR groups
		  (gDone,gToDo)					= fromRR groups
		  (GContext` share lDone lToDo)	= splitGroupIO group
		  gcontext						= {groupPublic=share,groupLocals=toRR lDone lToDo}
		  (l,gcontext)					= splitGContext (castType typeGContext gcontext)
		  (LocalIO` blockedIO)			= splitLocalIO l
		  blockedIO						= castType typeIOSt blockedIO
		  context						= {context & cGroups=toRR gDone gToDo}
		  (_,pState)					= cSwitchIn gcontext local context (IOStSetRuntimeState Running blockedIO)
		= pState
	where
		splitGroupIO {groupState,groupIO=groups}
			# (done,todo) = fromRR groups
			= GContext` groupState done todo
		splitLocalIO {localIOSt}
			= LocalIO`  localIOSt
	
	checkSyncMessageError :: !SchedulerEvent -> Maybe SwitchError
	checkSyncMessageError (ScheduleMsgEvent (SyncMessage {smError}))
		| isEmpty smError
			= Nothing
		| not (IsSingleton smError)
			= schedulerFatalError "checkSyncMessageError" "more than one MessageError returned"
		| otherwise
			= case (hd smError) of
				ReceiverUnable	-> Just SwitchReceiverUnable
				ReceiverUnknown	-> Just SwitchToReceiverDoesNotExist
	checkSyncMessageError _
		= Nothing
	
	getSyncMessageResponse :: !SchedulerEvent -> [SemiDynamic]
	getSyncMessageResponse (ScheduleMsgEvent (SyncMessage {smResp}))
		= smResp
	getSyncMessageResponse _
		= []


typeIsGContext :: !(IOSt .l .p) -> (UnguardType (GContext .p),!IOSt .l .p)
typeIsGContext ioState = (Unguard,ioState)

typeIsIOSt :: !(IOSt .l .p) -> (UnguardType (IOSt .l .p),!IOSt .l .p)
typeIsIOSt ioState = (Unguard,ioState)

typeIsLocal :: !(IOSt .l .p) -> (UnguardType (Maybe .l),!IOSt .l .p)
typeIsLocal ioState = (Unguard,ioState)


accContext :: !.(St Context .x) !(PSt .l .p) -> (!.x, !PSt .l .p)
accContext fun pState
	# (returnId,pState)					= accPIO IOStGetIOId pState
	# (gcontext,local,context,ioState)	= switchFromPSt pState
	# (groups,context)					= ContextGetGroups context
	# (typeGContext,ioState)			= typeIsGContext ioState
	# (typeIOSt, ioState)				= typeIsIOSt  ioState
	# (typeLocal,ioState)				= typeIsLocal ioState
	# localIO							= {localState=Just local,localIOSt=ioState}
	# group								= GContextToGroupIO (adddoneLocal localIO gcontext)
	# groups							= adddoneRR group groups
	# context							= {context & cGroups=groups}
	# (x, context)						= fun context
	# (groups,context)					= ContextGetGroups context
	# context							= {context & cGroups=resetRR groups}
	# pState							= switchToPSt typeGContext typeIOSt typeLocal returnId context
	= (x, pState)
where
	switchToPSt :: !(UnguardType (GContext .p)) !(UnguardType (IOSt .l .p)) !(UnguardType (Maybe .l)) !SystemId !Context
					-> PSt .l .p
	switchToPSt typeGContext typeIOSt typeLocal returnId context=:{cGroups}
		| not found						= schedulerFatalError "accContext" "interactive process not found"
		| closed						= snd (cSwitchIn gcontext1 (fromJust local1) {context1 & cModalProcess=Nothing} ioState2)
		| otherwise						= snd (cSwitchIn gcontext1 (fromJust local1)  context1 ioState2)
	where
		(found,groups)					= turnRRToProcessInGroups returnId cGroups
		(gDone,gToDo)					= fromRR groups
		(group,gToDo1)					= HdTl gToDo
		(GContext` share lDone lToDo)	= splitGroupIO group
		gcontext						= {groupPublic=share,groupLocals=toRR lDone lToDo}
		(l,gcontext1)					= splitGContext (castType typeGContext gcontext)
		(LocalIO`` local ioState)		= splitLocalIO l
		ioState1						= castType typeIOSt ioState
		local1							= castType typeLocal   local
		groups1							= toRR gDone gToDo1
		context1						= {context & cGroups=groups1}
		(closed,ioState2)				= IOStClosed ioState1
		
		splitGroupIO {groupState,groupIO=groups}
			# (done,todo) = fromRR groups
			= GContext` groupState done todo
		splitLocalIO {localState,localIOSt}
			= LocalIO`` localState localIOSt

switchFromPSt :: !(PSt .l .p) -> (!GContext .p,!.l,!Context,!IOSt .l .p)
switchFromPSt pState
	= cSwitchOut ([],emptyRR,emptyRR) pState

::	*GContext`		= E..p: GContext` p [*LocalIO p] [*LocalIO p]
::	*LocalIO`	p	= E..l: LocalIO`			(IOSt l p)
::	*LocalIO``	p	= E..l: LocalIO`` (Maybe l)	(IOSt l p)


turnRRToProcessInGroups :: !SystemId !*Groups -> (!Bool,!*Groups)
turnRRToProcessInGroups id gs
	= turnRRToProcessInGroups` id (resetRR gs)
where
	turnRRToProcessInGroups` :: !SystemId !*Groups -> (!Bool,!*Groups)
	turnRRToProcessInGroups` id groups
		# (notodo,groups)	= notodoRR groups
		| notodo
			= (False,groups)
		# (group,groups)	= getcurrentRR groups
		  (found,group)		= turnRRToProcessInGroup id group
		| found
			= (True,inserttodoRR group groups)
		| otherwise
			= turnRRToProcessInGroups` id (adddoneRR group groups)
	where
		turnRRToProcessInGroup :: !SystemId !GroupIO -> (!Bool,!GroupIO)
		turnRRToProcessInGroup id {groupState=p,groupIO=locals}
			# (found,locals) = turnRRToProcessInLocals id (resetRR locals)
			= (found,{groupState=p,groupIO=locals})
		where
			turnRRToProcessInLocals :: !SystemId !*(Locals .p) -> (!Bool,!*Locals .p)
			turnRRToProcessInLocals id locals
				# (notodo,locals)	= notodoRR locals
				| notodo
					= (False,locals)
				# (local,locals)	= getcurrentRR locals
				  (found,local)		= turnRRToProcess id local
				| found
					= (True,inserttodoRR local locals)
				| otherwise
					= turnRRToProcessInLocals id (adddoneRR local locals)
			where
				turnRRToProcess :: !SystemId !*(LocalIO .p) -> (!Bool,!*LocalIO .p)
				turnRRToProcess id l=:{localIOSt=ioState}
					# (ioid,ioState)	= IOStGetIOId ioState
					= (id==ioid,{l & localIOSt=ioState})


//	A deadlock situation arises if this process would be blocked.

checkDeadlock :: !SystemId !SystemId !*Groups -> (!Bool,!*Groups)
checkDeadlock returnId switchToId gs
	= checkDeadlock` [returnId] switchToId gs
where
	checkDeadlock` :: ![SystemId] !SystemId *Groups -> (!Bool,!*Groups)
	checkDeadlock` blockedprocs nextproc gs
	# ((nextprocfound,opt_id),gs)	= checkBlockedProcess nextproc gs
	| not nextprocfound
		= (False,gs)
	| isNothing opt_id
		= (False,gs)
	# nextproc						= fromJust opt_id
	  blockedprocs					= [nextproc:blockedprocs]
	  occurs						= Contains ((==) nextproc) blockedprocs
	| occurs
		= (True, gs)
	| otherwise
		= checkDeadlock` blockedprocs nextproc gs

/*	checkBlockedProcess id groups
		locates the interactive process identified by id in groups and checks if the process is blocked.
		If this is the case then the id is returned of the process for which this process is waiting.
		If this is not the case, then no id is returned.
*/
checkBlockedProcess :: !SystemId !*Groups -> (!Result SystemId,!*Groups)
checkBlockedProcess nextproc groups
	= accessGroups (checkInGroupIO nextproc) groups
where
	checkInGroupIO :: !SystemId !*GroupIO -> (!Result SystemId,!*GroupIO)
	checkInGroupIO nextproc groupIO=:{groupIO=locals}
		# (blocked,locals)	= checkInLocals nextproc locals
		= (blocked,{groupIO & groupIO=locals})
	where
		checkInLocals :: !SystemId !*(Locals .p) -> (!Result SystemId,!*Locals .p)
		checkInLocals nextproc locals
			= accessLocals (checkInLocal nextproc) locals
		where
			checkInLocal :: !SystemId !*(LocalIO .p) -> (!Result SystemId,!LocalIO .p)
			checkInLocal nextproc localIO=:{localIOSt=ioState}
				# (blocked,ioState)	= checkProcess nextproc ioState
				= (blocked,{localIO & localIOSt=ioState})
			where
				checkProcess :: !SystemId !(IOSt .l .p) -> (!Result SystemId,!IOSt .l .p)
				checkProcess ioid ioState
					# (ioid`,ioState)		= IOStGetIOId ioState
					| ioid<>ioid`
						= ((False,Nothing),ioState)
					# (runtime,ioState)		= IOStGetRuntimeState ioState
					  (isBlocked,blockedFor)= rsIsBlocked runtime
					| isBlocked
						= ((True,Just blockedFor),ioState)
					| otherwise
						= ((True,Nothing),ioState)


/*	The process with SystemId id has a (Blocked id`) RuntimeState.
*/
processIsBlocked :: !SystemId !Context -> (!Bool,!Context)
processIsBlocked id context=:{cGroups}
	# ((procfound,opt_id),groups)	= checkBlockedProcess id cGroups
	  context						= {context & cGroups=groups}
	| procfound						= (isJust opt_id,context)
	| otherwise						= (procfound,context)


::	Result r
	:==	(	!Bool			// object is found
		,	!Maybe r		// optional access information
		)

/*	Threading f::(IOSt .l .p) -> (Result r,IOSt .l .p) through *Locals 
	applies f to every IOSt member ioState of Locals until fst (f ioState) = (True,r) is done
	by defining the function gLocals:
	
		gLocals :: *(Locals .p) -> (Result r, *Locals .p)
		gLocals locals = accessLocals f locals
*/
accessLocals :: !((LocalIO .p)->(Result r,LocalIO .p)) !*(Locals .p) -> (!Result r,!*Locals .p)
accessLocals accLocal locals
	# (lsDone,lsToDo)	= fromRR locals
	  (rDone,lsDone) 	= accessLocalIOs accLocal lsDone
	| fst rDone
		= (rDone,toRR lsDone lsToDo)
	| otherwise
		# (rToDo,lsToDo)= accessLocalIOs accLocal lsToDo
		= (rToDo,toRR lsDone lsToDo)
where
	accessLocalIOs :: !(St (LocalIO .p) (Result r)) ![*LocalIO .p] -> (!Result r, ![*LocalIO .p])
	accessLocalIOs accLocal [local:locals]
		# (r, local)		= accLocal local
		| fst r
			= (r, [local:locals])
		| otherwise
			# (rs,locals)	= accessLocalIOs accLocal locals
			= (rs,[local:locals])
	accessLocalIOs _ []
		= ((False,Nothing),[])

/*	Threading f::(IOSt .l .p) -> (Result r,IOSt .l .p) through *Groups 
	applies f to every IOSt member ioState of Groups until fst(f ioState) = (True,r) is done
	by defining the function gGroups (if gLocals is defined as above):
	
		gGroups :: *Groups -> (Result r, *Groups)
		gGroups groups = accessGroups f` groups
		where	f` {groupState=p, groupIO=locals} = (r, {groupState=p, groupIO=locals`})
				where	(r,locals`) = gLocals locals
*/
accessGroups :: !(GroupIO->(Result r,GroupIO)) !*Groups -> (!Result r,!*Groups)
accessGroups accGroup groups
	# (gsDone,gsToDo)	= fromRR groups
	  (rDone,gsDone)	= accessGroupIOs accGroup gsDone
	| fst rDone
		= (rDone,toRR gsDone gsToDo)
	| otherwise
		# (rToDo,gsToDo)= accessGroupIOs accGroup gsToDo
		= (rToDo,toRR gsDone gsToDo)
where
	accessGroupIOs :: !(St GroupIO (Result r)) ![*GroupIO] -> (!Result r,![*GroupIO])
	accessGroupIOs accGroup [g:gs]
		# (r,g)			= accGroup g
		| fst r
			= (r, [g:gs])
		| otherwise
			# (rs,gs)	= accessGroupIOs accGroup gs
			= (rs,[g:gs])
	accessGroupIOs _ gs
		= ((False,Nothing),gs)


/*	The function castType is used to let the type checker assign the type
	determined by the first argument to the expression of the second argument. 
	This function contains abc code because it can't be typed conventionally. (RWS)
*/
::	UnguardType p
	=	Unguard

castType :: (UnguardType .p) .y -> .p
castType _ y = code {
	pop_a 1
	jmp_eval
	}
