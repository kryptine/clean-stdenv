implementation module scheduler


//	Clean Object I/O library, version 1.2


import	StdBool, StdList, StdTuple
import	osevent, ostime
from	ossystem			import OStickspersecond
from	ostoolbox			import OSNewToolbox, OSInitToolbox
import	commondef, devicefunctions, iostate, processstack, roundrobin, timertable, world
from	StdProcessDef		import ProcessInit
from	StdPSt				import accPIO, appPIO
from	StdProcessAttribute	import isProcessKindAttribute


::	*Environs
	=	{	envsEvents		:: !*OSEvents
		,	envsWorld		:: !*World
		}
::	*Context
	=	{	cEnvs			:: !*Environs			// The global environments
		,	cProcessStack	:: !ProcessStack		// The global process stack
		,	cMaxIONr		:: !SystemId			// The global maximum system number
		,	cProcesses		:: !*CProcesses			// All processes
		,	cModalProcess	:: !Maybe SystemId		// The SystemId of the interactive process that has a modal window
		,	cReceiverTable	:: !ReceiverTable		// The global receiver-process table
		,	cTimerTable		:: !TimerTable			// The table of all currently active timers
		,	cIdTable		:: !IdTable				// The table of all bound Ids
		,	cOSTime			:: !OSTime				// The current OSTime
		,	cIdSeed			:: !Int					// The global id generating number (actually the World)
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

ContextGetProcesses :: !Context -> (!CProcesses,!Context)
ContextGetProcesses context=:{cProcesses}
	= (cProcesses,{context & cProcesses=emptyRR})

ContextGetSleepTime :: !Context -> (!Int,!Context)
ContextGetSleepTime context=:{cTimerTable,cReceiverTable}
	# maybe_sleep	= getTimeIntervalFromTimerTable cTimerTable
	# maybe_receiver= getActiveReceiverTableEntry cReceiverTable
	  sleep			= if (isJust maybe_receiver) 0								// a receiver with a non-empty message queue exists
	  				 (if (isJust maybe_sleep)	(snd (fromJust maybe_sleep))	// a timer with given interval is waiting
	  				 							OSLongSleep)					// neither a receiver nor timer
	= (sleep,context)

ContextGetOSEvents :: !Context -> (!OSEvents,!Context)
ContextGetOSEvents context=:{cEnvs=envs=:{envsEvents=es}}
	# (es1,es2)		= OScopyEvents es
	= (es1,{context & cEnvs={envs & envsEvents=es2}})

ContextSetOSEvents :: !(!OSEvents,!Context) -> Context
ContextSetOSEvents (osEvents,context=:{cEnvs=envs})
	= {context & cEnvs={envs & envsEvents=osEvents}}


//	On RuntimeState:

rsIsBlocked :: !RuntimeState -> (!Bool,SystemId)
rsIsBlocked (Blocked ioid)	= (True, ioid)
rsIsBlocked _				= (False,NullSystemId)

rsIsClosed :: !RuntimeState -> Bool
rsIsClosed Closed			= True
rsIsClosed _				= False


//	Starting an interactive process.

initContext :: !(ProcessInit (PSt .l)) !String !.l !DocumentInterface !ProcessKind !*World -> (!Context,!*OSToolbox)
initContext ioDefInit ioDefAbout local documentInterface ioKind world
	# w						= loadWorld world
	# world					= storeWorld w world
	# initEnvs				= {envsEvents=OSnewEvents,envsWorld=world}
	# tb					= OSInitToolbox OSNewToolbox
	# (ostime,tb)			= OSGetTime tb
	= (	{	cEnvs			= initEnvs
		,	cProcessStack	= ioStack
		,	cMaxIONr		= InitSystemId
		,	cProcesses		= toRR [] [openLocalIO ioState local]
		,	cModalProcess	= initModalId
		,	cReceiverTable	= initialReceiverTable
		,	cTimerTable		= initialTimerTable
		,	cIdTable		= initialIdTable
		,	cOSTime			= ostime
		,	cIdSeed			= w
		,	cOSToolbox		= OSNewToolbox
		}
	  ,	tb
	  )
where
	initModalId				= Nothing
	show					= ioKind==InteractiveProcess
	ioStack					= [{psId=InitSystemId,psShow=show,psKind=ioKind}]
	ioState					= createNewIOSt [] ioDefInit ioDefAbout InitSystemId Nothing Nothing ShareGUI documentInterface ioKind

initContext` :: !*World -> (!Context,!*OSToolbox)
initContext` world
	# w						= loadWorld world
	# world					= storeWorld w world
	# initEnvs				= {envsEvents=OSnewEvents,envsWorld=world}
	# tb					= OSInitToolbox OSNewToolbox
	# (ostime,tb)			= OSGetTime tb
	= (	{	cEnvs			= initEnvs
		,	cProcessStack	= ioStack
		,	cMaxIONr		= InitSystemId
		,	cProcesses		= toRR [] []
		,	cModalProcess	= initModalId
		,	cReceiverTable	= initialReceiverTable
		,	cTimerTable		= initialTimerTable
		,	cIdTable		= initialIdTable
		,	cOSTime			= ostime
		,	cIdSeed			= w
		,	cOSToolbox		= OSNewToolbox
		}
	  ,	tb
	  )
where
	initModalId				= Nothing
	ioStack					= []

createNewIOSt :: ![ProcessAttribute (PSt .l)] !(ProcessInit (PSt .l)) String !SystemId !(Maybe SystemId) 
					!(Maybe GUIShare) !Bool !DocumentInterface !ProcessKind
	-> IOSt .l
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
handleContextOSEvent osEvent context=:{cEnvs=envs=:{envsEvents=osEvents},cProcessStack,cProcesses,cReceiverTable,cTimerTable,cOSTime,cOSToolbox}
//	PA: shift the time in the timertable.
	# (ostime,tb)				= OSGetTime cOSToolbox
	  timeshift					= toInt (ostime-cOSTime)
	  timertable				= shiftTimeInTimerTable timeshift cTimerTable
//	PA: determine whether a TimerEvent or ASyncMessage can be generated
	  (schedulerEvent,receivertable,timertable,osEvents)
	  							= toSchedulerEvent osEvent cReceiverTable timertable cOSTime osEvents
	  processes					= resetRR cProcesses
	# context					= {context & cEnvs			= {envs & envsEvents=osEvents}
										   , cProcesses		= processes
										   , cReceiverTable	= receivertable
										   , cTimerTable	= timertable
										   , cOSTime		= ostime
										   , cOSToolbox		= tb
								  }
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
		# (processes,context)	= ContextGetProcesses context
		# (ioStack,processes)	= activateTopOfGroups newTopIO ioStack (resetRR processes)
		= (replyToOS,{context & cProcessStack=ioStack,cProcesses=processes})


/*	PA: new function:
	in case a non-urgent, removable system event has been caught, check if a TimerEvent or ASyncMessage 
	can be generated instead. If both a TimerEvent and an ASyncMessage are available, use the OSTime to 
	decide which one to choose.
*/
zerotimelimit :: OSTime
zerotimelimit =: fromInt (max 1 (OStickspersecond/20))

toSchedulerEvent :: !OSEvent !ReceiverTable !TimerTable !OSTime !*OSEvents -> (!SchedulerEvent,!ReceiverTable,!TimerTable,!*OSEvents)
toSchedulerEvent osevent receivertable timertable osTime osEvents
	| eventIsUrgent
		= (schedulerEvent,receivertable,timertable,osEvents)
	| (not sure_timer) && not sure_receiver
		= (schedulerEvent,receivertable,timertable,osEvents)
	| sure_timer && sure_receiver
		| isEven (toInt osTime)
			= (timerEvent,receivertable,timertable`,osEvents`)
		// otherwise
			= (asyncEvent,receivertable`,timertable,osEvents`)
	| sure_timer
		= (timerEvent,receivertable,timertable`,osEvents`)
	| otherwise
		= (asyncEvent,receivertable`,timertable,osEvents)
where
	eventIsUrgent				= OSEventIsUrgent osevent
	maybe_timer					= getTimeIntervalFromTimerTable timertable
	(zerotimer,interval)		= fromJust maybe_timer
	maybe_receiver				= getActiveReceiverTableEntry receivertable
	sure_timer					= isJust maybe_timer && interval<=0
	sure_receiver				= isJust maybe_receiver
	schedulerEvent				= ScheduleOSEvent osevent []
	(asyncEvent,receivertable`)	= toASyncEvent (fromJust maybe_receiver) receivertable
	(timerEvent,timertable`)	= toTimerEvent timertable
//	osEvents`					= checkOSZeroTimerEvent zerotimer osTime osevent osEvents
	osEvents`					= checkOSZeroTimerEvent maybe_timer osTime osevent osEvents

//	In case the original event is a virtual zero timer event:
//		check if another should be inserted in the OSEvents to circumvent the event system call. 
//	In case the original event is a non urgent event:
//		check if an initial virtual zero timer event must be inserted to start circumventing event system calls.
//	checkOSZeroTimerEvent :: !Bool !OSTime !OSEvent !*OSEvents -> *OSEvents
	checkOSZeroTimerEvent :: !(Maybe (Bool,Int)) !OSTime !OSEvent !*OSEvents -> *OSEvents
//	checkOSZeroTimerEvent zerotimer osTime osevent osEvents
	checkOSZeroTimerEvent maybe_timer osTime osevent osEvents
		| isJust maybe_zerotimer_start && zerotimer
			| osTime-zerotimer_start<=zerotimelimit
				= OSinsertEvents [osevent] osEvents
			// otherwise
				= osEvents
		| isNothing maybe_zerotimer_start && zerotimer
			= OSinsertEvents [createOSZeroTimerEvent osTime] osEvents
		| otherwise
			= osEvents
	where
		(zerotimer,_)			= fromJust maybe_timer
		maybe_zerotimer_start	= getOSZeroTimerStartTime osevent
		zerotimer_start			= fromJust maybe_zerotimer_start
	
//	The receiver for which an ASyncMessage is generated is placed behind all other receivers, 
//	creating a round-robin order. Its asynchronous message queue length field is decreased.
	toASyncEvent :: !Id !ReceiverTable -> (!SchedulerEvent,!ReceiverTable)
	toASyncEvent rid receivertable
		#! rte					= fromJust (getReceiverTableEntry rid receivertable)
		#! rte					= {rte & rteASMCount=rte.rteASMCount-1}
		#! receivertable		= setReceiverTableEntry rte (snd (removeReceiverFromReceiverTable rid receivertable))
		= (ScheduleMsgEvent (ASyncMessage {asmRecLoc=rte.rteLoc}),receivertable)
	
//	The timer for which a TimerEvent is generated is determined by getActiveTimerInTable.
//	This function already takes care of fairness using a round robin scheme.
	toTimerEvent :: !TimerTable -> (!SchedulerEvent,!TimerTable)
	toTimerEvent timertable
		# (maybeTimerEvent,timertable)	= getActiveTimerInTimerTable timertable
		= (ScheduleTimerEvent (fromJust maybeTimerEvent),timertable)


handleEventForContext :: !Bool !SchedulerEvent !Context -> (!SchedulerEvent,!Context)
handleEventForContext eventDone schedulerEvent context=:{cProcesses=processes}
	# (notodo,processes)	= notodoRR processes
	| notodo
		= (schedulerEvent,{context & cProcesses=processes})
	# (process,processes)	= getcurrentRR processes
	# (quitted,process)		= processQuitted process
	# (isModal,process)		= processModal   process
	| quitted && not isModal
		= handleEventForContext eventDone schedulerEvent {context & cProcesses=processes}
	| quitted
		= handleEventForContext eventDone schedulerEvent {context & cProcesses=adddoneRR process processes}
	| otherwise
		# (eventDone,schedulerEvent,process,context)
						= handleEventForLocalIO eventDone schedulerEvent process {context & cProcesses=processes}
		= handleEventForContext eventDone schedulerEvent {context & cProcesses=adddoneRR process context.cProcesses}
where
	processQuitted :: !CProcess -> (!Bool,!CProcess)
	processQuitted localIO=:{localIOSt}
		# (closed,ioState) = IOStClosed localIOSt
		= (closed,{localIO & localIOSt=ioState})
	
	processModal :: !CProcess -> (!Bool,!CProcess)
	processModal localIO=:{localIOSt}
		# (optModal,ioState)= IOStGetIOIsModal localIOSt
		# (myId,ioState)	= IOStGetIOId ioState
		= (isJust optModal && myId==fromJust optModal,{localIO & localIOSt=ioState})

handleEventForLocalIO :: !Bool !SchedulerEvent !CProcess !Context
					 -> (!Bool,!SchedulerEvent,!CProcess,!Context)
handleEventForLocalIO eventDone schedulerEvent {localState=opt_local,localIOSt=ioState} context
	# (runtime,ioState)						= IOStGetRuntimeState ioState
	| fst (rsIsBlocked runtime)
		= (eventDone,schedulerEvent,{localState=opt_local, localIOSt=ioState},context)
	# (initIO,ioState)						= IOStGetInitIO ioState
	# (dummies,pState)						= cSwitchIn (fromJust opt_local) context ioState
	# pState								= initIO pState
	# (ioKind,pState)						= accPIO IOStGetProcessKind pState
	| ioKind==VirtualProcess
		# pState							= closeVirtualProcess pState
		# (local,context,ioState)			= cSwitchOut dummies pState
		= (eventDone,schedulerEvent,{localState=Just local,localIOSt=ioState},context)
	# (closed,pState)						= accPIO IOStClosed pState
	| closed
		# (local,context,ioState)			= cSwitchOut dummies pState
		= (eventDone,schedulerEvent,{localState=Just local,localIOSt=ioState},context)
	| otherwise
		# (deviceFunctions,pState)			= accPIO IOStGetDeviceFunctions pState
		  ioFunctions						= [(df.dEvent,df.dDoIO) \\ df<-deviceFunctions]
		# (eventDone,schedulerEvent,pState)	= handleEventForDevices ioFunctions eventDone schedulerEvent pState
		# (local,context,ioState)			= cSwitchOut dummies pState
		= (eventDone,schedulerEvent,{localState=Just local,localIOSt=ioState},context)
where
	closeVirtualProcess :: !(PSt .l) -> PSt .l
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

cSwitchIn :: !.l !Context !(IOSt .l) -> (!(![*World],!CProcesses),!PSt .l)
cSwitchIn local {cEnvs={envsEvents,envsWorld},cProcessStack,cMaxIONr,cProcesses,cModalProcess,cReceiverTable,cTimerTable,cIdTable,cOSTime,cIdSeed} ioState
	# ioState				= IOStSetProcessStack cProcessStack ioState
	# ioState				= IOStSetEvents envsEvents ioState
	# ioState				= IOStSetMaxIONr cMaxIONr ioState
	# (ioContext,ioState)	= IOStSwapIO ([envsWorld],cProcesses) ioState
	# ioState				= IOStSetIOIsModal cModalProcess ioState
	# ioState				= IOStSetIdTable cIdTable ioState
	# ioState				= IOStSetReceiverTable cReceiverTable ioState
	# ioState				= IOStSetTimerTable cTimerTable ioState
	# ioState				= IOStSetOSTime cOSTime ioState
	# ioState				= IOStSetIdSeed cIdSeed ioState
	# pState				= {ls=local,io=ioState}
	= (ioContext,pState)

cSwitchOut :: !(![*World],!CProcesses) !(PSt .l) -> (!.l,!Context,!IOSt .l)
cSwitchOut ioContext {ls,io}
	# (ostime,   ioState)		= IOStGetOSTime			io
	# (tt,       ioState)		= IOStGetTimerTable		ioState
	# (idseed,   ioState)		= IOStGetIdSeed			ioState
	# (ridlocs,  ioState)		= IOStGetReceiverTable	ioState
	# (idtable,  ioState)		= IOStGetIdTable		ioState
	# (modalId,  ioState)		= IOStGetIOIsModal		ioState
	# (ioContext,ioState)		= IOStSwapIO ioContext	ioState
	# (worlds,processes)		= ioContext
	# (maxIONr,  ioState)		= IOStGetMaxIONr		ioState
	# (ioStack,  ioState)		= IOStGetProcessStack	ioState
	# (es,	     ioState)		= IOStGetEvents			ioState
	# envs						= {envsEvents=es,envsWorld=hd worlds}
	# context					= {	cEnvs			= envs
								  ,	cProcessStack	= ioStack
								  ,	cMaxIONr		= maxIONr
								  ,	cProcesses		= processes
								  ,	cModalProcess	= modalId
								  ,	cReceiverTable	= ridlocs
								  ,	cTimerTable		= tt
								  ,	cIdTable		= idtable
								  ,	cOSTime			= ostime
								  ,	cIdSeed			= idseed
								  ,	cOSToolbox		= OSNewToolbox
								  }
	= (ls,context,ioState)

/*	handleEventForDevices in sequence lets the devices handle the scheduler event until it is handled
	or the process is terminated (IOStClosed returns True).
	Before handing over the event to the device DoIOFunction, the device first maps the event to a
	device event if possible using its EventFunction. 
*/	
handleEventForDevices :: ![!(!EventFunction (PSt .l),!DoIOFunction (PSt .l))] !Bool !SchedulerEvent (PSt .l)
																		  -> (!Bool,!SchedulerEvent, PSt .l)
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

handleOneEventForDevices :: !SchedulerEvent !(PSt .l) -> (!Bool,!SchedulerEvent,!PSt .l)
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

addVirtualProcess :: !(ProcessInit (PSt .l)) String .l !(PSt .l`) -> PSt .l`
addVirtualProcess ioDefInit ioDefAbout local pState
	# (nr,ioState)			= IOStNewMaxIONr pState.io
	# (parentId,ioState)	= IOStGetIOId					ioState
	# (guishare,ioState)	= getGUIShare     ShareGUI		ioState
	# ioState				= addSubProcessId ShareGUI nr	ioState
	# (ioStack, ioState)	= IOStGetProcessStack			ioState
	  ioStack				= pushProcessShowState {psId=nr,psShow=False,psKind=VirtualProcess} ioStack
	# ioState				= IOStSetProcessStack ioStack	ioState
	# (processes,ioState)	= IOStGetCProcesses				ioState
	# newIOSt				= createNewIOSt [] ioDefInit ioDefAbout nr (Just parentId) guishare ShareGUI NDI VirtualProcess
	# process				= openLocalIO newIOSt local
	# ioState				= IOStSetCProcesses (inserttodoRR process processes) ioState
	= {pState & io=ioState}


//	Create a data sharing interactive process.

addInteractiveProcess :: ![ProcessAttribute (PSt .l)] !(ProcessInit (PSt .l)) String .l !Bool !DocumentInterface !(PSt .l`) -> PSt .l`
addInteractiveProcess pAtts ioDefInit ioDefAbout local isSubProcess documentInterface pState
	# (nr,ioState)			= IOStNewMaxIONr pState.io
	# (parentId,ioState)	= IOStGetIOId						ioState
	# (guishare,ioState)	= getGUIShare     isSubProcess		ioState
	# ioState				= addSubProcessId isSubProcess nr	ioState
	# (ioStack, ioState)	= IOStGetProcessStack				ioState
	  ioStack				= pushProcessShowState {psId=nr,psShow=True,psKind=InteractiveProcess} ioStack
	# ioState				= IOStSetProcessStack ioStack		ioState
	# (processes,ioState)	= IOStGetCProcesses					ioState
	  parent				= if isSubProcess (Just parentId) Nothing
	  pAtts					= filter (isProcessKindAttribute documentInterface) pAtts
	# newIOSt				= createNewIOSt pAtts ioDefInit ioDefAbout nr parent guishare isSubProcess documentInterface InteractiveProcess
	# process				= openLocalIO newIOSt local
	# ioState				= IOStSetCProcesses (inserttodoRR process processes) ioState
	= {pState & io=ioState}

openLocalIO :: !(IOSt .l) !.l -> CProcess
openLocalIO ioState local
	= {	localState	= Just local
	  ,	localIOSt	= ioState
	  }

getGUIShare :: !Bool !(IOSt .l) -> (!Maybe GUIShare,!IOSt .l)
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

addSubProcessId :: !Bool !SystemId !(IOSt .l) -> IOSt .l
addSubProcessId isSubProcess nr ioState
	| not isSubProcess
		= ioState
	| otherwise
		# (subids,ioState)	= IOStGetSubProcessIds ioState
		  ioState			= IOStSetSubProcessIds [nr:subids] ioState
		= ioState


//	Make the proper interactive process active.

activateTopOfGroups :: !SystemId !ProcessStack !CProcesses -> (!ProcessStack,!CProcesses)
activateTopOfGroups topIONr ioStack processes
	# (emptytodo,processes)			= notodoRR processes
	| emptytodo
		= (ioStack,processes)
	# (process,processes)			= getcurrentRR processes
	  (activated,ioStack,process)	= activateTopProcess topIONr ioStack process
	| activated
		= (ioStack,inserttodoRR process processes)
	| otherwise
		# (ioStack,processes)		= activateTopOfGroups  topIONr ioStack processes
		= (ioStack,inserttodoRR process processes)
where
	activateTopProcess :: !SystemId !ProcessStack !CProcess -> (!Bool,!ProcessStack,!CProcess)
	activateTopProcess topIONr ioStack process=:{localIOSt=ioState}
		# (nr,ioState)			= IOStGetIOId ioState
		| nr<>topIONr
			= (False,ioStack ,{process & localIOSt=ioState})
		| otherwise
			# ioState			= IOStSetProcessStack ioStack ioState
			# (ioStack,ioState)	= IOStGetProcessStack ioState
			= (True,ioStack,{process & localIOSt=ioState})


/*	Quit this interactive or virtual process.
	It should be an impossible situation for the system to have to quit a blocked process (the guard should never hold).
	Quitting a process involves the following:
	-	Set the RuntimeState to Closed (quitProcess is the only function that does this)
	-	Force all sub processes to quit
	-	Inform the parent process about its termination
	-	Remove the process from the ProcessStack
	-	Close all devices
*/
quitProcess :: !(PSt .l) -> PSt .l
quitProcess pState
	# (rs,pState)					= accPIO IOStGetRuntimeState pState
	| fst (rsIsBlocked rs)
		= schedulerFatalError "quitProcess" "closeProcess applied to blocked process"
	| rsIsClosed rs
		= pState
	| otherwise
		# (deviceFunctions,pState)	= accPIO IOStGetDeviceFunctions pState
		# pState					= StrictSeq [df.dClose \\ df<-deviceFunctions] pState
		# ioState					= IOStSetRuntimeState Closed     pState.io
		# (nr,ioState)				= IOStGetIOId                    ioState
		# (subids,ioState)			= IOStGetSubProcessIds           ioState
		# ioState					= quitSubProcesses subids        ioState
		# ioState					= removeIOIdFromParentProcess nr ioState
		# (ioStack,ioState)			= IOStGetProcessStack            ioState
		  (_,ioStack)				= removeProcessShowState nr      ioStack
		# ioState					= IOStSetProcessStack ioStack    ioState
		# (osdinfo,ioState)			= IOStGetOSDInfo                 ioState
		# ioState					= appIOToolbox (OScloseOSDInfo osdinfo) ioState
		= {pState & io=ioState}


/*	quitSubProcesses searches for all processes in the current process administration
	with the given ids. Each of these processes is forced to quit by setting the initialisation
	functions to [appPIO quitProcess]. In this way all recursive descendent processes will
	be quitted as well.
*/
quitSubProcesses :: ![SystemId] !(IOSt .l) -> IOSt .l
quitSubProcesses ids ioState
	# (processes,ioState)	= IOStGetCProcesses ioState
	  (_,processes)			= quitLocalSubProcesses ids processes
	# ioState				= IOStSetCProcesses processes ioState
	= ioState
where
	quitLocalSubProcesses :: ![SystemId] !CProcesses -> (![SystemId],!CProcesses)
	quitLocalSubProcesses ids processes
		| isEmpty ids
			= (ids,processes)
		| otherwise
			# (done,todo)	= fromRR processes
			  (ids,done)	= quitLocalSubProcesses` ids done
			  (ids,todo)	= quitLocalSubProcesses` ids todo
			= (ids,toRR done todo)
	where
		quitLocalSubProcesses` :: ![SystemId] ![CProcess] -> (![SystemId],![CProcess])
		quitLocalSubProcesses` ids=:[] processes
			= (ids,processes)
		quitLocalSubProcesses` ids processes=:[]
			= (ids,processes)
		quitLocalSubProcesses` ids [process=:{localState,localIOSt=ioState}:processes]
			# (ioid,ioState)		= IOStGetIOId ioState
			  (hadId,_,ids)			= Remove ((==) ioid) NullSystemId ids
			| hadId
				# (subids,ioState)	= IOStGetSubProcessIds ioState
				# ioState			= IOStSetInitIO quitProcess ioState
				# process			= {localState=localState,localIOSt=ioState}
				# (ids,processes)	= quitLocalSubProcesses` (ids++subids) processes
				= (ids,[process:processes])
			| otherwise
				# process			= {localState=localState,localIOSt=ioState}
				# (ids,processes)	= quitLocalSubProcesses` ids processes
				= (ids,[process:processes])


/*	removeIOIdFromParentProcess searches for the parent process in the current process
	administration. It is a fatal error not to find this process. In the administration
	of the parent process the child process id is removed.
*/
removeIOIdFromParentProcess :: !SystemId !(IOSt .l) -> IOSt .l
removeIOIdFromParentProcess me ioState
	# (opt_parent,ioState)	= IOStGetParentId ioState
	| isNothing opt_parent
		= ioState
	# parent				= fromJust opt_parent
	# (locals,ioState)		= IOStGetCProcesses ioState
	# (done,locals)			= removeIOIdFromLocals me parent locals
	# ioState				= IOStSetCProcesses locals ioState
	| done
		= ioState
	| otherwise
		= schedulerFatalError "CloseProcess" "parent process could not be located"
where
	removeIOIdFromLocals :: !SystemId !SystemId !CProcesses -> (!Bool,!CProcesses)
	removeIOIdFromLocals me parent locals
		# (done,todo)			= fromRR locals
		  (removed,done)		= removeIOIdFromLocals` me parent done
		| removed
			= (removed,toRR done todo)
		| otherwise
			# (removed, todo)	= removeIOIdFromLocals` me parent todo
			= (removed,toRR done todo)
	where
		removeIOIdFromLocals` :: !SystemId !SystemId ![CProcess] -> (!Bool,![CProcess])
		removeIOIdFromLocals` me parent [process=:{localState,localIOSt=ioState}:processes]
			# (ioid,ioState)			= IOStGetIOId ioState
			| parent==ioid
				# (subids,ioState)		= IOStGetSubProcessIds ioState
				  (_,_,subids)			= Remove ((==) me) (dummy "removeIOIdFromLocals") subids
				# ioState				= IOStSetSubProcessIds subids ioState
				# process				= {localState=localState,localIOSt=ioState}
				= (True,[process:processes])
			| otherwise
				# process				= {localState=localState,localIOSt=ioState}
				# (removed,processes)	= removeIOIdFromLocals` me parent processes
				= (removed,[process:processes])
		removeIOIdFromLocals` _ _ _
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
cswitchProcess :: !SystemId !SchedulerEvent !(PSt .l) -> (!Maybe SwitchError,![SemiDynamic],!PSt .l)
cswitchProcess processId message pState
	| processId==returnId
		= (Just SwitchToYourself,    [],pState1)
	| not switchToExists
		= (Just SwitchToDoesNotExist,[],pState2)
	with
		context2						= {context1 & cProcesses=groups2}
		pState2							= switchToPSt typeIOSt returnId context2 local
	| inDeadlock
		= (Just SwitchEndsUpInDeadlock,[],pState2)
	with
		context2						= {context1 & cProcesses=groups3}
		pState2							= switchToPSt typeIOSt returnId context2 local
	| otherwise
		= (checkSyncMessageError message1,getSyncMessageResponse message1,pState2)
	with
		context2						= {context1 & cProcesses=groups3}
		(_,context3)					= CondHandleEvents (processIsBlocked processId) OSNewToolbox context2
		(groups4,context4)				= ContextGetProcesses context3
		context5						= {context4 & cProcesses=resetRR groups4}
		(message1,context6)				= handleEventForContext False message context5
		pState2							= switchToPSt typeIOSt returnId context6 local
where
	(returnId,pState1)					= accPIO IOStGetIOId pState
	(local,context,ioState)= switchFromPSt pState1
	(groups,context1)					= ContextGetProcesses context
	ioState1							= IOStSetRuntimeState (Blocked processId) ioState
	(typeIOSt, ioState3)				= typeIsIOSt  ioState1
	blockedLocalIO						= {localState=Nothing,localIOSt=ioState3}
	groups1								= adddoneRR blockedLocalIO groups
	(switchToExists,groups2)			= turnRRToProcessInGroups processId groups1
	(inDeadlock,groups3)				= checkDeadlock  returnId processId groups2
	
	switchToPSt :: !(UnguardType (IOSt .l)) !SystemId !Context .l -> PSt .l
	switchToPSt typeIOSt returnId context=:{cProcesses} local
		# (_,groups)					= turnRRToProcessInGroups returnId cProcesses
		  (group,groups)				= getcurrentRR groups
		  (gDone,gToDo)					= fromRR groups
		  {localIOSt=blockedIO}			= group
		  blockedIO						= castType typeIOSt blockedIO
		  context						= {context & cProcesses=toRR gDone gToDo}
		  (_,pState)					= cSwitchIn local context (IOStSetRuntimeState Running blockedIO)
		= pState
	
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

typeIsIOSt :: !(IOSt .l) -> (UnguardType (IOSt .l),!IOSt .l)
typeIsIOSt ioState = (Unguard,ioState)

typeIsLocal :: !(IOSt .l) -> (UnguardType (Maybe .l),!IOSt .l)
typeIsLocal ioState = (Unguard,ioState)


accContext :: !.(St Context .x) !(PSt .l) -> (!.x, !PSt .l)
accContext fun pState
	# (returnId,pState)			= accPIO IOStGetIOId pState
	# (local,context,ioState)	= switchFromPSt pState
	# (groups,context)			= ContextGetProcesses context
	# (typeIOSt, ioState)		= typeIsIOSt  ioState
	# (typeLocal,ioState)		= typeIsLocal ioState
	# localIO					= {localState=Just local,localIOSt=ioState}
	# groups					= adddoneRR localIO groups
	# context					= {context & cProcesses=groups}
	# (x, context)				= fun context
	# (groups,context)			= ContextGetProcesses context
	# context					= {context & cProcesses=resetRR groups}
	# pState					= switchToPSt typeIOSt typeLocal returnId context
	= (x, pState)
where
	switchToPSt :: !(UnguardType (IOSt .l)) !(UnguardType (Maybe .l)) !SystemId !Context -> PSt .l
	switchToPSt typeIOSt typeLocal returnId context=:{cProcesses}
		| not found				= schedulerFatalError "accContext" "interactive process not found"
		| closed				= snd (cSwitchIn (fromJust local1) {context1 & cModalProcess=Nothing} ioState2)
		| otherwise				= snd (cSwitchIn (fromJust local1)  context1 ioState2)
	where
		(found,groups)			= turnRRToProcessInGroups returnId cProcesses
		(gDone,gToDo)			= fromRR groups
		(group,gToDo1)			= HdTl gToDo
		{localState=local,localIOSt=ioState}
								= group
		ioState1				= castType typeIOSt ioState
		local1					= castType typeLocal   local
		groups1					= toRR gDone gToDo1
		context1				= {context & cProcesses=groups1}
		(closed,ioState2)		= IOStClosed ioState1

switchFromPSt :: !(PSt .l) -> (!.l,!Context,!IOSt .l)
switchFromPSt pState
	= cSwitchOut ([],emptyRR) pState

turnRRToProcessInGroups :: !SystemId !*CProcesses -> (!Bool,!*CProcesses)
turnRRToProcessInGroups id gs
	= turnRRToProcess id (resetRR gs)
where
	turnRRToProcess :: !SystemId !*CProcesses -> (!Bool,!*CProcesses)
	turnRRToProcess id locals
		# (notodo,locals)	= notodoRR locals
		| notodo
			= (False,locals)
		# (local,locals)	= getcurrentRR locals
		  (found,local)		= turnRRToProcess` id local
		| found
			= (True,inserttodoRR local locals)
		| otherwise
			= turnRRToProcess id (adddoneRR local locals)
	where
		turnRRToProcess` :: !SystemId !*CProcess -> (!Bool,!*CProcess)
		turnRRToProcess` id l=:{localIOSt=ioState}
			# (ioid,ioState)	= IOStGetIOId ioState
			= (id==ioid,{l & localIOSt=ioState})


//	A deadlock situation arises if this process would be blocked.

checkDeadlock :: !SystemId !SystemId !*CProcesses -> (!Bool,!*CProcesses)
checkDeadlock returnId switchToId gs
	= checkDeadlock` [returnId] switchToId gs
where
	checkDeadlock` :: ![SystemId] !SystemId *CProcesses -> (!Bool,!*CProcesses)
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
checkBlockedProcess :: !SystemId !*CProcesses -> (!Result SystemId,!*CProcesses)
checkBlockedProcess nextproc groups
	= accessLocals (checkInLocal nextproc) groups
where
	checkInLocal :: !SystemId !*CProcess -> (!Result SystemId,!CProcess)
	checkInLocal nextproc localIO=:{localIOSt=ioState}
		# (blocked,ioState)	= checkProcess nextproc ioState
		= (blocked,{localIO & localIOSt=ioState})
	where
		checkProcess :: !SystemId !(IOSt .l) -> (!Result SystemId,!IOSt .l)
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
processIsBlocked id context=:{cProcesses}
	# ((procfound,opt_id),groups)	= checkBlockedProcess id cProcesses
	  context						= {context & cProcesses=groups}
	| procfound						= (isJust opt_id,context)
	| otherwise						= (procfound,context)


::	Result r
	:==	(	!Bool			// object is found
		,	!Maybe r		// optional access information
		)

/*	Threading f::(IOSt .l .p) -> (Result r,IOSt .l .p) through *CProcesses 
	applies f to every IOSt member ioState of CProcesses until fst (f ioState) = (True,r) is done
	by defining the function gLocals:
	
		gLocals :: *CProcesses -> (Result r, *CProcesses)
		gLocals locals = accessLocals f locals
*/
accessLocals :: !(St CProcess (Result r)) !*CProcesses -> (!Result r,!*CProcesses)
accessLocals accLocal locals
	# (lsDone,lsToDo)	= fromRR locals
	  (rDone,lsDone) 	= accessLocalIOs accLocal lsDone
	| fst rDone
		= (rDone,toRR lsDone lsToDo)
	| otherwise
		# (rToDo,lsToDo)= accessLocalIOs accLocal lsToDo
		= (rToDo,toRR lsDone lsToDo)
where
	accessLocalIOs :: !(St CProcess (Result r)) ![*CProcess] -> (!Result r, ![*CProcess])
	accessLocalIOs accLocal [local:locals]
		# (r, local)		= accLocal local
		| fst r
			= (r, [local:locals])
		| otherwise
			# (rs,locals)	= accessLocalIOs accLocal locals
			= (rs,[local:locals])
	accessLocalIOs _ []
		= ((False,Nothing),[])

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
