implementation module iostate


//	Clean Object I/O library, version 1.2


import	StdBool, StdFunc, StdList, StdMisc
import	commondef, devicefunctions, devicesystemstate, processstack, receivertable, timertable
import	osdocumentinterface
from	osactivaterequests	import OSActivateRequest
from	osevent				import OSEvents, OSnewEvents
from	osguishare			import OSGUIShare
from	osmouse				import OSGetDoubleClickTime
from	ossystem			import OSWindowMetrics, OSDefaultWindowMetrics
from	ostime				import OSTime
from	ostoolbox			import OSNewToolbox, OSDummyToolbox
from	ostypes				import OSWindowPtr, OSNoWindowPtr
from	roundrobin			import RR, emptyRR, notodoRR


::	*PSt l
	=	{	ls				:: !l								// The local (and private) data of the process
		,	io				:: !*IOSt l							// The IOSt environment of the process
		}

::	*CProcesses													// The 'context-free' processes administration
	:==	RR *CProcess											//	is a round-robin
::	*CProcess													// The context-free process
	=	E. .l:
		{	localState	:: !Maybe l								//	its local state
		,	localIOSt	:: !*IOSt l								//	its context-free IOSt
		}
::	*IOSt l
	=	{	iounique		:: !*IOUnique l
		,	ioshare			:: !IOShare   l
		}
::	*IOUnique l
	=	{	ioevents		:: !*OSEvents						// The event stream environment
		,	ioworld			:: ![*World]						// The world environment
		,	ioprocesses		:: *CProcesses						// All other processes
		,	ioinit			:: !IdFun (PSt l)					// The initialisation functions of the process
		,	iotoolbox		:: !*OSToolbox						// The Mac continuation value
		}
::	IOShare	l
	=	{	ioid			:: !SystemId						// The Id of the process
		,	ionr			:: !SystemId						// The max SystemId of all processes
		,	ioparent		:: !Maybe SystemId					// If the process is a subprocess, then Just parentId, otherwise Nothing
		,	ioguishare		:: !Maybe GUIShare					// If the process shares GUI components, then Just _, otherwise Nothing
		,	iosubids		:: ![SystemId]						// The ids of the subprocesses of the process
		,	ioidseed		:: !Int								// The global id generating number (actually the World)
		,	iodevicefuncs	:: ![DeviceFunctions  (PSt l)]		// The currently active device functions
		,	iodevices		:: [DeviceSystemState (PSt l)]		// The GUI device states of the process
		,	ioatts			:: ![ProcessAttribute (PSt l)]		// The attributes of the process
		,	ioruntime		:: !RuntimeState					// The runtime state of the process
		,	ioosdinfo		:: !OSDInfo							// The OS document interface information of the process
		,	iokind			:: !ProcessKind						// The kind of the process (interactive or virtual)
		,	ioismodal		:: !Maybe SystemId					// If a process has some modal windows, then Just id, otherwise Nothing
		,	ioidtable		:: !IdTable							// The table of all bound Ids
		,	ioreceivertable	:: !ReceiverTable					// The table of the current whereabouts of receivers
		,	iotimertable	:: !TimerTable						// The table of all currently active timers
		,	ioostime		:: !OSTime							// The current OSTime
		,	ioactrequest	:: !ActivateRequests				// The issued activation requests
		,	iostack			:: !ProcessStack					// The stacking order of all processes
		,	iobutton		:: !ButtonFreqState					// The state of double MouseDowns
//PA---	,	iokeytrack		:: !Maybe KeyTrack					// If the process is handling Key(Repeat/Up), then Just _, otherwise Nothing
		,	ioinputtrack	:: !Maybe InputTrack				// The process is handling mouse/key input flags
		,	ioclipboard		:: !ClipboardState					// The state of the clipboard
		,	iooswmetrics	:: !OSWindowMetrics					// The window metrics
		,	iorcvdisabled	:: !Bool							// to check, whether a receiver was disabled (explicitly or via close) (MW11++)
		}
::	GUIShare
	:==	OSGUIShare
::	RuntimeState
	=	Running													// The process is running
	|	Blocked !SystemId										// The process is blocked for the process with given id
	|	Closed													// The process is closed

::	ActivateRequests	:== [OSActivateRequest]
::	ButtonFreqState
	=	{	bfstime		:: !Int									// Last time of a MouseDown
		,	bfsfreq		:: !ButtonFreq							// Nr of DoubleMouseDowns (modulo 3)
		,	bfsdddist	:: !DoubleDownDist						// The maximum distance for two MouseDowns
		,	bfspos		:: !Point2								// Last position MouseDown
		,	bfswindow	:: !OSWindowPtr							// Window in which last MouseDown occurred
		}
::	ButtonFreq			:== Int
::	DoubleDownDist		:== Int
//::	KeyTrack			:==	Int								// Message field of the Event of the key being tracked
::	InputTrack													// Input being tracked:
	=	{	itWindow	:: !OSWindowPtr							// the parent window
		,	itControl	:: !Int									// zero if parent window, otherwise item nr of control (>0)
		,	itKind		:: !InputTrackKind						// the input kinds being tracked
		}
::	InputTrackKind												// Input source kinds:
	=	{	itkMouse	:: !Bool								// mouse
		,	itkKeyboard	:: !Bool								// keyboard
		}
::	ClipboardState
	=	{	cbsCount	:: !Int									// ScrapCount of last access
		}

/*
iostateError :: String String -> .x
iostateError rule error = Error rule "iostate" error
*/

//	Access rules to the IOSt:

//	Creation of an initial, empty IOSt:

emptyIOSt :: !SystemId !(Maybe SystemId) !(Maybe GUIShare) !DocumentInterface !ProcessKind 
				![ProcessAttribute (PSt .l)] !(IdFun (PSt .l)) !(Maybe SystemId)
			-> IOSt .l
emptyIOSt ioId parentId guishare documentInterface processKind processAtts initIO modalId
	# (wMetrics,iounique)			= emptyIOUnique initIO
	= {	iounique= iounique
	  ,	ioshare	= {	ioid			= ioId
  				  ,	ionr			= NullSystemId
  				  ,	ioparent		= parentId
  				  ,	ioguishare		= guishare
  				  ,	iosubids		= []
  				  ,	ioidseed		= 0
  				  ,	iodevicefuncs	= []
  				  ,	iodevices		= []
  				  ,	ioatts			= processAtts
  				  ,	ioruntime		= Running
		 		  ,	ioosdinfo		= emptyOSDInfo documentInterface
  				  ,	iokind			= processKind
  				  ,	ioismodal		= modalId
  				  ,	ioidtable		= initialIdTable
				  ,	ioreceivertable	= initialReceiverTable
				  ,	iotimertable	= initialTimerTable
				  ,	ioostime		= fromInt 0
  				  ,	ioactrequest	= []
  				  ,	iostack			= emptyProcessStack
  				  ,	iobutton		= InitButtonFreqState
  		//		  ,	iokeytrack		= Nothing
  				  ,	ioinputtrack	= Nothing
  				  ,	ioclipboard		= InitClipboardState
  				  ,	iooswmetrics	= wMetrics
				  , iorcvdisabled	= False // MW11++
  				  }
	  }

emptyIOUnique :: !(IdFun (PSt .l)) -> (!OSWindowMetrics,!*IOUnique .l)
emptyIOUnique initIO
	# tb				= OSNewToolbox
	# (wMetrics,tb)		= OSDefaultWindowMetrics tb
	= (	wMetrics
	  ,	{	ioevents	= OSnewEvents
		,	ioworld		= []
		,	ioprocesses	= emptyRR
		,	ioinit		= initIO
		,	iotoolbox	= tb
		}
	  )

//	Access to the ButtonFreqState:

InitButtonFreqState	:==	{	bfstime		= 0
						,	bfsfreq		= 0
						,	bfsdddist	= 5
						,	bfspos		= zero
						,	bfswindow	= OSNoWindowPtr
						}

IOStButtonFreq :: !Int !Point2 !OSWindowPtr !(IOSt .l) -> (!Int,!IOSt .l)
IOStButtonFreq now pos curWindow ioState
	# (bfs,ioState)		= getButtonFreq ioState
	  newbfs			= {bfs & bfstime=now, bfspos=pos, bfswindow=curWindow}
	| curWindow<>bfs.bfswindow
		= (1,setButtonFreq {newbfs & bfsfreq=1} ioState)
	# (double,ioState)	= accIOToolbox OSGetDoubleClickTime ioState
	  oldpos			= bfs.bfspos
	  oldfreq			= bfs.bfsfreq
	  ddDist`			= Dist oldpos.x pos.x + Dist oldpos.y pos.y
	  dTime				= now-bfs.bfstime
	| dTime>double || ddDist`>bfs.bfsdddist
		= (1,setButtonFreq {newbfs & bfsfreq=1} ioState)
	| otherwise
		# newfreq		= oldfreq+1
		= (newfreq,setButtonFreq {newbfs & bfsfreq=newfreq} ioState)
where
	getButtonFreq :: !(IOSt .l) -> (!ButtonFreqState, !IOSt .l)
	getButtonFreq ioState=:{ioshare} = (ioshare.iobutton, ioState)
	
	setButtonFreq :: !ButtonFreqState !(IOSt .l) -> IOSt .l
	setButtonFreq bfs ioState=:{ioshare} = {ioState & ioshare={ioshare & iobutton=bfs}}

IOStSetDoubleDownDist :: !DoubleDownDist !(IOSt .l) -> IOSt .l
IOStSetDoubleDownDist ddDist ioState=:{ioshare}
	| ddDist==ioshare.iobutton.bfsdddist
		= ioState
	| otherwise
		= {ioState & ioshare={ioshare & iobutton={ioshare.iobutton & bfsdddist=max 0 ddDist}}}


//	Access rules to InputTrack:

IOStGetInputTrack :: !(IOSt .l) -> (!Maybe InputTrack,!IOSt .l)
IOStGetInputTrack ioState=:{ioshare} = (ioshare.ioinputtrack, ioState)

IOStSetInputTrack :: !(Maybe InputTrack) !(IOSt .l) -> IOSt .l
IOStSetInputTrack inputtrack ioState=:{ioshare} = {ioState & ioshare={ioshare & ioinputtrack=inputtrack}}


//	Access rules to IOAttributes:

IOStGetProcessAttributes :: !(IOSt .l) -> (![ProcessAttribute (PSt .l)], !IOSt .l)
IOStGetProcessAttributes ioState=:{ioshare} = (ioshare.ioatts, ioState)

IOStSetProcessAttributes :: ![ProcessAttribute (PSt .l)] !(IOSt .l) -> IOSt .l
IOStSetProcessAttributes atts ioState=:{ioshare} = {ioState & ioshare={ioshare & ioatts=atts}}


//	Access rules to the initial actions:

IOStGetInitIO :: !(IOSt .l) -> (!IdFun (PSt .l), !IOSt .l)
IOStGetInitIO ioState=:{iounique=unique=:{ioinit}} = (ioinit,{ioState & iounique={unique & ioinit=id}})

IOStSetInitIO :: !(IdFun (PSt .l)) !(IOSt .l) -> IOSt .l
IOStSetInitIO initIO ioState = {ioState & iounique={ioState.iounique & ioinit=initIO}}


//	Access rules to RuntimeState:

IOStClosed :: !(IOSt .l) -> (!Bool,!IOSt .l)
IOStClosed ioState=:{ioshare={ioruntime=Closed}}= (True,ioState)
IOStClosed ioState								= (False,ioState)

IOStGetRuntimeState :: !(IOSt .l) -> (!RuntimeState, !IOSt .l)
IOStGetRuntimeState ioState=:{ioshare} = (ioshare.ioruntime, ioState)

IOStSetRuntimeState :: !RuntimeState !(IOSt .l) -> IOSt .l
IOStSetRuntimeState runtime ioState=:{ioshare} = {ioState & ioshare={ioshare & ioruntime=runtime}}


//	Access rules to IOIsModal:

IOStGetIOIsModal :: !(IOSt .l) -> (!Maybe SystemId, !IOSt .l)
IOStGetIOIsModal ioState=:{ioshare} = (ioshare.ioismodal, ioState)

IOStSetIOIsModal :: !(Maybe SystemId) !(IOSt .l) -> IOSt .l
IOStSetIOIsModal optId ioState=:{ioshare} = {ioState & ioshare={ioshare & ioismodal=optId}}


//	Access rules to IdTable:

IOStGetIdTable :: !(IOSt .l) -> (!IdTable,!IOSt .l)
IOStGetIdTable ioState=:{ioshare} = (ioshare.ioidtable, ioState)

IOStSetIdTable :: !IdTable !(IOSt .l) -> IOSt .l
IOStSetIdTable idTable ioState=:{ioshare} = {ioState & ioshare={ioshare & ioidtable=idTable}}


//	Access rules to ReceiverTable:

IOStGetReceiverTable :: !(IOSt .l) -> (!ReceiverTable,!IOSt .l)
IOStGetReceiverTable ioState=:{ioshare} = (ioshare.ioreceivertable, ioState)

IOStSetReceiverTable :: !ReceiverTable !(IOSt .l) -> IOSt .l
IOStSetReceiverTable ioreceivertable ioState=:{ioshare} = {ioState & ioshare={ioshare & ioreceivertable=ioreceivertable}}


//	Access rules to TimerTable:

IOStGetTimerTable :: !(IOSt .l) -> (!TimerTable,!IOSt .l)
IOStGetTimerTable ioState=:{ioshare} = (ioshare.iotimertable, ioState)

IOStSetTimerTable :: !TimerTable !(IOSt .l) -> IOSt .l
IOStSetTimerTable tt ioState=:{ioshare} = {ioState & ioshare={ioshare & iotimertable=tt}}


//	Access rules to OSTime:

IOStGetOSTime :: !(IOSt .l) -> (!OSTime,!IOSt .l)
IOStGetOSTime ioState=:{ioshare} = (ioshare.ioostime,ioState)

IOStSetOSTime :: !OSTime !(IOSt .l) -> IOSt .l
IOStSetOSTime ostime ioState=:{ioshare} = {ioState & ioshare={ioshare & ioostime=ostime}}


//	Access rules to ActivateRequests:

IOStGetActivateRequests :: !(IOSt .l) -> (!ActivateRequests, !IOSt .l)
IOStGetActivateRequests ioState=:{ioshare} = (ioshare.ioactrequest, ioState)

IOStSetActivateRequests :: !ActivateRequests !(IOSt .l) -> IOSt .l
IOStSetActivateRequests ioReqs ioState=:{ioshare} = {ioState & ioshare={ioshare & ioactrequest=ioReqs}}


//	Access rules to the OSEvents environment:

IOStGetEvents :: !(IOSt .l) -> (!*OSEvents, !IOSt .l)
IOStGetEvents ioState=:{iounique=unique=:{ioevents}} = (ioevents,{ioState & iounique={unique & ioevents=OSnewEvents}})

IOStSetEvents :: !*OSEvents !(IOSt .l) -> IOSt .l
IOStSetEvents es ioState = {ioState & iounique={ioState.iounique & ioevents=es}}


//	Access rules to the World environment:

IOStGetWorld :: !(IOSt .l) -> (!*World, !IOSt .l)
IOStGetWorld ioState=:{iounique=unique=:{ioworld=[w:ws]}} = (w,{ioState & iounique={unique & ioworld=ws}})

IOStSetWorld :: !*World !(IOSt .l) -> IOSt .l
IOStSetWorld w ioState=:{iounique=unique=:{ioworld=ws}} = {ioState & iounique={unique & ioworld=[w:ws]}}


//	Access rules to CProcesses:

IOStGetCProcesses :: !(IOSt .l) -> (!CProcesses, !IOSt .l)
IOStGetCProcesses ioState=:{iounique=unique=:{ioprocesses}} = (ioprocesses,{ioState & iounique={unique & ioprocesses=emptyRR}})

IOStSetCProcesses :: !CProcesses !(IOSt .l) -> IOSt .l
IOStSetCProcesses processes ioState = {ioState & iounique={ioState.iounique & ioprocesses=processes}}


//	Access to the ProcessStack of the IOSt:

IOStGetProcessStack :: !(IOSt .l) -> (!ProcessStack, !IOSt .l)
IOStGetProcessStack ioState=:{ioshare} = (ioshare.iostack, ioState)

IOStSetProcessStack :: !ProcessStack !(IOSt .l) -> IOSt .l
IOStSetProcessStack ioStack ioState=:{ioshare} = {ioState & ioshare={ioshare & iostack=ioStack}}

SelectIOSt :: !(IOSt .l) -> IOSt .l
SelectIOSt ioState=:{ioshare} = {ioState & ioshare={ioshare & iostack=selectProcessShowState ioshare.ioid ioshare.iostack}}


//	Access rules to DocumentInterface:

IOStGetDocumentInterface :: !(IOSt .l) -> (!DocumentInterface, !IOSt .l)
IOStGetDocumentInterface ioState=:{ioshare} = (getOSDInfoDocumentInterface ioshare.ioosdinfo, ioState)


//	Access rules to OSDInfo:

IOStGetOSDInfo :: !(IOSt .l) -> (!OSDInfo,!IOSt .l)
IOStGetOSDInfo ioState=:{ioshare} = (ioshare.ioosdinfo, ioState)

IOStSetOSDInfo :: !OSDInfo !(IOSt .l) -> IOSt .l
IOStSetOSDInfo osdInfo ioState=:{ioshare} = {ioState & ioshare={ioshare & ioosdinfo=osdInfo}}


//	Access rules to ProcessKind:

IOStGetProcessKind :: !(IOSt .l) -> (!ProcessKind, !IOSt .l)
IOStGetProcessKind ioState=:{ioshare} = (ioshare.iokind, ioState)


//	Swapping of IOSt environments:

IOStSwapIO :: !(![*World],!CProcesses) !(IOSt .l) -> (!(![*World],!CProcesses),!IOSt .l)
IOStSwapIO (world`,cprocesses`) ioState=:{iounique=unique=:{ioworld,ioprocesses}}
	= ((ioworld,ioprocesses),{ioState & iounique={unique & ioworld=world`,ioprocesses=cprocesses`}})


//	Access to the SystemId of the IOSt:

IOStGetIOId :: !(IOSt .l) -> (!SystemId,!IOSt .l)
IOStGetIOId ioState=:{ioshare} = (ioshare.ioid,ioState)


//	Access to the max SystemId of the IOSt:

IOStGetMaxIONr :: !(IOSt .l) -> (!SystemId,!IOSt .l)
IOStGetMaxIONr ioState=:{ioshare} = (ioshare.ionr,ioState)

IOStSetMaxIONr :: !SystemId !(IOSt .l) -> IOSt .l
IOStSetMaxIONr maxId ioState=:{ioshare} = {ioState & ioshare={ioshare & ionr=maxId}}

IOStNewMaxIONr :: !(IOSt .l) -> (!SystemId,!IOSt .l)
IOStNewMaxIONr ioState=:{ioshare}
	= (newMaxId, {ioState & ioshare={ioshare & ionr=maxId1}})
where
	(maxId1,newMaxId) = IncrSystemId ioshare.ionr


//	Access to the parent Id of the IOSt:

IOStGetParentId :: !(IOSt .l) -> (!Maybe SystemId,!IOSt .l)
IOStGetParentId ioState=:{ioshare} = (ioshare.ioparent,ioState)


//	Access to the subprocess flag of the IOSt:

IOStGetGUIShare :: !(IOSt .l) -> (!Maybe GUIShare,!IOSt .l)
IOStGetGUIShare ioState=:{ioshare} = (ioshare.ioguishare,ioState)

IOStSetGUIShare :: !(Maybe GUIShare) !(IOSt .l) -> IOSt .l
IOStSetGUIShare guishare ioState=:{ioshare} = {ioState & ioshare={ioshare & ioguishare=guishare}}


//	Access to the SystemIds of the subprocess of the IOSt:

IOStGetSubProcessIds :: !(IOSt .l) -> (![SystemId],!IOSt .l)
IOStGetSubProcessIds ioState=:{ioshare} = (ioshare.iosubids,ioState)

IOStSetSubProcessIds :: ![SystemId] !(IOSt .l) -> IOSt .l
IOStSetSubProcessIds ids ioState=:{ioshare} = {ioState & ioshare={ioshare & iosubids=ids}}


//	Access to the global seed integer to generate all Ids (see StdId):

IOStGetIdSeed :: !(IOSt .l) -> (!Int,!IOSt .l)
IOStGetIdSeed ioState=:{ioshare}
	= (ioshare.ioidseed,ioState)

IOStSetIdSeed :: !Int !(IOSt .l) -> IOSt .l
IOStSetIdSeed seed ioState=:{ioshare}
	= {ioState & ioshare={ioshare & ioidseed=seed}}


//	Access to the ClipboardState of the IOSt:

InitClipboardState	:==	{cbsCount=0}

IOStGetClipboardState :: !(IOSt .l) -> (!ClipboardState, !IOSt .l)
IOStGetClipboardState ioState=:{ioshare} = (ioshare.ioclipboard,ioState)

IOStSetClipboardState :: !ClipboardState !(IOSt .l) -> IOSt .l
IOStSetClipboardState clipboard ioState=:{ioshare} = {ioState & ioshare={ioshare & ioclipboard=clipboard}}


//	Access to the OSWindowMetrics of the IOSt:

IOStGetOSWindowMetrics :: !(IOSt .l) -> (!OSWindowMetrics,!IOSt .l)
IOStGetOSWindowMetrics ioState=:{ioshare} = (ioshare.iooswmetrics,ioState)


//	Access to the DeviceFunctions:

IOStGetDeviceFunctions :: !(IOSt .l) -> (![DeviceFunctions (PSt .l)],!IOSt .l)
IOStGetDeviceFunctions ioState=:{ioshare} = (ioshare.iodevicefuncs,ioState)

IOStSetDeviceFunctions :: !(DeviceFunctions (PSt .l)) !(IOSt .l) -> IOSt .l
IOStSetDeviceFunctions funcs=:{dDevice} ioState=:{ioshare=ioshare=:{iodevicefuncs}}
	= {ioState & ioshare={ioshare & iodevicefuncs=setdevicefunctions (priorityDevice dDevice) dDevice funcs iodevicefuncs}}
where
	setdevicefunctions :: !Int !Device !(DeviceFunctions .pst) ![DeviceFunctions .pst] -> [DeviceFunctions .pst]
	setdevicefunctions p device funcs fs=:[dfunc=:{dDevice}:dfuncs]
		| device==dDevice
			= [funcs:dfuncs]
		| p>priorityDevice dDevice
			= [funcs:fs]
		| otherwise
			#! fs	= setdevicefunctions p device funcs dfuncs
			= [dfunc:fs]
	setdevicefunctions _ _ funcs _
		= [funcs]

IOStRemoveDeviceFunctions :: !Device !(IOSt .l) -> IOSt .l
IOStRemoveDeviceFunctions device ioState=:{ioshare=ioshare=:{iodevicefuncs}}
	= {ioState & ioshare={ioshare & iodevicefuncs=removedevicefunctions device iodevicefuncs}}
where
	removedevicefunctions :: !Device ![DeviceFunctions .pst] -> [DeviceFunctions .pst]
	removedevicefunctions device [dfunc=:{dDevice}:dfuncs]
		| device==dDevice
			= dfuncs
		| otherwise
			#! dfuncs	= removedevicefunctions device dfuncs
			= [dfunc:dfuncs]
	removedevicefunctions _ empty
		= empty

//	Access to the DeviceSystemStates:

IOStLastInteraction :: !(IOSt .l) -> (!Bool,!IOSt .l)
IOStLastInteraction ioState
	# (processes,ioState)	= IOStGetCProcesses ioState
	  (empty,processes)		= notodoRR processes
	# ioState				= IOStSetCProcesses processes ioState
	= (not empty,ioState)

IOStHasDevice :: !Device !(IOSt .l) -> (!Bool,!IOSt .l)
IOStHasDevice d ioState=:{ioshare={iodevices=ds}}
	= (devicesHaveDevice d ds, ioState)
where
	devicesHaveDevice :: !Device ![DeviceSystemState .ps] -> Bool
	devicesHaveDevice d [dState:dStates]	= toDevice dState==d || devicesHaveDevice d dStates
	devicesHaveDevice _ _					= False

IOStGetDevices :: !(IOSt .l) -> (![Device],!IOSt .l)
IOStGetDevices ioState=:{ioshare={iodevices=ds}} = (map toDevice ds,ioState)

IOStGetDevice :: !Device !(IOSt .l) -> (!Bool,DeviceSystemState (PSt .l),!IOSt .l)
/*
IOStGetDevice device {ioshare={iodevices=[]}}
	= iostateError ("IOStGetDevice ["+++toString device+++"]") "I/O operations on empty IOSt not allowed"
*/
IOStGetDevice d ioState=:{ioshare=ioshare=:{iodevices=ds}}
	# (found,device,ds)	= devicesGetDevice d ds
	= (found,device,{ioState & ioshare={ioshare & iodevices=ds}})
where
	devicesGetDevice :: !Device ![DeviceSystemState .pst] -> (!Bool,DeviceSystemState .pst,![DeviceSystemState .pst])
	devicesGetDevice d [dState:dStates]
		| toDevice dState==d
			= (True,dState,[dState:dStates])
		| otherwise
			# (found,device,dStates)	= devicesGetDevice d dStates
			= (found,device,[dState:dStates])
	devicesGetDevice d empty
	//	= iostateError "IOStGetDevice" (toString d+++" not present in IOSt")
		= (False,undef,empty)

IOStRemoveDevice :: !Device !(IOSt .l) -> IOSt .l
IOStRemoveDevice d ioState=:{ioshare}
	= {ioState & ioshare={ioshare & iodevices=devicesRemoveDevice d ioshare.iodevices}}
where
	devicesRemoveDevice :: !Device ![DeviceSystemState .ps] -> [DeviceSystemState .ps]
	devicesRemoveDevice d [dState:dStates]
		| toDevice dState==d		= dStates
		| otherwise					= [dState:devicesRemoveDevice d dStates]
	devicesRemoveDevice _ dStates	= dStates

IOStSetDevice :: !(DeviceSystemState (PSt .l)) !(IOSt .l) -> IOSt .l
IOStSetDevice d ioState=:{ioshare=ioshare=:{iodevices}}
	#! device	= toDevice d
	#! ds 		= devicesSetDevice (priorityDevice device) device d iodevices
	#! ioshare	= {ioshare & iodevices=ds}
	= {ioState & ioshare=ioshare}
where
	devicesSetDevice :: !Int !Device !(DeviceSystemState .ps) ![DeviceSystemState .ps] -> [DeviceSystemState .ps]
	devicesSetDevice p device dState2 ds=:[dState1:dStates]
		# device1	= toDevice dState1
		| device1==device
			= [dState2:dStates]
		| p>priorityDevice device1
			= [dState2:ds]
		| otherwise
			#! ds	= devicesSetDevice p device dState2 dStates
			= [dState1:ds]
	devicesSetDevice _ _ dState _
		= [dState]

// MW11..
IOStGetRcvDisabled	:: !(IOSt .l) -> (!Bool, !(IOSt .l))
IOStGetRcvDisabled io=:{ioshare={iorcvdisabled}}
	= (iorcvdisabled, io)

IOStSetRcvDisabled	:: !Bool !(IOSt .l) -> IOSt .l
IOStSetRcvDisabled iorcvdisabled io=:{ioshare}
	= { io & ioshare={ ioshare & iorcvdisabled=iorcvdisabled }}
// ..MW11

getIOToolbox :: !(IOSt .l) -> (!*OSToolbox,!IOSt .l)
getIOToolbox ioState=:{iounique=unique=:{iotoolbox}} = (iotoolbox,{ioState & iounique={unique & iotoolbox=OSDummyToolbox}})

setIOToolbox :: !*OSToolbox !(IOSt .l) -> IOSt .l
setIOToolbox tb ioState = {ioState & iounique={ioState.iounique & iotoolbox=tb}}

appIOToolbox :: !.(IdFun *OSToolbox) !(IOSt .l) -> IOSt .l
appIOToolbox f ioState=:{iounique=unique=:{iotoolbox}}
	#! tb	= f iotoolbox
	=  {ioState & iounique={unique & iotoolbox=tb}}

accIOToolbox :: !.(St *OSToolbox .x) !(IOSt .l) -> (!.x,!IOSt .l)
accIOToolbox f ioState=:{iounique=unique=:{iotoolbox}}
	#! (x,tb)	= f iotoolbox
	=  (x,{ioState & iounique={unique & iotoolbox=tb}})
