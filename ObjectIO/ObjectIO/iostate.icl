implementation module iostate


//	Clean Object I/O library, version 1.2


import	StdBool, StdFunc, StdList, StdMisc
import	commondef, devicefunctions, devicesystemstate, processstack, receivertable, timertable
import	osdocumentinterface
from	osactivaterequests	import OSActivateRequest
from	osevent				import OSEvents, OSnewEvents
from	osguishare			import OSGUIShare
from	osmouse				import OSGetDoubleClickTime
from	ostime				import OSTime
from	ostoolbox			import OSNewToolbox, OSDummyToolbox
from	oswindow			import OSWindowPtr, OSNoWindowPtr, OSWindowMetrics, OSDefaultWindowMetrics
from	roundrobin			import RR, emptyRR, notodoRR


::	*PSt l p
	=	{	ls				:: !l								// The local (and private) data of the process
		,	ps				:: !p								// The program state of the process' group
		,	io				:: !*IOSt l p						// The IOSt environment of the process
		}

::	*Groups
	:==	RR *GroupIO
::	*GroupIO
	=	E. .p:
		{	groupState		:: p
		,	groupIO			:: !*Locals p
		}
::	*Locals p
	:==	RR *(LocalIO p)
::	*LocalIO p
	=	E. .l:
		{	localState		:: !Maybe l
		,	localIOSt		:: !*IOSt l p
		}
::	*IOSt l p
	=	{	iounique		:: !*IOUnique l p
		,	ioshare			:: !IOShare   l p
		}
::	*IOUnique l p
	=	{	ioevents		:: !*OSEvents						// The event stream environment
		,	ioworld			:: ![*World]						// The world environment
		,	iolocal			:: *Locals p						// The process group members
		,	ioglobal		:: *Groups							// All other process groups
		,	ioinit			:: !IdFun (PSt l p)					// The initialisation functions of the process
		,	iotoolbox		:: !*OSToolbox						// The Mac continuation value
		}
::	IOShare	l p
	=	{	ioid			:: !SystemId						// The Id of the process
		,	ionr			:: !SystemId						// The max SystemId of all processes
		,	ioparent		:: !Maybe SystemId					// If the process is a subprocess, then Just parentId, otherwise Nothing
		,	ioguishare		:: !Maybe GUIShare					// If the process shares GUI components, then Just _, otherwise Nothing
		,	iosubids		:: ![SystemId]						// The ids of the subprocesses of the process
		,	ioidseed		:: !Int								// The global id generating number (actually the World)
		,	iodevicefuncs	:: ![DeviceFunctions  (PSt l p)]	// The currently active device functions
		,	iodevices		:: [DeviceSystemState (PSt l p)]	// The GUI device states of the process
		,	ioatts			:: ![ProcessAttribute (PSt l p)]	// The attributes of the process
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
				![ProcessAttribute (PSt .l .p)] !(IdFun (PSt .l .p)) !(Maybe SystemId)
			-> IOSt .l .p
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

emptyIOUnique :: !(IdFun (PSt .l .p)) -> (!OSWindowMetrics,!*IOUnique .l .p)
emptyIOUnique initIO
	# tb				= OSNewToolbox
	# (wMetrics,tb)		= OSDefaultWindowMetrics tb
	= (	wMetrics
	  ,	{	ioevents	= OSnewEvents
		,	ioworld		= []
		,	iolocal		= emptyRR
		,	ioglobal	= emptyRR
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

IOStButtonFreq :: !Int !Point2 !OSWindowPtr !(IOSt .l .p) -> (!Int,!IOSt .l .p)
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
	getButtonFreq :: !(IOSt .l .p) -> (!ButtonFreqState, !IOSt .l .p)
	getButtonFreq ioState=:{ioshare} = (ioshare.iobutton, ioState)
	
	setButtonFreq :: !ButtonFreqState !(IOSt .l .p) -> IOSt .l .p
	setButtonFreq bfs ioState=:{ioshare} = {ioState & ioshare={ioshare & iobutton=bfs}}

IOStSetDoubleDownDist :: !DoubleDownDist !(IOSt .l .p) -> IOSt .l .p
IOStSetDoubleDownDist ddDist ioState=:{ioshare}
	| ddDist==ioshare.iobutton.bfsdddist
		= ioState
	| otherwise
		= {ioState & ioshare={ioshare & iobutton={ioshare.iobutton & bfsdddist=max 0 ddDist}}}


/*	Access rules to KeyTrack:

IOStGetKeyTrack :: !(IOSt .l .p) -> (!Maybe KeyTrack,!IOSt .l .p)
IOStGetKeyTrack ioState=:{ioshare} = (ioshare.iokeytrack, ioState)

IOStSetKeyTrack :: !(Maybe KeyTrack) !(IOSt .l .p) -> IOSt .l .p
IOStSetKeyTrack keytrack ioState=:{ioshare} = {ioState & ioshare={ioshare & iokeytrack=keytrack}}
*/

//	Access rules to KeyTrack:

IOStGetInputTrack :: !(IOSt .l .p) -> (!Maybe InputTrack,!IOSt .l .p)
IOStGetInputTrack ioState=:{ioshare} = (ioshare.ioinputtrack, ioState)

IOStSetInputTrack :: !(Maybe InputTrack) !(IOSt .l .p) -> IOSt .l .p
IOStSetInputTrack inputtrack ioState=:{ioshare} = {ioState & ioshare={ioshare & ioinputtrack=inputtrack}}


//	Access rules to IOAttributes:

IOStGetProcessAttributes :: !(IOSt .l .p) -> (![ProcessAttribute (PSt .l .p)], !IOSt .l .p)
IOStGetProcessAttributes ioState=:{ioshare} = (ioshare.ioatts, ioState)

IOStSetProcessAttributes :: ![ProcessAttribute (PSt .l .p)] !(IOSt .l .p) -> IOSt .l .p
IOStSetProcessAttributes atts ioState=:{ioshare} = {ioState & ioshare={ioshare & ioatts=atts}}


//	Access rules to the initial actions:

IOStGetInitIO :: !(IOSt .l .p) -> (!IdFun (PSt .l .p), !IOSt .l .p)
IOStGetInitIO ioState=:{iounique=unique=:{ioinit}} = (ioinit,{ioState & iounique={unique & ioinit=id}})

IOStSetInitIO :: !(IdFun (PSt .l .p)) !(IOSt .l .p) -> IOSt .l .p
IOStSetInitIO initIO ioState = {ioState & iounique={ioState.iounique & ioinit=initIO}}


//	Access rules to RuntimeState:

IOStClosed :: !(IOSt .l .p) -> (!Bool,!IOSt .l .p)
IOStClosed ioState=:{ioshare={ioruntime=Closed}}= (True,ioState)
IOStClosed ioState								= (False,ioState)

IOStGetRuntimeState :: !(IOSt .l .p) -> (!RuntimeState, !IOSt .l .p)
IOStGetRuntimeState ioState=:{ioshare} = (ioshare.ioruntime, ioState)

IOStSetRuntimeState :: RuntimeState !(IOSt .l .p) -> IOSt .l .p
IOStSetRuntimeState runtime ioState=:{ioshare} = {ioState & ioshare={ioshare & ioruntime=runtime}}


//	Access rules to IOIsModal:

IOStGetIOIsModal :: !(IOSt .l .p) -> (!Maybe SystemId, !IOSt .l .p)
IOStGetIOIsModal ioState=:{ioshare} = (ioshare.ioismodal, ioState)

IOStSetIOIsModal :: !(Maybe SystemId) !(IOSt .l .p) -> IOSt .l .p
IOStSetIOIsModal optId ioState=:{ioshare} = {ioState & ioshare={ioshare & ioismodal=optId}}


//	Access rules to IdTable:

IOStGetIdTable :: !(IOSt .l .p) -> (!IdTable,!IOSt .l .p)
IOStGetIdTable ioState=:{ioshare} = (ioshare.ioidtable, ioState)

IOStSetIdTable :: !IdTable !(IOSt .l .p) -> IOSt .l .p
IOStSetIdTable idTable ioState=:{ioshare} = {ioState & ioshare={ioshare & ioidtable=idTable}}


//	Access rules to ReceiverTable:

IOStGetReceiverTable :: !(IOSt .l .p) -> (!ReceiverTable,!IOSt .l .p)
IOStGetReceiverTable ioState=:{ioshare} = (ioshare.ioreceivertable, ioState)

IOStSetReceiverTable :: !ReceiverTable !(IOSt .l .p) -> IOSt .l .p
IOStSetReceiverTable ioreceivertable ioState=:{ioshare} = {ioState & ioshare={ioshare & ioreceivertable=ioreceivertable}}


//	Access rules to TimerTable:

IOStGetTimerTable :: !(IOSt .l .p) -> (!TimerTable,!IOSt .l .p)
IOStGetTimerTable ioState=:{ioshare} = (ioshare.iotimertable, ioState)

IOStSetTimerTable :: !TimerTable !(IOSt .l .p) -> IOSt .l .p
IOStSetTimerTable tt ioState=:{ioshare} = {ioState & ioshare={ioshare & iotimertable=tt}}


//	Access rules to OSTime:

IOStGetOSTime :: !(IOSt .l .p) -> (!OSTime,!IOSt .l .p)
IOStGetOSTime ioState=:{ioshare} = (ioshare.ioostime,ioState)

IOStSetOSTime :: !OSTime !(IOSt .l .p) -> IOSt .l .p
IOStSetOSTime ostime ioState=:{ioshare} = {ioState & ioshare={ioshare & ioostime=ostime}}


//	Access rules to ActivateRequests:

IOStGetActivateRequests :: !(IOSt .l .p) -> (!ActivateRequests, !IOSt .l .p)
IOStGetActivateRequests ioState=:{ioshare} = (ioshare.ioactrequest, ioState)

IOStSetActivateRequests :: !ActivateRequests !(IOSt .l .p) -> IOSt .l .p
IOStSetActivateRequests ioReqs ioState=:{ioshare} = {ioState & ioshare={ioshare & ioactrequest=ioReqs}}


//	Access rules to the OSEvents environment:

IOStGetEvents :: !(IOSt .l .p) -> (!*OSEvents, !IOSt .l .p)
IOStGetEvents ioState=:{iounique=unique=:{ioevents}} = (ioevents,{ioState & iounique={unique & ioevents=OSnewEvents}})

IOStSetEvents :: !*OSEvents !(IOSt .l .p) -> IOSt .l .p
IOStSetEvents es ioState = {ioState & iounique={ioState.iounique & ioevents=es}}


//	Access rules to the World environment:

IOStGetWorld :: !(IOSt .l .p) -> (!*World, !IOSt .l .p)
IOStGetWorld ioState=:{iounique=unique=:{ioworld=[w:ws]}} = (w,{ioState & iounique={unique & ioworld=ws}})

IOStSetWorld :: !*World !(IOSt .l .p) -> IOSt .l .p
IOStSetWorld w ioState=:{iounique=unique=:{ioworld=ws}} = {ioState & iounique={unique & ioworld=[w:ws]}}


//	Access rules to Locals:

IOStGetLocals :: !(IOSt .l .p) -> (!Locals .p, !IOSt .l .p)
IOStGetLocals ioState=:{iounique=unique=:{iolocal}} = (iolocal,{ioState & iounique={unique & iolocal=emptyRR}})

IOStSetLocals :: !(Locals .p) !(IOSt .l .p) -> IOSt .l .p
IOStSetLocals local ioState = {ioState & iounique={ioState.iounique & iolocal=local}}


//	Access rules to Groups:

IOStGetGroups :: !(IOSt .l .p) -> (!Groups, !IOSt .l .p)
IOStGetGroups ioState=:{iounique=unique=:{ioglobal}} = (ioglobal,{ioState & iounique={unique & ioglobal=emptyRR}})

IOStSetGroups :: !Groups !(IOSt .l .p) -> IOSt .l .p
IOStSetGroups groups ioState = {ioState & iounique={ioState.iounique & ioglobal=groups}}


//	Access to the ProcessStack of the IOSt:

IOStGetProcessStack :: !(IOSt .l .p) -> (!ProcessStack, !IOSt .l .p)
IOStGetProcessStack ioState=:{ioshare} = (ioshare.iostack, ioState)

IOStSetProcessStack :: !ProcessStack !(IOSt .l .p) -> IOSt .l .p
IOStSetProcessStack ioStack ioState=:{ioshare} = {ioState & ioshare={ioshare & iostack=ioStack}}

SelectIOSt :: !(IOSt .l .p) -> IOSt .l .p
SelectIOSt ioState=:{ioshare} = {ioState & ioshare={ioshare & iostack=selectProcessShowState ioshare.ioid ioshare.iostack}}


//	Access rules to DocumentInterface:

IOStGetDocumentInterface :: !(IOSt .l .p) -> (!DocumentInterface, !IOSt .l .p)
IOStGetDocumentInterface ioState=:{ioshare} = (getOSDInfoDocumentInterface ioshare.ioosdinfo, ioState)


//	Access rules to OSDInfo:

IOStGetOSDInfo :: !(IOSt .l .p) -> (!OSDInfo,!IOSt .l .p)
IOStGetOSDInfo ioState=:{ioshare} = (ioshare.ioosdinfo, ioState)

IOStSetOSDInfo :: !OSDInfo !(IOSt .l .p) -> IOSt .l .p
IOStSetOSDInfo osdInfo ioState=:{ioshare} = {ioState & ioshare={ioshare & ioosdinfo=osdInfo}}


//	Access rules to ProcessKind:

IOStGetProcessKind :: !(IOSt .l .p) -> (!ProcessKind, !IOSt .l .p)
IOStGetProcessKind ioState=:{ioshare} = (ioshare.iokind, ioState)


//	Swapping of IOSt environments:

IOStSwapIO :: !(![*World],!Locals .p,!Groups) !(IOSt .l .p) -> (!(![*World],!Locals .p,!Groups),!IOSt .l .p)
IOStSwapIO (world`,locals`,globals`) ioState=:{iounique=unique=:{ioworld,iolocal,ioglobal}}
	= ((ioworld,iolocal,ioglobal),{ioState & iounique={unique & ioworld=world`,iolocal=locals`,ioglobal=globals`}})


//	Access to the SystemId of the IOSt:

IOStGetIOId :: !(IOSt .l .p) -> (!SystemId,!IOSt .l .p)
IOStGetIOId ioState=:{ioshare} = (ioshare.ioid,ioState)


//	Access to the max SystemId of the IOSt:

IOStGetMaxIONr :: !(IOSt .l .p) -> (!SystemId,!IOSt .l .p)
IOStGetMaxIONr ioState=:{ioshare} = (ioshare.ionr,ioState)

IOStSetMaxIONr :: !SystemId !(IOSt .l .p) -> IOSt .l .p
IOStSetMaxIONr maxId ioState=:{ioshare} = {ioState & ioshare={ioshare & ionr=maxId}}

IOStNewMaxIONr :: !(IOSt .l .p) -> (!SystemId,!IOSt .l .p)
IOStNewMaxIONr ioState=:{ioshare}
	= (newMaxId, {ioState & ioshare={ioshare & ionr=maxId1}})
where
	(maxId1,newMaxId) = IncrSystemId ioshare.ionr


//	Access to the parent Id of the IOSt:

IOStGetParentId :: !(IOSt .l .p) -> (!Maybe SystemId,!IOSt .l .p)
IOStGetParentId ioState=:{ioshare} = (ioshare.ioparent,ioState)


//	Access to the subprocess flag of the IOSt:

IOStGetGUIShare :: !(IOSt .l .p) -> (!Maybe GUIShare,!IOSt .l .p)
IOStGetGUIShare ioState=:{ioshare} = (ioshare.ioguishare,ioState)

IOStSetGUIShare :: !(Maybe GUIShare) !(IOSt .l .p) -> IOSt .l .p
IOStSetGUIShare guishare ioState=:{ioshare} = {ioState & ioshare={ioshare & ioguishare=guishare}}


//	Access to the SystemIds of the subprocess of the IOSt:

IOStGetSubProcessIds :: !(IOSt .l .p) -> (![SystemId],!IOSt .l .p)
IOStGetSubProcessIds ioState=:{ioshare} = (ioshare.iosubids,ioState)

IOStSetSubProcessIds :: ![SystemId] !(IOSt .l .p) -> IOSt .l .p
IOStSetSubProcessIds ids ioState=:{ioshare} = {ioState & ioshare={ioshare & iosubids=ids}}


//	Access to the global seed integer to generate all Ids (see StdId):

IOStGetIdSeed :: !(IOSt .l .p) -> (!Int,!IOSt .l .p)
IOStGetIdSeed ioState=:{ioshare}
	= (ioshare.ioidseed,ioState)

IOStSetIdSeed :: !Int !(IOSt .l .p) -> IOSt .l .p
IOStSetIdSeed seed ioState=:{ioshare}
	= {ioState & ioshare={ioshare & ioidseed=seed}}


//	Access to the ClipboardState of the IOSt:

InitClipboardState	:==	{cbsCount=0}

IOStGetClipboardState :: !(IOSt .l .p) -> (!ClipboardState, !IOSt .l .p)
IOStGetClipboardState ioState=:{ioshare} = (ioshare.ioclipboard,ioState)

IOStSetClipboardState :: !ClipboardState !(IOSt .l .p) -> IOSt .l .p
IOStSetClipboardState clipboard ioState=:{ioshare} = {ioState & ioshare={ioshare & ioclipboard=clipboard}}


//	Access to the OSWindowMetrics of the IOSt:

IOStGetOSWindowMetrics :: !(IOSt .l .p) -> (!OSWindowMetrics,!IOSt .l .p)
IOStGetOSWindowMetrics ioState=:{ioshare} = (ioshare.iooswmetrics,ioState)


//	Access to the DeviceFunctions:

IOStGetDeviceFunctions :: !(IOSt .l .p) -> (![DeviceFunctions (PSt .l .p)],!IOSt .l .p)
IOStGetDeviceFunctions ioState=:{ioshare} = (ioshare.iodevicefuncs,ioState)

IOStSetDeviceFunctions :: ![DeviceFunctions (PSt .l .p)] !(IOSt .l .p) -> IOSt .l .p
IOStSetDeviceFunctions funcs ioState=:{ioshare} = {ioState & ioshare={ioshare & iodevicefuncs=funcs}}


//	Access to the DeviceSystemStates:

IOStLastInteraction :: !(IOSt .l .p) -> (!Bool,!IOSt .l .p)
IOStLastInteraction ioState
	# (locals,ioState)		= IOStGetLocals ioState
	  (empty,locals)		= notodoRR locals
	# ioState				= IOStSetLocals locals ioState
	| not empty
		= (False,ioState)
	| otherwise
		# (groups,ioState)	= IOStGetGroups ioState
		  (empty,groups)	= notodoRR groups
		# ioState			= IOStSetGroups groups ioState
		= (empty,ioState)

IOStHasDevice :: !Device !(IOSt .l .p) -> (!Bool,!IOSt .l .p)
IOStHasDevice d ioState=:{ioshare={iodevices=ds}}
	= (devicesHaveDevice d ds, ioState)
where
	devicesHaveDevice :: !Device ![DeviceSystemState .ps] -> Bool
	devicesHaveDevice d [dState:dStates]	= toDevice dState==d || devicesHaveDevice d dStates
	devicesHaveDevice _ _					= False

IOStGetDevices :: !(IOSt .l .p) -> (![Device],!IOSt .l .p)
IOStGetDevices ioState=:{ioshare={iodevices=ds}} = (map toDevice ds,ioState)

IOStGetDevice :: !Device !(IOSt .l .p) -> (!Bool,DeviceSystemState (PSt .l .p),!IOSt .l .p)
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

IOStRemoveDevice :: !Device !(IOSt .l .p) -> IOSt .l .p
IOStRemoveDevice d ioState=:{ioshare}
	= {ioState & ioshare={ioshare & iodevices=devicesRemoveDevice d ioshare.iodevices}}
where
	devicesRemoveDevice :: !Device ![DeviceSystemState .ps] -> [DeviceSystemState .ps]
	devicesRemoveDevice d [dState:dStates]
		| toDevice dState==d		= dStates
		| otherwise					= [dState:devicesRemoveDevice d dStates]
	devicesRemoveDevice _ dStates	= dStates

IOStSetDevice :: !(DeviceSystemState (PSt .l .p)) !(IOSt .l .p) -> IOSt .l .p
IOStSetDevice d ioState=:{ioshare=ioshare=:{iodevices}}
	#! ds 		= devicesSetDevice priority d iodevices
	#! ioshare	= {ioshare & iodevices=ds}
	= {ioState & ioshare=ioshare}
where
	priority	= priorityDevice (toDevice d)
	
	devicesSetDevice :: !Int !(DeviceSystemState .ps) ![DeviceSystemState .ps] -> [DeviceSystemState .ps]
	devicesSetDevice p dState2 ds=:[dState1:dStates]
		# device1	= toDevice dState1
		| device1==toDevice dState2
			= [dState2:dStates]
		| p>priorityDevice device1
			= [dState2:ds]
		| otherwise
			#! ds	= devicesSetDevice p dState2 dStates
			= [dState1:ds]
	devicesSetDevice _ dState _
		= [dState]

// MW11..
IOStGetRcvDisabled	:: !(IOSt .l .p) -> (!Bool, !(IOSt .l .p))
IOStGetRcvDisabled io=:{ioshare={iorcvdisabled}}
	= (iorcvdisabled, io)

IOStSetRcvDisabled	:: !Bool !(IOSt .l .p) -> IOSt .l .p
IOStSetRcvDisabled iorcvdisabled io=:{ioshare}
	= { io & ioshare={ ioshare & iorcvdisabled=iorcvdisabled }}
// ..MW11

getIOToolbox :: !(IOSt .l .p) -> (!*OSToolbox,!IOSt .l .p)
getIOToolbox ioState=:{iounique=unique=:{iotoolbox}} = (iotoolbox,{ioState & iounique={unique & iotoolbox=OSDummyToolbox}})

setIOToolbox :: !*OSToolbox !(IOSt .l .p) -> IOSt .l .p
setIOToolbox tb ioState = {ioState & iounique={ioState.iounique & iotoolbox=tb}}

appIOToolbox :: !.(IdFun *OSToolbox) !(IOSt .l .p) -> IOSt .l .p
appIOToolbox f ioState=:{iounique=unique=:{iotoolbox}}
	#! tb	= f iotoolbox
	=  {ioState & iounique={unique & iotoolbox=tb}}

accIOToolbox :: !.(St *OSToolbox .x) !(IOSt .l .p) -> (!.x,!IOSt .l .p)
accIOToolbox f ioState=:{iounique=unique=:{iotoolbox}}
	#! (x,tb)	= f iotoolbox
	=  (x,{ioState & iounique={unique & iotoolbox=tb}})
