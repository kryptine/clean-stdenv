definition module iostate


//	Clean Object I/O library, version 1.2


import	osdocumentinterface
from	osevent				import OSEvents
from	osguishare			import OSGUIShare
from	osactivaterequests	import OSActivateRequest
from	oswindow			import OSWindowPtr, OSWindowMetrics
from	ostime				import OSTime
import	devicefunctions, devicesystemstate
from	processstack		import ProcessStack, ProcessShowState, ShowFlag, ProcessKind
from	receivertable		import ReceiverTable
from	roundrobin			import RR
from	timertable			import TimerTable


::	*IOSt l p
::	*PSt  l p
	=	{	ls			:: !l								// The local (and private) data of the process
		,	ps			:: !p								// The program state of the process' group
		,	io			:: !*IOSt l p						// The IOSt environment of the process
		}

::	*Groups
	:==	RR *GroupIO
::	*GroupIO
	=	E. .p:
		{	groupState	:: p
		,	groupIO		:: !*Locals p
		}
::	*Locals p
	:==	RR *(LocalIO p)
::	*LocalIO p
	=	E. .l:
		{	localState	:: !Maybe l
		,	localIOSt	:: !*IOSt l p
		}

::	RuntimeState
	=	Running												// The process is running
	|	Blocked !SystemId									// The process is blocked for the process with given id
	|	Closed												// The process is closed
::	GUIShare
	:==	OSGUIShare

::	ActivateRequests	:== [OSActivateRequest]
::	DoubleDownDist		:== Int
//::	KeyTrack			:==	Int							// Message field of the Event of the key being tracked
::	InputTrack												// Input being tracked:
	=	{	itWindow	:: !OSWindowPtr						// the parent window
		,	itControl	:: !Int								// zero if parent window, otherwise item nr of control (>0)
		,	itKind		:: !InputTrackKind					// the input kinds being tracked
		}
::	InputTrackKind											// Input source kinds:
	=	{	itkMouse	:: !Bool							// mouse
		,	itkKeyboard	:: !Bool							// keyboard
		}
::	ClipboardState
	=	{	cbsCount	:: !Int								// ScrapCount of last access
		}


//	Access-rules on the IOSt:

emptyIOSt				:: !SystemId !(Maybe SystemId) !(Maybe GUIShare) !DocumentInterface !ProcessKind 
							![ProcessAttribute (PSt .l .p)] !(IdFun (PSt .l .p)) !(Maybe SystemId)
						-> IOSt .l .p

IOStButtonFreq			:: !Int !Point2 !OSWindowPtr	!(IOSt .l .p) -> (!Int, !IOSt .l .p)
IOStSetDoubleDownDist	:: !DoubleDownDist			!(IOSt .l .p) -> IOSt .l .p
//IOStGetKeyTrack			:: !(IOSt .l .p) -> (!Maybe KeyTrack,				 !IOSt .l .p)
IOStGetInputTrack		:: !(IOSt .l .p) -> (!Maybe InputTrack,				 !IOSt .l .p)
IOStGetProcessAttributes:: !(IOSt .l .p) -> (![ProcessAttribute (PSt .l .p)],!IOSt .l .p)
IOStGetInitIO			:: !(IOSt .l .p) -> (!IdFun (PSt .l .p),			 !IOSt .l .p)
IOStClosed				:: !(IOSt .l .p) -> (!Bool,							 !IOSt .l .p)
IOStGetRuntimeState		:: !(IOSt .l .p) -> (!RuntimeState,					 !IOSt .l .p)
IOStGetIOIsModal		:: !(IOSt .l .p) -> (!Maybe SystemId,				 !IOSt .l .p)
IOStGetIdTable			:: !(IOSt .l .p) -> (!IdTable,						 !IOSt .l .p)
IOStGetReceiverTable	:: !(IOSt .l .p) -> (!ReceiverTable,				 !IOSt .l .p)
IOStGetTimerTable		:: !(IOSt .l .p) -> (!TimerTable,					 !IOSt .l .p)
IOStGetOSTime			:: !(IOSt .l .p) -> (!OSTime,						 !IOSt .l .p)
IOStGetActivateRequests	:: !(IOSt .l .p) -> (!ActivateRequests,				 !IOSt .l .p)
IOStGetEvents			:: !(IOSt .l .p) -> (!*OSEvents,					 !IOSt .l .p)
IOStGetWorld			:: !(IOSt .l .p) -> (!*World,						 !IOSt .l .p)
IOStGetLocals			:: !(IOSt .l .p) -> (!Locals .p,					 !IOSt .l .p)
IOStGetGroups			:: !(IOSt .l .p) -> (!Groups,						 !IOSt .l .p)
IOStGetProcessStack		:: !(IOSt .l .p) -> (!ProcessStack,					 !IOSt .l .p)
IOStGetDocumentInterface:: !(IOSt .l .p) -> (!DocumentInterface,			 !IOSt .l .p)
IOStGetOSDInfo			:: !(IOSt .l .p) -> (!OSDInfo,						 !IOSt .l .p)
IOStGetProcessKind		:: !(IOSt .l .p) -> (!ProcessKind,					 !IOSt .l .p)
IOStGetIOId				:: !(IOSt .l .p) -> (!SystemId,						 !IOSt .l .p)
IOStGetMaxIONr			:: !(IOSt .l .p) -> (!SystemId,						 !IOSt .l .p)
IOStNewMaxIONr			:: !(IOSt .l .p) -> (!SystemId,						 !IOSt .l .p)
IOStGetParentId			:: !(IOSt .l .p) -> (!Maybe SystemId,				 !IOSt .l .p)
IOStGetGUIShare			:: !(IOSt .l .p) -> (!Maybe GUIShare,				 !IOSt .l .p)
IOStGetSubProcessIds	:: !(IOSt .l .p) -> (![SystemId],					 !IOSt .l .p)
IOStGetIdSeed			:: !(IOSt .l .p) -> (!Int,							 !IOSt .l .p)
IOStGetClipboardState	:: !(IOSt .l .p) -> (!ClipboardState,				 !IOSt .l .p)
IOStGetOSWindowMetrics	:: !(IOSt .l .p) -> (!OSWindowMetrics,				 !IOSt .l .p)
IOStGetDeviceFunctions	:: !(IOSt .l .p) -> (![DeviceFunctions (PSt .l .p)], !IOSt .l .p)

//IOStSetKeyTrack			:: !(Maybe KeyTrack)					!(IOSt .l .p) -> IOSt .l .p
IOStSetInputTrack		:: !(Maybe InputTrack)					!(IOSt .l .p) -> IOSt .l .p
IOStSetProcessAttributes:: ![ProcessAttribute (PSt .l .p)]		!(IOSt .l .p) -> IOSt .l .p
IOStSetInitIO			:: !(IdFun (PSt .l .p))					!(IOSt .l .p) -> IOSt .l .p
IOStSetRuntimeState		:: RuntimeState							!(IOSt .l .p) -> IOSt .l .p
IOStSetIOIsModal		:: !(Maybe SystemId)	 				!(IOSt .l .p) -> IOSt .l .p
IOStSetIdTable			:: !IdTable								!(IOSt .l .p) -> IOSt .l .p
IOStSetReceiverTable	:: !ReceiverTable						!(IOSt .l .p) -> IOSt .l .p
IOStSetTimerTable		:: !TimerTable							!(IOSt .l .p) -> IOSt .l .p
IOStSetOSTime			:: !OSTime								!(IOSt .l .p) -> IOSt .l .p
IOStSetActivateRequests	:: !ActivateRequests					!(IOSt .l .p) -> IOSt .l .p
IOStSetEvents			:: !*OSEvents							!(IOSt .l .p) -> IOSt .l .p
IOStSetWorld			:: !*World								!(IOSt .l .p) -> IOSt .l .p
IOStSetLocals			:: !(Locals .p)							!(IOSt .l .p) -> IOSt .l .p
IOStSetGroups			:: !Groups								!(IOSt .l .p) -> IOSt .l .p
IOStSetProcessStack		:: !ProcessStack						!(IOSt .l .p) -> IOSt .l .p
SelectIOSt				::										!(IOSt .l .p) -> IOSt .l .p
IOStSetOSDInfo			:: !OSDInfo								!(IOSt .l .p) -> IOSt .l .p
IOStSetMaxIONr			:: !SystemId							!(IOSt .l .p) -> IOSt .l .p
IOStSetGUIShare			:: !(Maybe GUIShare)					!(IOSt .l .p) -> IOSt .l .p
IOStSetSubProcessIds	:: ![SystemId]							!(IOSt .l .p) -> IOSt .l .p
IOStSetIdSeed			:: !Int									!(IOSt .l .p) -> IOSt .l .p
IOStSetClipboardState	:: !ClipboardState						!(IOSt .l .p) -> IOSt .l .p
IOStSetDeviceFunctions	:: ![DeviceFunctions (PSt .l .p)]		!(IOSt .l .p) -> IOSt .l .p

IOStSwapIO				:: !(![*World],!Locals .p,!Groups)		!(IOSt .l .p) -> (!(![*World],!Locals .p,!Groups),!IOSt .l .p)

IOStLastInteraction		::										!(IOSt .l .p) -> (!Bool,	!IOSt .l .p)
IOStHasDevice			:: !Device								!(IOSt .l .p) -> (!Bool,	!IOSt .l .p)
IOStGetDevices			::										!(IOSt .l .p) -> (![Device],!IOSt .l .p)

IOStGetDevice			:: !Device								!(IOSt .l .p) -> (!DeviceSystemState (PSt .l .p),!IOSt .l .p)
IOStRemoveDevice		:: !Device								!(IOSt .l .p) -> IOSt .l .p
IOStSetDevice			:: !(DeviceSystemState (PSt .l .p))		!(IOSt .l .p) -> IOSt .l .p

getIOToolbox			::										!(IOSt .l .p) -> (!*OSToolbox, !IOSt .l .p)
setIOToolbox			:: !*OSToolbox							!(IOSt .l .p) -> IOSt .l .p
appIOToolbox			:: !.(IdFun *OSToolbox)					!(IOSt .l .p) -> IOSt .l .p
accIOToolbox			:: !.(St *OSToolbox .x)					!(IOSt .l .p) -> (!.x, !IOSt .l .p)
