definition module iostate


//	Clean Object I/O library, version 1.2


import	osdocumentinterface
from	osevent				import OSEvents
from	osguishare			import OSGUIShare
from	osactivaterequests	import OSActivateRequest
from	ossystem			import OSWindowMetrics
from	ostypes				import OSWindowPtr
from	ostime				import OSTime
import	devicefunctions, devicesystemstate
from	processstack		import ProcessStack, ProcessShowState, ShowFlag, ProcessKind
from	receivertable		import ReceiverTable
from	roundrobin			import RR
from	timertable			import TimerTable


::	*IOSt l
::	*PSt  l
	=	{	ls			:: !l					// The local (and private) data of the process
		,	io			:: !*IOSt l				// The IOSt environment of the process
		}

::	*CProcesses									// The 'context-free' processes administration
	:==	RR *CProcess							//	is a round-robin
::	*CProcess									// The context-free process
	=	E. .l:
		{	localState	:: !Maybe l				//	its local state
		,	localIOSt	:: !*IOSt l				//	its context-free IOSt
		}

::	RuntimeState
	=	Running									// The process is running
	|	Blocked !SystemId						// The process is blocked for the process with given id
	|	Closed									// The process is closed
::	GUIShare
	:==	OSGUIShare

::	ActivateRequests	:== [OSActivateRequest]
::	DoubleDownDist		:== Int
::	InputTrack									// Input being tracked:
	=	{	itWindow	:: !OSWindowPtr			// the parent window
		,	itControl	:: !Int					// zero if parent window, otherwise item nr of control (>0)
		,	itKind		:: !InputTrackKind		// the input kinds being tracked
		}
::	InputTrackKind								// Input source kinds:
	=	{	itkMouse	:: !Bool				// mouse
		,	itkKeyboard	:: !Bool				// keyboard
		}
::	ClipboardState
	=	{	cbsCount	:: !Int					// ScrapCount of last access
		}


//	Access-rules on the IOSt:

emptyIOSt					:: !SystemId !(Maybe SystemId) !(Maybe GUIShare) !DocumentInterface !ProcessKind 
								![ProcessAttribute (PSt .l)] !(IdFun (PSt .l)) !(Maybe SystemId)
							-> IOSt .l

IOStButtonFreq				:: !Int !Point2 !OSWindowPtr	!(IOSt .l) -> (!Int,!IOSt .l)
IOStSetDoubleDownDist		:: !DoubleDownDist				!(IOSt .l) -> IOSt .l
IOStGetInputTrack			:: !(IOSt .l) -> (!Maybe InputTrack,			!IOSt .l)
IOStGetProcessAttributes	:: !(IOSt .l) -> (![ProcessAttribute (PSt .l)],	!IOSt .l)
IOStGetInitIO				:: !(IOSt .l) -> (!IdFun (PSt .l),				!IOSt .l)
IOStClosed					:: !(IOSt .l) -> (!Bool,						!IOSt .l)
IOStGetRuntimeState			:: !(IOSt .l) -> (!RuntimeState,				!IOSt .l)
IOStGetIOIsModal			:: !(IOSt .l) -> (!Maybe SystemId,				!IOSt .l)
IOStGetIdTable				:: !(IOSt .l) -> (!*IdTable,					!IOSt .l)
IOStGetReceiverTable		:: !(IOSt .l) -> (!*ReceiverTable,				!IOSt .l)
IOStGetTimerTable			:: !(IOSt .l) -> (!*TimerTable,					!IOSt .l)
IOStGetOSTime				:: !(IOSt .l) -> (!OSTime,						!IOSt .l)
IOStGetActivateRequests		:: !(IOSt .l) -> (!ActivateRequests,			!IOSt .l)
IOStGetEvents				:: !(IOSt .l) -> (!*OSEvents,					!IOSt .l)
IOStGetWorld				:: !(IOSt .l) -> (!*World,						!IOSt .l)
IOStGetCProcesses			:: !(IOSt .l) -> (!CProcesses,					!IOSt .l)
IOStGetProcessStack			:: !(IOSt .l) -> (!ProcessStack,				!IOSt .l)
IOStGetDocumentInterface	:: !(IOSt .l) -> (!DocumentInterface,			!IOSt .l)
IOStGetOSDInfo				:: !(IOSt .l) -> (!OSDInfo,						!IOSt .l)
IOStGetProcessKind			:: !(IOSt .l) -> (!ProcessKind,					!IOSt .l)
IOStGetIOId					:: !(IOSt .l) -> (!SystemId,					!IOSt .l)
IOStGetMaxIONr				:: !(IOSt .l) -> (!SystemId,					!IOSt .l)
IOStNewMaxIONr				:: !(IOSt .l) -> (!SystemId,					!IOSt .l)
IOStGetParentId				:: !(IOSt .l) -> (!Maybe SystemId,				!IOSt .l)
IOStGetGUIShare				:: !(IOSt .l) -> (!Maybe GUIShare,				!IOSt .l)
IOStGetSubProcessIds		:: !(IOSt .l) -> (![SystemId],					!IOSt .l)
IOStGetIdSeed				:: !(IOSt .l) -> (!Int,							!IOSt .l)
IOStGetClipboardState		:: !(IOSt .l) -> (!ClipboardState,				!IOSt .l)
IOStGetOSWindowMetrics		:: !(IOSt .l) -> (!OSWindowMetrics,				!IOSt .l)
IOStGetDeviceFunctions		:: !(IOSt .l) -> (![DeviceFunctions (PSt .l)],	!IOSt .l)
IOStGetRcvDisabled			:: !(IOSt .l) -> (!Bool,						!IOSt .l)	/* MW11++ */

IOStSetInputTrack			:: !(Maybe InputTrack)				!(IOSt .l) -> IOSt .l
IOStSetProcessAttributes	:: ![ProcessAttribute (PSt .l)]		!(IOSt .l) -> IOSt .l
IOStSetInitIO				:: !(IdFun (PSt .l))				!(IOSt .l) -> IOSt .l
IOStSetRuntimeState			:: !RuntimeState					!(IOSt .l) -> IOSt .l
IOStSetIOIsModal			:: !(Maybe SystemId)	 			!(IOSt .l) -> IOSt .l
IOStSetIdTable				:: !*IdTable						!(IOSt .l) -> IOSt .l
IOStSetReceiverTable		:: !*ReceiverTable					!(IOSt .l) -> IOSt .l
IOStSetTimerTable			:: !*TimerTable						!(IOSt .l) -> IOSt .l
IOStSetOSTime				:: !OSTime							!(IOSt .l) -> IOSt .l
IOStSetActivateRequests		:: !ActivateRequests				!(IOSt .l) -> IOSt .l
IOStSetEvents				:: !*OSEvents						!(IOSt .l) -> IOSt .l
IOStSetWorld				:: !*World							!(IOSt .l) -> IOSt .l
IOStSetCProcesses			:: !CProcesses						!(IOSt .l) -> IOSt .l
IOStSetProcessStack			:: !ProcessStack					!(IOSt .l) -> IOSt .l
SelectIOSt					::									!(IOSt .l) -> IOSt .l
IOStSetOSDInfo				:: !OSDInfo							!(IOSt .l) -> IOSt .l
IOStSetMaxIONr				:: !SystemId						!(IOSt .l) -> IOSt .l
IOStSetSubProcessIds		:: ![SystemId]						!(IOSt .l) -> IOSt .l
IOStSetIdSeed				:: !Int								!(IOSt .l) -> IOSt .l
IOStSetClipboardState		:: !ClipboardState					!(IOSt .l) -> IOSt .l
IOStSetDeviceFunctions		:: !(DeviceFunctions (PSt .l))		!(IOSt .l) -> IOSt .l
IOStRemoveDeviceFunctions	:: !Device							!(IOSt .l) -> IOSt .l
IOStSetRcvDisabled			:: !Bool							!(IOSt .l) -> IOSt .l /* MW11++*/

IOStSwapIO					:: !(![*World],!CProcesses)	!(IOSt .l) -> (!(![*World],!CProcesses),!IOSt .l)

IOStLastInteraction			::									!(IOSt .l) -> (!Bool,	!IOSt .l)
IOStHasDevice				:: !Device							!(IOSt .l) -> (!Bool,	!IOSt .l)
IOStGetDevices				::									!(IOSt .l) -> (![Device],!IOSt .l)

IOStGetDevice				:: !Device							!(IOSt .l) -> (!Bool,DeviceSystemState (PSt .l),!IOSt .l)
//	IOStGetDevice retrieves the indicated device and returns True if found. If not found the device is undefined!
IOStRemoveDevice			:: !Device							!(IOSt .l) -> IOSt .l
IOStSetDevice				:: !(DeviceSystemState (PSt .l))	!(IOSt .l) -> IOSt .l

getIOToolbox				::									!(IOSt .l) -> (!*OSToolbox, !IOSt .l)
setIOToolbox				:: !*OSToolbox						!(IOSt .l) -> IOSt .l
appIOToolbox				:: !.(IdFun *OSToolbox)				!(IOSt .l) -> IOSt .l
accIOToolbox				:: !.(St *OSToolbox .x)				!(IOSt .l) -> (!.x, !IOSt .l)
