definition module osevent

//	Clean Object I/O library, version 1.2

from	StdInt			import ^,-
from	clCrossCall_12	import CrossCallInfo
from	ostoolbox		import OSToolbox
from	ostypes			import OSWindowPtr


::	*OSEvents

OSnewEvents		:: OSEvents
OSappendEvents	:: !*[OSEvent] !OSEvents -> OSEvents		// OSappendEvents adds events at the end of the queue
OSinsertEvents	:: !*[OSEvent] !OSEvents -> OSEvents		// OSinsertEvents adds events at the front of the queue


::	OSEvent
	:==	CrossCallInfo
::	OSSleepTime			// The max time the process allows multi-tasking
	:==	Int
OSNullEvent :: OSEvent	// OSNullEvent returns a valid non-informative event

// OSLongSleep :: OSSleepTime
OSLongSleep	:== 2^15-1
// OSNoSleep :: OSSleepTime
OSNoSleep	:== 0

//OShandleEvents		:: !(.s -> (Bool,.s)) !(.s -> (Int,.s)) !(OSEvent -> .s -> ([Int],.s)) !(!.s,!*OSToolbox) -> (!.s,!*OSToolbox)
OShandleEvents			:: !(.s -> (Bool,.s)) !(.s -> (OSEvents,.s)) !((OSEvents,.s) -> .s) !(.s -> (Int,.s)) !(OSEvent -> .s -> ([Int],.s)) !(!.s,!*OSToolbox) -> (!.s,!*OSToolbox)
OSEventIsUrgent			:: !OSEvent -> Bool
setReplyInOSEvent		:: ![Int] -> CrossCallInfo

/* createOS(Dea/A)ctivateWindowEvent creates the event the platform would generate for a genuine (de)activate event. */
createOSActivateWindowEvent		:: !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSDeactivateWindowEvent	:: !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)

/* createOS(Dea/A)ctivateControlEvent creates the event the platform would generate for a genuine (de)activate event. */
createOSActivateControlEvent	:: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSDeactivateControlEvent	:: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)

/* createOSLoose(Mouse/Key)Event creates the event for reporting loss of mouse/keyboard input (virtual event). */
createOSLooseMouseEvent	:: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSLooseKeyEvent	:: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
