definition module osevent

//	Clean Object I/O library, version 1.2

from	StdInt			import ^,-
from	clCrossCall_12	import CrossCallInfo
from	ostoolbox		import OSToolbox
from	ostime			import OSTime				// PA: new
from	ostypes			import OSWindowPtr
from	StdMaybe		import Maybe, Just, Nothing


::	*OSEvents

OSnewEvents		:: OSEvents
OScopyEvents	:: !OSEvents -> (!OSEvents,!OSEvents)
OSappendEvents	:: !*[OSEvent] !OSEvents -> OSEvents		// OSappendEvents adds events at the end of the queue
OSinsertEvents	:: !*[OSEvent] !OSEvents -> OSEvents		// OSinsertEvents adds events at the front of the queue
OSisEmptyEvents	:: !OSEvents -> (!Bool,!OSEvents)
OSremoveEvent	:: !OSEvents -> (!OSEvent,!OSEvents)


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

/*	createOS(Dea/A)ctivateWindowEvent creates the event the platform would generate for a genuine (de)activate event. */
createOSActivateWindowEvent		:: !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSDeactivateWindowEvent	:: !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)

/*	createOS(Dea/A)ctivateControlEvent creates the event the platform would generate for a genuine (de)activate event. */
createOSActivateControlEvent	:: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSDeactivateControlEvent	:: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)

/*	createOSLoose(Mouse/Key)Event creates the event for reporting loss of mouse/keyboard input (virtual event). */
createOSLooseMouseEvent	:: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSLooseKeyEvent	:: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)

/*	createOSZeroTimerEvent  creates the event for reporting continued zero timer (virtual event).
	getOSZeroTimerStartTime returns the registered time in the virtual event. Nothing is returned if wrong argument.
*/
createOSZeroTimerEvent	:: !OSTime -> OSEvent		// PA: new
getOSZeroTimerStartTime	:: !OSEvent -> Maybe OSTime	// PA: new
