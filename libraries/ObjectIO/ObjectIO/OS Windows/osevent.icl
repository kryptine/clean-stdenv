implementation module osevent

//	Clean Object I/O library, version 1.2

import	StdBool, StdList, StdMisc, StdTuple
import	clCrossCall_12, ostoolbox, ostypes
from	commondef	import HdTl, FatalError
//import	StdDebug, tracetypes


oseventFatalError :: String String -> .x
oseventFatalError function error
	= FatalError function "osevent" error


/*	The OSEvents environment keeps track of delayed events. 
*/
::	*OSEvents
	:== [OSEvent]


OSappendEvents :: !*[OSEvent] !OSEvents -> OSEvents
OSappendEvents newEvents osEvents
	= osEvents ++ newEvents

OSinsertEvents :: !*[OSEvent] !OSEvents -> OSEvents
OSinsertEvents newEvents osEvents
	= newEvents ++ osEvents

OSisEmptyEvents :: !OSEvents -> (!Bool,!OSEvents)
OSisEmptyEvents []
	= (True,  [])
OSisEmptyEvents osEvents
	= (False, osEvents)

OSremoveEvent :: !OSEvents -> (!OSEvent,!OSEvents)
OSremoveEvent [osEvent:osEvents]
	= (osEvent,osEvents)
OSremoveEvent []
	= oseventFatalError "OSremoveEvent" "OSEvents argument is empty"

OSnewEvents :: OSEvents
OSnewEvents = []


::	OSEvent
	:==	CrossCallInfo
::	OSSleepTime		// The max time the process allows multi-tasking
	:== Int

OSNullEvent :: OSEvent
OSNullEvent
	=	{	ccMsg	= CcWmIDLETIMER
		,	p1		= 0
		,	p2		= 0
		,	p3		= 0
		,	p4		= 0
		,	p5		= 0
		,	p6		= 0
		}

// OSLongSleep :: OSSleepTime
OSLongSleep	:== 2^15-1
// OSNoSleep :: OSSleepTime
OSNoSleep	:== 0

OShandleEvents :: !(.s -> (Bool,.s)) !(.s -> (OSEvents,.s)) !((OSEvents,.s) -> .s) !(.s -> (Int,.s)) !(OSEvent -> .s -> ([Int],.s)) !(!.s,!*OSToolbox) -> (!.s,!*OSToolbox)

OShandleEvents isFinalState getOSEvents setOSEvents getSleepTime handleOSEvent (state,tb)
	# (terminate,state)			= isFinalState state
	| terminate
		= (state,tb)
	# (osEvents,state)			= getOSEvents state
	# (noDelayEvents,osEvents)	= OSisEmptyEvents osEvents
	| noDelayEvents
		# state					= setOSEvents (osEvents,state)
		# (sleep,state)			= getSleepTime state
		  getEventCci			= {ccMsg=CcRqDOMESSAGE,p1=toInt (sleep<>OSLongSleep),p2=sleep,p3=0,p4=0,p5=0,p6=0}
		# (_,state,tb)			= IssueCleanRequest (rccitoevent handleOSEvent) getEventCci state tb
		= OShandleEvents isFinalState getOSEvents setOSEvents getSleepTime handleOSEvent (state,tb)
		with
			rccitoevent :: !(OSEvent -> .s -> ([Int],.s)) !OSEvent !.s !*OSToolbox -> (!OSEvent,!.s,!*OSToolbox)
			rccitoevent handleOSEvent osEvent=:{ccMsg} state tb
			//	# (reply,state)	= handleOSEvent (trace_n ("CcRqDOMESSAGE-->"+++toString osEvent) osEvent) state
				# (reply,state)	= handleOSEvent osEvent state
				= (setReplyInOSEvent reply,state,tb)
	| otherwise
		# (osEvent,osEvents)	= OSremoveEvent osEvents
		# state					= setOSEvents (osEvents,state)
	//	# (_,state)				= handleOSEvent (trace_n ("DelayedEvent-->"+++toString osEvent) osEvent) state
		# (_,state)				= handleOSEvent osEvent state
		= OShandleEvents isFinalState getOSEvents setOSEvents getSleepTime handleOSEvent (state,tb)

setReplyInOSEvent :: ![Int] -> CrossCallInfo
setReplyInOSEvent reply
	| isEmpty reply	= Return0Cci
	# (e1,reply)	= HdTl reply
	| isEmpty reply	= Return1Cci e1
	# (e2,reply)	= HdTl reply
	| isEmpty reply	= Return2Cci e1 e2
	# (e3,reply)	= HdTl reply
	| isEmpty reply	= Return3Cci e1 e2 e3
	# (e4,reply)	= HdTl reply
	| isEmpty reply	= Return4Cci e1 e2 e3 e4
	# (e5,reply)	= HdTl reply
	| isEmpty reply	= Return5Cci e1 e2 e3 e4 e5
	# (e6,_)		= HdTl reply
	| isEmpty reply	= Return6Cci e1 e2 e3 e4 e5 e6
	| otherwise		= oseventFatalError "setReplyInOSEvent" "number of reply codes > 6"

OSEventIsUrgent :: !OSEvent -> Bool
OSEventIsUrgent {ccMsg}
	= case ccMsg of
		CcWmDRAWCLIPBOARD	-> False	// PA: in a future version, use this event to evaluate a clipboard callback function.
		CcWmIDLETIMER		-> False
		CcWmTIMER			-> False
		_					-> True


/* createOS(Dea/A)ctivateWindowEvent creates the event the platform would generate for a genuine (de)activate event. */
createOSActivateWindowEvent :: !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSActivateWindowEvent wPtr tb = (Rq1Cci CcWmACTIVATE wPtr,tb)

createOSDeactivateWindowEvent :: !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSDeactivateWindowEvent wPtr tb = (Rq1Cci CcWmDEACTIVATE wPtr,tb)

/* createOS(Dea/A)ctivateControlEvent creates the event the platform would generate for a genuine (de)activate event. */
createOSActivateControlEvent :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSActivateControlEvent wPtr cPtr tb = (Rq2Cci CcWmSETFOCUS wPtr cPtr,tb)

createOSDeactivateControlEvent :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSDeactivateControlEvent wPtr cPtr tb = (Rq2Cci CcWmKILLFOCUS wPtr cPtr,tb)

/* createOSLoose(Mouse/Key)Event creates the event for reporting loss of mouse/keyboard input (virtual event). */
createOSLooseMouseEvent :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSLooseMouseEvent wPtr cPtr tb = (Rq2Cci CcWmLOSTMOUSE wPtr cPtr,tb)

createOSLooseKeyEvent :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSLooseKeyEvent wPtr cPtr tb = (Rq2Cci CcWmLOSTKEY wPtr cPtr,tb)
