implementation module osfileselect


//	Clean Object I/O library, version 1.2


import	StdBool, StdInt
import	clCrossCall_12, osevent
from	clCCall_12	import WinMakeCString, WinGetCStringAndFree, WinReleaseCString, CSTR
from	commondef	import FatalError
import code from "cCrossCallFileSelectors_121.obj"


osfileselectFatalError :: String String -> .x
osfileselectFatalError function error
	= FatalError function "osfileselect" error


OSinitialiseFileSelectors :: !*OSToolbox -> *OSToolbox
OSinitialiseFileSelectors _
	= code
	{
		.inline InstallCrossCallFileSelectors
			ccall InstallCrossCallFileSelectors "I-I"
		.end
	}

OSselectinputfile :: !(OSEvent->.s->.s) !.s !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
OSselectinputfile handleOSEvent state tb
	# (rcci,state,tb)	= IssueCleanRequest (callback handleOSEvent) (Rq0Cci CcRqFILEOPENDIALOG) state tb
	# (ok,name,tb)		= getinputfilename rcci tb
	= (ok,name,state,tb)
where
	getinputfilename :: !CrossCallInfo !*OSToolbox -> (!Bool,!String,!*OSToolbox)
	getinputfilename {ccMsg=CcRETURN2,p1=ok,p2=ptr} tb
		| ok==0
			= (False,"",tb)
		| otherwise
			# (pathname,tb)	= WinGetCStringAndFree ptr tb
			= (True,pathname,tb)
	getinputfilename {ccMsg=CcWASQUIT} tb
		= (False,"",tb)
	getinputfilename {ccMsg} _
		= osfileselectFatalError "OSselectinputfile" ("unexpected ccMsg field of return CrossCallInfo ("+++toString ccMsg+++")")

OSselectoutputfile :: !(OSEvent->.s->.s) !.s !String !String !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
OSselectoutputfile handleOSEvent state prompt filename tb
	# (promptptr,  tb)	= WinMakeCString prompt   tb
	# (filenameptr,tb)	= WinMakeCString filename tb
	# (rcci,state, tb)	= IssueCleanRequest (callback handleOSEvent) (Rq2Cci CcRqFILESAVEDIALOG promptptr filenameptr) state tb
	# tb				= WinReleaseCString promptptr   tb
	# tb				= WinReleaseCString filenameptr tb
	# (ok,name,tb)		= getoutputfilename rcci tb
	= (ok,name,state,tb)
where
	getoutputfilename :: !CrossCallInfo !*OSToolbox -> (!Bool,!String,!*OSToolbox)
	getoutputfilename {ccMsg=CcRETURN2,p1=ok,p2=ptr} tb
		| ok==0
			= (False,"",tb)
		| otherwise
			# (path,tb) = WinGetCStringAndFree ptr tb
			= (True,path,tb)
	getoutputfilename {ccMsg=CcWASQUIT} tb
		= (False,"",tb)
	getoutputfilename {ccMsg} _
		= osfileselectFatalError "OSselectoutputfile" ("unexpected ccMsg field of return CrossCallInfo ("+++toString ccMsg+++")")

OSselectdirectory :: !(OSEvent->.s->.s) !.s !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
OSselectdirectory handleOSEvent state tb
	# (rcci,state,tb)	= IssueCleanRequest (callback handleOSEvent) (Rq0Cci CcRqDIRECTORYDIALOG) state tb
	# (ok,name,tb)		= getinputfilename rcci tb
	= (ok,name,state,tb)
where
	getinputfilename :: !CrossCallInfo !*OSToolbox -> (!Bool,!String,!*OSToolbox)
	getinputfilename {ccMsg=CcRETURN2,p1=ok,p2=ptr} tb
		| ok==0
			= (False,"",tb)
		| otherwise
			# (pathname,tb)	= WinGetCStringAndFree ptr tb
			= (True,pathname,tb)
	getinputfilename {ccMsg=CcWASQUIT} tb
		= (False,"",tb)
	getinputfilename {ccMsg} _
		= osfileselectFatalError "OSselectdirectory" ("unexpected ccMsg field of return CrossCallInfo ("+++toString ccMsg+++")")

//	callback lifts a function::(OSEvent -> .s -> .s) to
//        a crosscallfunction::(CrossCallInfo -> .s -> *OSToolbox -> (CrossCallInfo,.s,*OSToolbox))
callback :: !(OSEvent->.s->.s) !CrossCallInfo !.s !*OSToolbox -> (!CrossCallInfo,!.s,!*OSToolbox)
callback handleOSEvent cci state tb = (Return0Cci,handleOSEvent cci state,tb)
