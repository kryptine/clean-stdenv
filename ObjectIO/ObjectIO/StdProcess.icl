implementation module StdProcess


//	Clean object I/O library, version 1.2


import	StdFunc, StdList
import	StdProcessDef
import	devicefunctions, iostate, processdevice, scheduler
from	commondef			import StateMap2, Contains, Cond, RectSize, StrictSeq
from	processstack		import setProcessShowState
from	StdProcessAttribute	import isProcessOpenFiles, isProcessToolbar
import	osdocumentinterface
from	ostypes				import Rect
from	ossystem			import OSGetProcessWindowDimensions


//	General process topology creation functions:

class Processes pdef where
	startProcesses :: !pdef !*World   -> *World
	openProcesses  :: !pdef !(PSt .l) -> PSt .l

instance Processes [pdef]	| Processes pdef where
	startProcesses :: ![pdef] !*World -> *World | Processes pdef
	startProcesses pDefs world
		# (initContext, tb)	= initContext (StateMap2 openProcesses (reverse pDefs)) "" 0 NDI VirtualProcess world
		# (finalContext,tb)	= handleEvents initContext tb
		= closeContext finalContext tb
	
	openProcesses :: ![pdef] !(PSt .l) -> PSt .l | Processes pdef
	openProcesses pDefs pState
		= addVirtualProcess (StateMap2 openProcesses (reverse pDefs)) "" (0,0) pState

instance Processes Process  where
	startProcesses :: !Process !*World -> *World
	startProcesses pDef world
		# (initContext, tb)	= initContext (openProcesses pDef) "" 0 NDI VirtualProcess world
		# (finalContext,tb)	= handleEvents initContext tb
		= closeContext finalContext tb
	
	openProcesses :: !Process !(PSt .l) -> PSt .l
	openProcesses (Process NDI local init atts) pState
		= addInteractiveProcess atts (init o ProcessFunctions.dOpen) "" local NotShareGUI NDI pState
	openProcesses (Process SDI local init atts) pState
		= addInteractiveProcess atts (init o ProcessFunctions.dOpen) "" local NotShareGUI SDI pState		// PA: openSDIwindow added before init
	openProcesses (Process MDI local init atts) pState
		= addInteractiveProcess atts (init o ProcessFunctions.dOpen) "" local NotShareGUI MDI pState		// PA: openMDIwindow added before init


//	Specialised process creation functions:

startIO :: !DocumentInterface !.l !(ProcessInit (PSt .l)) ![ProcessAttribute (PSt .l)] !*World -> *World
startIO documentInterface local init atts world
	= startProcesses
		(Process documentInterface local init (if (documentInterface==MDI) [ProcessNoWindowMenu:atts] atts)) world


//	Close this interactive process.

closeProcess :: !(PSt .l) -> PSt .l
closeProcess pState = quitProcess pState


//	Hide or show this interactive process.

ShouldHide :== True
ShouldShow :== False

hideProcess :: !(PSt .l) -> PSt .l
hideProcess pState
	= hide_show ShouldHide pState

showProcess :: !(PSt .l) -> PSt .l
showProcess pState
	= hide_show ShouldShow pState

hide_show :: !Bool !(PSt .l) -> PSt .l
hide_show shouldHide pState=:{io}
	# (nr,ioState)				= IOStGetIOId io
	# (ioStack,ioState)			= IOStGetProcessStack ioState
	# ioStack					= setProcessShowState nr (not shouldHide) ioStack
	# ioState					= IOStSetProcessStack ioStack ioState
	# (deviceFunctions,ioState)	= IOStGetDeviceFunctions ioState
	  hideOrShow				= if shouldHide [df.dHide \\ df<-deviceFunctions] [df.dShow \\ df<-deviceFunctions]
	= StrictSeq hideOrShow {pState & io=ioState}


/* RWS ..
//	Get the current position of the ProcessWindow (on Macintosh: zero)

getProcessWindowPos :: !(IOSt .l) -> (!Point2,!IOSt .l)
getProcessWindowPos ioState
	= (zero,ioState)


//	Get the current size of the ProcessWindow (on Macintosh: ScreenSize)

getProcessWindowSize :: !(IOSt .l) -> (!Size,!IOSt .l)
getProcessWindowSize ioState
	# (tb,ioState)		= getIOToolbox ioState
	# (_,_,sR,sB,tb)	= QScreenRect tb
	# ioState			= setIOToolbox tb ioState
	= ({w=sR,h=sB-TitleBarWidth},ioState)
*/
//	Get the current position of the ProcessWindow (on Macintosh: zero)

getProcessWindowPos :: !(IOSt .l) -> (!Point2,!IOSt .l)
getProcessWindowPos ioState
	# (tb,ioState)		= getIOToolbox ioState
	# (osdinfo,ioState)	= IOStGetOSDInfo ioState					// PA+++
	# (rect,tb)			= OSGetProcessWindowDimensions osdinfo tb	// PA: OSDInfo argument added
	# ioState			= setIOToolbox tb ioState
	= ({x=rect.rleft,y=rect.rtop},ioState)

//	Get the current size of the ProcessWindow (on Macintosh: ScreenSize)

getProcessWindowSize :: !(IOSt .l) -> (!Size,!IOSt .l)
getProcessWindowSize ioState
	# (tb,ioState)		= getIOToolbox ioState
	# (osdinfo,ioState)	= IOStGetOSDInfo ioState					// PA+++
	# (rect,tb)			= OSGetProcessWindowDimensions osdinfo tb	// PA: OSDInfo argument added
	# ioState			= setIOToolbox tb ioState
	= (RectSize rect,ioState)
/* ... RWS */
