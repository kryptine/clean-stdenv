implementation module StdProcess


//	Clean object I/O library, version 1.2


import	StdFunc, StdList
import	StdProcessDef
import	devicefunctions, iostate, processdevice, scheduler
from	commondef			import StateMap2, Contains, Cond, RectSize, StrictSeq
from	documentinterface	import setOSDInfoInMenuDevice
from	processstack		import setProcessShowState
from	StdProcessAttribute	import isProcessOpenFiles, isProcessToolbar
import	osdocumentinterface
from	toolbar				import openToolbar
from	ostypes				import Rect
from	oswindow			import OSGetProcessWindowDimensions


//	General process topology creation functions:

class Processes pdef where
	startProcesses :: !pdef !*World      -> *World
	openProcesses  :: !pdef !(PSt .l .p) -> PSt .l .p

instance Processes (ProcessGroup pdef)	| shareProcesses pdef where
	startProcesses :: !(ProcessGroup pdef) !*World -> *World	| shareProcesses pdef
	startProcesses (ProcessGroup public pDef) world
		# (initContext, tb)	= initContext (shareProcesses pDef) "" (0,public) NDI VirtualProcess world
		# (finalContext,tb)	= handleEvents initContext tb
		= closeContext finalContext tb
	
	openProcesses :: !(ProcessGroup pdef) !(PSt .l .p) -> PSt .l .p	| shareProcesses pdef
	openProcesses (ProcessGroup public pDef) pState
		= addVirtualProcess (shareProcesses pDef) "" (0,public) pState

instance Processes [pdef]	| Processes pdef where
	startProcesses :: ![pdef] !*World -> *World	| Processes pdef
	startProcesses pDefs world
		# (initContext, tb)	= initContext (StateMap2 openProcesses (reverse pDefs)) "" (0,0) NDI VirtualProcess world
		# (finalContext,tb)	= handleEvents initContext tb
		= closeContext finalContext tb
	
	openProcesses :: ![pdef] !(PSt .l .p) -> PSt .l .p	| Processes pdef
	openProcesses pDefs pState
		= addVirtualProcess (StateMap2 openProcesses (reverse pDefs)) "" (0,0) pState

instance Processes (:^: pdef1 pdef2)	| Processes pdef1 & Processes pdef2 where
	startProcesses :: !(:^: pdef1 pdef2) !*World -> *World	| Processes pdef1 & Processes pdef2
	startProcesses (pdef1:^:pdef2) world
		# (initContext, tb)	= initContext (openProcesses pdef2 o openProcesses pdef1) "" (0,0) NDI VirtualProcess world
		# (finalContext,tb)	= handleEvents initContext tb
		= closeContext finalContext tb
	
	openProcesses :: !(:^: pdef1 pdef2) !(PSt .l .p) -> PSt .l .p	| Processes pdef1 & Processes pdef2
	openProcesses (pdef1:^:pdef2) pState
		= addVirtualProcess (openProcesses pdef2 o openProcesses pdef1) "" (0,0) pState


class shareProcesses pdef :: !(pdef .p) !(PSt .l .p) -> PSt .l .p

instance shareProcesses Process where
	shareProcesses :: !(Process .p) !(PSt .l .p) -> PSt .l .p
	shareProcesses (Process NDI local init atts) pState
		= addInteractiveProcess atts (init o ProcessFunctions.dOpen) "" local NotShareGUI NDI pState
	shareProcesses (Process SDI local init atts) pState
		= addInteractiveProcess atts (init o openSDIwindow o ProcessFunctions.dOpen) "" local NotShareGUI SDI pState		// PA: openSDIwindow added before init
	where
		openSDIwindow :: !(PSt .l .p) -> PSt .l .p
		openSDIwindow pState=:{io}
			# (atts,ioState)	= IOStGetProcessAttributes io
			  acceptOpenFiles	= Contains isProcessOpenFiles atts
			# (osSDInfo,ioState)= accIOToolbox (OSopenSDI acceptOpenFiles) ioState
			# osdinfo			= OSSDInfo osSDInfo
			# ioState			= IOStSetOSDInfo osdinfo ioState
			# ioState			= openToolbar ioState
			# pState			= {pState & io=ioState}
			# pState			= setOSDInfoInMenuDevice osdinfo pState
			= pState
	shareProcesses (Process MDI local init atts) pState
		= addInteractiveProcess atts (init o openMDIwindow o ProcessFunctions.dOpen) "" local NotShareGUI MDI pState		// PA: openMDIwindow added before init
	where
		openMDIwindow :: !(PSt .l .p) -> PSt .l .p
		openMDIwindow pState=:{io}
			# (atts,ioState)	= IOStGetProcessAttributes io
			  acceptOpenFiles	= Contains isProcessOpenFiles atts
			  hasToolbarAtt		= Contains isProcessToolbar atts
			# (tb,ioState)		= getIOToolbox ioState
			# (osMDInfo,tb)		= OSopenMDI (not hasToolbarAtt) acceptOpenFiles tb
			# ioState			= setIOToolbox tb ioState
			  osdinfo			= OSMDInfo osMDInfo
			# ioState			= IOStSetOSDInfo osdinfo ioState
			# ioState			= openToolbar ioState
			# pState			= {pState & io=ioState}
			# pState			= setOSDInfoInMenuDevice osdinfo pState
			= pState

instance shareProcesses	(ListCS pdef)	| shareProcesses pdef where
	shareProcesses :: !(ListCS pdef .p) !(PSt .l .p) -> PSt .l .p	| shareProcesses pdef
	shareProcesses (ListCS pdefs) pState = seq (map shareProcesses pdefs) pState

instance shareProcesses	(:~: pdef1 pdef2)	| shareProcesses pdef1 & shareProcesses pdef2 where
	shareProcesses :: !(:~: pdef1 pdef2 .p) !(PSt .l .p) -> PSt .l .p	| shareProcesses pdef1 & shareProcesses pdef2
	shareProcesses (pDef1:~:pDef2) pState = shareProcesses pDef1 (shareProcesses pDef2 pState)


//	Specialised process creation functions:

startIO :: !DocumentInterface !.l !.p !(ProcessInit (PSt .l .p)) ![ProcessAttribute (PSt .l .p)] !*World -> *World
startIO documentInterface local public init atts world
	= startProcesses
		(ProcessGroup public
			(Process documentInterface local init (if (documentInterface==MDI) [ProcessNoWindowMenu:atts] atts)
			)
		) world


//	Close this interactive process.

closeProcess :: !(PSt .l .p) -> PSt .l .p
closeProcess pState = quitProcess pState


//	Hide or show this interactive process.

ShouldHide :== True
ShouldShow :== False

hideProcess :: !(PSt .l .p) -> PSt .l .p
hideProcess pState
	= hide_show ShouldHide pState

showProcess :: !(PSt .l .p) -> PSt .l .p
showProcess pState
	= hide_show ShouldShow pState

hide_show :: !Bool !(PSt .l .p) -> PSt .l .p
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

getProcessWindowPos :: !(IOSt .l .p) -> (!Point2,!IOSt .l .p)
getProcessWindowPos ioState
	= (zero,ioState)


//	Get the current size of the ProcessWindow (on Macintosh: ScreenSize)

getProcessWindowSize :: !(IOSt .l .p) -> (!Size,!IOSt .l .p)
getProcessWindowSize ioState
	# (tb,ioState)		= getIOToolbox ioState
	# (_,_,sR,sB,tb)	= QScreenRect tb
	# ioState			= setIOToolbox tb ioState
	= ({w=sR,h=sB-TitleBarWidth},ioState)
*/
//	Get the current position of the ProcessWindow (on Macintosh: zero)

getProcessWindowPos :: !(IOSt .l .p) -> (!Point2,!IOSt .l .p)
getProcessWindowPos ioState
	# (tb,ioState)		= getIOToolbox ioState
	# (osdinfo,ioState)	= IOStGetOSDInfo ioState					// PA+++
	# (rect,tb)			= OSGetProcessWindowDimensions osdinfo tb	// PA: OSDInfo argument added
	# ioState			= setIOToolbox tb ioState
	= ({x=rect.rleft,y=rect.rtop},ioState)

//	Get the current size of the ProcessWindow (on Macintosh: ScreenSize)

getProcessWindowSize :: !(IOSt .l .p) -> (!Size,!IOSt .l .p)
getProcessWindowSize ioState
	# (tb,ioState)		= getIOToolbox ioState
	# (osdinfo,ioState)	= IOStGetOSDInfo ioState					// PA+++
	# (rect,tb)			= OSGetProcessWindowDimensions osdinfo tb	// PA: OSDInfo argument added
	# ioState			= setIOToolbox tb ioState
	= (RectSize rect,ioState)
/* ... RWS */
