implementation module StdPSt


//	Clean Object I/O library, version 1.2


import	StdBool, StdFile, StdFileSelect, StdTuple
import	StdSound, StdTime
import	deviceevents
from	commondef			import StrictSeq, StrictSeqList, Cond
from	iostate				import PSt, IOSt, appIOToolbox, accIOToolbox, IOStGetWorld, IOStSetWorld, IOStGetDocumentInterface, 
									IOStGetProcessAttributes, IOStSetProcessAttributes
from	StdProcessAttribute	import isProcessActivate, isProcessDeactivate
from	scheduler			import handleOneEventForDevices
import	osbeep, osfileselect
from	clCCall_12			import WinPlaySound
from	ospicture			import peekScreen
from	ostoolbox			import OSNewToolbox, WorldGetToolbox, WorldSetToolbox


/*	PSt is an environment instance of the class FileEnv (see StdFile).
*/
instance FileEnv (PSt .l .p) where
	accFiles :: !.(*Files -> (.x,*Files)) !*(PSt .l .p) -> (!.x,!*PSt .l .p)
	accFiles accfun pState=:{io}
		# (world,io)		= IOStGetWorld io
		  (x,world)			= accFiles accfun world
		  pState			= {pState & io=IOStSetWorld world io}
		= (x,pState)
	
	appFiles :: !.(*Files -> *Files) !*(PSt .l .p) -> *PSt .l .p
	appFiles appfun pState=:{io}
		# (world,io)		= IOStGetWorld io
		  world				= appFiles appfun world
		  pState			= {pState & io=IOStSetWorld world io}
		= pState


/*	PSt is an environment instance of the class FileSelectEnv (see StdFileSelect).
*/
instance FileSelectEnv (PSt .l .p) where
	selectInputFile :: !(PSt .l .p) -> (!Maybe String,!PSt .l .p)
	selectInputFile pState
		# (ok,name,pState,_)	= OSselectinputfile handleOSEvent pState OSNewToolbox
		= (if ok (Just name) Nothing,pState)
	
	selectOutputFile:: !String !String !(PSt .l .p) -> (!Maybe String,!PSt .l .p)
	selectOutputFile prompt originalName pState
		# (ok,name,pState,_)	= OSselectoutputfile handleOSEvent pState prompt originalName OSNewToolbox
		= (if ok (Just name) Nothing,pState)
	
	selectDirectory :: !(PSt .l .p) -> (!Maybe String,!PSt .l .p)
	selectDirectory pState
		# (ok,name,pState,_)	= OSselectdirectory handleOSEvent pState OSNewToolbox
		= (if ok (Just name) Nothing,pState)


//	handleOSEvent turns handleOneEventForDevices into the form required by OSselect(in/out)putfile.
handleOSEvent :: !OSEvent !(PSt .l .p) -> PSt .l .p
handleOSEvent osEvent pState
	= thd3 (handleOneEventForDevices (ScheduleOSEvent osEvent []) pState)


/*	PSt is an environment instance of the class TimeEnv (see StdTime).
*/
instance TimeEnv (PSt .l .p) where
	getBlinkInterval :: !(PSt .l .p) -> (!Int,!PSt .l .p)
	getBlinkInterval pState=:{io}
		# (world,io)		= IOStGetWorld io
		# (blink,world)		= getBlinkInterval world
		# pState			= {pState & io=IOStSetWorld world io}
		= (blink,pState)
	
	getCurrentTime :: !(PSt .l .p) -> (!Time,!PSt .l .p)
	getCurrentTime pState=:{io}
		# (world,io)		= IOStGetWorld io
		# (time,world)		= getCurrentTime world
		# pState			= {pState & io=IOStSetWorld world io}
		= (time,pState)
	
	getCurrentDate :: !(PSt .l .p) -> (!Date,!PSt .l .p)
	getCurrentDate pState=:{io}
		# (world,io)		= IOStGetWorld io
		# (date,world)		= getCurrentDate world
		# pState			= {pState & io=IOStSetWorld world io}
		= (date,pState)


/*	accScreenPicture provides access to an initial Picture as it would be created in
	a window or control.
*/
class accScreenPicture env :: !.(St *Picture .x) !*env -> (!.x,!*env)

instance accScreenPicture World where
	accScreenPicture :: !.(St *Picture .x) !*World -> (!.x,!*World)
	accScreenPicture fun world
		# (tb,world)	= WorldGetToolbox world
		# (x,tb)		= peekScreen fun tb
		# world			= WorldSetToolbox tb world
		= (x,world)
instance accScreenPicture (IOSt .l .p) where
	accScreenPicture :: !.(St *Picture .x) !(IOSt .l .p) -> (!.x,!IOSt .l .p)
	accScreenPicture fun ioState
		= accIOToolbox (peekScreen fun) ioState


/*	Emit the alert sound.
*/
beep :: !(IOSt .l .p) -> IOSt .l .p
beep ioState = appIOToolbox OSBeep ioState


instance playSoundFile (PSt .l .p) where
	playSoundFile :: !String !(PSt .l .p) -> (!Bool,!PSt .l .p)
	playSoundFile soundFileName pState=:{io}
		# (ok,io)	= accIOToolbox (WinPlaySound soundFileName) io
		= (ok,{pState & io=io})


/* RWS ---
/*	Set the shape of the cursor globally. This shape overrules the local cursor shapes of windows.
*/
setCursor :: !CursorShape !(IOSt .l .p) -> IOSt .l .p
setCursor shape ioState
#	(cInfo,ioState)	= IOStGetDialogCursorInfo ioState
	(cInfo,ioState)	= accIOToolbox (cursorinfoSetGlobalCursor shape cInfo) ioState
	ioState			= IOStSetDialogCursorInfo cInfo ioState
=	ioState


/*	resetCursor undoes the effect of SetCursor.
*/
resetCursor :: !(IOSt .l .p) -> IOSt .l .p
resetCursor ioState
#	(cInfo,ioState)	= IOStGetDialogCursorInfo ioState
	(cInfo,ioState)	= accIOToolbox (cursorinfoResetGlobalCursor cInfo) ioState
	ioState			= IOStSetDialogCursorInfo cInfo ioState
=	ioState


/*	obscureCursor hides the cursor until the mouse is moved.
*/
obscureCursor :: !(IOSt .l .p) -> IOSt .l .p
obscureCursor ioState = appIOToolbox QObscureCursor ioState


/*	setDoubleDownDistance sets the double down distance of the mouse. Negative values are set to zero.
*/
setDoubleDownDistance :: !Int !(IOSt .l .p) -> IOSt .l .p
setDoubleDownDistance newDDDist ioState = IOStSetDoubleDownDist newDDDist ioState

--- RWS */

/*	getDocumentInterface retrieves the DocumentInterface of an interactive process.
*/
getDocumentInterface :: !(IOSt .l .p) -> (!DocumentInterface, !IOSt .l .p)
getDocumentInterface ioState = IOStGetDocumentInterface ioState


/*	Operations on the attributes of an interactive process:
*/
setProcessActivate :: !(IdFun (PSt .l .p)) !(IOSt .l .p) -> IOSt .l .p
setProcessActivate activateF ioState
	# (pAtts,ioState)	= IOStGetProcessAttributes ioState
	= IOStSetProcessAttributes (setProcessAttribute isProcessActivate (ProcessActivate activateF) pAtts) ioState

setProcessDeactivate :: !(IdFun (PSt .l .p)) !(IOSt .l .p) -> IOSt .l .p
setProcessDeactivate deactivateF ioState
	# (pAtts,ioState)	= IOStGetProcessAttributes ioState
	= IOStSetProcessAttributes (setProcessAttribute isProcessDeactivate (ProcessDeactivate deactivateF) pAtts) ioState

setProcessAttribute :: !(Cond (ProcessAttribute .ps)) !(ProcessAttribute .ps) ![ProcessAttribute .ps] -> [ProcessAttribute .ps]
setProcessAttribute cond pAtt` [pAtt:pAtts]
	| cond pAtt	= [pAtt`:pAtts]
	| otherwise	= [pAtt :setProcessAttribute cond pAtt` pAtts]
setProcessAttribute _ pAtt` _
	= [pAtt`]


//	Coercing PSt component operations to PSt operations.

appListPIO :: ![.IdFun (IOSt .l .p)] !(PSt .l .p) -> PSt .l .p
appListPIO fs pState=:{io} = {pState & io=StrictSeq fs io}

appListPLoc :: ![.IdFun .l] !(PSt .l .p) -> PSt .l .p
appListPLoc fs pState=:{ls} = {pState & ls=StrictSeq fs ls}

appListPPub :: ![.IdFun .p] !(PSt .l .p) -> PSt .l .p
appListPPub fs pState=:{ps} = {pState & ps=StrictSeq fs ps}

appPIO :: !.(IdFun (IOSt .l .p)) !(PSt .l .p) -> PSt .l .p
appPIO f pState=:{io} = {pState & io=f io}

appPLoc :: !.(IdFun .l) !(PSt .l .p) -> PSt .l .p
appPLoc f pState=:{ls} = {pState & ls=f ls}

appPPub :: !.(IdFun .p) !(PSt .l .p) -> PSt .l .p
appPPub f pState=:{ps} = {pState & ps=f ps}


//	Accessing PSt component operations.

accListPIO :: ![.St (IOSt .l .p) .x] !(PSt .l .p) -> (![.x],!PSt .l .p)
accListPIO fs pState=:{io}
	# (xs,io) = StrictSeqList fs io
	= (xs,{pState & io=io})

accListPLoc :: ![.St .l .x] !(PSt .l .p) -> (![.x],!PSt .l .p)
accListPLoc fs pState=:{ls}
	# (xs,ls) = StrictSeqList fs ls
	= (xs,{pState & ls=ls})

accListPPub :: ![.St .p .x] !(PSt .l .p) -> (![.x],!PSt .l .p)
accListPPub fs pState=:{ps}
	# (xs,ps) = StrictSeqList fs ps
	= (xs,{pState & ps=ps})

accPIO :: !.(St (IOSt .l .p) .x) !(PSt .l .p) -> (!.x,!PSt .l .p)
accPIO f pState=:{io}
	# (x,io) = f io
	= (x,{pState & io=io})

accPLoc :: !.(St .l .x) !(PSt .l .p) -> (!.x,!PSt .l .p)
accPLoc f pState=:{ls}
	# (x,ls) = f ls
	= (x,{pState & ls=ls})

accPPub :: !.(St .p .x) !(PSt .l .p) -> (!.x,!PSt .l .p)
accPPub f pState=:{ps}
	# (x,ps) = f ps
	= (x,{pState & ps=ps})
