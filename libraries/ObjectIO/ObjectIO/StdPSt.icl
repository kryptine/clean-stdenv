implementation module StdPSt


//	Clean Object I/O library, version 1.2.1


import	StdBool, StdFunc
import	commondef, iostate
from	StdIOCommon			import IdFun
from	StdProcessAttribute	import isProcessActivate, isProcessDeactivate
from	osbeep				import OSBeep
from	ospicture			import peekScreen
from	ostoolbox			import OSToolbox, WorldGetToolbox, WorldSetToolbox



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
instance accScreenPicture (IOSt .l) where
	accScreenPicture :: !.(St *Picture .x) !(IOSt .l) -> (!.x,!IOSt .l)
	accScreenPicture fun ioState
		= accIOToolbox (peekScreen fun) ioState


/*	Emit the alert sound.
*/
beep :: !(IOSt .l) -> IOSt .l
beep ioState = appIOToolbox OSBeep ioState


/* RWS ---
/*	Set the shape of the cursor globally. This shape overrules the local cursor shapes of windows.
*/
setCursor :: !CursorShape !(IOSt .l) -> IOSt .l
setCursor shape ioState
#	(cInfo,ioState)	= IOStGetDialogCursorInfo ioState
	(cInfo,ioState)	= accIOToolbox (cursorinfoSetGlobalCursor shape cInfo) ioState
	ioState			= IOStSetDialogCursorInfo cInfo ioState
=	ioState


/*	resetCursor undoes the effect of SetCursor.
*/
resetCursor :: !(IOSt .l) -> IOSt .l
resetCursor ioState
#	(cInfo,ioState)	= IOStGetDialogCursorInfo ioState
	(cInfo,ioState)	= accIOToolbox (cursorinfoResetGlobalCursor cInfo) ioState
	ioState			= IOStSetDialogCursorInfo cInfo ioState
=	ioState


/*	obscureCursor hides the cursor until the mouse is moved.
*/
obscureCursor :: !(IOSt .l) -> IOSt .l
obscureCursor ioState = appIOToolbox QObscureCursor ioState


/*	setDoubleDownDistance sets the double down distance of the mouse. Negative values are set to zero.
*/
setDoubleDownDistance :: !Int !(IOSt .l) -> IOSt .l
setDoubleDownDistance newDDDist ioState = IOStSetDoubleDownDist newDDDist ioState

--- RWS */

/*	getDocumentInterface retrieves the DocumentInterface of an interactive process.
*/
getDocumentInterface :: !(IOSt .l) -> (!DocumentInterface, !IOSt .l)
getDocumentInterface ioState = IOStGetDocumentInterface ioState


/*	Operations on the attributes of an interactive process:
*/
setProcessActivate :: !(IdFun (PSt .l)) !(IOSt .l) -> IOSt .l
setProcessActivate activateF ioState
	# (pAtts,ioState)	= IOStGetProcessAttributes ioState
	= IOStSetProcessAttributes (setProcessAttribute isProcessActivate (ProcessActivate activateF) pAtts) ioState

setProcessDeactivate :: !(IdFun (PSt .l)) !(IOSt .l) -> IOSt .l
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

appListPIO :: ![.IdFun (IOSt .l)] !(PSt .l) -> PSt .l
appListPIO fs pState=:{io} = {pState & io=StrictSeq fs io}

appListPLoc :: ![.IdFun .l] !(PSt .l) -> PSt .l
appListPLoc fs pState=:{ls} = {pState & ls=StrictSeq fs ls}

appPIO :: !.(IdFun (IOSt .l)) !(PSt .l) -> PSt .l
appPIO f pState=:{io} = {pState & io=f io}

appPLoc :: !.(IdFun .l) !(PSt .l) -> PSt .l
appPLoc f pState=:{ls} = {pState & ls=f ls}


//	Accessing PSt component operations.

accListPIO :: ![.St (IOSt .l) .x] !(PSt .l) -> (![.x],!PSt .l)
accListPIO fs pState=:{io}
	# (xs,io) = StrictSeqList fs io
	= (xs,{pState & io=io})

accListPLoc :: ![.St .l .x] !(PSt .l) -> (![.x],!PSt .l)
accListPLoc fs pState=:{ls}
	# (xs,ls) = StrictSeqList fs ls
	= (xs,{pState & ls=ls})

accPIO :: !.(St (IOSt .l) .x) !(PSt .l) -> (!.x,!PSt .l)
accPIO f pState=:{io}
	# (x,io) = f io
	= (x,{pState & io=io})

accPLoc :: !.(St .l .x) !(PSt .l) -> (!.x,!PSt .l)
accPLoc f pState=:{ls}
	# (x,ls) = f ls
	= (x,{pState & ls=ls})
