implementation module StdPSt


//	Clean Object I/O library, version 1.2


import	StdEnv, StdFileSelect
import	StdSound, StdTime
import	deviceevents, commondef
from	iostate				import PSt, IOSt, appIOToolbox, accIOToolbox, IOStGetWorld, IOStSetWorld, IOStGetDocumentInterface, 
									IOStGetProcessAttributes, IOStSetProcessAttributes
from	StdProcessAttribute	import isProcessActivate, isProcessDeactivate
from	scheduler			import handleOneEventForDevices
import	osbeep, osfileselect
from	clCCall_12			import WinPlaySound
from	ospicture			import peekScreen
from	ostoolbox			import OSNewToolbox, WorldGetToolbox, WorldSetToolbox
import	StdReceiver, receiverid, receiverhandle, receiverdevice, channelenv // MW11++


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

// MW11..
instance FileEnv	(IOSt .l .p)
  where
	accFiles accfun io
		# (world,io)		= IOStGetWorld io
		  (x,world)			= accFiles accfun world
		  io				=IOStSetWorld world io
		= (x,io)
	appFiles appfun io
		# (world,io)		= IOStGetWorld io
		  world				= appFiles appfun world
		  io				= IOStSetWorld world io
		= io
// ..MW11

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
/* MW11 was
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
*/

instance TimeEnv (PSt .l .p) where
	getBlinkInterval :: !(PSt .l .p) -> (!Int,!PSt .l .p)
	getBlinkInterval pState
		= accPIO getBlinkInterval pState
	
	getCurrentTime :: !(PSt .l .p) -> (!Time,!PSt .l .p)
	getCurrentTime pState
		= accPIO getCurrentTime pState
	
	getCurrentDate :: !(PSt .l .p) -> (!Date,!PSt .l .p)
	getCurrentDate pState
		= accPIO getCurrentDate pState

	getCurrentTick :: !(PSt .l .p) -> (!Tick,!PSt .l .p)
	getCurrentTick pState
		= accPIO getCurrentTick pState

// MW11..
instance TimeEnv (IOSt .l .p) where
	getBlinkInterval :: !(IOSt .l .p) -> (!Int,!IOSt .l .p)
	getBlinkInterval io
		# (world,io)		= IOStGetWorld io
		  (blink,world)		= getBlinkInterval world
		= (blink,IOStSetWorld world io)
	
	getCurrentTime :: !(IOSt .l .p) -> (!Time,!IOSt .l .p)
	getCurrentTime io
		# (world,io)		= IOStGetWorld io
		  (time,world)		= getCurrentTime world
		= (time,IOStSetWorld world io)
	
	getCurrentDate :: !(IOSt .l .p) -> (!Date,!IOSt .l .p)
	getCurrentDate io
		# (world,io)		= IOStGetWorld io
		  (date,world)		= getCurrentDate world
		= (date, IOStSetWorld world io)

	getCurrentTick :: !(IOSt .l .p) -> (!Tick,!IOSt .l .p)
	getCurrentTick io
		# (world,io)		= IOStGetWorld io
		  (tick,world)		= getCurrentTick world
		= (tick, IOStSetWorld world io)

instance ChannelEnv (PSt .l .p)
  where
	channelEnvKind env
		= (PST, env)
	mb_close_inet_receiver_without_id reallyDoIt id_pair pSt=:{io}
		= { pSt & io = mb_close_inet_receiver_without_id True id_pair io }

instance Ids (PSt .l .p)
  where
	openId pSt
		= accPIO openId pSt
	openIds	i pSt=:{io}
		= accPIO (openIds i) pSt
	openRId	pSt
		= accPIO openRId pSt
	openRIds i pSt
		= accPIO (openRIds i) pSt
	openR2Id pSt
		= accPIO openR2Id pSt
	openR2Ids i pSt
		= accPIO (openR2Ids i) pSt

	
/*	IOSt is also an environment instance of the class ChannelEnv	*/

instance ChannelEnv (IOSt .l .p)
  where
	channelEnvKind env
		= (IOST, env)
	mb_close_inet_receiver_without_id False _ ioState
		= ioState
	mb_close_inet_receiver_without_id True id_pair ioState
	#! (closed, ioState)	= IOStClosed ioState
	| closed
		= ioState
	# (receivers,ioState)	= IOStGetDevice ReceiverDevice ioState
	  rsHs					= (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
	  (found,rsH,rsHs)		= Remove (inetReceiverStateIdentified1 id_pair) undef rsHs
	# ioState				= IOStSetDevice (ReceiverSystemState {rReceivers=rsHs}) ioState
	| not found
		= ioState
	| otherwise
		# id				= rsH.rHandle.rId
		  (idtable,ioState)	= IOStGetIdTable ioState
		  ioState			= IOStSetIdTable (snd (removeIdFromIdTable id idtable)) ioState
		  ioState				= unbindRId id ioState
		  ioState				= IOStSetRcvDisabled True ioState // MW11++
		  connectedIds			= rsH.rHandle.rConnected
		  ioState				= seq (map closeReceiver connectedIds) ioState
		  inetInfo				= rsH.rHandle.rInetInfo
		  (_,_,_,closeFun)		= fromJust inetInfo
		  ioState				= appIOToolbox closeFun ioState
		= ioState

inetReceiverStateIdentified1 :: !(!EndpointRef`, !InetReceiverCategory`) !(ReceiverStateHandle .ps) -> Bool
inetReceiverStateIdentified1 x {rHandle} = inetReceiverIdentified x rHandle
// ..MW11

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
