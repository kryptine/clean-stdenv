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
instance FileSystem (PSt .l) where
	fopen :: !{#Char} !Int !(PSt .l) -> (!Bool,!*File,!PSt .l)
	fopen fName fMode pState
		# ((ok,file),pState)	= accFiles (fopen` fName fMode) pState
		= (ok,file,pState)
	where
		fopen` :: !{#Char} !Int !*Files -> (!(!Bool,!*File),!*Files)
		fopen` fName fMode files
			# (ok,file,files)	= fopen fName fMode files
			= ((ok,file),files)
	
	fclose :: !*File !(PSt .l) -> (!Bool,!PSt .l)
	fclose file pState
		= accFiles (fclose file) pState
	
	stdio :: !(PSt .l) -> (!*File,!PSt .l)
	stdio pState
		= accFiles stdio pState
	
	sfopen :: !{#Char} !Int !(PSt .l) -> (!Bool,!File,!PSt .l)
	sfopen fName fMode pState
		# ((ok,sfile),pState)	= accFiles (sfopen` fName fMode) pState
		= (ok,sfile,pState)
	where
		sfopen` :: !{#Char} !Int !*Files -> (!(!Bool,!File),!*Files)
		sfopen` fName fMode files
			# (ok,file,files)	= sfopen fName fMode files
			= ((ok,file),files)

/*	PSt is an environment instance of the class FileEnv (see StdFile).
*/
instance FileEnv (PSt .l) where
	accFiles :: !.(*Files -> (.x,*Files)) !*(PSt .l) -> (!.x,!*PSt .l)
	accFiles accfun pState=:{io}
		# (world,io)		= IOStGetWorld io
		  (x,world)			= accFiles accfun world
		  pState			= {pState & io=IOStSetWorld world io}
		= (x,pState)
	
	appFiles :: !.(*Files -> *Files) !*(PSt .l) -> *PSt .l
	appFiles appfun pState=:{io}
		# (world,io)		= IOStGetWorld io
		  world				= appFiles appfun world
		  pState			= {pState & io=IOStSetWorld world io}
		= pState

// MW11..
instance FileEnv	(IOSt .l)
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
instance FileSelectEnv (PSt .l) where
	selectInputFile :: !(PSt .l) -> (!Maybe String,!PSt .l)
	selectInputFile pState
		# (ok,name,pState,_)	= OSselectinputfile handleOSEvent pState OSNewToolbox
		= (if ok (Just name) Nothing,pState)
	
	selectOutputFile:: !String !String !(PSt .l) -> (!Maybe String,!PSt .l)
	selectOutputFile prompt originalName pState
		# (ok,name,pState,_)	= OSselectoutputfile handleOSEvent pState prompt originalName OSNewToolbox
		= (if ok (Just name) Nothing,pState)
	
	selectDirectory :: !(PSt .l) -> (!Maybe String,!PSt .l)
	selectDirectory pState
		# (ok,name,pState,_)	= OSselectdirectory handleOSEvent pState OSNewToolbox
		= (if ok (Just name) Nothing,pState)


//	handleOSEvent turns handleOneEventForDevices into the form required by OSselect(in/out)putfile.
handleOSEvent :: !OSEvent !(PSt .l) -> PSt .l
handleOSEvent osEvent pState
	= thd3 (handleOneEventForDevices (ScheduleOSEvent osEvent []) pState)


/*	PSt is an environment instance of the class TimeEnv (see StdTime).
*/
/* MW11 was
instance TimeEnv (PSt .l) where
	getBlinkInterval :: !(PSt .l) -> (!Int,!PSt .l)
	getBlinkInterval pState=:{io}
		# (world,io)		= IOStGetWorld io
		# (blink,world)		= getBlinkInterval world
		# pState			= {pState & io=IOStSetWorld world io}
		= (blink,pState)
	
	getCurrentTime :: !(PSt .l) -> (!Time,!PSt .l)
	getCurrentTime pState=:{io}
		# (world,io)		= IOStGetWorld io
		# (time,world)		= getCurrentTime world
		# pState			= {pState & io=IOStSetWorld world io}
		= (time,pState)
	
	getCurrentDate :: !(PSt .l) -> (!Date,!PSt .l)
	getCurrentDate pState=:{io}
		# (world,io)		= IOStGetWorld io
		# (date,world)		= getCurrentDate world
		# pState			= {pState & io=IOStSetWorld world io}
		= (date,pState)
*/

instance TimeEnv (PSt .l) where
	getBlinkInterval :: !(PSt .l) -> (!Int,!PSt .l)
	getBlinkInterval pState
		= accPIO getBlinkInterval pState
	
	getCurrentTime :: !(PSt .l) -> (!Time,!PSt .l)
	getCurrentTime pState
		= accPIO getCurrentTime pState
	
	getCurrentDate :: !(PSt .l) -> (!Date,!PSt .l)
	getCurrentDate pState
		= accPIO getCurrentDate pState

	getCurrentTick :: !(PSt .l) -> (!Tick,!PSt .l)
	getCurrentTick pState
		= accPIO getCurrentTick pState

// MW11..
instance TimeEnv (IOSt .l) where
	getBlinkInterval :: !(IOSt .l) -> (!Int,!IOSt .l)
	getBlinkInterval io
		# (world,io)		= IOStGetWorld io
		  (blink,world)		= getBlinkInterval world
		= (blink,IOStSetWorld world io)
	
	getCurrentTime :: !(IOSt .l) -> (!Time,!IOSt .l)
	getCurrentTime io
		# (world,io)		= IOStGetWorld io
		  (time,world)		= getCurrentTime world
		= (time,IOStSetWorld world io)
	
	getCurrentDate :: !(IOSt .l) -> (!Date,!IOSt .l)
	getCurrentDate io
		# (world,io)		= IOStGetWorld io
		  (date,world)		= getCurrentDate world
		= (date, IOStSetWorld world io)

	getCurrentTick :: !(IOSt .l) -> (!Tick,!IOSt .l)
	getCurrentTick io
		# (world,io)		= IOStGetWorld io
		  (tick,world)		= getCurrentTick world
		= (tick, IOStSetWorld world io)

instance ChannelEnv (PSt .l)
  where
	channelEnvKind env
		= (PST, env)
	mb_close_inet_receiver_without_id reallyDoIt id_pair pSt=:{io}
		= { pSt & io = mb_close_inet_receiver_without_id True id_pair io }

instance Ids (PSt .l)
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

instance ChannelEnv (IOSt .l)
  where
	channelEnvKind env
		= (IOST, env)
	mb_close_inet_receiver_without_id False _ ioState
		= ioState
	mb_close_inet_receiver_without_id True id_pair ioState
	#! (closed,ioState)			= IOStClosed ioState
	| closed
		= ioState
	# (found,receivers,ioState)	= IOStGetDevice ReceiverDevice ioState
	| not found			// PA: guard added
		= ioState
	# rsHs						= (ReceiverSystemStateGetReceiverHandles receivers).rReceivers
	  (found,rsH,rsHs)			= Remove (inetReceiverStateIdentified1 id_pair) undef rsHs
	# ioState					= IOStSetDevice (ReceiverSystemState {rReceivers=rsHs}) ioState
	| not found
		= ioState
	| otherwise
		# id					= rsH.rHandle.rId
		  (idtable,ioState)		= IOStGetIdTable ioState
		  ioState				= IOStSetIdTable (snd (removeIdFromIdTable id idtable)) ioState
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
instance accScreenPicture (IOSt .l) where
	accScreenPicture :: !.(St *Picture .x) !(IOSt .l) -> (!.x,!IOSt .l)
	accScreenPicture fun ioState
		= accIOToolbox (peekScreen fun) ioState


/*	Emit the alert sound.
*/
beep :: !(IOSt .l) -> IOSt .l
beep ioState = appIOToolbox OSBeep ioState


instance playSoundFile (PSt .l) where
	playSoundFile :: !String !(PSt .l) -> (!Bool,!PSt .l)
	playSoundFile soundFileName pState=:{io}
		# (ok,io)	= accIOToolbox (WinPlaySound soundFileName) io
		= (ok,{pState & io=io})


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
