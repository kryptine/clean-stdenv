implementation module StdPStClass


//	Clean Object I/O library, version 1.2

import	StdFile, StdTuple
import	iostate, StdFileSelect, StdSound, StdTime
from	scheduler			import handleOneEventForDevices
from	StdPSt				import accPIO
from	clCCall_12			import WinPlaySound
import	osfileselect
from	ostoolbox			import OSNewToolbox


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
// ..MW11

instance playSoundFile (PSt .l) where
	playSoundFile :: !String !(PSt .l) -> (!Bool,!PSt .l)
	playSoundFile soundFileName pState=:{io}
		# (ok,io)	= accIOToolbox (WinPlaySound soundFileName) io
		= (ok,{pState & io=io})
