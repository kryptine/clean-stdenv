definition module StdPSt


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdPSt defines operations on PSt and IOSt that are not abstract device related.
//	********************************************************************************


import	StdFile, StdFileSelect, StdSound, StdTime
from	StdFunc		import St
from	StdIOBasic	import IdFun
from	StdIOCommon	import DocumentInterface, MDI, SDI, NDI
from	StdPicture	import Picture
from	iostate		import PSt, IOSt
from	channelenv	import  ChannelEnv // MW11++


/*	PSt is an environment instance of the following classes:
	- FileEnv		(see StdFile)
	- FileSelectEnv	(see StdFileSelect)
	- TimeEnv		(see StdTime)
	- playSoundFile (see StdSound)
	- ChannelEnv	(see StdChannels) // MW11++
	- Ids			(see StdId) // MW11++
	
	IOSt is also an environment instance of the classes FileEnv, TimeEnv & ChannelEnv
*/
instance FileEnv		(PSt .l .p), (IOSt .l .p) // MW11 added IOSt
instance FileSelectEnv	(PSt .l .p)
instance TimeEnv		(PSt .l .p), (IOSt .l .p) // MW11 added IOSt
instance playSoundFile	(PSt .l .p)
instance ChannelEnv 	(PSt .l .p), (IOSt .l .p) // MW11 added IOSt
instance Ids		 	(PSt .l .p)


/*	accScreenPicture provides access to an initial Picture as it would be created in
	a window or control.
*/
class accScreenPicture env :: !.(St *Picture .x) !*env -> (!.x,!*env)

instance accScreenPicture World
instance accScreenPicture (IOSt .l .p)


beep :: !(IOSt .l .p) -> IOSt .l .p
/*	beep emits the alert sound.
*/


//	Operations on the global cursor:

/* RWS ---
setCursor		:: !CursorShape !(IOSt .l .p) -> IOSt .l .p
resetCursor		::              !(IOSt .l .p) -> IOSt .l .p
obscureCursor	::              !(IOSt .l .p) -> IOSt .l .p
/*	setCursor		overrules the shape of the cursor of all windows. 
	resetCursor		removes the overruled cursor shape of all windows.
	obscureCursor	hides the cursor until the mouse is moved.
*/


//	Operations on the DoubleDownDistance:

setDoubleDownDistance :: !Int !(IOSt .l .p) -> IOSt .l .p
/*	setDoubleDownDistance sets the maximum distance the mouse is allowed to move to 
	generate a ButtonDouble(Triple)Down button state. Negative values are set to 
	zero.
*/
--- RWS */

//	Operations on the DocumentInterface of an interactive process:

getDocumentInterface :: !(IOSt .l .p) -> (!DocumentInterface, !IOSt .l .p)
/*	getDocumentInterface returns the DocumentInterface of the interactive process.
*/


//	Operations on the attributes of an interactive process:

setProcessActivate	:: !(IdFun (PSt .l .p)) !(IOSt .l .p) -> IOSt .l .p
setProcessDeactivate:: !(IdFun (PSt .l .p)) !(IOSt .l .p) -> IOSt .l .p
/*	These functions set the ProcessActivate and ProcessDeactivate attribute of the 
	interactive process respectively.
*/


//	Coercing PSt component operations to PSt operations.

appListPIO	:: ![.IdFun (IOSt .l .p)]	!(PSt .l .p) ->			 PSt .l .p
appListPLoc	:: ![.IdFun .l]				!(PSt .l .p) ->			 PSt .l .p
appListPPub	:: ![.IdFun .p]				!(PSt .l .p) ->			 PSt .l .p

appPIO		:: !.(IdFun (IOSt .l .p))	!(PSt .l .p) ->			 PSt .l .p
appPLoc		:: !.(IdFun .l)				!(PSt .l .p) ->			 PSt .l .p
appPPub		:: !.(IdFun .p)				!(PSt .l .p) ->			 PSt .l .p

//	Accessing PSt component operations.

accListPIO	:: ![.St (IOSt .l .p) .x]	!(PSt .l .p) -> (![.x],	!PSt .l .p)
accListPLoc	:: ![.St .l           .x]	!(PSt .l .p) -> (![.x],	!PSt .l .p)
accListPPub	:: ![.St .p           .x]	!(PSt .l .p) -> (![.x],	!PSt .l .p)

accPIO		:: !.(St (IOSt .l .p) .x)	!(PSt .l .p) -> (!.x,	!PSt .l .p)
accPLoc		:: !.(St .l           .x)	!(PSt .l .p) -> (!.x,	!PSt .l .p)
accPPub		:: !.(St .p           .x)	!(PSt .l .p) -> (!.x,	!PSt .l .p)
