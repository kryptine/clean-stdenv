definition module StdPStClass


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdPStClass collects (PSt .l) and (IOSt .l) class instances.
//	********************************************************************************


import	StdFile, StdFileSelect, StdSound, StdTime
from	iostate		import PSt, IOSt


/*	PSt is an environment instance of the following classes:
	- FileSystem	(see StdFile)
	- FileEnv		(see StdFile)
	- FileSelectEnv	(see StdFileSelect)
	- TimeEnv		(see StdTime)
	- playSoundFile (see StdSound)
	- ChannelEnv	(see StdChannels) // MW11++
	- Ids			(see StdId) // MW11++
	
	IOSt is also an environment instance of the classes FileEnv, TimeEnv & ChannelEnv
*/
instance FileSystem		(PSt .l)
instance FileEnv		(PSt .l), (IOSt .l) // MW11 added IOSt
instance FileSelectEnv	(PSt .l)
instance TimeEnv		(PSt .l), (IOSt .l) // MW11 added IOSt
instance playSoundFile	(PSt .l)
