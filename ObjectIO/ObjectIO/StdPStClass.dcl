definition module StdPStClass


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	StdPStClass collects (PSt .l) and (IOSt .l) class instances.
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


import	StdFile, StdFileSelect, StdTime
from	iostate		import :: PSt, :: IOSt


/*	PSt is an environment instance of the following classes:
	- FileSystem	(see StdFile)
	- FileEnv		(see StdFile)
	- FileSelectEnv	(see StdFileSelect)
	- TimeEnv		(see StdTime)
	
	IOSt is also an environment instance of the classes FileEnv, TimeEnv
*/
instance FileSystem		(PSt .l)
instance FileEnv		(PSt .l), (IOSt .l)
instance FileSelectEnv	(PSt .l)
instance TimeEnv		(PSt .l), (IOSt .l)
