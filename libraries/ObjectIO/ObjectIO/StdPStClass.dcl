definition module StdPStClass


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	StdPStClass collects (PSt .l) and (IOSt .l) class instances.
//	Author: Peter Achten
//	Modified: 14 March 2003 for Clean 2.0.3 
//	********************************************************************************


import	StdFile, StdFileSelect, StdTime
from	iostate		import :: PSt, :: IOSt


/*	PSt is an environment instance of the following classes:
	- FileSystem	(see StdFile)
	- FileEnv		(see StdFile)
	- FileSelectEnv	(see StdFileSelect)
	- TimeEnv		(see StdTime)
	
	and so is IOSt, except FileSelectEnv.
*/
instance FileSystem		(PSt .l), (IOSt .l)
instance FileEnv		(PSt .l), (IOSt .l)
instance FileSelectEnv	(PSt .l)
instance TimeEnv		(PSt .l), (IOSt .l)
