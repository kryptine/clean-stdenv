definition module StdProcessDef


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	StdProcessDef contains the types to define interactive processes.
//	Author: Peter Achten
//	Modified: 14 September 2001 for Clean 2.0
//	********************************************************************************


import	StdIOCommon
from	iostate import :: PSt


::  Process
	=	E. .l: Process
					DocumentInterface			// The process DocumentInterface
					l							// The process private state
					(ProcessInit      (PSt l))	// The process initialisation
					[ProcessAttribute (PSt l)]	// The process attributes

/*	NDI processes can't open windows and menus.
	SDI processes can have at most one window open.
	MDI processes can open an arbitrary number of device instances. 
*/

::	ProcessInit pst
	:==	IdFun pst
