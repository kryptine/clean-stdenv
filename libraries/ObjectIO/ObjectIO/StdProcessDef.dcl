definition module StdProcessDef


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdProcessDef contains the types to define interactive processes.
//	********************************************************************************


import	StdIOCommon
from	iostate	import PSt, IOSt


::  ProcessGroup pdef
	=	E. .p: ProcessGroup
					p								// The processgroup public state
					(pdef p)						// The processgroup members
::  Process p						
	=	E. .l: Process
					DocumentInterface				// The process DocumentInterface
					l								// The process local state
					(ProcessInit      (PSt l p))	// The process initialisation
					[ProcessAttribute (PSt l p)]	// The process attributes

/*	NDI processes can't open windows and menus.
	SDI processes can have at most one window open.
	MDI processes can open an arbitrary number of device instances. 
*/

::	ProcessInit pst
	:==	IdFun pst
