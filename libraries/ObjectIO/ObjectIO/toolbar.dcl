definition module toolbar

//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************

import iostate

/*	openToolbar
		creates the requested toolbar for a MDI or SDI process. 
		openToolbar has no effect in case IOSt belongs to a NDI process. 
		openToolbar causes a run-time error if a toolbar already exists.
*/
openToolbar		:: !(IOSt .l) -> IOSt .l
