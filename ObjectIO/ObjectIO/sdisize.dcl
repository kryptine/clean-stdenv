definition module sdisize


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************

from	StdIOBasic	import Size
from	iostate		import IOSt
import	ostypes

/*	getSDIWindowSize retrieves the OSWindowPtr and current Size of the WindowViewFrame of the SDI window
	if this is a SDI process. 
	Otherwise, the Size returned is zero, and the OSWindowPtr is OSNoWindowPtr.
*/
getSDIWindowSize :: !(IOSt .l) -> (!Size,!OSWindowPtr,!IOSt .l)

/*	resizeSDIWindow wPtr oldviewframesize newviewframesize
		resizes the SDI window so the viewframe does not change in size. 
		oldviewframesize is the size of the ViewFrame(!) before the view frame changed size.
		newviewframesize is the size of the ViewFrame(!) after  the view frame changed size.
*/
resizeSDIWindow :: !OSWindowPtr !Size !Size !(IOSt .l) -> IOSt .l
