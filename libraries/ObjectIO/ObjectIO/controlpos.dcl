definition module controlpos


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


import	ossystem, ostoolbox
import	windowhandle


/*	movewindowviewframe moves the current view frame of the WindowHandle by the given Vector. 
*/
movewindowviewframe	:: !OSWindowMetrics !Vector2 !WIDS !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst, !*OSToolbox)

