definition module windowdraw


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Drawing operations on windows.
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************

import	windowhandle
import	ossystem, ostoolbox


/*	drawwindowlook	applies the Look of the given WindowHandle.
	drawwindowlook`	is the same as drawwindowlook, except that when the Rect result of the function argument
					is not zero, it is added to the UpdateState argument before the Look is applied.
	drawinwindow	applies the drawing function to the given WindowHandle.
	These functions assume that WindowHandle refers to a Window with a valid ClipState.
*/
drawwindowlook	:: !OSWindowMetrics !OSWindowPtr !(IdFun *Picture)     !UpdateState   !(WindowHandle .ls .pst) !*OSToolbox
																				   -> (!WindowHandle .ls .pst, !*OSToolbox)
drawwindowlook` :: !OSWindowMetrics !OSWindowPtr !(St *Picture [OSRect]) !UpdateState !(WindowHandle .ls .pst) !*OSToolbox
																				   -> (!WindowHandle .ls .pst, !*OSToolbox)
drawinwindow	:: !OSWindowMetrics !OSWindowPtr !.(St *Picture .x)                   !(WindowHandle .ls .pst) !*OSToolbox
																			    -> (.x,!WindowHandle .ls .pst, !*OSToolbox)


