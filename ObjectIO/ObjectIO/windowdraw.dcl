definition module windowdraw


//	Clean Object I/O library, version 1.2

//	Drawing operations on windows.

from	ostoolbox	import OSToolbox
from	oswindow	import OSWindowMetrics
import	windowhandle


/*	drawwindowlook	applies the Look of the given WindowHandle.
	drawwindowlook`	is the same as drawwindowlook, except that when the Rect result of the function argument
					is not zero, it is added to the UpdateState argument before the Look is applied.
	drawinwindow	applies the drawing function to the given WindowHandle.
	These functions assume that WindowHandle refers to a Window with a valid ClipState.
*/
drawwindowlook	:: !OSWindowMetrics !OSWindowPtr !(IdFun *Picture)     !UpdateState !(WindowHandle .ls .pst) !*OSToolbox
																				 -> (!WindowHandle .ls .pst, !*OSToolbox)
drawwindowlook` :: !OSWindowMetrics !OSWindowPtr !(St *Picture [Rect]) !UpdateState !(WindowHandle .ls .pst) !*OSToolbox
																				 -> (!WindowHandle .ls .pst, !*OSToolbox)
drawinwindow	:: !OSWindowMetrics !OSWindowPtr !.(St *Picture .x)                 !(WindowHandle .ls .pst) !*OSToolbox
																			  -> (.x,!WindowHandle .ls .pst, !*OSToolbox)


