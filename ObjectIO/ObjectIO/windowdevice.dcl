definition module windowdevice


//	Clean Object I/O library, version 1.2


from	ostoolbox	import OSToolbox
from	oswindow	import OSWindowMetrics
from	iostate		import PSt, IOSt
import	devicefunctions, windowhandle


WindowFunctions			:: DeviceFunctions (PSt .l .p)

windowStateSizeAction	:: !OSWindowMetrics !Bool !WindowSizeActionInfo !(WindowStateHandle .pst) !*OSToolbox
																	 -> (!WindowStateHandle .pst, !*OSToolbox)
/*	windowStateSizeAction wMetrics isActive info
		resizes the argument window. The isActive Boolean is True iff the window is the active window. 
*/