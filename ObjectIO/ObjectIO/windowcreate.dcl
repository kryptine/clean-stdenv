definition module windowcreate


//	Clean Object I/O library, version 1.2

from	oswindow	import DelayActivationInfo
from	iostate		import PSt, IOSt
import	windowhandle


/*	Open a window.
	The Id             argument is the validated Id of the window/dialogue.
	The WindowLSHandle argument should be an initialised handle of the new window. 
	The window/dialogue will be filled with concrete controls and a concrete window will be created. 
	After opening the window its optional initial actions are also evaluated.
*/
openwindow			:: !Id !(WindowLSHandle .ls (PSt .l)) !(PSt .l) -> PSt .l
openmodalwindow		:: !Id !(WindowLSHandle .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!Maybe .ls,!PSt .l)

/*	bufferDelayedEvents buffers the activate/deactivate events.
*/
bufferDelayedEvents	:: ![DelayActivationInfo] !(IOSt .l) -> IOSt .l

/*	WindowBound-checks for normal windows.
*/
checkZeroWindowBound:: !(IOSt .l) -> (!Bool,!IOSt .l)
decreaseWindowBound	:: !(IOSt .l) -> IOSt .l
