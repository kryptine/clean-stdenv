definition module windowdispose


//	Clean Object I/O library, version 1.2


import	windowhandle
from	iostate				import PSt, IOSt, InputTrack, InputTrackKind
from	windowaccess		import WID
from	osdocumentinterface	import OSDInfo
from	osevent				import OSEvent, CrossCallInfo
from	ostoolbox			import OSToolbox
from	oswindow			import DelayActivationInfo


disposeWindow				:: !WID !(PSt .l .p) -> PSt .l .p
/*	disposeWindow disposes all system resources associated with the indicated window if it exists.
*/

//disposeCursorInfo			:: !CursorInfo !(IOSt .l .p) -> IOSt .l .p
/*	disposeCursorInfo disposes all system resources associated with the given CursorInfo.
	PA: not yet implemented
*/

disposeWindowStateHandle	:: !OSDInfo !(Maybe InputTrack) !(WindowStateHandle .pst) !(OSEvent -> .s -> ([Int],.s)) .s !*OSToolbox
									     -> (!(![Id],![Id],![DelayActivationInfo],![FinalModalLS],!Maybe InputTrack),.s,!*OSToolbox)
/*	disposeWindowStateHandle disposes all system resources associated with the given WindowStateHandle.
	It returns the freed receiver ids, control ids, delayed (de)activation event, and the final local modal dialog state.
	(When timers are also part of windows, timer ids will also be returned.)
*/

disposeWItemHandle			::  !OSWindowPtr !(WItemHandle .ls .pst) !*OSToolbox -> (!(![Id],![Id],!IdFun *OSToolbox),!*OSToolbox)
/*	disposeWItemHandle returns all freed receiver ids and a function that (recursively) disposes all system 
	resources associated with the given WItemHandle. 
	(When timers are also part of windows, timer ids will also be returned.)
*/
