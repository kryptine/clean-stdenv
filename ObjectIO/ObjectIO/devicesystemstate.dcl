definition module devicesystemstate

//	Clean Object I/O library, version 1.2

import	device
import	menuhandle
import	processhandle
import	receiverhandle
import	timerhandle
import	windowhandle

::	DeviceSystemState pst
	=	ProcessSystemState	(ProcessHandles		pst)
	|	MenuSystemState		(MenuHandles		pst)
	|	ReceiverSystemState	(ReceiverHandles	pst)
	| 	TimerSystemState	(TimerHandles		pst)
	|	WindowSystemState	(WindowHandles		pst)

toDevice								:: !(DeviceSystemState .pst) -> Device

/*	The following are PARTIAL access functions:
*/
MenuSystemStateGetMenuHandles			:: !(DeviceSystemState .pst) -> MenuHandles     .pst
ProcessSystemStateGetProcessHandles		:: !(DeviceSystemState .pst) -> ProcessHandles  .pst
ReceiverSystemStateGetReceiverHandles	:: !(DeviceSystemState .pst) -> ReceiverHandles .pst
TimerSystemStateGetTimerHandles			:: !(DeviceSystemState .pst) -> TimerHandles    .pst
WindowSystemStateGetWindowHandles		:: !(DeviceSystemState .pst) -> WindowHandles   .pst
