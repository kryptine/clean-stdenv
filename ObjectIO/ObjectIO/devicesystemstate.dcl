definition module devicesystemstate

//	Clean Object I/O library, version 1.2

import	device
import	timerhandle
import	menuhandle
import	windowhandle
import	receiverhandle

::	DeviceSystemState pst
	= 	TimerSystemState	(TimerHandles		pst)
	|	MenuSystemState		(MenuHandles		pst)
	|	WindowSystemState	(WindowHandles		pst)
	|	ReceiverSystemState	(ReceiverHandles	pst)

toDevice								:: !(DeviceSystemState .pst) -> Device


/*	The following are PARTIAL access functions:
*/
TimerSystemStateGetTimerHandles			:: !(DeviceSystemState .pst) -> TimerHandles    .pst
MenuSystemStateGetMenuHandles			:: !(DeviceSystemState .pst) -> MenuHandles     .pst
WindowSystemStateGetWindowHandles		:: !(DeviceSystemState .pst) -> WindowHandles   .pst
ReceiverSystemStateGetReceiverHandles	:: !(DeviceSystemState .pst) -> ReceiverHandles .pst
