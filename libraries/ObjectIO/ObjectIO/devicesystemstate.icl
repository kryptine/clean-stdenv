implementation module devicesystemstate


//	Clean Object I/O library, version 1.2


import commondef, device
import timerhandle
import menuhandle
import processhandle
import windowhandle
import receiverhandle


devicesystemstateFatalError :: String String -> .x
devicesystemstateFatalError rule error
	= FatalError rule "devicesystemstate" error


::	DeviceSystemState pst
	=	MenuSystemState		(MenuHandles		pst)
	|	ProcessSystemState	(ProcessHandles		pst)
	|	ReceiverSystemState	(ReceiverHandles	pst)
	| 	TimerSystemState	(TimerHandles		pst)
	|	WindowSystemState	(WindowHandles		pst)

toDevice :: !(DeviceSystemState .ps) -> Device
toDevice (MenuSystemState		_) = MenuDevice
toDevice (ProcessSystemState	_) = ProcessDevice
toDevice (ReceiverSystemState	_) = ReceiverDevice
toDevice (TimerSystemState		_) = TimerDevice
toDevice (WindowSystemState		_) = WindowDevice


MenuSystemStateGetMenuHandles :: !(DeviceSystemState .pst) -> MenuHandles .pst
MenuSystemStateGetMenuHandles (MenuSystemState msHs)
	= msHs
MenuSystemStateGetMenuHandles _
	= devicesystemstateFatalError "MenuSystemStateGetMenuHandles" "argument is no MenuSystemState"

ProcessSystemStateGetProcessHandles :: !(DeviceSystemState .pst) -> ProcessHandles .pst
ProcessSystemStateGetProcessHandles (ProcessSystemState psHs)
	= psHs
ProcessSystemStateGetProcessHandles _
	= devicesystemstateFatalError "ProcessSystemStateGetProcessHandles" "argument is no ProcessSystemState"

ReceiverSystemStateGetReceiverHandles :: !(DeviceSystemState .pst) -> ReceiverHandles .pst
ReceiverSystemStateGetReceiverHandles (ReceiverSystemState rsHs)
	= rsHs
ReceiverSystemStateGetReceiverHandles _
	= devicesystemstateFatalError "ReceiverSystemStateGetReceiverHandles" "argument is no ReceiverSystemState"

TimerSystemStateGetTimerHandles :: !(DeviceSystemState .pst) -> TimerHandles .pst
TimerSystemStateGetTimerHandles (TimerSystemState tsHs)
	= tsHs
TimerSystemStateGetTimerHandles _
	= devicesystemstateFatalError "TimerSystemStateGetTimerHandles" "argument is no TimerSystemState"

WindowSystemStateGetWindowHandles :: !(DeviceSystemState .pst) -> WindowHandles .pst
WindowSystemStateGetWindowHandles (WindowSystemState wsHs)
	= wsHs
WindowSystemStateGetWindowHandles _
	= devicesystemstateFatalError "WindowSystemStateGetWindowHandles" "argument is no WindowSystemState"
