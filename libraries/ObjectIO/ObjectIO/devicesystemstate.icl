implementation module devicesystemstate


//	Clean Object I/O library, version 1.2


import commondef, device
import timerhandle
import menuhandle
import windowhandle
import receiverhandle


devicesystemstateFatalError :: String String -> .x
devicesystemstateFatalError rule error
	= FatalError rule "devicesystemstate" error


::	DeviceSystemState ps
	= 	TimerSystemState	(TimerHandles		ps)
	|	MenuSystemState		(MenuHandles		ps)
	|	WindowSystemState	(WindowHandles		ps)
	|	ReceiverSystemState	(ReceiverHandles	ps)

toDevice :: !(DeviceSystemState .ps) -> Device
toDevice (TimerSystemState		_) = TimerDevice
toDevice (MenuSystemState		_) = MenuDevice
toDevice (WindowSystemState		_) = WindowDevice
toDevice (ReceiverSystemState	_) = ReceiverDevice


TimerSystemStateGetTimerHandles :: !(DeviceSystemState .pst) -> TimerHandles .pst
TimerSystemStateGetTimerHandles (TimerSystemState tsHs)
	= tsHs
TimerSystemStateGetTimerHandles _
	= devicesystemstateFatalError "devicesystemstateFatalError" "argument is no TimerSystemState"

MenuSystemStateGetMenuHandles :: !(DeviceSystemState .pst) -> MenuHandles .pst
MenuSystemStateGetMenuHandles (MenuSystemState msHs)
	= msHs
MenuSystemStateGetMenuHandles _
	= devicesystemstateFatalError "devicesystemstateFatalError" "argument is no MenuSystemState"

WindowSystemStateGetWindowHandles :: !(DeviceSystemState .pst) -> WindowHandles .pst
WindowSystemStateGetWindowHandles (WindowSystemState wsHs)
	= wsHs
WindowSystemStateGetWindowHandles _
	= devicesystemstateFatalError "devicesystemstateFatalError" "argument is no WindowSystemState"

ReceiverSystemStateGetReceiverHandles :: !(DeviceSystemState .pst) -> ReceiverHandles .pst
ReceiverSystemStateGetReceiverHandles (ReceiverSystemState rsHs)
	= rsHs
ReceiverSystemStateGetReceiverHandles _
	= devicesystemstateFatalError "devicesystemstateFatalError" "argument is no ReceiverSystemState"
