definition module device

//	Clean Object I/O library, version 1.2

from	StdString	import String
import	StdOverloaded

::	Device						// The set of devices
	=	TimerDevice
	|	MenuDevice
	|	WindowDevice
	|	ReceiverDevice
	|	ProcessDevice

/* RWS +++ from deviceaccess */
instance ==			Device
instance toString	Device

priorityDevice :: !Device -> Int
Devices							// The device list in order of descending priority
	:== [	ReceiverDevice
		,	TimerDevice
		,	MenuDevice
		,	WindowDevice
		,	ProcessDevice
		]
