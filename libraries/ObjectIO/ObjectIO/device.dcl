definition module device

//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************

import StdOverloaded, StdString

::	Device						// The set of devices
	=	TimerDevice
	|	MenuDevice
	|	WindowDevice
	|	ReceiverDevice
	|	ProcessDevice

/* RWS +++ from deviceaccess */
instance ==			Device
instance toString	Device

priorityDevice	:: !Device -> Int
devices			:: [Device]		// The device list in order of descending priority
