definition module menuevent


//	Clean Object I/O library, version 1.2

/*	menuevent defines the DeviceEventFunction for the menu device.
	This function is placed in a separate module because it is platform dependent.
*/


import	deviceevents, devicesystemstate, menuhandle
from	iostate	import PSt, IOSt


menuEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)

MenuHandlesGetMenuStateHandles :: !(MenuHandles .pst) -> (![MenuStateHandle .pst], !MenuHandles .pst)
// PA: moved from menudevice
