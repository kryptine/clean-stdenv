definition module menudevice


//	Clean Object I/O library, version 1.2


import	devicefunctions
from	iostate	import PSt, IOSt


menuFunctions			:: DeviceFunctions (PSt .l)

ioStIsActive			:: !(IOSt .l) -> (!Bool,!IOSt .l)
activateMenuSystem		:: !(IOSt .l) -> IOSt .l
