definition module menudevice


//	Clean Object I/O library, version 1.2


import	devicefunctions
from	iostate	import PSt, IOSt


MenuFunctions			:: DeviceFunctions (PSt .l .p)

IOStIsActive			:: !(IOSt .l .p) -> (!Bool,!IOSt .l .p)
ActivateMenuSystem		:: !(IOSt .l .p) -> IOSt .l .p
