definition module menudevice


//	Clean Object I/O library, version 1.2


import	devicefunctions
from	iostate	import PSt, IOSt


MenuFunctions			:: DeviceFunctions (PSt .l)

IOStIsActive			:: !(IOSt .l) -> (!Bool,!IOSt .l)
ActivateMenuSystem		:: !(IOSt .l) -> IOSt .l
