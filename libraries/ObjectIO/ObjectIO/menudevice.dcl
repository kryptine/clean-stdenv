definition module menudevice


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


import	devicefunctions
from	iostate import :: IOSt, :: PSt


menuFunctions			:: DeviceFunctions (PSt .l)

ioStIsActive			:: !(IOSt .l) -> (!Bool,!IOSt .l)
activateMenuSystem		:: !(IOSt .l) -> IOSt .l
