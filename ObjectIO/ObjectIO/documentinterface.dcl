definition module documentinterface

//	Clean Object I/O library, version 1.2

from	iostate				import PSt, IOSt
from	osdocumentinterface	import OSDInfo

/*	setOSDInfoInMenuDevice stores process document interface information in the menu device.
*/
setOSDInfoInMenuDevice :: !OSDInfo !(PSt .l .p) -> PSt .l .p

/*	closeOSDInfo is the final call needed to close the proper document interface resources.
	It should be called only after all other devices have been closed. 
*/
closeOSDInfo :: !(IOSt .l .p) -> IOSt .l .p
