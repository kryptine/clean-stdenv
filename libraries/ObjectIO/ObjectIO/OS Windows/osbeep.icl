implementation module osbeep

//	Clean Object I/O library, version 1.2

from	clCCall_12	import WinBeep
from	ostoolbox	import OSToolbox

OSBeep :: !*OSToolbox -> *OSToolbox
OSBeep toolbox
	=	WinBeep toolbox
