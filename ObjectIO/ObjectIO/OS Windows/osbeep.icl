implementation module osbeep

//	Clean Object I/O library, version 1.2

from	clCCall_12	import winBeep
from	ostoolbox	import OSToolbox

osBeep :: !*OSToolbox -> *OSToolbox
osBeep toolbox
	= winBeep toolbox
