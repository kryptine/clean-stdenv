definition module osfileselect

//	Clean Object I/O library, version 1.2

import	StdString
from	ostoolbox	import OSToolbox
import	osevent

OSselectinputfile	:: !(OSEvent->.s->.s) !.s                 !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
OSselectoutputfile	:: !(OSEvent->.s->.s) !.s !String !String !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
OSselectdirectory	:: !(OSEvent->.s->.s) !.s                 !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
