definition module Help

import	deltaIOSystem
from	deltaEventIO	import :: IOState
from	StdFile			import :: Files

/*	General utility for handling the AboutDialog of a Clean application.
	This module uses the 0.8 I/O library.
*/

MakeAboutDialog	:: String String (*s -> *((IOState *s) -> (*s,IOState *s))) *Files
											 -> (DialogDef *s (IOState *s), *Files)
ShowHelp		:: String (IOState s) -> IOState s
