definition module tmfile

from	StdString	import String
from	StdFile		import Files
from	tm			import Turing, Transition, Tape, State, Head

WriteTuringToFile	:: Turing	!String !*Files	-> (!Bool,!*Files)
ReadTuring			::			!String !*Files	-> (!(!Int,!Turing),!*Files)
RemovePath			::			!String			-> String
