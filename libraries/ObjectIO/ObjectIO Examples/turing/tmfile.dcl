definition module tmfile

from	StdString	import String
from	StdFile		import FileSystem
from	tm			import Turing, Transition, Tape, State, Head

WriteTuringToFile	:: Turing	!String !*env	-> (!Bool,!*env)           | FileSystem env
ReadTuring			::			!String !*env	-> (!(!Int,!Turing),!*env) | FileSystem env
RemovePath			::			!String			-> String
