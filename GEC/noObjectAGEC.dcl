definition module noObjectAGEC

import StdAGEC

derive gGEC NoObject

::	NoObject a = NoObject a

noObjectAGEC :: a -> AGEC a	| gGEC{|*|} a	// identity, no OBJ pulldown menu constructed.
