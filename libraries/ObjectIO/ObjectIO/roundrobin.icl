implementation module roundrobin


//	Clean Object I/O library, version 1.2


import	StdList, StdString
from	commondef	import FatalError


roundrobinFatalError :: String String -> .x
roundrobinFatalError rule error
	= FatalError rule "roundrobin" error

::	RR x
	=	{	done:: !.[x]		// The elements that are done (in reverse order)
		,	todo:: !.[x]		// The current element and the elements to do (in order)
		}

emptyRR :: RR .x
emptyRR = {done=[],todo=[]}

toRR :: !u:[.x] !u:[.x] -> u:RR .x
toRR done todo = {done=done,todo=todo}

fromRR :: !u:(RR .x) -> (!u:[.x],!u:[.x])
fromRR {done,todo} = (done,todo)

isEmptyRR :: !u:(RR .x) -> (!Bool,!u:RR .x)
isEmptyRR rr=:{done=[],todo=[]}	= (True, rr)
isEmptyRR rr					= (False,rr)

nodoneRR :: !u:(RR .x) -> (!Bool,!u:RR .x)
nodoneRR rr=:{done=[]}			= (True, rr)
nodoneRR rr						= (False,rr)

notodoRR :: !u:(RR .x) -> (!Bool,!u:RR .x)
notodoRR rr=:{todo=[]}			= (True, rr)
notodoRR rr						= (False,rr)

resetRR :: !u:(RR .x) -> u:RR .x
resetRR {done=[x:xs],todo}		= resetRR {done=xs,todo=[x:todo]}
resetRR rr						= rr

adddoneRR :: .x !u:(RR .x) -> u:RR .x
adddoneRR x rr=:{done}			= {rr & done=[x:done]}

inserttodoRR :: .x !u:(RR .x) -> u:RR .x
inserttodoRR x rr=:{todo}		= {rr & todo=[x:todo]}

appendtodoRR :: .x !u:(RR .x) -> u:RR .x
appendtodoRR x rr=:{todo}		= {rr & todo=todo++[x]}

getcurrentRR :: !u:(RR .x) -> (!.x,!u:RR .x)
getcurrentRR rr=:{todo=[current:todo]} = (current,{rr & todo=todo})
getcurrentRR _ = roundrobinFatalError "getcurrentRR" "todo field is empty"
