implementation module roundrobin


//	Clean Object I/O library, version 1.2


from	StdList		import ++
from	StdString	import String
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

toRR :: ![.x] ![.x] -> RR .x
toRR done todo = {done=done,todo=todo}

fromRR :: !(RR .x) -> (![.x],![.x])
fromRR {done,todo} = (done,todo)

isEmptyRR :: !(RR .x) -> (!Bool,!RR .x)
isEmptyRR rr=:{done=[],todo=[]}	= (True, rr)
isEmptyRR rr					= (False,rr)

nodoneRR :: !(RR .x) -> (!Bool,!RR .x)
nodoneRR rr=:{done=[]}			= (True, rr)
nodoneRR rr						= (False,rr)

notodoRR :: !(RR .x) -> (!Bool,!RR .x)
notodoRR rr=:{todo=[]}			= (True, rr)
notodoRR rr						= (False,rr)

resetRR :: !(RR .x) -> RR .x
resetRR {done=[x:xs],todo}		= resetRR {done=xs,todo=[x:todo]}
resetRR rr						= rr

adddoneRR :: .x !(RR .x) -> RR .x
adddoneRR x rr=:{done}			= {rr & done=[x:done]}

inserttodoRR :: .x !(RR .x) -> RR .x
inserttodoRR x rr=:{todo}		= {rr & todo=[x:todo]}

appendtodoRR :: .x !(RR .x) -> RR .x
appendtodoRR x rr=:{todo}		= {rr & todo=todo++[x]}

getcurrentRR :: !(RR .x) -> (!.x,!RR .x)
getcurrentRR rr=:{todo=[current:todo]} = (current,{rr & todo=todo})
getcurrentRR _ = roundrobinFatalError "getcurrentRR" "todo field is empty"
