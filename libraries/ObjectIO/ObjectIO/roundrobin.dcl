definition module roundrobin


//	Clean Object I/O library, version 1.2


::	RR x
	=	{	done:: !.[x]		// The elements that are done (in reverse order)
		,	todo:: !.[x]		// The current element and the elements to do (in order)
		}

emptyRR		::					RR .x				// emptyRR                      = {[],[]}

toRR		:: ![.x] ![.x]	 -> RR .x				// toRR done todo               = {done,todo}
fromRR		::		!(RR .x) -> (![.x],![.x])		// fromRR {done,todo}           = (done,todo)

isEmptyRR	::		!(RR .x) -> (!Bool,!RR .x)		// isEmptyRR rr                 = rr==emptyRR
nodoneRR	::		!(RR .x) -> (!Bool,!RR .x)		// nodoneRR  rr                 = rr=={[],_}
notodoRR	::		!(RR .x) -> (!Bool,!RR .x)		// notodoRR  rr                 = rr=={_,[]}

resetRR		::		!(RR .x) -> RR .x				// resetRR        {done,todo}   = {[],(reverse done)++todo}
adddoneRR	:: .x	!(RR .x) -> RR .x				// adddoneRR    x {done,todo}   = {[x:done],todo}
inserttodoRR:: .x	!(RR .x) -> RR .x				// inserttodoRR x {done,todo}   = {done,[x:todo]}
appendtodoRR:: .x	!(RR .x) -> RR .x				// appendtodoRR x {done,todo}   = {done,todo++[x]}
getcurrentRR::		!(RR .x) -> (!.x,!RR .x)		// getcurrentRR {done,[x:todo]} = (x,{done,todo})
