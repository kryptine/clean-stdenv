definition module StdGecComb

// The GecComb enables to connect a CGEC-circuit with a datastructure for which an editor can be created
// Any new inout value "a" will flow into the circuit and the result is shown in inout value "b"

import GecArrow, StdAGEC
derive gGEC GecComb


:: GecComb a b =	{ inout :: (a,b)
					, gec	 :: GecCircuit a b
					}

AGECtoCGEC :: String	(AGEC a) 		-> (GecCircuit a a) 	| gGEC{|*|} a		// Create CGEC in indicated window 
CGECtoAGEC :: 			(GecCircuit a a ) a 	-> (AGEC a) 	| gGEC{|*|} a		// Use CGEC as AGEC 

