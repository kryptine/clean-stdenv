implementation module StdGecComb

import genericgecs
import StdGECExt, store, StdFunc, StdMisc
import StdAGEC
import modeAGEC, tupleAGEC
import GecArrow
/*

gecConst :: b -> CGEC a b 
gecConst b = GEC \recba  pst ->
		(	{	value	= b
			,	get		= \pSt -> (b,pSt)
			,	set		= \upd a pSt -> pSt
			}, pst)
*/


AGECtoCGEC :: String (AGEC a) -> (GecCircuit a a) | gGEC{|*|} a
AGECtoCGEC sa agec =  (\a -> agec ^= a) @>> edit sa >>@ (\agec -> (^^ agec))

CGECtoAGEC :: (GecCircuit a a ) a -> (AGEC a) | gGEC{|*|} a
CGECtoAGEC cgec a 
= mkAGEC { toGEC   = \a _ -> {inout = (Hide a,Hide a), gec = (\(Hide a) -> a) @>> cgec >>@ (\a -> Hide a)}
		 , fromGEC = \{inout = (a,Hide b)} = b
		 , updGEC  = id
		 , value   = a
		 } "CGECtoAGEC"


gGEC{|GecComb|} gGECa gGECb args pSt
	= abort "Cannot make up function value for DataGec"	

