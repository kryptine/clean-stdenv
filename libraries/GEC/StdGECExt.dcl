definition module StdGECExt

import StdIO
import genericgecs//, StdBimap
	
:: PState a ps :== a *(PSt ps) -> *(PSt ps)

//	createGEC  will store "a" in a GEC box, will call the given call back function
//  which will be called when the GEC box has changed (see UpdateReason parameter).
//	the first  set function returned can be used to store a new value in the GEC box causing no call back call
//	the second set function returned can be used to store a new value in the GEC box  do causing a call back call

createGEC      :: String OutputOnly a (Update a (PSt .ps)) *(PSt .ps) -> *(PState a .ps,PState a .ps,*(PSt .ps)) | gGEC{|*|} a & bimap{|*|} ps
createNGEC 	   :: String OutputOnly a (Update a (PSt .ps)) *(PSt .ps) -> *((GECVALUE a *(PSt .ps)),*(PSt .ps)) | gGEC{|*|} a & bimap{|*|} ps

mk_GEC         :: !(!String,!a) 												!*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a 
apply_GEC      :: !(!String,a -> b) 	!(!String,!a) 							!*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a & gGEC{|*|} b
apply2_GEC     :: !(!String,b -> c) 	!(!String,a -> b)	!(!String,!a) 		!*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a & gGEC{|*|} b & gGEC{|*|} c 
apply_GEC2     :: !(!String,a b -> c) 	!(!String,!a) 		!(!String,!b)  		!*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a & gGEC{|*|} b & gGEC{|*|} c
self_GEC       :: !(a -> a) 			!(!String,!a) 							!*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a 
selfapply_GEC  :: !(a b -> a)			!(!String,!a) 		!(!String,!b) 		!*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a & gGEC{|*|} b 
pred_GEC       :: !(a -> Bool) 			!(!String,!a) 							!*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a
mutual_GEC     :: !a					!(!String,b -> a) 	!(!String,a -> b)  	!*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a & gGEC{|*|} b 

selfStateGEC   :: !(a s -> (a,s))		!(!String,!a) s							!*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a 
selfStateGECps :: !(a s *(PSt .ps) -> (a,s,*(PSt .ps)))
										!(!String,!a) s							!*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a 
timer_GEC 	   :: !((!Int,!a)->(!Int,!a)) !(!String,(!Int,!a)) 					!*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a 
