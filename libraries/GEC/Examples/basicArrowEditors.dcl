definition module basicArrowEditors

/*********************************************************************************
*                                                                                *
*   This module contains some handy basic editors based an GEC arrow combinators *
*                                                                                *
*********************************************************************************/

import StdGEC
                                               
startGEC 	:: ((PSt Void) -> (PSt Void)) *World -> *World

mkGEC 		:: String  t 			      (PSt ps) -> (PSt ps) | gGEC{|*|} t & bimap{|*|} ps
selfGEC 	:: String (t -> t) t 		  (PSt ps) -> (PSt ps) | gGEC{|*|} t & bimap{|*|} ps
applyGECs 	:: (String,String) (a -> b) a (PSt ps) -> (PSt ps) | gGEC{|*|} a & gGEC{|*|} b & bimap{|*|} ps
apply2GECs  :: (String,String,String) (a -> b -> c) a b 
										  (PSt ps) -> (PSt ps) | gGEC{|*|} a & gGEC{|*|} b & gGEC{|*|} c & bimap{|*|} ps                     
mutualGEC :: (String,String)(a -> b) (b -> a) a
		  								   (PSt ps) -> (PSt ps) | gGEC{|*|} a & gGEC{|*|} b & bimap{|*|} ps                     
