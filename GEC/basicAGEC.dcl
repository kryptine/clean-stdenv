definition module basicAGEC

import StdClass
import StdAGEC
import modeAGEC, buttonAGEC, tupleAGEC, updownAGEC

// examples of abstract editors

idAGEC 			:: a   					-> AGEC a 		| gGEC {|*|} a				// identity editor  
hidAGEC 		:: a 					-> AGEC a 		 							// identity, no editor created
predAGEC 		:: (a -> (Bool,a)) a 		-> AGEC a 		| gGEC {|*|} a 				// id editor only accepts values satifying the given predicate
modeAGEC 		:: (Mode a) 			-> AGEC a 		| gGEC {|*|} a				// convert Mode to AGEC
applyAGEC 		:: (b -> a) (AGEC b) 	-> AGEC a 		| gGEC {|*|} a & gGEC {|*|} b // apply fba; show both b and a
counterAGEC		:: a					-> AGEC a		| gGEC {|*|} a & IncDec a 
horlistAGEC 	:: [a] 					-> AGEC [a]		| gGEC {|*|} a 				// all elements of a list displayed in a row
vertlistAGEC 	:: [a] 					-> AGEC [a] 	| gGEC {|*|} a 				// all elements of a list displayed in a column
vert2listAGEC 	:: a    [a] 			-> AGEC [a] 	| gGEC {|*|} a  			// column list, one can add a default val and delete elements at the tail
listAGEC 		:: Bool [a] 			-> AGEC [a] 	| gGEC {|*|} a				// list editor, set True for finite lists  
table_hv_AGEC	:: [[a]] 				-> AGEC [[a]] 	| gGEC {|*|} a  			// horizontal lists placed below each other
table_vh_AGEC	:: [[a]] 				-> AGEC [[a]] 	| gGEC {|*|} a  			// vertical lists placed next to each other

