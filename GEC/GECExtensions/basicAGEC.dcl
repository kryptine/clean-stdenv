definition module basicAGEC

import StdClass
import StdAGEC
import modeGEC, buttonGEC, tupleGEC, updownGEC

// examples of abstract editors

idGEC 			:: a   					-> AGEC a 		| gGEC {|*|} a				// identity editor  
hidGEC 			:: a 					-> AGEC a 		| gGEC {|*|} a 				// identity, no editor created
constGEC 		:: a 					-> AGEC a 					 				// identity, no editor created, constant
modeGEC 		:: (Mode a) 			-> AGEC a 		| gGEC {|*|} a				// convert Mode to AGEC
applyAGEC 		:: (b -> a) (AGEC b) 	-> AGEC a 		| gGEC {|*|} a & gGEC {|*|} b // apply fba; show both b and a
counterGEC		:: a					-> AGEC a		| gGEC {|*|} a & IncDec a 
horlistGEC 		:: [a] 					-> AGEC [a]		| gGEC {|*|} a 				// all elements of a list displayed in a row
vertlistGEC 	:: [a] 					-> AGEC [a] 	| gGEC {|*|} a 				// all elements of a list displayed in a column
listGEC 		:: Bool [a] 			-> AGEC [a] 	| gGEC {|*|} a				// list editor, set True for finite lists  
tableGEC 		:: [[a]] 				-> AGEC [[a]] 	| gGEC {|*|} a  			// a vertical list of horizontal lists
