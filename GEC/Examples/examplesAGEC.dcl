definition module examplesAGEC

import StdClass
import StdAGEC, modeGEC, buttonGEC, tupleGEC, updownGEC

// examples of abstract editors

idGEC 			:: a   					-> AGEC a 		| gGEC {|*|} a				// identity editor  
hidGEC 			:: a 					-> AGEC a 		| gGEC {|*|} a 				// identity, no editor created
constGEC 		:: a 					-> AGEC a 					 				// identity, no editor created, constant
modeGEC 		:: (Mode a) 			-> AGEC a 		| gGEC {|*|} a				// convert Mode to AGEC
applyAGEC 		:: (b -> a) (AGEC b) 	-> AGEC a 		| gGEC {|*|} a & gGEC {|*|} b // apply fba; show both b and a
horlistGEC 		:: [a] 					-> AGEC [a]		| gGEC {|*|} a 				// all elements of a list displayed in a row
vertlistGEC 	:: [a] 					-> AGEC [a] 	| gGEC {|*|} a 				// all elements of a list displayed in a column
tableGEC 		:: [[a]] 				-> AGEC [[a]] 	| gGEC {|*|} a  			// a vertical list of horizontal lists

counterGEC 		:: a   					-> AGEC a 		| IncDec a & gGEC {|*|} a	// counter with UpDown buttons 
calcGEC 		:: a [[(Button,a->a)]] 	-> AGEC a 		| gGEC {|*|} a 				// apply pressed function to argument
listGEC 		:: Bool [a] 			-> AGEC [a] 	| gGEC {|*|} a				// list editor, set True for finite lists  

intcalcGEC		:: Int 					-> AGEC Int									// create Int calculator with calculator buttons
realcalcGEC 	:: Real 				-> AGEC Real								// create Real calculator with calculator buttons
