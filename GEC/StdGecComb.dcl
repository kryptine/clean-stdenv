definition module StdGecComb

import genericgecs, StdAGEC

// A CGEC a b is a Cirquit of Graphical Editor Components with input a and output b
// A circuit consists of any combination of GEC-Editors, functions and constants
// A cirquit can be constructed with help of the CGEC Combinators below 

:: CGEC a b

CGEC :: !(CGEC a b) a *(PSt .ps) -> *PSt .ps		// the I/O function to display the CGEC-circuit given the initial value a

// To create a GEC circuit:

gecEdit 		:: String 	-> CGEC a a | gGEC{|*|} a				// Create a GEC editor  for type a 
gecDisplay 		:: String 	-> CGEC a a | gGEC{|*|} a 				// Create a GEC display for type a
gecFun 			:: (a -> b) -> CGEC a b 							// Promote function to invisible GEC
gecConst 		:: b 		-> CGEC a b 							// Promote constant value to invisible GEC ignoring any input

// Combinators to combine GEC circuits

(|>>>|) infixl 4 	:: !(CGEC a b) !(CGEC b c) 	-> CGEC a c 
(|<<<|) infixl 4 	:: !(CGEC b c) !(CGEC a b) 	-> CGEC a c 
gecfirst 			:: !(CGEC a b) 				-> CGEC (a,c) (b,c)
gecsecond 			:: !(CGEC a b) 				-> CGEC (c,a) (c,b) 
(|***|) infix  5 	:: !(CGEC a b) !(CGEC c d) 	-> CGEC (a, c) (b, d)
(|&&&|) infix  5 	:: !(CGEC a b) !(CGEC a c) 	-> CGEC a (b, c)
gecloop				:: !(CGEC (a,c) (b,c)) 		-> CGEC a b

(|&|)   infixl 4	:: !(CGEC a b) !(CGEC b c) 		-> CGEC a c				// A sequence of two Gecs 
(|<>|)  infix  5 	:: !(CGEC a b) !(CGEC c d) 		-> CGEC (a, c) (b, d)	// Split inputs, join outputs
(|>|)   infix  5 	:: !(CGEC a b) !(CGEC a c) 		-> CGEC a (b, c)		// Join outputs
%| 					:: !(CGEC a a) 					-> CGEC a a				// Construct feedback loop

(@|)    infixl 6 	:: (a -> b) !(CGEC b c) 		-> CGEC a c				// Convert input before
(|@)    infixl 6 	:: !(CGEC a b) (b -> c) 		-> CGEC a c				// Convert output after
(@|@)   infixl 6 	:: !(a -> b, b -> a) !(CGEC b b)-> CGEC a a				// Combination of previous

gecIO 				:: (A. .ps: a *(PSt .ps) -> *PSt .ps) -> CGEC a a	// For doing special I/O in between

// The GecComb enables to connect a CGEC-circuit with a datastructure for which an editor can be created
// Any new inout value "a" will flow into the circuit and the result is shown in inout value "b"

derive gGEC GecComb

:: GecComb a b =	{ inout :: (a,b)
					, gec	 :: CGEC a b
					}

AGECtoCGEC :: String (AGEC a) 	-> (CGEC a a) 	| gGEC{|*|} a
CGECtoAGEC :: (CGEC a a ) a 	-> (AGEC a) 	| gGEC{|*|} a

// Simple functions for frequently occuring combinations of circuits 						

mkGEC 			:: String						-> CGEC a a			| gGEC{|*|} a
applyGEC 		:: String (a -> b)				-> CGEC a b  		| gGEC{|*|} a & gGEC{|*|} b
apply2GEC 		:: String (b -> c) (a -> b)		-> CGEC a c			| gGEC{|*|} a & gGEC{|*|} b & gGEC{|*|} c 
applyGEC2 		:: String (a b -> c)			-> CGEC (a,b) c		| gGEC{|*|} a & gGEC{|*|} b & gGEC{|*|} c
selfGEC 		:: String (a -> a) 				-> CGEC a a			| gGEC{|*|} a 
mutualGEC 		:: String (b -> a)(a -> b) 		-> CGEC a a 		| gGEC{|*|} a & gGEC{|*|} b 
predGEC 		:: String (a -> Bool) 			-> CGEC a a 		| gGEC{|*|} a
//selfState_GECps :: (A..ps : a -> .(s -> .(*(PSt .ps) -> *(a,s,*(PSt .ps))))) !(!String,!a) s 
//																		-> CGEC (a,(Mode s)) (a,(Mode s)) | gGEC{|*|} a & gGEC{|*|} s 
