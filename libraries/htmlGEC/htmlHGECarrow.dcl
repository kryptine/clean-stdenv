definition module htmlHGECarrow

// arrow implementation for Html GEC's
// (c) 2005 MJP


import htmlDataDef, htmlHandler
import StdArrow

// GEC circuit from a to b, for Html

:: GecCircuit a b

instance Arrow GecCircuit

// all "real" editors in the circuit will deliver a piece of html code in [Body]

startCircuit 	:: (GecCircuit a b) a HSt -> ((b,[Body]),HSt) 

edit 			:: String -> GecCircuit a a |  gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
display 		:: String -> GecCircuit a a |  gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
	
feedback 		:: (GecCircuit a b) (GecCircuit b a) -> (GecCircuit a b)

self 			:: (a -> a) (GecCircuit a a) -> GecCircuit a a

