definition module htmlHGECarrow

// arrow implementation for Html GEC's
// (c) 2005 MJP


import htmlDataDef, htmlHandler
import StdArrow

// GEC circuit from a to b, for Html

:: GecCircuit a b

instance Arrow GecCircuit

// all "real" editors in the circuit will add a piece of html code in [Body]

startCircuit 	:: (GecCircuit a b) a *HSt -> ((b,[Body]),*HSt) 

// an editor shows the value which can be modified by the application user using a browser
// a display just shows the value
// a store applies the function to the stored value

edit 			:: String 	-> GecCircuit a a 			|  gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
display 		:: String 	-> GecCircuit a a 			|  gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
store 			:: String a -> GecCircuit (a -> a) a 	|  gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
	
feedback 		:: (GecCircuit a b) (GecCircuit b a) -> (GecCircuit a b)

self 			:: (a -> a) (GecCircuit a a) -> GecCircuit a a

// to lift library functions to the circuit domain

lift :: String HMode (String HMode a *HSt -> ((b,Body),*HSt)) -> (GecCircuit a b)
