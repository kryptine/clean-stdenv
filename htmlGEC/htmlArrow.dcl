definition module htmlArrow

// arrow implementation for Html GEC's
// (c) 2005 MJP


import htmlDataDef, htmlHandler
import StdArrow

// GEC circuit from a to b, for Html

:: GecCircuit a b

instance Arrow GecCircuit

// all "real" editors in the circuit will add a piece of html code in [Body]

startCircuit 	:: (GecCircuit a b) a *HSt -> (Form b,*HSt) 

// an editor shows the value which can be modified by the application user using a browser
// a display just shows the value
// a store applies the function to the stored value

edit 			:: FormId 	-> GecCircuit a a 			|  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
display 		:: FormId 	-> GecCircuit a a 			|  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
store 			:: FormId a -> GecCircuit (a -> a) a 	|  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
	
feedback 		:: (GecCircuit a b) (GecCircuit b a) -> (GecCircuit a b)

self 			:: (a -> a) (GecCircuit a a) -> GecCircuit a a

(`bindC`)  infix 0 :: (GecCircuit a b) (b -> GecCircuit b c) -> (GecCircuit a c)
(`bindCI`) infix 0 :: (GecCircuit a b) ((Form b) -> GecCircuit b c) -> (GecCircuit a c)

// to lift library functions to the circuit domain

lift :: FormId (FormId a *HSt -> (Form b,*HSt)) -> (GecCircuit a b)
