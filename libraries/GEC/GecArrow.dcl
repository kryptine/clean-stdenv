definition module GecArrow

import StdArrow, StdGECExt

(@>>) infixr 8 //:: (a -> b) (arr b c) -> arr a c
(@>>) f g :== arr f >>> g
 
(>>@) infixr 8 //:: (arr a b) (b -> c) -> arr a c
(>>@) g f :== g >>> arr f

:: GecCircuit a b

// Initialize GecCircuit circuit

startCircuit :: !(GecCircuit a b) a *(PSt .ps) -> *PSt .ps

// Lift visual editors to GecCircuit's

edit 		:: String -> GecCircuit a a | gGEC{|*|} a 
display 	:: String -> GecCircuit a a | gGEC{|*|} a
gecMouse	:: String -> GecCircuit a MouseState					// Assign a mouse to a fresh window

// Arrow instance for GecCircuit

instance Arrow GecCircuit
instance ArrowLoop GecCircuit
instance ArrowCircuit GecCircuit

// Other GecCircuit combinators

//feedback :: !(GecCircuit a a) -> GecCircuit a a

gecIO :: (A. .ps: a *(PSt .ps) -> *(b, *PSt .ps)) -> GecCircuit a b
