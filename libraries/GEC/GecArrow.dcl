definition module GecArrow

import StdArrow, StdGECExt

:: GecCircuit a b

// Initialize GecCircuit circuit

startCircuit :: !(GecCircuit a b) a *(PSt .ps) -> *PSt .ps

// Lift visual editors to GecCircuit's

edit 		:: String -> GecCircuit a a | gGEC{|*|} a
display 	:: String -> GecCircuit a a | gGEC{|*|} a
gecMouse	:: String -> GecCircuit a MouseState					// Assign a mouse to a fresh window

// Arrow instance for GecCircuit

instance Arrow GecCircuit
instance ArrowChoice GecCircuit
instance ArrowLoop GecCircuit
instance ArrowCircuit GecCircuit

// Other GecCircuit combinators

probe :: String -> GecCircuit a a | toString a

self :: (GecCircuit a a) (GecCircuit a a) -> GecCircuit a a
feedback :: (GecCircuit a a) -> GecCircuit a a

sink :: GecCircuit a Void
source :: (GecCircuit a b) -> GecCircuit Void b
flowControl :: (IncludeUpdate -> a -> Maybe (IncludeUpdate, b)) -> GecCircuit a b

gecIO :: (A. .ps: a *(PSt .ps) -> *(b, *PSt .ps)) -> GecCircuit a b

derive generate GecCircuit
