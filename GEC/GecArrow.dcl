definition module GecArrow

import StdGECExt

class Arrow arr
where
	arr :: (a -> b) -> arr a b
	(>>>) infixr 1 :: !(arr a b) !(arr b c) -> arr a c
	first :: !(arr a b) -> arr (a, c) (b, c)

//second :: (arr a b) -> arr (c, a) (c, b)
second g :== arr swap >>> first g >>> arr swap where swap t = (snd t, fst t)

//returnA :: arr a a
returnA :== arr id

(<<<<) infixr 1 //:: (arr b c) (arr a b) -> arr a c
(<<<<) l r :== r >>> l

(***) infixr 3 //:: (arr a b) (arr c d) -> arr (a, c) (b, d)
(***) l r :== first l >>> second r

(&&&) infixr 3 //:: (arr a b) (arr a c) -> arr a (b, c)
(&&&) l r :== arr (\x -> (x, x)) >>> (l *** r)

(@>>) infixr 8 //:: (a -> b) (arr b c) -> arr a c
(@>>) f g :== arr f >>> g
 
(>>@) infixr 8 //:: (arr a b) (b -> c) -> arr a c
(>>@) g f :== g >>> arr f

class ArrowLoop arr | Arrow arr
where
	loop :: !(arr (a, b) (c, b)) -> arr a c

//fix :: (arr (a, b) b) -> arr a b
fix g :== loop (g >>> arr \b -> (b, b))

class ArrowCircuit arr | ArrowLoop arr
where
	delay :: a -> arr a a
	
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
