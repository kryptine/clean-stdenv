definition module GecArrow

import StdGECExt

class Arrow arr
where
	arr :: (a -> b) -> arr a b
	(>>>) infixr 1 :: !(arr a b) !(arr b c) -> arr a c
	first :: !(arr a b) -> arr (a, c) (b, c)

	second :: !(arr a b) -> arr (c, a) (c, b)
	second gecab :== arr swap >>> first gecab >>> arr swap where swap t = (snd t, fst t)
	
	returnA :: arr a a
	returnA :== arr id
	
	(<<<<) infixr 1 :: !(arr b c) !(arr a b) -> arr a c
	(<<<<) l r :== r >>> l
	
	(***) infixr 3 :: !(arr a b) !(arr c d) -> arr (a, c) (b, d)
	(***) l r :== first l >>> second r
	
	(&&&) infixr 3 :: !(arr a b) !(arr a c) -> arr a (b, c)
	(&&&) l r :== arr (\x -> (x, x)) >>> (l *** r)
	
	(@>>) infixr 9 :: (a -> b) !(arr b c) -> arr a c
	(@>>) f gec :== arr f >>> gec
	 
	(<<@) infixr 9 :: !(arr a b) (b -> c) -> arr a c
	(<<@) gec f :== gec >>> arr f
	
class ArrowLoop arr
where
	loop :: !(arr (a, c) (b, c)) -> arr a b

:: GecArr a b

// Initialize GecArr circuit

gecCreate :: !(GecArr a b) a !*(PSt .ps) -> *PSt .ps

// Lift visual editors to GecArr's

gecEdit :: String -> GecArr a a | gGEC{|*|} a 
gecDisplay :: String -> GecArr a a | gGEC{|*|} a

// Arrow instance for GecArr

instance Arrow GecArr
instance ArrowLoop GecArr

// Other GecArr combinators

gecFix :: !(GecArr a a) -> GecArr a a
gecIO :: (A. .ps: a *(PSt .ps) -> *(b, *PSt .ps)) -> GecArr a b
