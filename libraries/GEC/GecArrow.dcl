definition module GecArrow

import StdGECExt

class Arrow arr
where
	arr :: (a -> b) -> arr a b
	(>>>) infixr 1 :: !(arr a b) !(arr b c) -> arr a c
	first :: !(arr a b) -> arr (a, c) (b, c)

	second :: !(arr a b) -> arr (c, a) (c, b)
	second gecab :== gecArr swap |>>>| gecFirst gecab |>>>| gecArr swap where swap t = (snd t, fst t)

	returnA :: arr a a
	returnA :== arr id
	
	(<<<<) infixr 1 :: !(arr b c) !(arr a b) -> arr a c
	(<<<<) l r :== r >>> l

	(***) infixr 3 :: !(arr a b) !(arr c d) -> arr (a, c) (b, d)
	(|***|) l r :== first l >>> second r

	(&&&) infixr 3 :: !(arr a b) !(arr a c) -> arr a (b, c)
	(&&&) l r :== arr (\x -> (x, x)) >>> (l *** r)


class ArrowLoop arr
where
	loop :: !(arr (a, c) (b, c)) -> arr a b

:: GecArr a b

// Initialize GecArr circuit

gecCreate :: !(GecArr a b) a !*(PSt .ps) -> (b, !*PSt .ps)

// Lift visual editors to GecArr's

gecEdit :: String -> GecArr a a | gGEC{|*|} a 
gecDisplay :: String -> GecArr a a | gGEC{|*|} a

// Arrow instance for GecArr

instance Arrow GecArr
instance ArrowLoop GecArr

// Other GecArr combinators


(@>>) infixl 6 :: (a -> b) !(GecArr b c) -> GecArr a c
(<<@) infixl 6 :: !(GecArr a b) (b -> c) -> GecArr a c

gecFix :: !(GecArr a a) -> GecArr a a
