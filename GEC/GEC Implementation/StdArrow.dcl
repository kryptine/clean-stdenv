definition module StdArrow

from StdFunc import id
from StdTuple import fst, snd

class Arrow arr
where
	arr :: (a -> b) -> arr a b
	(>>>) infixr 1 :: !(arr a b) !(arr b c) -> arr a c
	first :: !(arr a b) -> arr (a, c) (b, c)

class ArrowApply arr | Arrow arr
where
	app :: arr (arr a b, a) b

class ArrowLoop arr | Arrow arr
where
	loop :: !(arr (a, b) (c, b)) -> arr a c

class ArrowCircuit arr | ArrowLoop arr
where
	delay :: a -> arr a a

//second :: (arr a b) -> arr (c, a) (c, b) | Arrow arr
second g :== arr swap >>> first g >>> arr swap where swap t = (snd t, fst t)

//returnA :: arr a a | Arrow arr
returnA :== arr id

(<<<<) infixr 1 //:: (arr b c) (arr a b) -> arr a c | Arrow arr
(<<<<) l r :== r >>> l

(***) infixr 3 //:: (arr a b) (arr c d) -> arr (a, c) (b, d) | Arrow arr
(***) l r :== first l >>> second r

(&&&) infixr 3 //:: (arr a b) (arr a c) -> arr a (b, c) | Arrow arr
(&&&) l r :== arr (\x -> (x, x)) >>> (l *** r)

//fix :: (arr (a, b) b) -> arr a b | ArrowLoop arr
fix g :== loop (g >>> arr \b -> (b, b))

instance Arrow (->)
instance ArrowApply (->)
instance ArrowLoop (->)
