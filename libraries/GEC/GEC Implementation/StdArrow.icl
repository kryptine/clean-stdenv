implementation module StdArrow

from StdFunc import id
from StdTuple import fst, snd

instance Arrow (->)
where
	arr f = \x -> f x
	(>>>) l r = \x -> r (l x)
	first f = \(x, y) -> (f x, y)

instance ArrowApply (->)
where
	app = \(f, x) -> f x

instance ArrowLoop (->)
where
	loop f = \a -> let cb = f (a, snd cb) in fst cb
