definition module GenMonad

import StdGeneric, StdMaybe, StdList

class Monad m where
	ret :: .a -> .(m .a)
 	(>>=) infixl 5 :: .(m .a) .(.a -> .(m .b))  -> .(m .b)

:: StMonad s a = { st_monad :: .(s -> *(a, s)) }
derive bimap StMonad
instance Monad Maybe, [], (StMonad .s)

generic gMapLM a b :: .a -> .m .b | Monad m
derive gMapLM c, PAIR, EITHER, CONS, FIELD
derive gMapLM [], Maybe, (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

generic gMapRM a b :: .a -> .m .b | Monad m
derive gMapRM c, PAIR, EITHER, CONS, FIELD
derive gMapRM [], Maybe, (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

