implementation module GenMonad

import StdGeneric, StdMaybe, StdList, StdFunc


generic gMapLM a b :: .a -> .m .b | Monad m
gMapLM{|c|} x 						= ret x
gMapLM{|PAIR|} fx fy (PAIR x y) 		= fx x >>= \x1 -> fy y >>= \y1 -> ret (PAIR x1 y1)  
gMapLM{|EITHER|} fl fr x 			= mapMEITHER fl fr x 
gMapLM{|CONS|} f (CONS x)			= f x >>= ret o CONS
gMapLM{|FIELD|} f (FIELD x)			= f x >>= ret o FIELD
 
generic gMapRM a b :: .a -> .m .b | Monad m
gMapRM{|c|} x 						= ret x
gMapRM{|PAIR|} fx fy (PAIR x y) 		= fy y >>= \y1 -> fx x >>= \x1 -> ret (PAIR x1 y1)  
gMapRM{|EITHER|} fl fr x 			= mapMEITHER fl fr x 
gMapRM{|CONS|} f (CONS x)			= f x >>= ret o CONS
gMapRM{|FIELD|} f (FIELD x)			= f x >>= ret o FIELD

mapMEITHER fl fr (LEFT x) 	= fl x >>= \x1 -> ret (LEFT x1) 
mapMEITHER fl fr (RIGHT x) 	= fr x >>= \x1 -> ret (RIGHT x1)  

derive gMapLM [], Maybe, (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
derive gMapRM [], Maybe, (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

//----------------------------------------------------------------------	
instance Monad Maybe where
	ret x = Just x
	(>>=) Nothing f = Nothing
	(>>=) (Just x) f = f x

instance Monad [] where
	ret x = [x]
	//(>>=) xs f = flatten (map f xs)	// uniqueness typing makes it a problem because f is shared
	(>>=) [x:xs] f = f x

//-----------------------
// state monad 

retStMonad :: .a -> .(StMonad .s .a)
retStMonad x = {st_monad = (\s -> (x, s))} 

bindStMonad :: !.(StMonad .a .b) .(.b -> .(StMonad .a .c)) -> .(StMonad .a .c)
bindStMonad {st_monad} f = {st_monad  = \s -> let (a, s1) = st_monad s in (f a).st_monad s1}

mapFst f (x, y) = (f x, y)
mapStMonad :: .(a:a -> .b) !v:(StMonad s:s a:a) -> .(StMonad s:s .b), [v <= a,v <= s]
mapStMonad f {st_monad} = {st_monad = mapFst f o st_monad}

instance Monad (StMonad .s) where
	ret x 		= retStMonad x
	(>>=) x f 	= bindStMonad x f

derive bimap (,)	
derive bimap StMonad 	