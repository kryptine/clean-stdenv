implementation module StdMaybe

//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdMaybe defines the Maybe type.
//	********************************************************************************

from	StdFunc			import St
from	StdOverloaded	import ==
from	StdIOBasic		import IdFun

::	Maybe x
	=	Just x
	|	Nothing

isJust :: !(Maybe .x) -> Bool
isJust Nothing	= False
isJust _		= True

isNothing :: !(Maybe .x) -> Bool
isNothing Nothing	= True
isNothing _		= False

u_isJust :: !u:(Maybe v:x) -> (!Bool, !u:(Maybe v:x)), [u<=v]
u_isJust nothing=:Nothing
	= (False, nothing)
u_isJust just
	= (True, just)

u_isNothing :: !u:(Maybe v:x) -> (!Bool, !u:(Maybe v:x)), [u<=v]
u_isNothing nothing=:Nothing
	= (True, nothing)
u_isNothing just
	= (False,just)

fromJust :: !(Maybe .x) -> .x
fromJust (Just x) = x

accMaybe :: .(St u:x .a) !v:(Maybe u:x) -> (!Maybe .a,!v:(Maybe u:x)), [v<=u]
accMaybe f (Just x)
	# (a,x) = f x
	= (Just a,Just x)
accMaybe _ nothing
	= (Nothing,nothing)

mapMaybe :: .(.x -> .y) !(Maybe .x) -> Maybe .y
mapMaybe f (Just x) = Just (f x)
mapMaybe _ nothing  = Nothing

instance == (Maybe x) | == x where
	(==) Nothing  maybe	= case maybe of
							Nothing -> True
							just    -> False
	(==) (Just a) maybe	= case maybe of
							Just b  -> a==b
							nothing -> False
