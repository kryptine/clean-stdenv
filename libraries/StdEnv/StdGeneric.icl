implementation module StdGeneric

generic bimap a b :: Bimap .a .b

id x = x
bimapId :: Bimap .a .a
bimapId = { map_to = id, map_from = id }

(o) infixr  9  :: u:(.a -> .b) v:(.c -> .a) -> w:(.c -> .b), [w <= u,w <= v]
(o) f g = \x-> f (g x)

bimap{|c|} = { map_to = id, map_from = id }
bimap{|PAIR|} bx by = { map_to= map_to, map_from=map_from }
where
	map_to (PAIR x y) 	= PAIR (bx.map_to x) (by.map_to y)
	map_from (PAIR x y) 	= PAIR (bx.map_from x) (by.map_from y)
bimap{|EITHER|} bl br = { map_to= map_to, map_from=map_from }
where	
	map_to (LEFT x) 	= LEFT (bl.map_to x)
	map_to (RIGHT x)	= RIGHT (br.map_to x)
	map_from (LEFT x) 	= LEFT (bl.map_from x)
	map_from (RIGHT x) 	= RIGHT (br.map_from x)

bimap{|(->)|} barg bres = { map_to = map_to, map_from = map_from }
where
	//map_to f	x	= bres.map_to (f (barg.map_from x)) 
	//map_from f x	= bres.map_from (f (barg.map_to x)) 
	map_to f		= bres.map_to o f o barg.map_from
	map_from f 		= bres.map_from o f o barg.map_to

bimap{|CONS|} barg = { map_to= map_to, map_from=map_from }
where
	map_to   (CONS x) = CONS (barg.map_to x)
	map_from (CONS x) = CONS (barg.map_from x)

bimap{|FIELD|} barg = { map_to= map_to, map_from=map_from }
where
	map_to   (FIELD x) = FIELD (barg.map_to x)
	map_from (FIELD x) = FIELD (barg.map_from x)
	
bimap{|Bimap|} x y = {map_to = map_to, map_from = map_from}
where
	map_to 	{map_to, map_from} = 
		{ map_to 	= y.map_to o map_to o x.map_from
		, map_from 	= x.map_to o map_from o y.map_from
		}
	map_from {map_to, map_from} = 
		{ map_to 	= y.map_from o map_to o x.map_to
		, map_from 	= x.map_from o map_from o y.map_to
		}
 	 							 	