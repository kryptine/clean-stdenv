implementation module GecArrow

import StdGECExt

:: GecArr a b = GecArr !.(A. .ps: (GecSet b ps) (GecGet a ps) *(PSt ps) -> *(GecSet a ps, GecGet b ps, *PSt ps))

:: GecSet a ps :== IncludeUpdate a *(PSt ps) -> *PSt ps
:: GecGet a ps :== *(PSt ps) -> *(a, *PSt ps)

gecCreate :: !(GecArr a b) a !*(PSt .ps) -> *PSt .ps
gecCreate (GecArr g) a env = env1
where
	(seta, getb, env1) = g setb geta env

	geta env = (a, env)

	setb _ _ env = env

gecEdit :: String -> GecArr a a | gGEC{|*|} a 
gecEdit title = GecArr k
where
	k seta geta env = (gecSetValue, gecGetValue, env2)
	where
		(a, env1) = geta env
		({gecGetValue, gecSetValue}, env2) = createNGEC title Interactive a (\r -> seta (includeUpdate r)) env1

gecDisplay :: String -> GecArr a a | gGEC{|*|} a 
gecDisplay title = GecArr k
where
	k seta geta env = (gecSetValue, gecGetValue, env2)
	where
		(a, env1) = geta env
		({gecGetValue, gecSetValue}, env2) = createNGEC title OutputOnly a (\r -> seta (includeUpdate r)) env1

instance Arrow GecArr
where
	arr f = GecArr k
	where
		k setb geta env = (seta, getb, env)
		where
			getb env = (f a, env1)
			where
				(a, env1) = geta env
			
			seta u a env = setb u (f a) env
	
	(>>>) (GecArr l) (GecArr r) = GecArr k
	where 
		k setc geta env = (seta, getc, env2)
		where
			(seta, getb, env1) = l setb geta env
			(setb, getc, env2) = r setc getb env1
	
	first (GecArr g) = GecArr k
	where
		k setbc getac env = (setac, getbc, env1)
		where
			(seta, getb, env1) = g setb geta env
			
			geta env = (a, env1)
			where
				((a, _), env1) = getac env
	
			getbc env = ((b, c), env2)
			where
				(b, env1) = getb env
				((_, c), env2) = getac env1
			
			setb u b env = setbc u (b, c) env1
			where
				((_, c), env1) = getac env
	
			setac u (a, c) env = setbc u (b, c) (seta u a env1)
			where
				(b, env1) = getb env
	
instance ArrowLoop GecArr
where
	loop (GecArr g) = GecArr k
	where 
		k setb geta env = (seta, getb, env1)
		where
			(setac, getbc, env1) = g setbc getac env
			
			getb env = (b, env1)
			where
				((b, _), env1) = getbc env
	
			getac env = ((a, c), env2)
			where
				(a, env1) = geta env
				((_, c), env2) = getbc env1
	
			seta u a env = setac u (a, c) env1
			where
				((_, c), env1) = getbc env
	
			setbc u (b, c) env = setb u b (setac NoUpdate (a, c) env1)
			where
				(a, env1) = geta env
			
gecFix :: !(GecArr a a) -> GecArr a a
gecFix (GecArr g) = GecArr k
where 
	k seta geta env = (seta`, geta`, env1)
	where
		(seta`, geta`, env1) = g seta`` geta env
		
		seta`` r a env = seta r a (seta` NoUpdate a env)

gecIO :: (A. .ps: a *(PSt .ps) -> *(b, *PSt .ps)) -> GecArr a b
gecIO f = GecArr k
where
	k setb geta env = (seta, getb, env)
	where
		getb env = f a env1
		where
			(a, env1) = geta env

		seta u a env = setb u b env1
		where
			(b, env1) = f a env

includeUpdate :: !UpdateReason -> *IncludeUpdate
includeUpdate Changed = YesUpdate
includeUpdate _ = NoUpdate
