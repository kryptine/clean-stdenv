implementation module GecArrow

import StdGECExt

:: GecCircuit a b = GecCircuit (A. .ps: (GecSet b ps) (GecGet a ps) *(PSt ps) -> *(GecSet a ps, GecGet b ps, *PSt ps))

:: GecSet a ps :== IncludeUpdate a *(PSt ps) -> *PSt ps
:: GecGet a ps :== *(PSt ps) -> *(a, *PSt ps)

startCircuit :: (GecCircuit a b) a *(PSt .ps) -> *PSt .ps
startCircuit (GecCircuit k) a env
	# (seta, getb, env1) = k setb geta env
	= env1
where
	geta env = (a, env)
	setb _ _ env = env

edit :: String -> GecCircuit a a | gGEC{|*|} a 
edit title = GecCircuit k
where
	k seta geta env
		# (a, env1) = geta env
		# ({gecGetValue, gecSetValue}, env2) = createNGEC title Interactive a (\r -> seta (includeUpdate r)) env1
		= (gecSetValue, gecGetValue, env2)

display :: String -> GecCircuit a a | gGEC{|*|} a 
display title = GecCircuit k
where
	k seta geta env
		# (a, env1) = geta env
		# ({gecGetValue, gecSetValue}, env2) = createNGEC title OutputOnly a (\r -> seta (includeUpdate r)) env1
		= (gecSetValue, gecGetValue, env2)

instance Arrow GecCircuit
where
	arr f = GecCircuit k
	where
		k setb geta env = (seta, getb, env)
		where
			getb env 
				# (a, env1) = geta env
				= (f a, env1)
			
			seta u a env = setb u (f a) env
	
	(>>>) (GecCircuit l) (GecCircuit r) = GecCircuit k
	where 
		k setc geta env = (seta, getc, env2)
		where
			(seta, getb, env1) = l setb geta env
			(setb, getc, env2) = r setc getb env1
	
	first (GecCircuit g) = GecCircuit k
	where
		k setbc getac env = (setac, getbc, env1)
		where
			(seta, getb, env1) = g setb geta env
			
			geta env 
				# ((a, c), env1) = getac env
				= (a, env1)
	
			getbc env 
				# (b, env1) = getb env
				# ((_, c), env2) = getac env1
				= ((b, c), env2)
			
			setb u b env 
				# ((_, c), env1) = getac env
				= setbc u (b, c) env1
	
			setac u (a, c) env
			 	# env1 = seta u a env
				# (b, env2) = getb env1
				= setbc u (b, c) env2

feedback :: (GecCircuit a a) -> GecCircuit a a
feedback (GecCircuit g) = GecCircuit k
where 
	k seta geta env 
		# (a, env1) = geta` env1
		# env1 = seta` NoUpdate a env1
		= (seta`, geta`, env1)
	where
		(seta`, geta`, env1) = g seta`` geta env

		seta`` u a env = seta u a (seta` NoUpdate a env)
/*
gecFixViaLoop` :: (GecCircuit a a) -> GecCircuit a a
gecFixViaLoop` g = arr double >>> loop` (arr snd >>> g >>> arr double)
where
	double x = (x, x)

gecFixViaLoop`ViaGecLoop :: (GecCircuit a a) -> GecCircuit a a
gecFixViaLoop`ViaGecLoop g = arr double >>> loop`ViaGecLoop (arr snd >>> g >>> arr double)
where
	double x = (x, x)

gecFixViaLoop`ViaGecLoopViaLoop` :: (GecCircuit a a) -> GecCircuit a a
gecFixViaLoop`ViaGecLoopViaLoop` g = arr double >>> loop`ViaGecLoopViaLoop` (arr snd >>> g >>> arr double)
where
	double x = (x, x)

gecFixViaLoop`ViaLoop :: (GecCircuit a a) -> GecCircuit a a
gecFixViaLoop`ViaLoop g = arr double >>> loop`ViaLoop (arr snd >>> g >>> arr double)
where
	double x = (x, x)

/*gecLoop :: (GecCircuit (a, c) (b, c)) -> GecCircuit a b
gecLoop (GecCircuit g) = GecCircuit k
where 
	k setb geta env = (seta, getb, env1)
	where
		(setac, getbc, env1) = g setbc getac env
		
		getb env 
			# ((b, _), env1) = getbc env
			= (b, env1)

		getac env 
			# (a, env1) = geta env
			# ((_, c), env2) = getbc env1
			= ((a, c), env2)

		seta u a env 
			# ((_, c), env1) = getbc env
			= setac u (a, c) env1

		setbc u (b, c) env 
			# env1 = setb u b env
			# (a, env2) = geta env1
			= setac NoUpdate (a, c) env2*/

gecLoopViaLoop` :: (GecCircuit (a, c) (b, c)) -> GecCircuit a b
gecLoopViaLoop` g = arr (\a -> (a, Nothing)) >>> loop` (arr f >>> g >>> arr f`)
where 
	f (b, Just c) = (b, c)
	f (b, _) = (b, abort "loop cycle")
	f` (b, c) = (b, Just c)

loop`ViaGecLoop :: (GecCircuit (a, c) (b, c)) -> GecCircuit (a, c) b
loop`ViaGecLoop g = arr just >>> loop (arr maybe >>> g >>> arr nothing) >>> arr fst
where 
	just (a, c) = (a, Just c)
	nothing (b, c) = ((b, Nothing), c)
	maybe ((a, Just c), _) = (a, c)
	maybe ((a, _), c) = (a, c)

loop`ViaLoop :: (GecCircuit (a, c) (b, c)) -> GecCircuit (a, c) b
loop`ViaLoop g = arr just >>> loop (arr maybe >>> g >>> arr nothing) >>> arr fst
where 
	just (a, c) = (a, Just c)
	nothing (b, c) = ((b, Nothing), c)
	maybe ((a, Just c), _) = (a, c)
	maybe ((a, _), c) = (a, c)

loop`ViaGecLoopViaLoop` :: (GecCircuit (a, c) (b, c)) -> GecCircuit (a, c) b
loop`ViaGecLoopViaLoop` g = arr just >>> gecLoopViaLoop` (arr maybe >>> g >>> arr nothing) >>> arr fst
where 
	just (a, c) = (a, Just c)
	nothing (b, c) = ((b, Nothing), c)
	maybe ((a, Just c), _) = (a, c)
	maybe ((a, _), c) = (a, c)

loop` :: (GecCircuit (a, c) (b, c)) -> GecCircuit (a, c) b
loop` (GecCircuit g) = GecCircuit k
where 
	k setb getac env = (setac, getb, env1)
	where
		(setac, getbc, env1) = g setbc getac env
		
		getb env 
			# ((b, _), env1) = getbc env
			= (b, env1)

		setbc u (b, c) env 
			# env1 = setb u b env
			# ((a, _), env2) = getac env1
			= setac NoUpdate (a, c) env2
*/
gecIO :: (A. .ps: a *(PSt .ps) -> *(b, *PSt .ps)) -> GecCircuit a b
gecIO f = GecCircuit k
where
	k setb geta env = (seta, getb, env)
	where
		getb env 
			# (a, env1) = geta env
			= f a env1

		seta u a env 
			# (b, env1) = f a env
			= setb u b env1

includeUpdate :: !UpdateReason -> *IncludeUpdate
includeUpdate Changed = YesUpdate
includeUpdate _ = NoUpdate
