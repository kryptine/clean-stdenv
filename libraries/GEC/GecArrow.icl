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
		# ({gecGetValue, gecSetValue}, env2) = createNGEC title Interactive True a (\r -> seta (includeUpdate r)) env1
		= (gecSetValue, gecGetValue, env2)

display :: String -> GecCircuit a a | gGEC{|*|} a 
display title = GecCircuit k
where
	k seta geta env
		# (a, env1) = geta env
		# ({gecGetValue, gecSetValue}, env2) = createNGEC title OutputOnly False a (\r -> seta (includeUpdate r)) env1
		= (gecSetValue, gecGetValue, env2)

gecMouse :: String -> GecCircuit a MouseState
gecMouse title = GecCircuit k
where
	k seta geta env
		# (a, env1) = geta env
		# ({gecGetValue, gecSetValue}, env2) = createMouseGEC title Interactive (\r -> seta (includeUpdate r)) env1
		= (/*gecSetValue*/\upd a pSt -> pSt , gecGetValue, env2)

/*
gecMouse title = GEC \rec=:{set} pst ->
	let ({gecGetValue, gecSetValue}, pst1) = createMouseGEC title Interactive (\r -> set (maybeUpdate r)) pst
	in  ({value	= MouseLost, get = gecGetValue, set = \upd a pSt -> pSt /*gecSetValue*/}, pst1)

*/

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
