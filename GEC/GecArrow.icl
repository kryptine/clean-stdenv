implementation module GecArrow

import StdArrow, StdGECExt
import store, StdDebug

:: GecCircuit a b = GecCircuit (A. .ps: (GecSet b ps) (GecGet a ps) *(PSt ps) -> *(GecSet a ps, GecGet b ps, *PSt ps))

:: GecSet a ps :== IncludeUpdate a *(PSt ps) -> *PSt ps
:: GecGet a ps :== *(PSt ps) -> *(a, *PSt ps)

startCircuit :: !(GecCircuit a b) a *(PSt .ps) -> *PSt .ps
startCircuit (GecCircuit k) a env
	# (seta, getb, env) = k setb geta env
	  env = seta YesUpdate a env
	= env
where
	geta env = (a, env)
	setb _ _ env = env

edit :: String -> GecCircuit a a | gGEC{|*|} a 
edit title = GecCircuit k
where
	k seta geta env
		# (a, env) = geta env
		  ({gecGetValue, gecSetValue}, env) = createNGEC title Interactive True a (\r -> seta (includeUpdate r)) env
		= (gecSetValue, gecGetValue, env)

display :: String -> GecCircuit a a | gGEC{|*|} a 
display title = GecCircuit k
where
	k seta geta env
		# (a, env) = geta env
		  ({gecGetValue, gecSetValue}, env) = createNGEC title OutputOnly True a (\r -> seta (includeUpdate r)) env
		= (gecSetValue, gecGetValue, env)

gecMouse :: String -> GecCircuit a MouseState
gecMouse title = GecCircuit k
where
	k seta geta env
		# (a, env) = geta env
		  ({gecGetValue, gecSetValue}, env) = createMouseGEC title Interactive (\r -> seta (includeUpdate r)) env
		= (\_ _ env -> env , gecGetValue, env)

instance Arrow GecCircuit
where
	arr f = GecCircuit k
	where
		k setb geta env = (seta, getb, env)
		where
			getb env 
				# (a, env) = geta env
				= (f a, env)
			
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
				# (ac, env) = getac env
				= (fst ac, env)
	
			getbc env 
				# (b, env) = getb env
				  (ac, env) = getac env
				= ((b, snd ac), env)
			
			setb u b env 
				# (ac, env) = getac env
				= setbc u (b, snd ac) env
	
			setac u ac env
			 	# env = seta u (fst ac) env
				  (b, env) = getb env
				= setbc u (b, snd ac) env

instance ArrowLoop GecCircuit
where
	loop (GecCircuit g) = GecCircuit k
	where
		k setc geta env = (seta, getc, env4)
		where
			(id, env1) = openStoreId env
			(setab, getcb, env2) = g setcb getab env1
			(cb, env3) = getcb env2
			(_, env4) = openStore id (Just (snd cb)) env3
	
			setcb u cb env
				# env = writeStore id (snd cb) env
				= setc u (fst cb) env
			
			getab env
				# (a, env) = geta env
				  (b, env) = getStore id env
				= ((a, b), env)
			
			seta u a env
				# (b, env) = getStore id env
				= setab u (a, b) env
	
			getc env
				# (cb, env) = getcb env
				= (fst cb, env)

		getStore id env
			# (ok, env) = valueStored id env
			| not ok = (cycle, env)
			= readStore id env
		where
			cycle => abort "Run-time error: cycle in loop detected"

instance ArrowCircuit GecCircuit
where
	delay a = GecCircuit k
	where
		k seta geta env = (seta`, geta`, env2)
		where
			(id, env1) = openStoreId env
			(_, env2) = openStore id (Just a) env1
				
			geta` env 
				# (a, env) = readStore id env
				= (a, env)

			seta` u a` env
				# (a, env) = readStore id env
				  env = seta u a env
				= writeStore id a` env

feedback :: !(GecCircuit a a) -> GecCircuit a a
feedback (GecCircuit g) = GecCircuit k
where 
	k seta geta env
		# (a, env) = geta` env1
		  env = seta` NoUpdate a env
		= (seta`, geta`, env)
	where
		(seta`, geta`, env1) = g seta`` geta env

		seta`` u a env = seta u a (seta` NoUpdate a env)

gecIO :: (A. .ps: a *(PSt .ps) -> *(b, *PSt .ps)) -> GecCircuit a b
gecIO f = GecCircuit k
where
	k setb geta env = (seta, getb, env)
	where
		getb env 
			# (a, env) = geta env
			= f a env

		seta u a env 
			# (b, env) = f a env
			= setb u b env

includeUpdate :: !UpdateReason -> *IncludeUpdate
includeUpdate Changed = YesUpdate
includeUpdate _ = NoUpdate
