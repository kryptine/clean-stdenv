implementation module GecArrow

import StdArrow, StdGECExt
import store

:: GecCircuit a b = GecCircuit !.(A. .ps: (GecSet b ps) (GecGet a ps) *(PSt ps) -> *(GecSet a ps, GecGet b ps, *PSt ps))

:: GecSet a ps :== IncludeUpdate a *(PSt ps) -> *PSt ps
:: GecGet a ps :== *(PSt ps) -> *(a, *PSt ps)

runCircuit (GecCircuit k) = k

startCircuit :: !(GecCircuit a b) a *(PSt .ps) -> *PSt .ps
startCircuit g a env
	# (seta, getb, env) = runCircuit g setb geta env
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
	
	(>>>) l r = GecCircuit k
	where 
		k setc geta env = (seta, getc, env2)
		where
			(seta, getb, env1) = runCircuit l setb geta env
			(setb, getc, env2) = runCircuit r setc getb env1
	
	first g = GecCircuit k
	where
		k setbc getac env = (setac, getbc, env1)
		where
			(seta, getb, env1) = runCircuit g setb geta env
			
			geta env 
				# (ac, env) = getac env
				= (fst ac, env)
	
			getbc env 
				# (ac, env) = getac env
				  (b, env) = getb env
				= ((b, snd ac), env)
			
			setb u b env
				# (ac, env) = getac env
				= setbc u (b, snd ac) env
	
			setac u ac env
				# env = seta u (fst ac) env
			 	  (b, env) = getb env
			 	= setbc NoUpdate (b, snd ac) env

instance ArrowChoice GecCircuit
where
	left g = GecCircuit k
	where
		k setbc getac env = (setac, getbc, env1)
		where
			(seta, getb, env1) = runCircuit g setb geta env

			setac u (LEFT a) env = seta u a env
			setac u (RIGHT c) env = setbc u (RIGHT c) env

			getbc env
				# (ac, env) = getac env
				= case ac of
					LEFT _ 
						# (b, env) = getb env
						-> (LEFT b, env)
					RIGHT c -> (RIGHT c, env)

			setb u b env = setbc u (LEFT b) env

			geta env
				# (ac, env) = getac env
				= case ac of
					LEFT a -> (a, env)
					RIGHT _ -> (abort "Internal error in left", env)

instance ArrowLoop GecCircuit
where
	loop g = GecCircuit k
	where
		k setc geta env = (seta, getc, env4)
		where
			(id, env1) = openStoreId env
			(setab, getcb, env2) = runCircuit g setcb getab env1
			(cb, env3) = getcb env2
			(_, env4) = openStore id (Just (snd cb)) env3
	
			setcb u cb env
				# env = writeStore id (snd cb) env
				= setc u (fst cb) env
			
			getab env
				# (a, env) = geta env
				  (b, env) = fromStore id env
				= ((a, b), env)
			
			seta u a env
				# (b, env) = fromStore id env
				= setab u (a, b) env
	
			getc env
				# (cb, env) = getcb env
				= (fst cb, env)

		fromStore id env
			# (ok, env) = valueStored id env
			| not ok = (abort "Run-time error: cycle in loop detected", env)
			= readStore id env

instance ArrowCircuit GecCircuit
where
	delay a = GecCircuit k
	where
		k seta geta env = (seta`, geta`, env2)
		where
			(id, env1) = openStoreId env
			(_, env2) = openStore id (Just a) env1
				
			geta` env = readStore id env

			seta` u a` env
				# (a, env) = readStore id env
				  env = seta u a env
				= writeStore id a` env

feedback :: (GecCircuit a a) -> GecCircuit a a
feedback g = GecCircuit k
where 
	k seta geta env = (seta`, geta`, env4)
	where
		(id, env1) = openStoreId env
		(a, env2) = geta env1
		(_, env3) = openStore id (Just a) env2
		(seta`, geta`, env4) = runCircuit g seta`` geta`` env3

		geta`` env = readStore id env

		seta`` NoUpdate a env = env
		seta`` YesUpdate a env 
			# env = writeStore id a env
			  env = seta` NoUpdate a env
			= seta YesUpdate a env

sink :: GecCircuit a Void
sink = GecCircuit k
where
	k setb geta env = (\_ _ env -> env, \env -> (Void, env), env)

source :: (GecCircuit a b) -> GecCircuit Void b
source g = sink >>> arr (\_ -> abort "Internal error in source") >>> g

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
