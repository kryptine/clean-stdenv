implementation module GecArrow

import StdArrow, StdGECExt
import store, GenDefault, StdDebug

:: GecCircuit a b = GecCircuit !.(A. .ps: (GecSet b ps) *(PSt ps) -> *(GecSet a ps, *PSt ps))

:: GecSet a ps :== IncludeUpdate a *(PSt ps) -> *PSt ps

runCircuit (GecCircuit k) :== k

startCircuit :: !(GecCircuit a b) a *(PSt .ps) -> *PSt .ps
startCircuit g a env
	# (seta, env) = runCircuit g setb env
	  env = seta YesUpdate a env
	= env
where
	setb _ _ env = env

edit :: String -> GecCircuit a a | gGEC{|*|}, generate{|*|} a 
edit title = GecCircuit k
where
	k seta env
		# (a, env) = gDefaultVal env
		  ({gecSetValue, gecGetValue}, env) = createNGEC title Interactive True a  (\r -> seta (includeUpdate r)) env
		= (seta` gecGetValue gecSetValue seta, env)
	
	seta` gecGetValue gecSetValue seta NoUpdate a env
		# env = gecSetValue NoUpdate a env
		  (a`, env) = gecGetValue env
		= seta NoUpdate a` env
	seta` _ gecSetValue _ u a env = gecSetValue u a env
	
display :: String -> GecCircuit a a | gGEC{|*|}, generate{|*|} a 
display title = GecCircuit k
where
	k seta env
		# (a, env) = gDefaultVal env
		  ({gecSetValue, gecGetValue}, env) = createNGEC title OutputOnly True a  (\r -> seta (includeUpdate r)) env
		= (seta` gecGetValue gecSetValue seta, env)
	
	seta` gecGetValue gecSetValue seta NoUpdate a env
		# env = gecSetValue NoUpdate a env
		  (a`, env) = gecGetValue env
		= seta NoUpdate a` env
	seta` _ gecSetValue _ u a env = gecSetValue u a env

gecMouse :: String -> GecCircuit a MouseState
gecMouse title = GecCircuit k
where
	k seta env
		# (_, env) = createMouseGEC title Interactive (\r -> seta (includeUpdate r)) env
		= (\_ _ env -> env, env)

:: First a = Updated a | NotUpdated a

firstValue (Updated x) = x
firstValue (NotUpdated x) = x

instance Arrow GecCircuit
where
	arr f = GecCircuit k
	where
		k setb env = (seta setb, env)

		seta setb u a env = setb u (f a) env
	
	(>>>) l r = GecCircuit k
	where 
		k setc env 
			# (setb, env) = runCircuit r setc env
			  (seta, env) = runCircuit l setb env
			= (seta, env)
	
	first g = GecCircuit k
	where
		k setbc env = (setac, env5)
		where
			(id_b, env1) = openStoreId env
			(_, env2) = openStore id_b (Just (NotUpdated (abort "instance first GecCircuit: no value of type b"))) env1
			(id_c, env3) = openStoreId env2
			(_, env4) = openStore id_c (Just (abort "instance first GecCircuit: no value of type c")) env3
			(seta, env5) = runCircuit g setb env4
			
			setb u b env 
				# env = writeStore id_b (Updated b) env
				  (c, env) = readStore id_c env
				= setbc u (b, c) env
	
			setac u ac env 
				# c = snd ac
				  (f, env) = readStore id_b env
				  env = writeStore id_b (NotUpdated (firstValue f)) env
				  env = writeStore id_c c env
				  env = seta u (fst ac) env
				  (f, env) = readStore id_b env
				= case f of
					NotUpdated b -> setbc u (b, c) env
					_ -> env

instance ArrowChoice GecCircuit
where
	left g = GecCircuit k
	where
		k setbc env = (setac, env1)
		where
			(seta, env1) = runCircuit g setb env

			setac u (LEft a) env = seta u a env
			setac u (RIght c) env = setbc u (RIght c) env

			setb u b env = setbc u (LEft b) env

instance ArrowLoop GecCircuit
where
	loop g = GecCircuit k
	where
		k setc env 
			# (id_b, env) = openStoreId env
			  (_, env) = openStore id_b Nothing env
			  (setab, env) = runCircuit g (setcb setc id_b) env
	 		= (seta setab id_b, env)

		setcb setc id_b u cb env
			# env = writeStore id_b (snd cb) env
			= setc u (fst cb) env
		
		seta setab id_b u a env = env2
		where
			env1 = setab u (a, b) env
			(b, env2) = readStore id_b env1

instance ArrowCircuit GecCircuit
where
	delay a = GecCircuit k
	where
		k seta env 
			# (id_a, env) = openStoreId env
			  (_, env) = openStore id_a (Just a) env
			= (seta` seta id_a, env)

		seta` seta id_a u a` env
			# (a, env) = readStore id_a env
			  env = seta u a env
			= writeStore id_a a` env

probe :: String -> GecCircuit a a | toString a
probe s = GecCircuit k
where
	k seta env = (seta` seta, env)
	
	seta` seta u a env
		| trace_t (s +++ ": ")
		= f seta u a env
	where
		f seta u a env
			| trace_tn a
			= seta u a env

feedback :: (GecCircuit a a) -> GecCircuit a a
feedback g = GecCircuit k
where 
	k seta env = (seta`, env4)
	where
		(seta`, env4) = runCircuit g seta`` env

		seta`` NoUpdate a env = env
		seta`` u a env 
			# env = seta u a env
			= seta` NoUpdate a env
/*
initial :: a -> GecCircuit a a
initial a = GecCircuit k
where
	k seta env = (seta`, env2)
	where
		(id, env1) = openStoreId env
		(_, env2) = openStore id (Just a) env1
			
		seta` u a env
			# env = writeStore id a env
			= seta u a env
*/
sink :: GecCircuit a Void
sink = GecCircuit k
where
	k setb env = (\_ _ env -> env, env)

source :: (GecCircuit a b) -> GecCircuit Void b
source g = sink >>> arr (\_ -> abort "Cannot write to source") >>> g

flowControl :: (IncludeUpdate -> a -> Maybe (IncludeUpdate, b)) -> GecCircuit a b
flowControl f = GecCircuit k
where
    k setb env = (seta, env)
    where
        seta u a env = case f u a of
            Just (u`, b) -> setb u` b env
            _ -> env

gecIO :: (A. .ps: a *(PSt .ps) -> *(b, *PSt .ps)) -> GecCircuit a b
gecIO f = GecCircuit k
where
	k setb env = (seta, env)
	where
		seta u a env 
			# (b, env) = f a env
			= setb u b env

includeUpdate :: !UpdateReason -> *IncludeUpdate
includeUpdate Changed = YesUpdate
includeUpdate _ = NoUpdate

generate{|GecCircuit|} ga gb trace stream
	# (b, trace, _, stream) = gb trace stream
	= (arr (const b), trace, \_ -> 0, stream)

