implementation module GecArrow

import StdArrow, StdGECExt
import store, GenDefault, StdDebug

:: GecCircuit a b = GecCircuit !.(A. .ps: (GecSet b ps) *(PSt ps) -> *(GecSet a ps, *PSt ps))

:: GecSet a ps :== IncludeUpdate a *(PSt ps) -> *PSt ps

runCircuit (GecCircuit k) = k

startCircuit :: !(GecCircuit a b) a *(PSt .ps) -> *PSt .ps
startCircuit g a env
	# (seta, env) = runCircuit g setb env
	  env = seta YesUpdate a env
	= env
where
	setb _ _ env = env

edit :: String -> GecCircuit a a | gGEC{|*|} a 
edit title = gecEdit True title
	
display :: String -> GecCircuit a a | gGEC{|*|} a 
display title = gecEdit False title

gecEdit edit title = GecCircuit k
where
	k seta env 
		# (id_rec, env) = openStoreId env
		  (_, env) = openStore id_rec Nothing env
		= (gecEdit_seta seta id_rec, env)
		
	gecEdit_seta seta id_rec u a env
		# (ok, env) = valueStored id_rec env
		| not ok
				# (r, env) = createNGEC title (if edit Interactive OutputOnly) True a  (\r -> seta (includeUpdate r)) env
				  env = writeStore id_rec r env
				= gecEdit_seta seta id_rec u a env
		# (rec, env) = readStore id_rec env
		= gecEdit_seta` rec seta u a env
	
	gecEdit_seta` {gecSetValue, gecGetValue} seta NoUpdate a env
		# env = gecSetValue NoUpdate a env
		  (a`, env) = gecGetValue env
		= seta NoUpdate a` env
	gecEdit_seta` {gecSetValue} _ u a env = gecSetValue u a env

gecMouse :: String -> GecCircuit a MouseState
gecMouse title = GecCircuit k
where
	k seta env
		# (_, env) = createMouseGEC title Interactive (\r -> seta (includeUpdate r)) env
		= (\_ _ env -> env, env)

instance Arrow GecCircuit
where
	arr f = GecCircuit k
	where
		k setb env = (arr_seta setb, env)

		arr_seta setb u a env = setb u (f a) env
	
	(>>>) l r = GecCircuit k
	where 
		k setc env 
			# (setb, env) = runCircuit r setc env
			= runCircuit l setb env

	first g = GecCircuit k
	where
		k setbc env 
			# (id_c, env) = openStoreId env
			  (_, env) = openStore id_c Nothing env
			  (seta, env) = runCircuit g (first_setb id_c setbc) env
			= (first_setac id_c seta, env)
		
		first_setac id_c seta u ac env
			# env = writeStore id_c (snd ac) env
			= seta u (fst ac) env

		first_setb id_c setbc u b env 
			# (c, env) = readStore id_c env
			= setbc u (b, c) env

instance ArrowChoice GecCircuit
where
	left g = GecCircuit k
	where
		k setbc env 
			# (seta, env) = runCircuit g (left_setb setbc) env
			= (left_setac seta setbc, env)

		left_setac seta setbc u (LEft a) env = seta u a env
		left_setac seta setbc u (RIght c) env = setbc u (RIght c) env

		left_setb setbc u b env = setbc u (LEft b) env

instance ArrowLoop GecCircuit
where
	loop g = GecCircuit k
	where
		k setc env = (seta setab id_b, env3)
		where
			(id_b, env1) = openStoreId env
			(_, env2) = openStore id_b Nothing env1
			(setab, env3) = runCircuit g (setcb setc id_b) env2

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
		(seta`, env4) = runCircuit g feedback_seta`` env

		feedback_seta`` NoUpdate a env = seta YesUpdate a env
		feedback_seta`` YesUpdate a env 
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
	# (b, trace, f, stream) = gb trace stream
	= (arr (const b), trace, \_ -> 0, stream)

