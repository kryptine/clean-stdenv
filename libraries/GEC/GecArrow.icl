implementation module GecArrow

import StdArrow, StdGECExt
import store, GenDefault, StdDebug

:: GecCircuit a b = GecCircuit !.(A. .ps: (GecSet b ps) *(PSt ps) -> *(GecSet a ps, *PSt ps))

:: GecSet a ps :== IncludeUpdate a *(PSt ps) -> *PSt ps

runCircuit (GecCircuit k) = k

startCircuit :: !(GecCircuit a b) a *(PSt .ps) -> *PSt .ps
startCircuit g a env
	# (seta, env) = runCircuit g startCircuit_setb env
	= seta YesUpdate a env
where
	startCircuit_setb _ _ env = env

edit :: String -> GecCircuit a a | gGEC{|*|} a 
edit title = gecEdit True title
	
display :: String -> GecCircuit a a | gGEC{|*|} a 
display title = gecEdit False title

gecEdit edit title = GecCircuit k
where
	k seta env 
		# (id_rec, env) = openStore` Nothing env
		= (gecEdit_seta seta id_rec, env)
		
	gecEdit_seta seta id_rec u a env
		# (ok, env) = valueStored id_rec env
		  (rec, env) = case ok of
					  	True -> readStore id_rec env
					  	_
							# (rec, env) = createNGEC title (if edit Interactive OutputOnly) 
											True a (\r -> seta (includeUpdate r)) env
							  env = writeStore id_rec rec env
							-> (rec, env)
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
		k setb env = (arr_seta setb f, env)

		arr_seta setb f u a env = setb u (f a) env
	
	(>>>) l r = GecCircuit k
	where 
		k setc env 
			# (setb, env) = runCircuit r setc env
			= runCircuit l setb env

	first g = GecCircuit k
	where
		k setbc env 
			# (id_c, env) = openStore` Nothing env
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
		k setc env 
			# (id_b, env) = openStore` Nothing env
			  (setab, env) = runCircuit g (loop_setcb setc id_b) env
			= (loop_seta setab id_b, env)

		loop_setcb setc id_b u cb env
			# env = writeStore id_b (snd cb) env
			= setc u (fst cb) env
		
		loop_seta setab id_b u a env = env`
		where
			(b, env`) = readStore id_b (setab u (a, b) env) 

instance ArrowCircuit GecCircuit
where
	delay a = GecCircuit k
	where
		k seta env 
			# (id_a, env) = openStore` (Just a) env
			= (delay_seta seta id_a, env)

		delay_seta seta id_a u a` env
			# (a, env) = readStore id_a env
			  env = seta u a env
			= writeStore id_a a` env

probe :: String -> GecCircuit a a | toString a
probe s = GecCircuit k
where
	k seta env = (probe_seta seta, env)
	
	probe_seta seta u a env
		| trace (s +++ ": ") False = undef
		| trace_n a False = undef
		= seta u a env

self :: (GecCircuit a a) (GecCircuit a a) -> GecCircuit a a
self g f = GecCircuit k
where 
	k seta env = (self_seta id_u gseta, env````)
	where
		(id_u, env`) = openStoreId env
		(_, env``) = openStore id_u (Just YesUpdate) env`
		(fseta, env```) = runCircuit f gseta env``
		(gseta, env````) = runCircuit g (self_setrec id_u seta fseta) env```

	self_setrec id_u setout setrec NoUpdate a env 
		# (u, env) = readStore id_u env
		= setout u a env
	self_setrec id_u setout setrec YesUpdate a env = setrec NoUpdate a env

	self_seta id_u seta u a env 
		# env = writeStore id_u u env
		= seta u a env

	/* = arr addLEFT >>> feedback (first g >>> arr selectX >>> f >>> arr addRIGHT) >>> arr fst
	where
		addLEFT x = (x, LEFT x)
		selectX (x, LEFT _) = x
		selectX (_, RIGHT x) = x
		addRIGHT x = (x, RIGHT x)*/

feedback :: (GecCircuit a a) -> GecCircuit a a
feedback g = GecCircuit k	// = self g (arr id)
where 
	k seta env = (feedback_seta id_u gseta, env```)
	where
		(id_u, env`) = openStoreId env
		(_, env``) = openStore id_u (Just YesUpdate) env`
		(gseta, env```) = runCircuit g (feedback_setrec id_u seta gseta) env``

	feedback_setrec id_u  setout setrec NoUpdate a env 
		# (u, env) = readStore id_u env
		= setout u a env
	feedback_setrec id_u  setout setrec YesUpdate a env = setrec NoUpdate a env

	feedback_seta id_u seta u a env 
		# env = writeStore id_u u env
		= seta u a env

sink :: GecCircuit a Void
sink = GecCircuit k
where
	k _ env = (\_ _ env -> env, env)

source :: (GecCircuit a b) -> GecCircuit Void b
source g = GecCircuit k
where
	k setb env 
		# (_, env) = runCircuit g setb env
		= (\_ _ env -> env, env)

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

openStore` :: !(Maybe a) !(PSt .ps) -> (!(StoreId a), !PSt .ps)
openStore` maybe env
	# (id, env) = openStoreId env
	  (_, env) = openStore id maybe env
	= (id, env)

generate{|GecCircuit|} ga gb trace stream
	# (b, trace, f, stream) = gb trace stream
	= (arr (const b), trace, \_ -> 0, stream)

