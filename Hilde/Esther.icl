module Esther

import StdEnv, EstherScript, EstherStdEnv, EstherFamkeEnv, DynamicFileSystem, FamkeProcess, StdDebug, EstherBackend

Start :: !*World -> *World
Start world = StartProcess Esther world

Esther :: !*World -> *World
Esther env
	# (console, env) = stdio env
	  console = fwrites "Esther>" console
	  (continue, input, console) = freadline` console
  	  (_, env) = fclose console env
	| input == "" = env
	# (result, env) = (interpret input catchAllIO \d env -> (handler d, env)) env
	  (console, env) = stdio env
	  console = foldl (\f x -> fwrites x f) console result
	  console = fwrites "\n" console
  	  (_, env) = fclose console env
	| not continue = env
	= Esther env
where
	freadline` file
		# (line, file) = freadline file
		| size line > 0 && line.[size line - 1] == '\n' = (True, line % (0, size line - 2), file)
		= (False, line, file)

	interpret input env
		# (maybe, {env}) = compose input {builtin = [("Esther", dynamic Esther :: *World -> *World)] ++ famkeEnv ++ stdEnv, env = env}
		  (r, env) = case maybe of NoException d -> eval d env; Exception d -> raiseDynamic d
		  (v, t) = toStringDynamic r 
		= (v ++ [" :: ", t], env)
	where
		eval :: !Dynamic !*env -> (!Dynamic, !*env) | TC env
		eval (f :: A.a: *a -> *a) env 
			#!f = f
			= (dynamic f :: A.a: *a -> *a, env)
		eval (f :: *env^ -> *env^) env 
			# env = trace_n " < *World -> *World > " env
			= (dynamic UNIT, f env)
		eval (f :: A.a b: *a -> *(b, *a)) env 
			#!f = f
			= (dynamic f :: A.a b: *a -> *(b, *a), env)
		eval (f :: *env^ -> *(a, *env^)) env 
			# env = trace_n " < *World -> *(a, *World) > " env
			  (x, env) = f env
			#!x = x
			= (dynamic x :: a, env)
		eval (x :: a) env
			#!x = x
			= (dynamic x :: a, env)

	handler d=:(_ :: A.a: a) = ["*** Error: ":take 1000 v] ++ [" :: " , t, " ***"]
	where
		(v, t) = toStringDynamic d
	handler (EstherError s :: EstherError) = ["*** Esther: ", s, " ***"]
	handler d = ["*** Error: ":take 1000 v] ++ [" :: " , t, " ***"]
	where
		(v, t) = toStringDynamic d
