module Esther

import StdEnv, EstherScript, EstherStdEnv, EstherFamkeEnv, DynamicFileSystem, FamkeProcess, StdDebug

Start :: !*World -> *World
Start world = StartProcess Esther world

Esther :: !*World -> *World
Esther env
	# st = {searchPath = [[]], builtin = [("Esther", dynamic Esther :: *World -> *World)] ++ stdEnv ++ famkeEnv, env = env}
	  st=:{env} = shell st
	= env
where
	shell st=:{env}
		# (console, env) = stdio env
		  console = fwrites "Esther>" console
		  (continue, input, console) = freadline` console
	  	  (_, env) = fclose console env
		| input == "" = {st & env = env}
		# (result, st=:{env}) = (interpret input catchAllIO (\d env -> (handler d, env))) {st & env = env}
		  (console, env) = stdio env
		  console = foldl (\f x -> fwrites x f) console result
		  console = fwrites "\n" console
	  	  (_, env) = fclose console env
		| not continue = {st & env = env}
		= shell {st & env = env}
	where
		freadline` :: !*File -> (!Bool, !String, !*File)
		freadline` file
			# (line, file) = freadline file
			| size line > 0 && line.[size line - 1] == '\n' = (True, line % (0, size line - 2), file)
			= (False, line, file)
	
	interpret :: !String !*(Esther *env) -> (![String], !*Esther *env) | TC, DynamicFileSystem, bimap{|*|}, ExceptionEnv env
	interpret input st
		# (d, st=:{env}) = compose input st
		  (d, env) = eval d env
		  (v, t) = toStringDynamic d
		= (v ++ [" :: ", t], {st & env = env})
	where
		eval :: !Dynamic !*env -> (!Dynamic, !*env) | TC env
		eval d=:(_ :: A.a: *a -> *a) env = (d, env)
		eval (f :: *env^ -> *env^) env 
			# env = trace_n " < *World -> *World > " env
			= (dynamic UNIT, f env)
		eval d=:(_ :: A.a b: *a -> *(b, *a)) env = (d, env)
		eval (f :: *env^ -> *(a, *env^)) env 
			# env = trace_n " < *World -> *(a, *World) > " env
			  (x, env) = f env
			= (dynamic x :: a, env)
		eval d env = (d, env)

	handler d=:(_ :: A.a: a) = ["*** Error: ":take 1000 v] ++ [" :: " , t, " ***"]
	where
		(v, t) = toStringDynamic d
	handler (EstherError s :: EstherError) = ["*** Esther: ", s, " ***"]
	handler d = ["*** Error: ":take 1000 v] ++ [" :: " , t, " ***"]
	where
		(v, t) = toStringDynamic d
