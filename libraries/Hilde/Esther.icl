module Esther

import StdEnv, EstherScript, EstherStdEnv, EstherFamkeEnv, DynamicFileSystem, FamkeProcess, StdDebug

/*Start :: !*World -> *World
Start world
	# (console, world) = stdio world
	  st = {searchPath = [], searchCache = [], buildin = stdEnv ++ famke, env = world}
	  (console, st=:{env=world}) = shell console st
	  (_, world) = fclose console world
	= world*/
	 
Start :: !*World -> *World
Start world = StartProcess Esther world

Esther :: !*World -> *World
Esther env
	# st = {searchPath = [], builtin = [("Esther", dynamic Esther :: *World -> *World)] ++ stdEnv ++ famkeEnv, env = env}
	  st=:{env} = shell st
	= env
where
	shell st=:{env}
		# (console, env) = stdio env
		  console = fwrites "Esther>" console
		  (input, console) = freadline` console
	  	  (_, env) = fclose console env
		| input == "" = {st & env = env}
		# (result, st=:{env}) = (interpret input catchAllIO handler) {st & env = env}
		  (console, env) = stdio env
		  console = foldl (\f x -> fwrites x f) console result
		  console = fwrites "\n" console
	  	  (_, env) = fclose console env
		= shell {st & env = env}
	where
		freadline` :: !*File -> (!String, !*File)
		freadline` file
			# (line, file) = freadline file
			| size line > 0 && line.[size line - 1] == '\n' = (line % (0, size line - 2), file)
			= (line, file)
	
	interpret :: !String !*(Esther *env) -> (![String], !*Esther *env) | TC, DynamicFileSystem, bimap{|*|} env
	interpret input st
		# (d, st=:{env}) = compose input st
		  (d, env) = eval d env
		  (v, t) = toStringDynamic d
		= (v ++ [" :: ", t], {st & env = env})
	where
		eval :: !Dynamic !*env -> (!Dynamic, !*env) | TC env
		eval (f :: *env^ -> *env^) env = trace "<*World -> *World>" (dynamic UNIT, f env)
		eval (f :: *env^ -> *(a, *env^)) env 
			# (x, env) = f env
			= trace "<*World -> *(a, *World)>" (dynamic x :: a, env)
		eval d env = (d, env)

	handler ((ApplyTypeError df dx) :: ComposeException) env = (["*** Error: cannot apply ", tf, " to ", tx, " ***"], env)
	where
		(vf, tf) = toStringDynamic df
		(vx, tx) = toStringDynamic dx
	handler (UnsolvableOverloading :: ComposeException) env = (["*** Error: unsolvable overloading ***"], env)
	handler d env = (["*** Error: ":v] ++ [" ***"], env)
	where
		(v, t) = toStringDynamic d
