module Esther

import FamkeProcess, EstherScript, EstherStdEnv, EstherFamkeEnv, DynamicFileSystem
import StdFile, StdArray, StdBool, StdList, StdString

Start :: !*World -> *World
Start world = StartProcess Esther world

Esther :: !*World -> *World
Esther env
	# (ok, d, env) = dynamicRead ENV_CWD env
	  cwd = if ok (case d of (path :: DynamicPath) -> path) []
	  (console, env) = stdio env
	  console = fwrites (foldr (\x y -> "/" +++ x +++ y) ">" cwd) console
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
		# (maybe, {env}) = compose input {builtin = Builtin, env = env}
		  (r, env) = case maybe of NoException d -> eval d env; Exception d -> raiseDynamic d
		  (v, t) = toStringDynamic r 
		= (v ++ [" :: ", t], env)
	where
		eval :: !Dynamic !*env -> (!Dynamic, !*env) | TC env
		eval (f :: A.a: *a -> *a) env 
			#!f = f
			= (dynamic f :: A.a: *a -> *a, env)
		eval (f :: *env^ -> *env^) env 
//			# env = trace_n " < *World -> *World > " env
			= (dynamic UNIT, f env)
		eval (f :: A.a b: *a -> *(b, *a)) env 
			#!f = f
			= (dynamic f :: A.a b: *a -> *(b, *a), env)
		eval (f :: *env^ -> *(a, *env^)) env 
//			# env = trace_n " < *World -> *(a, *World) > " env
			# (x, env) = f env
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

Builtin =: estherEnv ++ famkeEnv

estherEnv :: [(String, Dynamic)]
estherEnv = 
	[	("Esther", dynamic Esther :: *World -> *World)
	,	("StdEnv", dynamic writeStdEnv :: *World -> *(Bool, *World))
	,	("DynamicDirectory", dynamic DynamicDirectory)
	,	("DynamicFile", dynamic DynamicFile)
	,	("ls", dynamic ls :: DynamicPath *World -> *(DynamicDirectory, *World))
	,	("mkdir", dynamic mkdir :: DynamicPath *World -> *World)
	,	("rmdir", dynamic rmdir :: DynamicPath *World -> *World)
	,	("cwd", dynamic [] :: DynamicPath)
	,	("cd", dynamic cd :: DynamicPath *World -> *World)
	,	("cp", dynamic cp :: DynamicPath DynamicPath *World -> *World)
	]
where
	relativepath path env
		# (ok, dyn, env) = dynamicRead ENV_CWD env
		  cwd = if ok (case dyn of (p :: DynamicPath) -> p; _ -> []) []
		= (merge cwd path, env)
	where
		merge cwd [] = cwd
		merge cwd [".":xs] = merge cwd xs
		merge cwd ["..":xs] = merge (init cwd) xs
		merge cwd [x:xs] = merge (cwd ++ [x]) xs
		
	writeStdEnv :: !*World -> (!Bool, !*World)
	writeStdEnv env 
		# (p, env) = relativepath [] env
		= f p stdEnv env
	where
		f p [(n, d):xs] env
			# (ok, env) = dynamicWrite (p ++ [n]) d env
			| not ok = (False, env)
			= f p xs env
		f p _ env = (True, env)
	
	ls path env
		# (p, env) = relativepath path env
		  (ok, dyn, env) = dynamicRead p env
		| not ok = raise "ls: directory not found"
		= case dyn of
			(_ :: A.a: a) -> raise "ls: not a directory"
			(list :: DynamicDirectory) -> (list, env)
			_ -> raise "ls: not a directory"

	mkdir path env
		# (p, env) = relativepath path env
		  (ok, env) = dynamicWrite p (dynamic [] :: DynamicDirectory) env
		| not ok = raise "mkdir: cannot make directory"
		= env

	rmdir path env
		# (p, env) = relativepath path env
		  (ok, env) = dynamicRemove p env
		| not ok = raise "rmdir: cannot remove directory"
		= env

	cd path env
		# (p, env) = relativepath path env
		  (ok, env) = dynamicExists p env
		| not ok = raise "cd: cannot find directory"
		# (ok, env) = dynamicWrite ENV_CWD (dynamic p :: DynamicPath) env
		| not ok = raise "cd: cannot change current working directory"
		= env

	cp src dst env
		# (p1, env) = relativepath src env
		  (p2, env) = relativepath dst env
		  (ok, dyn, env) = dynamicRead p1 env
		| not ok = raise "cp: cannot read source"
		# (ok, env) = dynamicWrite p2 dyn env
		| not ok = raise "cp: cannot write destination"
		= env
