implementation module EstherFamkeEnv

import StdList, FamkeProcess, DynamicFileSystem, StdException

:: T a = C !a

famkeEnv :: [(String, Dynamic)]
famkeEnv = 
	[
	]	
	++ famkeProcess ++ famkeFiles
where
	famkeProcess = 
		[	("newProcess", dynamic newProcess :: (*World -> *World) *World -> *(ProcessId, *World))
		,	("joinProcess", dynamic joinProcess :: ProcessId *World -> *World)
		,	("killProcess", dynamic killProcess :: ProcessId *World -> *World)
		,	("shutdown", dynamic shutdown :: *World -> *World)
		]
	
	famkeFiles =
		[	("DynamicDirectory", dynamic DynamicDirectory)
		,	("DD", dynamic DynamicDirectory)
		,	("C", dynamic C :: A.a: a -> T a)
		,	("DynamicFile", dynamic DynamicFile)
		,	("ls", dynamic ls :: [String] *World -> *(DynamicDirectory, *World))
		]
	where
		ls path env
			# (ok, dyn, env) = dynamicRead path env
			| not ok = raise "ls: directory not found"
			= case dyn of
				(_ :: A.a: a) -> raise "ls: not a directory"
				(list :: DynamicDirectory) -> (list, env)
				_ -> raise "ls: not a directory"

