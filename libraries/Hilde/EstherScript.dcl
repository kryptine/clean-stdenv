definition module EstherScript

import EstherPostParser, EstherTransform, DynamicFileSystem

:: Esther env =
	{	searchPath	:: ![DynamicPath]
	,	builtin		:: ![(String, Dynamic)]
	,	env			:: !env
	}

:: EstherError = EstherError !String

compose :: !String !*(Esther *env) -> (!Dynamic, !*Esther *env) | DynamicFileSystem, bimap{|*|} env
evaluate :: !Bool a !Dynamic !*(Esther *env) -> (!a, !*Esther *env) | TC a & TC, DynamicFileSystem, ExceptionEnv, bimap{|*|} env

instance resolveFilename (Esther *env) | DynamicFileSystem env
instance resolveInstance (Esther *env) | DynamicFileSystem env
instance ExceptionEnv (Esther *env) | ExceptionEnv env
