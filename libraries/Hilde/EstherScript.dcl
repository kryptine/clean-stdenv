definition module EstherScript

import EstherPostParser, EstherTransform, DynamicFileSystem

:: Esther env =
	{	searchPath	:: ![DynamicPath]
	,	searchCache	:: ![(String, DynamicPath)]
	,	buildin		:: ![(String, Dynamic)]
	,	env			:: !env
	}

:: EstherError = EstherError !String

compose :: !String !*(Esther *env) -> (!Dynamic, !*Esther *env) | DynamicFileSystem, ExceptionEnv, bimap{|*|} env

derive transform NTstatement

instance resolveFilename (Esther *env) | DynamicFileSystem env
instance resolveInstance (Esther *env) | DynamicFileSystem env
