definition module EstherPostParser

import EstherParser

:: InfixException
	= InfixRightArgumentMissing
	| InfixLeftArgumentMissing
	| UnsolvableInfixOrder

class resolveFilename env :: !String !*env -> (!Dynamic, !GenConsPrio, !*env)

generic resolveNames e :: !e ![String] !*env -> (!e, ![String], !*env) | resolveFilename env
derive resolveNames NTexpression, NTstatement

desugar :: !NTsugar -> NTexpression

generic fixInfix e :: !e -> e
derive fixInfix NTexpression, NTstatement
