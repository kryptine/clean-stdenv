definition module EstherPostParser

import EstherParser

:: PostParseException
	= InfixRightArgumentMissing
	| InfixLeftArgumentMissing
	| UnsolvableInfixOrder
	| NameNotFound !String

class resolveFilename env :: !String !*env -> (!Maybe (Dynamic, GenConsPrio), !*env)

generic resolveNames e :: !e ![String] !*env -> (!e, ![String], !*env) | resolveFilename env
derive resolveNames NTexpression, NTstatement

desugar :: !NTsugar -> NTexpression

generic fixInfix e :: !e -> e
derive fixInfix NTexpression, NTstatement
