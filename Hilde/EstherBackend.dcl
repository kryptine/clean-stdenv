definition module EstherBackend

import EstherParser, StdMaybe

:: ComposeException 
	= ApplyTypeError !(Dynamic) !(Dynamic)
	| UnboundVariable !String
	| InstanceNotFound !String !Dynamic
	| InvalidInstance !String !Dynamic
	| UnsolvableOverloading

:: Core
	= CoreApply !Core !Core
	| CoreCode !Dynamic 
	| CoreVariable !String
	| CoreDynamic

class resolveInstance env :: !String !Dynamic !*env -> (!Maybe Dynamic, !*env)

class generateCode t :: !Core !*env -> (!t, !*env) | resolveInstance env

overloaded :: !String !Dynamic -> Dynamic
overloaded2 :: !String !String !Dynamic -> Dynamic
overloaded3 :: !String !String !String !Dynamic -> Dynamic

abstract :: !String !Core -> Core
abstract_ :: !Core -> Core

instance generateCode Dynamic

toStringDynamic :: !Dynamic -> ([String], String)

