implementation module EstherScript

import EstherPostParser, EstherTransform, DynamicFileSystem
import StdBool, StdList, StdMisc, StdParsComb

compose :: !String !*(Esther *env) -> (!Dynamic, !*Esther *env) | DynamicFileSystem, ExceptionEnv, bimap{|*|} env
compose input env = (compile input catchAllIO handler) env
where
	compile input env
		# syntax = parseStatement input
		  (syntax, fv, env`) = resolveNames{|*|} syntax [] env
		  syntax = fixInfix{|*|} syntax
		  core = transform{|*|} syntax
		= generateCode core env`

	handler d env = (dynamic EstherError v :: EstherError, env)
	where
		(v, t) = toStringDynamic d

transform{|NTstatement|} (Expression e) = transform{|*|} e

instance resolveFilename (Esther *env) | DynamicFileSystem env
where
	resolveFilename name state = searchCache True name (searchBuildin name (raise ("Cannot find file: " +++ name))) state 
	where
		searchCache retry name else state=:{searchPath, searchCache=cache, env}
			# (ok, path, prio) = findFile name cache
			  (ok, dyn, env) = if ok (dynamicRead path env) (False, undef, env)
			| not ok
				| not retry = else {state & env = env}
				# (cache, env) = cacheSearchPath searchPath env
				= searchCache False name else {state & searchCache = cache, env = env}
			= (dyn, prio, {state & env = env})
		
		searchBuildin name else state=:{buildin} 
			# (ok, dyn, prio) = findFile name buildin
			| not ok = else state
			= (dyn, prio, state)
	
		findFile n [] = (False, undef, undef)
		findFile n [(x, d):xs] = case begin (sp (parse (fromString n)) <& sp eof) (fromString x) of
			[([], p)] -> (True, d, p)
			_ -> findFile n xs
		where
			parse name = symbol '(' &> sptoken name &> spsymbol ')' &> sp prio
					<!> token name &> yield GenConsNoPrio
			where
				prio = token ['infixl'] &> digitOr9 <@ GenConsPrio GenConsAssocLeft
					<!> token ['infixr'] &> digitOr9 <@ GenConsPrio GenConsAssocRight
					<!> token ['infix'] &> digitOr9 <@ GenConsPrio GenConsAssocNone
					<!> yield (GenConsPrio GenConsAssocLeft 9)
				digitOr9 = sp digit <!> yield 9

instance resolveInstance (Esther *env) | DynamicFileSystem env
where
	resolveInstance n t env = (i, env`)
	where
//		(i, _, env`) = resolveType (toString (typeCodeOfDynamic t)) env 
		(i, _, env`) = resolveType (outermostType t) env
		
		resolveType s env = resolveFilename ("instance " +++ n +++ " " +++ s) env

		outermostType :: !Dynamic -> String
		outermostType d
			# (ok, n, type) = f (typeCodeOfDynamic d)
			| not ok = raise "No outermost type constructor"
			= toString type
		where
			f (TypeScheme _ type) = f type
			f type=:(TypeCons cons) = (True, 0, type)
			f (TypeApp x y)
				# (ok, n, x) = f x
				= (ok, n + 1, TypeApp x (TypeVar n))
			f type = (False, 0, type)
	
cacheSearchPath :: ![DynamicPath] !*env -> (![(String, DynamicPath)], !*env) | DynamicFileSystem env
cacheSearchPath [p:ps] env 
	# (ok, d, env) = dynamicRead p env
	| not ok = cacheSearchPath ps env
	= case d of
		(dir :: DynamicDirectory)
			# (searchCache, env) = cacheSearchPath ps env
			-> ([(f, p ++ [f]) \\ DynamicFile f <- dir] ++ searchCache, env)
		_ -> cacheSearchPath ps env
cacheSearchPath _ env = ([], env)

instance ExceptionEnv (Esther *env) | ExceptionEnv env
where
	catchAllIO function handler = catch
	where
		catch state 
			#!(state`, state) = unsafeShare state
			= function state unsafeCatchAll (\d -> handler d state`)
		where
			unsafeShare :: !*a -> (!*a, !*a)
			unsafeShare _ = code inline {
					push_a	0
				}
