implementation module EstherScript

import EstherPostParser, EstherTransform
import StdBool, StdList, StdMisc, StdFunc, StdTuple, StdParsComb, DynamicFileSystem

compose :: !String !*env -> (!MaybeException Dynamic, !*env) | resolveFilename, ExceptionEnv, bimap{|*|} env
compose input env 
	# (maybe, env) = getExceptionIO (compile input) env
	= case maybe of
		Exception d -> (Exception (handler d), env)
		NoException d 
			# (maybe, env) = getException d env
			-> case maybe of
				Exception d -> (Exception (handler d), env)
				else -> (else, env)
where
	compile :: !String !*env -> (!Dynamic, !*env) | resolveFilename, bimap{|*|} env
	compile input env
		# syntax = parseStatements input
		  (syntax, fv, env`) = resolveNames{|*|} syntax [] env
		  syntax = fixInfix{|*|} syntax
		  core = transform{|*|} syntax
		= generateCode core env`

	handler :: !Dynamic -> Dynamic
	handler ((ApplyTypeError df dx) :: ComposeException) = dynamic EstherError ("cannot apply `" +++ tf +++ "' to `" +++ tx +++ "'")
	where
		(vf, tf) = toStringDynamic df
		(vx, tx) = toStringDynamic dx
	handler (UnboundVariable v :: ComposeException) = dynamic EstherError ("unbound variable (internal error) `" +++ v +++ "'")
	handler (InstanceNotFound c dt :: ComposeException) = dynamic EstherError ("`instance " +++ c +++ " " +++ snd (toStringDynamic dt) +++ "' not found")
	handler (InvalidInstance c dt :: ComposeException) = dynamic EstherError ("`instance " +++ c +++ " " +++ snd (toStringDynamic dt) +++ "' is invalid (type not an instance of the class type)")
	handler (UnsolvableOverloading :: ComposeException) = dynamic EstherError ("unsolvable overloading")
	handler (InfixRightArgumentMissing :: PostParseException) = dynamic EstherError ("right argument of infix operator is missing")
	handler (InfixLeftArgumentMissing :: PostParseException) = dynamic EstherError ("left argument of infix operator is missing")
	handler (UnsolvableInfixOrder :: PostParseException) = dynamic EstherError ("conflicting priorities of infix operators")
	handler (NameNotFound n :: PostParseException) = dynamic EstherError ("file `" +++ n +++ "' not found")
	handler (CaseBadConstructorArity :: TransformException) = dynamic EstherError ("constructor in pattern has too many or too little arguments")
	handler (NotSupported s :: TransformException) = dynamic EstherError ("feature not (yet) supported: `" +++ s +++ "'")
	handler (_ :: ParseException) = dynamic EstherError ("parser error")
	handler d = d

instance resolveFilename World
where
	resolveFilename name env
		# (ok, dyn, env) = dynamicRead ENV_CWD env
		  cwd = if ok (case dyn of (p :: DynamicPath) -> p; _ -> []) []
		  (ok, dyn, env) = dynamicRead ENV_PATH env
		  path = [cwd:if ok (case dyn of (p :: [DynamicPath]) -> p; _ -> [[]]) [[]]]
		  (cache, env) = cacheSearchPath path env
		  (ok, file, prio) = findFile name cache
		| not ok = (Nothing, env)
		# (ok, dyn, env) = dynamicRead file env
		| not ok = (Nothing, env)
		= (Just (dyn, prio), env)
	where
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

instance resolveFilename (EstherBuiltin *env) | resolveFilename env
where
	resolveFilename name eb=:{builtin, env}
		# (maybe, env) = resolveFilename name env
		  eb = {eb & env = env}
		= case maybe of
			Nothing
				# (ok, dyn, prio) = findFile name builtin
				| not ok -> (maybe, eb)
				-> (Just (dyn, prio), eb)
			_ -> (maybe, eb)
	
instance ExceptionEnv (EstherBuiltin *env) | ExceptionEnv env
where
	catchAllIO f g = catch
	where
		catch eb=:{builtin, env}
			# (x, env) = (f` catchAllIO g`) env
			= (x, {eb & env = env})
		where
			f` env
				# (x, {env}) = f {builtin = builtin, env = env}
				= (x, env)
	
			g` d env
				# (x, {env}) = g d {builtin = builtin, env = env}
				= (x, env)