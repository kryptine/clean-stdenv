implementation module EstherScript

import EstherPostParser, EstherTransform, DynamicFileSystem
import StdBool, StdList, StdMisc, StdParsComb, StdFunc, StdTuple

compose :: !String !*(Esther *env) -> (!Dynamic, !*Esther *env) | DynamicFileSystem, bimap{|*|}, ExceptionEnv env
compose input env = (compile input catchAllIO (\d env -> (raiseDynamic (handler d), env))) env 
where
	compile :: !String !*(Esther *env) -> (!Dynamic, !*Esther *env) | DynamicFileSystem, bimap{|*|} env
	compile input env
		# syntax = parseStatement input
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

evaluate :: !Bool a !Dynamic !*(Esther *env) -> (!a, !*Esther *env) | TC a & TC, DynamicFileSystem, ExceptionEnv, bimap{|*|} env
evaluate unsafe def input esther 
	# forceType = dynamic id :: a^ -> a^
	  core = CoreApply (CoreCode forceType) (CoreCode input)
	  (output, esther) = (generateCode core catchAllIO handler) esther
	= case output of
		(x :: a^) -> (x, esther)
		(f :: *env^ -> *(a^, *env^)) | unsafe
			# esther=:{env} = esther
			  (x, env) = f env
			-> (x, {esther & env = env})
		_ -> (def, esther)
where
	handler d env = (dynamic EstherError (foldr (+++) "" v) :: EstherError, env)
	where
		(v, t) = toStringDynamic d

instance resolveFilename (Esther *env) | DynamicFileSystem env
where
	resolveFilename name state=:{searchPath, builtin, env}
		# (ok, dyn, prio) = findFile name builtin
		| ok = (Just (dyn, prio), {state & env = env})
		# (cache, env) = cacheSearchPath searchPath env
		  (ok, path, prio) = findFile name cache
		  (ok, dyn, env) = if ok (dynamicRead path env) (False, undef, env)
		| ok = (Just (dyn, prio), {state & env = env})
		= (Nothing, {state & env = env})
	where
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
	resolveInstance n t env = case resolveFilename ("instance " +++ n +++ " " +++ (outermostType t)) env of
		(Just (inst, _), env) -> (Just inst, env)
		(_, env) -> (Nothing, env)
	where
/*		outermostType :: !Dynamic -> String
		outermostType d
			# (ok, n, type) = f (typeCodeOfDynamic d)
			| not ok = raise ("No outermost type constructor: " +++ snd (toStringDynamic d))
			= toString type
		where
			f (TypeScheme _ type) = f type
			f type=:(TypeCons cons) = (True, 0, type)
			f (TypeApp x y)
				# (ok, n, x) = f x
				= (ok, n + 1, TypeApp x (TypeVar n))
			f type = (False, 0, type)*/
	
		outermostType :: !Dynamic -> String
		outermostType d = toString (snd (f (typeCodeOfDynamic d)))
		where
			f (TypeScheme _ type) = f type
			f (TypeUnique type) = f type
			f type=:(TypeCons cons) = (0, type)
			f (TypeApp t1 t2)
				# (n, t1) = f t1
				= (n + 1, TypeApp t1 (TypeVar n))
			f type = (0, type)

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
