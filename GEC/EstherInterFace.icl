implementation module EstherInterFace

import StdEnv, EstherPostParser, EstherTransform, StdDynamic//, StdDynamicFileIO


Start world 
	# (console, world)	= stdio world
	# (console,world)	= shell 1 console world
	= fclose console world
 
unifyable :: !Dynamic !Dynamic -> (!Bool,!Dynamic)
unifyable old=:(d1::a) new=:(d2::a) = (True,new)
unifyable old new		   			= (False,old)

stringToDynamic :: !String !*World -> (!Dynamic,!*World)
stringToDynamic s world = (compose s catchAllIO handler) world


compose :: !String !*World -> (!Dynamic, !*World)
compose input world
	# syntax = parseStatement input
	  (syntax, fv, world) = resolveNames{|*|} syntax [] world
	  syntax = fixInfix{|*|} syntax
	  core = transform{|*|} syntax
	= (generateCode core.node, world)

shell :: !Int !*File !*World -> (!*File,!*World)
shell lineNr console world
	# console 			= fwrites (toString lineNr +++ ":" +++ "> ") console
	  (cmdline, console) = freadline` console
	| cmdline == "exit" = (console,world)
	| cmdline == "" 	= shell lineNr console world
	# (d, world) 		= (compose cmdline catchAllIO handler) world
	  (v, t) 			= prettyDynamic d
	  console 			= fwrites ("\n" +++ cmdline +++ "\n==>\n" +++ v +++ " :: " +++ t +++ "\n") console
	= shell (lineNr + 1) console world
where
	freadline` :: !*File -> (!String, !*File)
	freadline` file
		# (line, file) = freadline file
		| line == "" = ("", file)
		| line.[size line - 1] == '\n' = (line % (0, size line - 2), file)
		= (line, file)

	eval :: !Int !(a -> b) !a -> b
	eval 1 f x = f x
	eval n f x = eval (force (f x) (n - 1)) f x
	where
		force :: !.a !.b -> .b
		force _ r = r

import StdParsComb, Directory

instance resolve World
where
	resolveFilename n world
		# dir = ".\\MyDynamics"
		  ((_, path), world) = pd_StringToPath dir world
		  ((_, list), world) = getDirectoryContents path world
		  (dir, world) = pathToPD_String path world
		  (list, world) = readDynamics dir list world
		  (d, p, _) = resolveFilename n (FileList list)
		= (d, p, world)
	where
		readDynamics dir [] world = ([], world)
		readDynamics dir [{fileName}:xs] world
			# (ok, d, world) = readDynamic` (dir +++ "\\" +++ fileName) world
			  (ys, world) = readDynamics dir xs world
			| ok = ([(fileName % (0, size fileName - 5), d):ys], world)
			= (ys, world)

readDynamic` :: String *f -> (Bool,Dynamic,*f) | FileSystem f
readDynamic` s f = (False,dynamic "readDynamic switched off\n",f)

:: FileList = FileList ![(String, Dynamic)]

instance resolve FileList
where
	resolveFilename n env=:(FileList list) = (d, p, env)
	where
		(d, p) = findFile n (builtin)
//		(d, p) = findFile n (list ++ builtin)

		findFile n [] = raise ("Cannot read file: " +++ n)
		findFile n [(x, d):xs] = case begin (sp (parse (fromString n)) <& sp eof) (fromString x) of
			[([], p)] -> (d, p)
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

handler (error :: ComposeException) w = (dynamic composeError error :: String, w)
where
	composeError (ApplyTypeError {node=df, src=srcf} {node=dx, src=srcx}) = "Cannot apply " +++ srcf +++ " :: " +++ toString (typeCodeOfDynamic df) +++ " to " +++ srcx +++ " :: " +++ toString (typeCodeOfDynamic dx)
	composeError (UnboundVariable s) = "Unbound variable " +++ s
	composeError _ = "Poef"
handler d w = (d, w)

builtin = 
	[	("(+) infixl 6", dynamic (+) :: Int Int -> Int)
	,	("(-) infixl 6", dynamic (-) :: Int Int -> Int)
	,	("(*) infixl 7", dynamic (*) :: Int Int -> Int)
	,	("(^) infixr 8", dynamic (^) :: Int Int -> Int)
	,	("(infixK) infixr 8", dynamic (infixK) :: Int Int -> Int)
	,	("Cons", dynamic \x xs -> [x:xs] :: A.a: a [a] -> [a])
	,	("Nil", dynamic [] :: A.a: [a])
	,	("if", dynamic IF :: A.a: Bool a a -> a) 
	,	("take", dynamic take :: A.a: Int [a] -> [a])
	,	("(!!)", dynamic (!!) :: A.a: [a] Int -> a)
	,	("(>) infix 4", dynamic (>) :: Int Int -> Bool)
	,	("(<) infix 4", dynamic (<) :: Int Int -> Bool)
	,	("dec", dynamic dec :: Int -> Int)
	,	("(<=) infix 4", dynamic (<=) :: Int Int -> Bool)
	,	("(+R) infixl 6", dynamic (+) :: Real Real -> Real)
	,	("(-R) infixl 6", dynamic (-) :: Real Real -> Real)
	,	("(*R) infixl 7", dynamic (*) :: Real Real -> Real)
	,	("(>R) infixl 4", dynamic (>) :: Real Real -> Bool)
	,	("const", dynamic const :: A.a b: a b -> a)
	,	("inc", dynamic inc :: Int -> Int)
	,	("undef", dynamic undef :: A.a: a)
	,	("map", dynamic map :: A.a b: (a -> b) [a] -> [b])
	]

	
IF x y z = if x y z

(infixK) infixr 8 
(infixK) x y = y
