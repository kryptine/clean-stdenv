implementation module EstherStdEnv

import EstherBackend
import StdEnv

stdEnv :: [(String, Dynamic)]
stdEnv = 
	[	("if", dynamic IF :: A.a: Bool a a -> a)
//	,	("(>>>>) infix 0", overloaded "TC" (dynamic (undef, >>>>) :: A.a: (a, (a -> Dynamic) a String -> *World -> *(Bool, *World))))
	]	
	++ stdOverloaded ++ stdClass ++ stdInt ++ stdReal ++ stdList 
	++ stdFunc ++ stdMisc ++ stdBool ++ stdString ++ stdTuple
where
	IF x y z = if x y z
//	>>>> tc x n = dynamicWrite [n] (tc x)

	stdOverloaded = 
		[	("(+) infixl 6", overloaded "+" (dynamic (undef, id) :: A.a: (a, (a a -> a) a a -> a)))
		,	("(-) infixl 6", overloaded "-" (dynamic (undef, id) :: A.a: (a, (a a -> a) a a -> a)))
		,	("(*) infixl 7", overloaded "*" (dynamic (undef, id) :: A.a: (a, (a a -> a) a a -> a)))
		,	("(^) infixr 8", overloaded "^" (dynamic (undef, id) :: A.a: (a, (a a -> a) a a -> a)))
		,	("(==) infix 4", overloaded "==" (dynamic (undef, id) :: A.a: (a, (a a -> Bool) a a -> Bool)))
		,	("(+++) infixr 5", overloaded "+++" (dynamic (undef, id) :: A.a: (a, (a a -> Bool) a a -> Bool)))
		,	("one", overloaded "one" (dynamic (undef, id) :: A.a: (a, a -> a)))
		,	("(<) infix 4", overloaded "<" (dynamic (undef, id) :: A.a: (a, (a a -> Bool) a a -> Bool)))
		]
	
	stdClass =
		[	("inc", overloaded2 "+" "one" (dynamic (undef, undef, \p o x -> p o x) :: A.a: (a, a, (a a -> a) a a -> a)))
		,	("dec", overloaded2 "-" "one" (dynamic (undef, undef, \m o x -> m o x) :: A.a: (a, a, (a a -> a) a a -> a)))
		,	("(<>) infix 4", overloaded "==" (dynamic (undef, \eq x y -> not (eq x y)) :: A.a: (a, (a a -> Bool) a a -> Bool)))
		,	("(>) infix 4", overloaded "<" (dynamic (undef, \less x y -> less y x) :: A.a: (a, (a a -> Bool) a a -> Bool)))
		,	("(<=) infix 4", overloaded "<" (dynamic (undef, \less x y -> not (less y x)) :: A.a: (a, (a a -> Bool) a a -> Bool)))
		,	("(>=) infix 4", overloaded "<" (dynamic (undef, \less x y -> not (less x y)) :: A.a: (a, (a a -> Bool) a a -> Bool)))
		,	("min", overloaded "<" (dynamic (undef, \less x y -> if (less x y) x y) :: A.a: (a, (a a -> Bool) a a -> a)))
		,	("max", overloaded "<" (dynamic (undef, \less x y -> if (less x y) y x) :: A.a: (a, (a a -> Bool) a a -> a)))
		]
	
	stdInt =
		[	("instance < Int", dynamic (<) :: Int Int -> Bool)
		,	("instance + Int", dynamic (+) :: Int Int -> Int)
		,	("instance * Int", dynamic (*) :: Int Int -> Int)
		,	("instance - Int", dynamic (-) :: Int Int -> Int)
		,	("instance == Int", dynamic (==) :: Int Int -> Bool)
		,	("instance one Int", dynamic 1)
		]
	
	stdReal =
		[	("instance + Real", dynamic (+) :: Real Real -> Real)
		,	("instance - Real", dynamic (-) :: Real Real -> Real)
		,	("instance * Real", dynamic (*) :: Real Real -> Real)
		,	("instance == Real", dynamic (==) :: Real Real -> Bool)
		,	("instance one Real", dynamic 1.0)
		,	("instance < Real", dynamic (<) :: Real Real -> Bool)
		]
			
	stdList =
		[	("instance == [a]", overloaded "==" (dynamic (undef, eqList) :: A.a: (a, (a a -> Bool) [a] [a] -> Bool)))
		,	("take", dynamic take :: A.a: Int [a] -> [a])
		,	("map", dynamic map :: A.a b: (a -> b) [a] -> [b])
		,	("(!!)", dynamic (!!) :: A.a: [a] Int -> a)
		,	("drop", dynamic drop :: A.a: Int [a] -> [a])
		]
	where
		eqList eq [x:xs] [y:ys] = eq x y && eqList eq xs ys
		eqList _ [] [] = True
		eqList _ _ _ = False
	
	stdFunc =
		[	("id", dynamic id :: A.a: a -> a)
		,	("const", dynamic const :: A.a b: a b -> a)
		,	("(o) infixr 9", dynamic (o) :: A.a b c: (b -> c) (a -> b) a -> c)
		,	("`bind` infixr 9", dynamic (`bind`) :: A.a b s: (*s ->  *(a, *s)) (a *s -> *(b, *s)) *s -> *(b, *s))
		,	("return", dynamic return :: A.a s: a *s -> *(a, *s))
		]
	
	stdMisc =
		[	("undef", dynamic undef :: A.a: a)
		,	("abort", dynamic abort :: A.a: String -> a)
		]
	where
		undef = raise UndefEvaluated
		abort msg = raise (AbortEvaluated msg)

	stdBool =
		[	("not", dynamic not)
		,	("(||) infixr 2", dynamic (||))
		,	("(&&) infixr 3", dynamic (&&))
		,	("instance == Bool", dynamic (==) :: Bool Bool -> Bool)
		]

	stdString =
		[	("instance +++ {#Char}", dynamic (+++) :: String String -> String)
		]

	stdTuple =
		[	("snd", dynamic snd :: A.a b: (a, b) -> b)
		,	("fst", dynamic fst :: A.a b: (a, b) -> a)
		,	("fst3", dynamic fst3 :: A.a b c: (a, b, c) -> a)
		,	("snd3", dynamic snd3 :: A.a b c: (a, b, c) -> b)
		,	("thd3", dynamic thd3 :: A.a b c: (a, b, c) -> c)
		]
		