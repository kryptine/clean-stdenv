module gentest

import StdEnv, GenLib

:: Tree a b = Tip a | Bin b (Tree a b) (Tree a b)
:: Rose a = Rose a .[Rose a]
:: Fork a = Fork a a
:: Sequ a = SequEmpty | SequZero .(Sequ .(Fork a)) | SequOne a .(Sequ .(Fork a))
:: InfCons 
	= :+: infixl 2 InfCons InfCons
	| :-: infixl 2 InfCons InfCons
	| :*: infixl 3 InfCons InfCons
	| :->: infixr 4 InfCons InfCons
	| U
	| I Int 
:: Rec a b c = { rec_fst :: a, rec_snd :: b, rec_thd :: c }	

derive bimap [], (,), Maybe
derive bimap Tree, Rose, Fork, Sequ

derive gEq 				Tree, Rose, Fork, Sequ, InfCons, Rec
derive gLexOrd 			Tree, Rose, Fork, Sequ
derive gMap 			Tree, Rose, Fork, Sequ
derive gMapLSt 			Tree, Rose, Fork, Sequ
derive gMapRSt 			Tree, Rose, Fork, Sequ
derive gReduceLSt 		Tree, Rose, Fork, Sequ
derive gReduceRSt 		Tree, Rose, Fork, Sequ
derive gReduce 			Tree, Rose, Fork, Sequ
derive gZip				Tree, Rose, Fork, Sequ
derive gMaybeZip 		Tree, Rose, Fork, Sequ
derive gPrint			Tree, Rose, Fork, Sequ, InfCons, Rec
derive gParse			Tree, Rose, Fork, Sequ, InfCons, Rec

testEq :: [Bool]
testEq =	
	[ [1,2,3] === [1,2,3]
	, [1,2,3] =!= [1,2,3,4]
	, [1,2,3] =!= [1,2,4] 
	, Bin 1 (Tip 2.0) (Tip 3.0) === Bin 1 (Tip 2.0) (Tip 3.0)
	, Rose 1 [Rose 2 [], Rose 3 []] === Rose 1 [Rose 2 [], Rose 3 []]
	, SequZero (SequOne (Fork 1 2) (SequOne (Fork (Fork 3 4) (Fork 5 6)) SequEmpty)) === SequZero (SequOne (Fork 1 2) (SequOne (Fork (Fork 3 4) (Fork 5 6)) SequEmpty))
 	]

testLexOrd = 
	[ ([1,2,3] =?= [1,2,3]) === EQ 
	, ([1,2,3] =?= [1,2,3,4]) === LT
	, ([1,2,4] =?= [1,2,3,4]) === GT
	, (Rose 1 [Rose 2 [], Rose 3 []] =?= Rose 1 [Rose 2 [], Rose 3 []]) === EQ 
	, (Rose 1 [Rose 2 [], Rose 3 []] =?= Rose 1 [Rose 2 [], Rose 3 [], Rose 4 []]) === LT
	, (Rose 1 [Rose 2 [], Rose 4 []] =?= Rose 1 [Rose 2 [], Rose 3 [], Rose 4 []]) === GT
	]
	
testMap =
	[ gMap{|*->*|} inc [1,2,3] === [2,3,4]
	, gMap{|*->*->*|} inc dec (Bin 1 (Tip 2.0) (Tip 3.0)) === Bin 0 (Tip 3.0) (Tip 4.0)
	, gMap{|*->*|} inc (Rose 1 [Rose 2 [], Rose 3 []]) === Rose 2 [Rose 3 [], Rose 4 []] 
	, gMap{|*->*|} inc (SequZero (SequOne (Fork 1 2) (SequOne (Fork (Fork 3 4) (Fork 5 6)) SequEmpty)))
		=== SequZero (SequOne (Fork 2 3) (SequOne (Fork (Fork 4 5) (Fork 6 7)) SequEmpty))
	]

testMapRSt =
	[ gMapRSt{|*->*|} (\x st-> (dec x, [x:st])) [1,2,3] [] === ([0,1,2], [1,2,3]) 
	]		

testMapLSt =
	[ gMapLSt{|*->*|} (\x st-> (dec x, [x:st])) [1,2,3] [] === ([0,1,2], [3,2,1]) 
	]		

testReduceRSt =
	[ 
	]

testReduceLSt =
	[ 
	]

testParsePrint =
	[ test 1 
	, test 123
	, test -123

	, test 1.09
	, test -123.456
	, test 1.23E-12
	, test 1.23E+12
	, test 1.23E5

	, test True
	, test False

	, test 'a'
	, test '\n'
	, test '"'
	, test '\''
	//, test "Hello"
	//, test "Hello\n"
	//, test "Hello \"string\""

	, test nil
	, test [1]
	, test [1,2,3]

	, test (arr nil)
	, test (arr [1])
	, test (arr [1,2,3])

	, test {rec_fst=1, rec_snd='a', rec_thd=1.2}

	, test (Bin 'a' (Tip 1) (Bin 'b' (Tip 2) (Bin 'c' (Tip 3) (Tip 4))))
	, test (Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [Rose 5 []]])

	, test (U :+: U)
	, test (U :+: U :+: U)
	, test (U :->: U :->: U)
	, test (U :+: U :*: U)
	, test (U :*: U :->: U)
	, test (I 1 :+: I 2 :+: I 3)
	, test (I 1 :*: I 2 :+: I 3)
	, test (I 1 :+: I 2 :*: I 3)
	, test (I 1 :+: I 2 :*: I 3 :+: I 4)
	, test (I 1 :+: (I 2 :+: I 3) :+: I 4)

	, test [I 1 :+: I 2 :+: I 3, I 4 :->: I 5 :->: I 6]
	, test (arr [I 1 :+: I 2 :+: I 3, I 4 :->: I 5 :->: I 6])
	, test 
		{	rec_fst = I 1 :+: I 2 :+: I 3
		, 	rec_snd = I 4 :->: I 5 :->: I 6
		,	rec_thd = I 7 :*: I 8 :+: I 9
		}
	]
where
	test x = case parseString (printToString x) of
		Nothing -> False
		Just y -> x === y

	nil :: [Int]
	nil = []

	arr :: [a] -> {a}
	arr xs = {x\\x<-xs}

Start :: [[Bool]]	
Start
	# result = foldr (&&) True (flatten tests)
	| result
		= [[result]]
		= tests
where
	tests =
		[ testEq
		, testLexOrd
		, testMap
		, testMapRSt
		, testMapLSt
		, testReduceRSt
		, testReduceLSt
		, testParsePrint
		]
