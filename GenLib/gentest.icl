module gentest

import StdEnv, GenLib

:: Tree a b = Tip a | Bin b (Tree a b) (Tree a b)
:: Rose a = Rose a .[Rose a]
:: Fork a = Fork a a
:: Sequ a = SequEmpty | SequZero .(Sequ .(Fork a)) | SequOne a .(Sequ .(Fork a))

derive bimap [], (,), Maybe
derive bimap Tree, Rose, Fork, Sequ

derive gEq 				Tree, Rose, Fork, Sequ
derive gLexOrd 			Tree, Rose, Fork, Sequ
derive gMap 			Tree, Rose, Fork, Sequ
derive gMapLSt 			Tree, Rose, Fork, Sequ
derive gMapRSt 			Tree, Rose, Fork, Sequ
derive gReduceLSt 		Tree, Rose, Fork, Sequ
derive gReduceRSt 		Tree, Rose, Fork, Sequ
derive gReduce 			Tree, Rose, Fork, Sequ
derive gZip				Tree, Rose, Fork, Sequ
derive gMaybeZip 		Tree, Rose, Fork, Sequ

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
		]
