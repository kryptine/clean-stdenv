module GenericDatabase

import Gerda, StdEnv

:: R = {naam :: [Char], leeftijd :: Real, rec :: Maybe R}
:: A = C R
:: B = D A String | E (Char, Int) | F
:: N = C1 | C2 | C3
:: Tree a b = Bin !(Tree a b) !a !(Tree a b) | Tip !b
:: Rose a = Rose a [Rose a]
:: GRose m a = GRose a (m (GRose m a))
:: Test` = Constr` Int

Start world 
	# (g, world) = openGerda "Clean Data Structures" world
//	  x = 42
//	  x = [[1]]
//	  x = (1, 3.1415927, 'a', [C2, C1, C3])
//	  x = [3, 5, 7]
//	  x = [C3, C1, C2, C1, C3, C2]
//	  x = [[[[[[[1]]]]]]]
//	  x = ["test"]
//	  x = ['aapnoot']
//	  x = [['aap'], ['noot']]
//	  x = Bin (Tip 'a') 42 (Tip 'b')
//	  x = Rose 1 [Rose 2 [], Rose 3 []]
//	  x = [[C1, C2], [C3, C2, C1]]
//	  r1 = {naam = ['aap'], leeftijd = 13.5, rec = Nothing} 
//	  r2 = {naam = ['aap'], leeftijd = 13.5, rec = Just r1}
//	  x = array {r1, r2, r2, r1}
//	  x = ["Hello", "world"]
//	  x = Rose 1 [Rose 2 [], Rose 3 []]
//	  x = "a" +++ {'b' \\ _ <- [1..1000]}
//	  x = GRose (1, 'a', 0.5, "bud") [GRose (2, 'b', 0.75, "another bud") [], GRose (3, 'c', 0.875, "yet another bud") []]
//	  x = [1..10000]
//	  x = array {strictArray {1, 2, 3}, strictArray {1 .. 100}}
//	  x = Constr` 42
	  x = PAIR Nothing Nothing
	  g = writeGerda "test`.1" x g
//	  g = writeGerda "test2" x2 g
	  (y, g) = readGerda "test`.1" g
//	  (y2, g) = readGerda "test2" g
/*	= case y `typeOf` x of
		Just x`
		  # g = writeGerda "test2" x` g
		    (z, g) = readGerda "test" g
		  -> (z `typeOf` x, closeGerda g world)*/
	= (y `typeOf` x, closeGerda g world)
//	  x = gerdaObject 42
//	  g = writeGerda "test" x g
//	  (y, g) = readGerda "test" g
//	  f = case y of Just {gerdaWrite} -> gerdaWrite; _ -> const id
//	  h = case y of Just {gerdaRead} -> gerdaRead; _ -> (\g -> (undef, g))
//	  g = f 123 g
//	  (w, g) = h g
//	  (z, g) = readGerda "test" g
//	= (y `typeOf` x, Just w `typeOf` x.gerdaValue, z `typeOf` x, closeGerda g world)
where
	(`typeOf`) :: !(Maybe (EITHER Int Int)) (PAIR (Maybe Int) (Maybe Int)) -> Maybe (EITHER Int Int)
	(`typeOf`) x _ = x

	x :: Phantom [Int]
	x = Opera

	x2 :: Phantom Char
	x2 = Opera

	array :: !{a} -> {a}
	array x = x

	strictArray :: !{!a} -> {!a}
	strictArray x = x

derive gerda Tree, Rose, R, N, (,), (,,), (,,,), GRose, Test`, Phantom

:: Phantom a = Opera

/*
:: T3 a b c = C3 a b c | D3 | E3

:: T a = C (T a) | D (T a) | E (T (a, a)) | F (Rose a)
:: Rose a = Node [Rose a] | Leaf a

:: W a = W1 (R a)
:: R a = R1 (W a) | R2 a

:: T4 a = T4 [[[[[[[[[[a]]]]]]]]]]

:: T5 a = T5 [[[[(Real, [[a]]) -> ([[[a]]], Int)]]]]

:: T6 a b = T6a (T7 b a)

:: T7 a b = T7a (T6 a b) | T7b a

:: T8 a b c d e f g h = T8a (T8 b c d e f g h a) | T8b a

:: T9 a b c d = T9a [T9 Int [(Real, ((String, a), Char))] [[((Int, b), Int)]] [[[c]]]] | T9b (Real, d)

:: T1 a = T1 a
:: T2 a = T2 (T1 a)

:: S1 a = S1 a
:: S2 a = S2 (S1 a)
:: S3 a = S3R (S1 a) | S3 (S2 a)

:: X1 a b c = X1a (X2 a c b)
:: X2 a b c = X2a (X3 b a c)
:: X3 a b c = X3a (X4 b c a)
:: X4 a b c = X4a (X5 c a b)
:: X5 a b c = X5a (X6 c b a)
:: X6 a b c = X6a a | X6b (X1 b b c) | X6c (X1 b c c)

:: Rec a b = {f1 :: Rose a, f2 :: Rec [b] [a]}

:: ADT a b = Rec a b
*/