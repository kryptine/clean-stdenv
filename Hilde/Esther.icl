module Esther

import StdEnv, EstherScript, EstherStdEnv, DynamicFileSystem

class f a :: a b b

instance f Bimap
where
	f = undef

Start :: !*World -> String
Start world
	# st = {searchPath = [], searchCache = [], buildin = stdEnv, env = world}
	  (d, st) = compose input st
	  (v, t) = toStringDynamic d
	= "\n" +++ input +++ "\n==>\n" +++ v +++ " :: " +++ t +++ "\n"
where
//	input = "[1,3 .. 10]"
//	input = "(+) one"
//	input = "(\\((1, 4), (2, 3)) -> True) ((1, 4), (2, 3))"
//	input = "let f = \\xs -> case xs of (Cons 3 _) -> True in f (Cons 3 Nil)"
//	input = "let x = Cons 1 Nil in case x of (Cons 1 Nil)  -> True; _ -> False"
//	input = "case [False, True, False] of [True] -> False; [False :_] -> True"
//	input = "(\\x -> case x of (1, \"a\") -> True; _ -> False) (1, \"a\")"
//	input = "(\\(1, y) -> True) (1, \"a\")"
//	input = "\\x y z -> (1 2)"
//	input = "let x = [1 : y]; y = [2 : x] in (\\n -> take n x) 10"
//	input = "let fac = \\x -> if (x >R 1.0) (x *R fac (x -R 1.0)) 1.0 in fac 170.0"
//	input = "let (x, y) = (1, x) in y"
//	input = "let tak = \\x y z -> if (x <= y) z (tak (tak (dec x) y z) (tak (dec y) z x) (tak (dec z) x y)) in tak 24 16 8"
//	input = "let nfib = \\n -> if (n < 2) 1 (nfib (n - 1) + nfib (n - 2) + 1) in nfib 32"
//	input = "let ones = [1 : ones] in ones"
//	input = "(\\x -> const (x + 1) undef) 41"
//	input = "\\x -> x + True"
//	input = "2 ^ 3 infixK 2"
//	input = "1 - 4 - 9"
//	input = "1 - 4 * 9 - 10"
//	input = "3 + const 1 2"
//	input = "[ 1 , 4 .. 100 ]"
//	input = "(\\x -> max (inc x) x) 1"
//	input = "f\\ x y = 42"
//	input = "[ x \\\\ x <- [ 1 .. 100 ] ]"
	input = "2 * 3 * 4"

/*	eval :: !Int !(a -> b) !a -> b
	eval 1 f x = f x
	eval n f x = eval (force (f x) (n - 1)) f x
	where
		force :: !.a !.b -> .b
		force _ r = r*/
