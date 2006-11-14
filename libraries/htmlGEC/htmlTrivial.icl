implementation module htmlTrivial

import StdMaybe, StdGeneric, StdEnv

derive bimap Maybe, (,)

// converting strings to lists and backwards

mkString	:: ![Char] -> *String
mkString	listofchar	= {c \\ c <- listofchar }

mkList		:: !String -> [Char]
mkList		string		= [c \\ c <-: string ]

//	Useful string concatenation function
(<+++) infixl :: !String !a -> String | toString a
(<+++) str x = str +++ toString x

(??) infixl 9 :: ![a] !a -> Int | == a
(??) [a:as] b
	| a==b		= 0
	| otherwise	= 1 + as??b
(??) [] _
	= -1

const2 :: .a !.b -> .b
const2 _ x = x
