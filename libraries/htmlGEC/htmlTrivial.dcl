definition module htmlTrivial

import StdMaybe, StdGeneric

derive bimap Maybe, (,)

// utility 

mkString		:: ![Char] -> *String
mkList			:: !String -> [Char]
FindSubstr		:: .[a] !.[a] -> (!Bool,!Int) | == a
stl				:: !u:[.a] -> v:[.a], [u <= v]

//	Useful string concatenation function
(<+++) infixl	:: !String !a -> String | toString a

(??) infixl 9	:: ![a] !a -> Int | == a

const2			:: .a !.b -> .b
