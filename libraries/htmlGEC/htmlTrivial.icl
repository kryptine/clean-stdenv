implementation module htmlTrivial

import StdMaybe, StdGeneric, StdEnv

derive bimap Maybe, (,)

// converting strings to lists and backwards

mkString :: [Char] -> *String
mkString listofchar = {c \\ c <- listofchar }

mkList :: String -> [Char]
mkList string = [c \\ c <-: string ]

//	Useful string concatenation function
(<+++) infixl :: !String !a -> String | toString a
(<+++) str x = str +++ toString x

isNil :: [a] -> Bool
isNil [] = True
isNil _ = False


