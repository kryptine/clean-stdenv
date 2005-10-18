implementation module htmlTrivial

import StdMaybe, StdGeneric, StdEnv

derive bimap Maybe, (,)

// converting strings to lists and backwards

mkString :: [Char] -> *String
mkString listofchar = {elem \\ elem <- listofchar}

mkList :: String -> [Char]
mkList string = [e \\ e <-: string]

//	Useful string concatenation function
(<$) infixl :: !String !a -> String | toString a
(<$) str x = str +++ toString x
