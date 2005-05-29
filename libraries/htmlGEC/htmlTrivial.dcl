definition module htmlTrivial

import StdMaybe, StdGeneric

derive bimap Maybe, (,)

// utility 

mkString 	:: [Char] -> String
mkList 		:: String -> [Char]
