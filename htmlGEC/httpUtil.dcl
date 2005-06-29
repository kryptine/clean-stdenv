definition module httpUtil

// (c) 2005 Paul de Mast
// HIO - Breda

import StdEnv
import StdMaybe
import StdLibMisc

/****************************************************************************
	as -- bs removes all elements in bs from as
****************************************************************************/
(--) infixl 5 :: [a] [a] -> [a] | Eq a

/**********************************************************************
	General sorting and ordening of a list of elements	
***********************************************************************/
sortOn		:: [(t t -> Bool)]	[t]		-> [t]
groupOn		:: [t -> a]			[t]		-> [[t]] | ==, < a
splitWith	:: (a -> Bool) 		[a]		-> ([a], [a])


/**********************************************************************
	Some String utilities:
***********************************************************************/
/* words string = list of words in the string */
words 		:: !a -> [String] | toString a

wordsWith	:: !.Char !String	-> [String]

/* a String is split in a part until the seperator and the rest */
splitAfter :: !.Char !String -> (!String,!String)
cSplit :: !.Char !String -> (!String,!String)
sSplit :: !String !String -> (!String,!String)

/* flatWith listOfStrings seperator stringlist = concatenates strings with seperator */
flatWith :: !a ![b]			-> String | toString a & toString b

/* endWith listOfStrings suffix stringlist = concatenates strings with suffix after each string*/
endWith :: !a ![b] -> String | toString a & toString b

/* unwords = flatWith ' ' */
unwords :: ![a] -> String | toString a

/* unlines = flatWith '\n' */
unlines :: ![a] -> String | toString a

trim		:: String			-> String	// remove whitespace at start and end of a String
trimQuotes	:: String			-> String	// ""text"" -> "text"

substring	:: String String	-> Bool
match		:: String String	-> Bool

stringToUpper :: String -> String
stringToLower :: String -> String

/**********************************************************************
	Instances on Maybe:
***********************************************************************/
instance < (Maybe a) | < a
instance toString (Maybe a) | toString a

/**********************************************************************
	To read all the characters from one File in one readacces
	returns: a String containing all characters in a file
***********************************************************************/
readFile :: *File -> (String, *File)

/**********************************************************************
	To read all the lines from one File
	returns: a list of lines without the "\n"
***********************************************************************/
readStrings :: *File -> ([String], *File)


/**********************************************************************
	To save a list of files: [(fileName,fileContents)]
***********************************************************************/
exportFiles :: [(String,String)] *Files -> ([String],*Files)

/**********************************************************************
	Some funtion from the Haskell prelude:
***********************************************************************/
// from the Haskell prelude:
(hseq) infixr 0 ::  !.a .b -> .b

($)    infixr 0  
($) f  x   :== f x

instance == (Either a b) | == a & == b

lookup :: a [(a,.b)] -> Maybe .b | == a;

foldr1 :: (.a -> .(.a -> .a)) ![.a] -> .a;

concatMap :: (.a -> [w:b]) -> u:([.a] -> v:[w:b]), [u <= v, u <= w]

fromMaybe              :: a (Maybe a) -> a
