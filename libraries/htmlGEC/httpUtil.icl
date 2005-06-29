implementation module httpUtil
import StdEnv, StdLib, StdMaybe


/****************************************************************************
	as -- bs removes all elements in bs from as
****************************************************************************/
(--) infixl 5 :: [a] [a] -> [a] | Eq a
(--) as bs = removeMembers as bs

/**********************************************************************
	General sorting and ordening of a list of elements	
***********************************************************************/
sortOn :: [(t t -> Bool)] [t] -> [t]
sortOn ps items
= sortBy (combined ps) items
where
	combined	:: [ (t t -> Bool) ] t t -> Bool
	combined [] x y			= True
	combined [p:ps] x y		= (p x y == p y x && combined ps x y) || p x y
	
groupOn :: [t -> a] [t] -> [[t]] | ==, < a
groupOn fs xs = (groupBy eqf o sortOn (map smf fs)) xs
where
	eqf a b		= [f a \\ f <- fs] == [f b \\ f <- fs]
	smf f a b	= f a <  f b

splitWith	:: (a -> Bool) 		[a]		-> ([a], [a])
splitWith _     []	=	([],[])
splitWith p [x:xs]
	| p x			=	([x:as],bs) 
	| otherwise		=	(as,[x:bs])
	where
		(as,bs)		= splitWith p xs



/**********************************************************************
	Some String utilities:
***********************************************************************/
words :: !a -> [String] | toString a
words a
	| size s == 0	= []
	| otherwise		= [s%(b,e-1) \\ (b,e) <- bes2]
where
	s = toString a
	bes1 = [i \\ i <- [1..(size s - 1)] | (isSpace s.[i]) <> (isSpace s.[i-1])]
	// alleen nog de vraag of je zo niet het laatste woord mist?
	// zo ja moeten we nog (size s - 1) aan de staart van bes1 toevoegen...
	// waarschijnlijk handigst in zip` met
	// zip` [b] = [(b,size s - 1)]
	bes2
		| isSpace s.[0]
			= zip` bes1
			= zip` [0:bes1]

	zip` [b] = [(b,size s)]
	zip` [b,e:r] = [(b,e):zip` r]
	zip` _ = []

wordsWith :: !.Char !String	-> [String]
wordsWith c s
	| s=="" && r==""	= []
	| r==""				= [s]
	| otherwise			= [f : wordsWith c r]
where
	(f,r)				= cSplit c s

splitAfter :: !.Char !String -> (!String,!String)
splitAfter c s = (s%(b1,e1),s%(b2,e2))
where
	sp = findPos c s 0 e2
	b1 = 0
	e1 = sp
	b2 = sp + 1
	e2 = size s

	findPos :: Char String Int Int -> Int
	findPos c s i end
		| i >= end		= end
		| s.[i] == c	= i
		| otherwise		= findPos c s (inc i) end

cSplit :: !.Char !String -> (!String,!String)
cSplit c s = (s%(b1,e1),s%(b2,e2))
where
	sp = findPos c s 0 e2
	b1 = 0
	e1 = sp - 1
	b2 = sp + 1
	e2 = size s

	findPos :: Char String Int Int -> Int
	findPos c s i end
		| i >= end		= end
		| s.[i] == c	= i
		| otherwise		= findPos c s (inc i) end

sSplit :: !String !String -> (!String,!String)
sSplit sep s = (s%(b1,e1),s%(b2,e2))
where
	sp = findPos sep s 0 e2
	b1 = 0
	e1 = sp - 1
	b2 = sp + ic + 1
	e2 = size s
	ic = size sep-1

	findPos :: String String Int Int -> Int
	findPos c s i end
		| i >= end			= end
		| s%(i,i+ic) == c	= i
		| otherwise			= findPos c s (inc i) end

unwords :: ![a] -> String | toString a
unwords ss		= flatWith " " ss

unlines :: ![a] -> String | toString a
unlines ss		= flatWith "\n" ss

/*flatWith :: !a ![b] -> String | toString a & toString b
flatWith s []    = ""
flatWith s [h]   = toString h
flatWith s [h:t] = toString h +++ toString s +++ flatWith s t
*/

flatWith :: !a ![b] -> String | toString a & toString b
flatWith sep items
= copyLines (createArray (sum (map size lines) + nrOfSep * (size ssep)) ' ') 0 lines
where
	nrOfSep
		| isEmpty items = 0
		| otherwise		= length items - 1
		
	lines	= map toString items
	ssep	= toString sep
	
	copyLines result n []	= result
	copyLines result n [l]
		# (_,result) = sup (size l) n 0 result l
		= result
		
	copyLines result n [l:ls]
		# (n,result) = sup (size l) n 0 result l
		# (n,result) = sup (size ssep) n 0 result ssep 
		= copyLines result n ls


endWith :: !a ![b] -> String | toString a & toString b
endWith suffix items
= copyLines (createArray (sum (map size lines) + nrOfSep * (size ssuffix)) ' ') 0 lines
where
	nrOfSep	= length items
		
	lines	= map toString items
	ssuffix	= toString suffix
	
	copyLines result n []	= result
		
	copyLines result n [l:ls]
		# (n,result) = sup (size l) n 0 result l
		# (n,result) = sup (size ssuffix) n 0 result ssuffix 
		= copyLines result n ls

sup :: !.Int Int !Int *String String -> (Int,.String)
sup l i j s h
	| j >= l	= (i,s)
	#! s		= {s & [i] = h.[j]}
	= sup l (inc i) (inc j) s h

substring :: String String -> Bool
substring s1 s2 = ss (fromString s1) (fromString s2)
where
	ss :: [Char] [Char] -> Bool
	ss p [] = p ==[]
	ss p xs = take (length p) xs == p || ss p (tl xs)

match :: String String -> Bool
match p s = match` (fromString p) (fromString s)
where
	match` :: [Char] [Char] 	-> Bool 
	match` p        []			=  all ((==) '*') p
	match` []       ss			=  ss == []
	match` ['*':ps] ss			=  match` ps ss
								|| match` ['*' : ps] (tl ss)
	match` ['?':ps] [s:ss]		=  match` ps ss 
	match` [p  :ps] [s:ss]		=  p==s 
								&& match` ps ss 



trim	:: String		-> String
trim s
	| s == ""			= ""
	| otherwise			= s % ((start 0),(end sizeS))
where		end n
				| not (isSpace s.[n]) || n <= 0		= n
				| otherwise 						= end (n-1)

			start n
				| not (isSpace s.[n]) || n >= sizeS	= n
				| otherwise 					= start (n+1)
			
			sizeS = size s - 1

trimQuotes	:: String		-> String
trimQuotes s
	| s == ""			= ""
	| otherwise			= s % ((start 0),(end sizeS))
where		end n
				| (s.[n] <> '\'' && s.[n] <> '\"')  || n <= 0		= n
				| otherwise 						= end (n-1)

			start n
				| (s.[n] <> '\'' && s.[n] <> '\"') || n >= sizeS	= n
				| otherwise 						= start (n+1)
			
			sizeS = size s - 1

stringToUpper :: String -> String
stringToUpper cs
= {toUpper c \\ c <-: cs}

stringToLower :: String -> String
stringToLower cs
= {toLower c \\ c <-: cs}

/**********************************************************************
	Instances on Maybe:
***********************************************************************/
instance < (Maybe a) | < a
where	(<) (Just a) (Just b)	= a < b
		(<) Nothing _			= True
		(<) _ Nothing			= False

instance toString (Maybe a) | toString a
where	toString (Just a) 	= toString a
		toString Nothing	= ""
	
	

/**********************************************************************
	To read all the characters from one File in one readacces
	returns: a String containing all characters in a file
***********************************************************************/
readFile :: *File -> (String, *File)
readFile file
	#	(ok,file)	= fseek file 0 FSeekEnd
	|	not ok		= abort "seek to end of file does not succeed\n"
	#	(pos,file)	= fposition	file
	#	(ok,file)	= fseek file (~pos) FSeekCur
	|	not ok		= abort "seek to begin of file does not succeed\n"
	#	(s, file)	= freads file pos
	=	(s, file)

/**********************************************************************
	To read all the lines from one File
	returns: a list of lines without the "\n"
***********************************************************************/
readStrings :: *File -> ([String], *File)
readStrings file
	# (eof, file)	= fend file
	|  eof			= ([], file)
	# (s, file)		= freadline file
	# s`			= s%(0,size s - 2)
	# (ss, file)	= readStrings file
	| otherwise		= ([s` : ss], file)
	
/**********************************************************************
	To save a list of files: [(fileName,fileContents)] 
	returns: a list of errors and a new fileenvironment
***********************************************************************/
exportFiles :: [(String,String)] *Files -> ([String],*Files)
exportFiles [] files
	= ([],files)
exportFiles [(fn,fc):htmls] files
	# (errors,files)			= exportFiles htmls files
	# (open,htmlfile,files)		= fopen fn FWriteText files
	| not open					= (["could not open: "+++fn+++"\n" : errors],files)
	# (close,files)				= (fclose (htmlfile <<< fc)) files
	| not close					= (["could not close: "+++fn+++"\n" : errors],files)
	| otherwise					= (errors,files)

/**********************************************************************
	Some funtion from the Haskell prelude:
***********************************************************************/
// from the Haskell prelude:
(hseq) infixr 0 ::  !.a .b -> .b
(hseq) a b = b

($)    infixr 0  
($) f  x   :== f x

instance == (Either a b) | == a & == b
 where
   (==) (Left x) (Left y) = y==x
   (==) (Right x) (Right y) = y==x
   (==) _ _ = False

lookup :: a [(a,.b)] -> Maybe .b | == a;
lookup k []       = Nothing
lookup k [(x,y):xys]
      | k==x      = Just y
      | otherwise = 	lookup k xys

foldr1 :: (.a -> .(.a -> .a)) ![.a] -> .a;
foldr1 f [x]      = x
foldr1 f [x:xs]   = f x (foldr1 f xs)

concatMap :: (.a -> [w:b]) -> u:([.a] -> v:[w:b]), [u <= v, u <= w]
concatMap f       = flatten o map f

fromMaybe              :: a (Maybe a) -> a
fromMaybe d Nothing    =  d
fromMaybe d (Just a)   =  a


