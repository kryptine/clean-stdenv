implementation module htmlEncodeDecode

// encoding and decoding of information

import StdEnv, ArgEnv, StdMaybe
import GenParse, GenPrint, htmlDataDef


// encoding 

encodeInfo :: a -> String | gPrint{|*|} a
encodeInfo inp = encoding  
where
	encoding = mkString (urlEncode (mkList (printToString inp)))

encodeUpdate :: a -> String | gPrint{|*|} a
encodeUpdate inp = "\"?" +++  encodeInfo inp +++ ";\""  

encodeHidden :: (String, String) -> String // | gPrint{|*|} a
encodeHidden (id,state) = ";i;" +++  encodeInfo id +++  ";s;" +++ state 

// decoding

// all input information is obtained via the arguments given to the executable

GetArgs :: String 
GetArgs = foldl (+++) "" [strings \\ strings <-: getCommandLine]

ThisExe :: String
ThisExe = mkString (urlDecode (takeWhile ((<>) '#') (mkList GetArgs)))

MyPhP :: String
MyPhP = (mkString (takeWhile ((<>) '.') (mkList ThisExe))) +++ ".php"

UpdateInfo :: (String,String,String,String) // update , new , id, state
UpdateInfo
# input 			= mkList GetArgs
# (garbage,input) 	= scan '?' input 		// get rid of garbage
# (update, input)	= scan ';' input
# (new,    input)	= scan ';'  input
# (notused,input)	= scan '='  input
# input				= skipping [';i;']  input
# (formid, input)	= scan ';' input
# input				= skipping ['s;'] input
# (state,  input)	= scan ';' input
= 	( decode update
	, decode (Tl new)			// skip '='
	, decode formid
	, decode state)
where
	scan c list = case (span ((<>) c) list) of
					(x,[])	= (x,[])
					(x,y)	= (x,tl y)
	Tl [] = []
	Tl list = tl list

	skipping [c:cs] list=:[x:xs]
	| c == x 		= skipping cs xs
	| otherwise 	= list
	skipping any    list = list
	
	decode n = mkString (urlDecode n)

CheckUpdateInfo :: (Maybe a, Maybe b, Maybe c, Maybe d) | gParse{|*|} a & gParse{|*|} b & gParse{|*|} c & gParse{|*|} d
CheckUpdateInfo = (parseString update, parseString new, parseString id, parseString state)
where
	(update, new, id, state) = UpdateInfo


:: UpdateInfo a b c d
		= 	{	updateVal :: Maybe a	// value of submitted input field
			,	newVal	  :: Maybe b	// new value it should get, if applicable	
			,	stateVal  :: Maybe c	// all states stored in html page
			,	idVal	  :: Maybe d	// identification of Clean data structure which has been updated
			}

ParseUpdateInfo :: UpdateInfo a b c d | gParse{|*|} a & gParse{|*|} b & gParse{|*|} c & gParse{|*|} d
ParseUpdateInfo = 	{	updateVal 	=	parseString update
					, 	newVal		=	parseString new
					, 	stateVal	=	parseString state
					, 	idVal		=	parseString id
					}
where
	(update, new, state, id) = UpdateInfo


traceHtmlInput :: Body
traceHtmlInput
=	Body 	[	T "this executable      : " , B ThisExe, Br 
			, 	T "my php script        : " , B MyPhP, Br 
			, 	T "update				: " , B update, Br 
			, 	T "new value		  	: " , B new, Br 
			, 	T "id of form			: " , B id, Br 
			, 	T "state			  	: " , B state, Br 
			,	T "input received       : " , B GetArgs, Br 
			,	T "input plain decoded  : " , B (convert GetArgs), Br
			]
where
	(update,new,id,state) = UpdateInfo
	convert s = mkString (urlDecode (mkList s))

decodeInfo :: String -> Maybe a | gParse{|*|} a // parsing input parameters submitted to applications
decodeInfo s = parseString (mkString (urlDecode (mkList s)))

// url encoding decoding

mkString :: [Char] -> String
mkString listofchar = {elem \\ elem <- listofchar}

mkList :: String -> [Char]
mkList string = [e \\ e <-: string]


urlEncode :: [Char] -> [Char]
urlEncode [] = []
urlEncode [x:xs] 
| isAlphanum x = [x  : urlEncode xs]
| otherwise    = ['%': charhex (toInt x) ++ urlEncode xs]
where
	charhex num = [toHex (num/16), toHex (num rem 16)]
	toHex i = HexNumber !! i

HexNumber = ['0123456789ABCDEF']

urlDecode :: [Char] -> [Char]
urlDecode [] 				= []
urlDecode ['%',hex1,hex2:xs]= [hexchar hex1 hex2:urlDecode xs]
where
	hexchar hex1 hex2 = toChar (fromHex hex1 * 16 + fromHex hex2)
	fromHex c = hd [i \\ h <- HexNumber & i <- [0..15] | h == c]
urlDecode [x:xs] 			= [x:urlDecode xs]

urlEncodeS :: String -> String
urlEncodeS s = (mkString o urlEncode o mkList) s

urlDecodeS :: String -> String
urlDecodeS s = (mkString o urlDecode o mkList) s



