implementation module htmlEncodeDecode

// encoding and decoding of information

import StdEnv, ArgEnv, StdMaybe
import htmlDataDef, htmlTrivial
import GenPrint
import GenParse

:: UpdValue 	= UpdI Int							// new integer value
				| UpdR Real							// new real value
				| UpdC String						// choose indicated constructor 
				| UpdS String						// new piece of text


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

UpdateInfo :: (String,String,String) // id + update , new , state
UpdateInfo
# input 			= mkList GetArgs
# (garbage,input) 	= scan '?' input 		// get rid of garbage
# (update, input)	= scan ';' input
# (new,    input)	= scan ';'  input
# (notused,input)	= scan '='  input
# input				= skipping [';i;']  input
# (state, input)	= scan ';' input
//# input				= skipping ['s;'] input
//# (state,  input)	= scan ';' input
= 	( decode update
	, decode (Tl new)			// skip '='
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

traceHtmlInput :: Body
traceHtmlInput
=	Body 	[	T "this executable      : " , B ThisExe, Br 
			, 	T "my php script        : " , B MyPhP, Br 
			, 	T "update				: " , B update, Br 
			, 	T "new value		  	: " , B new, Br 
			, 	T "state			  	: " , B state, Br 
			,	T "input received       : " , B GetArgs, Br 
			,	T "input plain decoded  : " , B (convert GetArgs), Br
			]
where
	(update,new,state) = UpdateInfo
	convert s = mkString (urlDecode (mkList s))

CheckUpdateId :: String
CheckUpdateId 		
# (upd,new,state) = UpdateInfo
= case parseString upd of
	Just ("",0,UpdI 0) = ""
	Just (id,_,_)   = id 
	else = ""
derive gParse UpdValue

CheckUpdate :: (Maybe a, Maybe b) | gParse{|*|} a & gParse{|*|} b 
CheckUpdate 
# (upd,new,state) = UpdateInfo
= (parseString upd, parseString new)

CheckGlobalState :: String
CheckGlobalState 
# (upd,new,state) = UpdateInfo
= state

ShiftState :: String -> (Maybe (String,a),String) | gParse{|*|} a 
ShiftState input
# input 			= mkList input
# (result,input) 	= scan '$' input 		// get rid of garbage
= 	(parseString (mkString result), mkString input)
where
	scan c list = case (span ((<>) c) list) of
					(x,[])	= (x,[])
					(x,y)	= (x,tl y)
	Tl [] = []
	Tl list = tl list
	

derive gParse (,), (,,)



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
| otherwise    = urlEncodeChar x ++ urlEncode xs

urlEncodeChar x = ['%', toHex (toInt x/16) , toHex (toInt x rem 16)]
where
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

urlEncodeState :: [(String,String)] -> String
urlEncodeState [] = urlEncodeS "$"
urlEncodeState [(x,y):xsys] = urlEncodeS "(\"" +++ x +++ urlEncodeS "\"," +++
							  y +++ urlEncodeS ")$" +++ urlEncodeState xsys 

/*
urlEncodeState :: [(String,String)] -> String
urlEncodeState [] = urlEncodeS "[]"
urlEncodeState [(x,y):xsys] = urlEncodeS "[(\"" +++ x +++ urlEncodeS "\"," +++
							  y +++ urlEncodeS "):" +++ urlEncodeState xsys +++ 
							  	urlEncodeS "]"

*/
