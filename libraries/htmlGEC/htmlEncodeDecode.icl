implementation module htmlEncodeDecode

// encoding and decoding of information

import StdEnv, ArgEnv, StdMaybe


import GenParse, htmlPrint, htmlDataDef

GetArgs :: String 
GetArgs = foldl (+++) "" [strings \\ strings <-: getCommandLine]

ThisExe :: String
ThisExe = mkString (urlDecode (takeWhile ((<>) '#') (mkList GetArgs)))

MyPhP :: String
MyPhP = (mkString (takeWhile ((<>) '.') (mkList ThisExe))) +++ ".php"

UpdateInfo :: (String,String,String,String)
UpdateInfo
# input 			= urlDecode (mkList GetArgs)
# (garbage,input) 	= scan '?' input 		// get rid of garbage
# (update, input)	= scan ';' input
# (new,    input)	= scan ';' input
# (x,      input)	= scan ';' input
# (state,  input)	= scan ';' input
# (who,    input)	= scan ';' input
= (mkString update, mkString (Tl new), mkString (urlDecode state), mkString (Tl who))
where
	scan c list = case (span ((<>) c) list) of
					(x,[])	= (x,[])
					(x,y)	= (x,tl y)
	Tl [] = []
	Tl list = tl list

CheckUpdateInfo :: (Maybe a, Maybe b, Maybe c, Maybe d) | gParse{|*|} a & gParse{|*|} b & gParse{|*|} c & gParse{|*|} d
CheckUpdateInfo = (parseString update, parseString new, parseString state, parseString id)
where
	(update, new, state, id) = UpdateInfo

traceHtmlInput :: Body
traceHtmlInput
=	Body 	[
			   T "input received       : " , B GetArgs, Br 
			,  T "input plain decoded  : " , B (convert GetArgs), Br 
			, T "this executable      : " , B ThisExe, Br 
			, T "my php script        : " , B MyPhP, Br 
			, T "update				  : " , B update, Br 
			, T "new				  : " , B new, Br 
			, T "state				  : " , B state, Br 
			, T "who				  : " , B who, Br 
			]
where
	(update,new,state,who) = UpdateInfo
	convert s = mkString (urlDecode (mkList s))


encodeInfo :: a -> String | gPrint{|*|} a
encodeInfo inp = encoding  
where
	encoding = mkString (urlEncode (mkList (printToString inp)))

decodeInfo :: String -> Maybe a | gParse{|*|} a // parsing input parameters submitted to applications
decodeInfo s = parseString (mkString (urlDecode (mkList s)))

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

// encoding of special info

encodeUpdate :: a -> String | gPrint{|*|} a
encodeUpdate inp = "\"?" +++  encodeInfo inp +++ ";\""  

encodeHidden :: a -> String | gPrint{|*|} a
encodeHidden inp = "x;" +++  encodeInfo inp +++ ";"  



