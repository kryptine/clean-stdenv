implementation module htmlEncodeDecode

// encoding and decoding of information

import StdEnv, ArgEnv, StdMaybe
import htmlDataDef, htmlTrivial
import GenPrint, GenParse

:: UpdValue 	= UpdI Int							// new integer value
				| UpdR Real							// new real value
				| UpdC String						// choose indicated constructor 
				| UpdS String						// new piece of text

mscan c list = case (span ((<>) c) list) of  // scan like span but it removes character
				(x,[])	= (x,[])
				(x,y)	= (x,tl y)

// encoding and decoding of Clean values 

encodeInfo :: a -> String | gPrint{|*|} a
encodeInfo inp = encoding  
where
	encoding = mkString (urlEncode (mkList (printToString inp)))


decodeInfo :: String -> Maybe a | gParse{|*|} a
decodeInfo s = parseString (mkString (urlDecode (mkList s)))

// all input information from the browser is obtained once via the arguments passed to this executable
// defined as CAFs such that they are calculated only once

GetArgs :: String 
GetArgs =: foldl (+++) "" [strings \\ strings <-: getCommandLine]

ThisExe :: String
ThisExe 
# (thisexe,_,_,_) = UpdateInfo
=: thisexe

MyPhP :: String
MyPhP =: (mkString (takeWhile ((<>) '.') (mkList ThisExe))) +++ ".php"

UpdateInfo :: (!String,!String,!String,!String) // executable, id + update , new , state
UpdateInfo
# input 			= mkList GetArgs
# (thisexe,input) 	= mscan '#' input 		// get rid of garbage
# input				= skipping ['#UD=']  input
# (update, input)	= mscan '=' input
# (new,    input)	= mscan ';' input
# input				= skipping ['GS=']  input
# (state, input)	= mscan ';' input
=: case (decode thisexe, decode update, decode new, decode state) of
		(thisexe,"CS",new,state) -> (thisexe,new,"",state)
	    else		    -> else
where
	Tl [] = []
	Tl list = tl list

	skipping [c:cs] list=:[x:xs]
	| c == x 		= skipping cs xs
	| otherwise 	= list
	skipping any    list = list
	
	decode n = mkString (urlDecode n)

traceHtmlInput :: BodyTag
traceHtmlInput
=:	BodyTag	[	Txt "this executable    : " , B [] ThisExe, Br 
			, 	Txt "my php script      : " , B [] MyPhP, Br 
			, 	Txt "update				: " , B [] update, Br 
			, 	Txt "new value		  	: " , B [] new, Br 
			, 	Txt "state			  	: " , BodyTag (showstate (mkList state)), Br 
			,	Txt "decoded input  	: " , B [] (convert GetArgs), Br
			,	Txt "encoded input      : " , B [] GetArgs, Br 
			]
where
	(executable,update,new,state) = UpdateInfo
	convert s = mkString (urlDecode (mkList s))

	showstate :: [Char] -> [BodyTag]
	showstate [] 			= []
	showstate listofchar	= [Br, B [] (mkString first)] ++ showstate (tl second)
	where
		(first,second) = span ((<>) '$') listofchar

CheckUpdateId :: String
CheckUpdateId 		
# (_,upd,_,_) = UpdateInfo
=: case parseString upd of
	Just ("",0,UpdI 0) = ""
	Just (id,_,_)   = id 
	else = ""
derive gParse UpdValue

StrippedCheckUpdateId :: String
StrippedCheckUpdateId
=: mkString (takeWhile ((<>) '_') (mkList CheckUpdateId))

AnyInput :: String
AnyInput
# (_,_,new,_) = UpdateInfo
=: new

CheckUpdate :: (!Maybe a, !Maybe b) | gParse{|*|} a & gParse{|*|} b 
CheckUpdate 
# (_,upd,new,_) = UpdateInfo
= (parseString upd, parseString new)


CheckGlobalState :: GlobalState
CheckGlobalState 
# (_,_,_,state) = UpdateInfo
=: splitString (mkList state)
where
	splitString [] 			= []
	splitString listofchar	= [mktuple first : splitString second]
	where
		(first,second) = mscan '$' listofchar

		mktuple :: [Char] -> (!String,!String)
		mktuple	elem = (mkString formid,mkString (stl (reverse (stl (reverse formvalue)))))
		where
			(formid,formvalue) = mscan '"' (stl (stl elem)) // skip '("'
			stl [] = []
			stl [x:xs] = xs 

findState :: String GlobalState -> (Maybe a) | gParse{|*|} a 
findState curformid []	= Nothing
findState curformid [(formid,formstate):nextstate]
| curformid == formid		= parseString formstate
| otherwise					= findState curformid nextstate

	

derive gParse (,), (,,)


// low level url encoding decoding

mkString :: [Char] -> String
mkString listofchar = {elem \\ elem <- listofchar}

mkList :: String -> [Char]
mkList string = [e \\ e <-: string]

urlEncode :: [Char] -> [Char]
urlEncode [] = []
urlEncode [x:xs] 
| isAlphanum x = [x  : urlEncode xs]
| otherwise    = urlEncodeChar x ++ urlEncode xs

urlEncodeChar x 
# (c1,c2) = charToHex x
= ['%', c1 ,c2]

urlDecode :: [Char] -> [Char]
urlDecode [] 				= []
urlDecode ['%',hex1,hex2:xs]= [hexToChar(hex1, hex2):urlDecode xs]
urlDecode [x:xs] 			= [x:urlDecode xs]

charToHex :: !Char -> (!Char, !Char)
charToHex c = (toChar (digitToHex (i >> 4)), toChar (digitToHex (i bitand 15)))
where
        i = toInt c
        digitToHex :: !Int -> Int
        digitToHex d
                | d <= 9 = d + toInt '0'
                = d + (toInt 'A' - 10)

hexToChar :: !(!Char, !Char) -> Char
hexToChar (a, b) = toChar (hexToDigit (toInt a) << 4 + hexToDigit (toInt b))
where
        hexToDigit :: !Int -> Int
        hexToDigit i
                | i <= toInt '9' = i - toInt '0'
                = i - (toInt 'A' - 10)

urlEncodeS :: String -> String
urlEncodeS s = (mkString o urlEncode o mkList) s

urlDecodeS :: String -> String
urlDecodeS s = (mkString o urlDecode o mkList) s

urlEncodeState :: [(String,String)] -> String
urlEncodeState [] = urlEncodeS "$"
urlEncodeState [(x,y):xsys] = urlEncodeS "(\"" +++ x +++ urlEncodeS "\"," +++
							  y +++ urlEncodeS ")$" +++ urlEncodeState xsys 

// script for transmitting name and value of changed input 

callClean :: String
callClean =: "toclean(this)"

globalFormName :: String
globalFormName =: "CleanForm"

updateInpName :: String
updateInpName =: "UD"

globalInpName :: String
globalInpName =: "GS"

selectorInpName :: String
selectorInpName =: "CS"

addScript :: GlobalState -> BodyTag
addScript globalstate
=	BodyTag
	[ submitscript    globalFormName updateInpName
	, globalstateform globalFormName updateInpName globalInpName (SV encodedglobalstate) 
	]
where
	encodedglobalstate = urlEncodeState (globalstate)
	
	removedup []  = []
	removedup [(id,body):rest] = [(id,body):[(nid,nbody) \\ (nid,nbody) <- removedup rest | nid <> id]]

submitscript :: String String -> BodyTag
submitscript formname updatename
=	Script []
	(	" function toclean(inp)" +++
		" { document." +++
			formname  +++ "." +++
			updatename +++ ".value=inp.name+\"=\"+inp.value;" +++
			"document." +++ formname +++ ".submit(); }"
	)

// form that contains global state and empty input form for storing updated input
	
globalstateform :: String String String Value -> BodyTag
globalstateform formname updatename globalname globalstate
=	Form 	[ Frm_Name formname
			, Frm_Action MyPhP
			, Frm_Method Post
			]
			[ Input [ Inp_Name updatename
					, Inp_Type Inp_Hidden
					] ""
			, Input [ Inp_Name globalname
					, Inp_Type Inp_Hidden
					, Inp_Value globalstate
					] ""
			]		 

