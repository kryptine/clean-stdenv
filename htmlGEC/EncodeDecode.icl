 implementation module EncodeDecode

// encoding and decoding of information
// (c) 2005 MJP

import StdEnv, ArgEnv, StdMaybe, Directory
import htmlDataDef, htmlTrivial, htmlFormData
import GenPrint, GenParse
import dynamic_string

// Serializing Html states...

EncodeHtmlStates :: ![HtmlState] -> String
EncodeHtmlStates [] = "$"
EncodeHtmlStates [(id,lifespan,storageformat,state):xsys] 
	= encodeString
	  (	"(\"" +++ 								// begin mark
		fromLivetime lifespan storageformat +++ // character encodes lifetime and kind of encoding
		id +++ 									// id of state 
	  	"\"," +++ 								// delimiter
	  	 state +++ 								// encoded state 
	  	")" 
	  )	+++
	  "$" +++ 									// end mark
	  EncodeHtmlStates xsys
where
	fromLivetime Page 		PlainString		= "N"	// encode Lifespan & StorageFormat in first character
	fromLivetime Session 	PlainString		= "S"
	fromLivetime Persistent PlainString		= "P"
	fromLivetime Page 		StaticDynamic	= "n"
	fromLivetime Session 	StaticDynamic	= "s"
	fromLivetime Persistent StaticDynamic	= "p"

// de-serialize Html State

DecodeHtmlStates :: String -> [HtmlState]
DecodeHtmlStates state = toHtmlState` (mkList state)
where
	toHtmlState` :: [Char] -> [HtmlState]
	toHtmlState` [] 		= []
	toHtmlState` listofchar	= [mkHtmlState (mkList (decodeChars first)) : toHtmlState` second]
	where
		(first,second) = mscan '$' listofchar	// search for end mark

		mkHtmlState :: [Char] -> HtmlState
		mkHtmlState	elem = 
			( mkString (stl fid)									// decode unique identification
			, toLivetime fid										// decode livetime from character
			, toStorageFormat fid									// decode storage format from character
			, mkString (stl (reverse (stl (reverse formvalue)))) 	// decode state
			)
		where
			(fid,formvalue) = mscan '"' (stl (stl elem)) 			// skip '("'
			stl [] = []
			stl [x:xs] = xs 
			
			toLivetime ['N':_] = Page
			toLivetime ['n':_] = Page
			toLivetime ['S':_] = Session
			toLivetime ['s':_] = Session
			toLivetime ['P':_] = Persistent
			toLivetime ['p':_] = Persistent
			toLivetime _   	   = Page

			toStorageFormat ['N':_] = PlainString
			toStorageFormat ['S':_] = PlainString
			toStorageFormat ['P':_] = PlainString
			toStorageFormat ['n':_] = StaticDynamic
			toStorageFormat ['p':_] = StaticDynamic
			toStorageFormat ['s':_] = StaticDynamic
			toStorageFormat _   	= PlainString

// reconstruct HtmlState out of the information obtained from browser

DecodeHtmlStatesAndUpdate :: ServerKind (Maybe String) -> ([HtmlState],String,String)
DecodeHtmlStatesAndUpdate serverkind args
# (_,triplet,update,state) = DecodeArguments serverkind args
= ([states \\states=:(id,_,_,nstate) <- DecodeHtmlStates state | id <> "" || nstate <> ""],triplet,update) // to be sure that no rubisch is passed on

// Parse and decode low level information obtained from server 
// In case of using a php script and external server:

DecodeArguments External _ = DecodePhpArguments
where
//	DecodePhpArguments :: (!String,!String,!String,!String) // executable, id + update , new , state
	DecodePhpArguments
	# input 			= [c \\ c <-: GetArgs | not (isControl c) ] // get rid of communication noise
	# (thisexe,input) 	= mscan '#' input 		// get rid of garbage
	# input				= skipping ['#UD=']  input
	# (update, input)	= mscan '=' input
	# (new,    input)	= mscan ';' input
	# input				= skipping ['GS=']  input
	# (state, input)	= mscan ';' input
	=: case toString update of
			"CS" -> (toString thisexe, decodeChars new, "", toString state)
			else -> (toString thisexe, decodeChars update, toString new, toString state)
/*	=: case (decodeChars thisexe, decodeChars update, decodeChars new, decodeChars state) of
			(thisexe,"CS",new,state) -> (thisexe,new,"",state)
		    else		    -> else*/

	GetArgs :: String 
	GetArgs =: foldl (+++) "" [strings \\ strings <-: getCommandLine]

// In case of using the internal server written in Clean:

DecodeArguments Internal (Just args) = DecodeCleanServerArguments args
where
	DecodeCleanServerArguments :: String -> (!String,!String,!String,!String) // executable, id + update , new , state
	DecodeCleanServerArguments args
	# input 			= [c \\ c <-: args | not (isControl c) ] // get rid of communication noise
	# (thisexe,input) 	= mscan '\"' input 				// get rid of garbage
	# input				= skipping ['UD\"']  input
	# (update, input)	= mscan '=' input // should give triplet
	# (new,    input)	= mscan '-' input // should give update value <<< *** Bug for negative integers??? ***
	# (_,input)			= mscan '=' input
	# input				= skipping ['\"GS\"']  input
	# (found,index) 	= FindSubstr ['---'] input
	# state				= if found (take index input) ['']
	= case toString update of
			"CS" -> ("clean", decodeChars new, "", toString state)
			else -> ("clean", decodeChars update, toString new, toString state)

	FindSubstr substr list = FindSubstr` list 0 
	where
		FindSubstr` list=:[] index = (False,0)
		FindSubstr` list=:[x:xs] index
		| substr == take lsubstr list = (True,index)
		| otherwise = FindSubstr` xs (index + 1)
	
		lsubstr = length substr

// traceHtmlInput utility used to see what kind of rubbish is received

traceHtmlInput :: ServerKind (Maybe String) -> BodyTag
traceHtmlInput serverkind args
=	BodyTag	[	STable [] [ [B [] "Triplet:", Txt triplet]
							,[B [] "Update:", Txt update]
							,[B [] "Identifier:", B [] "Lifetime:", B [] "Format:", B [] "Value:"]
							:[[Txt id, Txt (showl life), Txt (showf storage), Txt (shows storage state)] 
							 \\ (id,life,storage,state) <- htmlState]]
				, Br
				, Txt string
			]
where
	(Just string) = args

	(htmlState,triplet,update) = DecodeHtmlStatesAndUpdate serverkind args

	showl life = case life of Persistent -> "Persistent"; Session -> "Session"; _ -> "Page"
	showf storage = case storage of PlainString -> "String";  _ -> "Dynamic"
	shows PlainString s = s
	shows _ d = d//"cannot show dynamic value" 
	
// global names setting depending on kind of server used

ThisExe :: ServerKind -> String
ThisExe External 
# (thisexe,_,_,_) = DecodeArguments External Nothing 
= thisexe
ThisExe Internal 
= "clean"

MyPhP :: ServerKind -> String
MyPhP External = (mkString (takeWhile ((<>) '.') (mkList (ThisExe External)))) +++ ".php"
MyPhP Internal = "clean"

MyDir :: ServerKind -> String
MyDir serverkind = (mkString (takeWhile ((<>) '.') (mkList (ThisExe serverkind))))

// writing and reading of persistent states to a file

writeState :: !String !String !String !*NWorld -> *NWorld 
writeState directory filename serializedstate env
#(_,env) = case getFileInfo mydir env of
			((DoesntExist,fileinfo),env) -> createDirectory mydir env
			(_,env) -> (NoDirError,env)
# (ok,file,env)	= fopen (directory +++ "/" +++ filename) FWriteData env
| not ok 		= env
# file			= fwrites serializedstate file
# (ok,env)		= fclose file env
= env
where
	mydir = RelativePath [PathDown directory]

readStringState :: !String !String !*NWorld -> (!String,!*NWorld) 
readStringState directory filename env
#(_,env) = case getFileInfo mydir env of
			((DoesntExist,fileinfo),env) -> createDirectory mydir env
			(_,env) -> (NoDirError,env)
# (ok,file,env)	= fopen (directory +++ "/" +++ filename) FReadData env
| not ok 		= ("",env)
# (string,file)	= freads file big
| not ok 		= ("",env)
# (ok,env)		= fclose file env
# string		= mkString (removeBackslashQuote (mkList string))
= (string,env) 
where
	big = 100000
	mydir = RelativePath [PathDown directory]

	removeBackslashQuote [] 			= []
	removeBackslashQuote ['\\\"':xs] 	= ['\"':removeBackslashQuote xs]
	removeBackslashQuote [x:xs] 		= [x:removeBackslashQuote xs]

readDynamicState :: !String !String !*NWorld -> (!String,!*NWorld) 
readDynamicState directory filename env
#(_,env) = case getFileInfo mydir env of
			((DoesntExist,fileinfo),env) -> createDirectory mydir env
			(_,env) -> (NoDirError,env)
# (ok,file,env)	= fopen (directory +++ "/" +++ filename) FReadData env
| not ok 		= ("",env)
# (string,file)	= freads file big
| not ok 		= ("",env)
# (ok,env)		= fclose file env
= (string,env) 
where
	big = 100000
	mydir = RelativePath [PathDown directory]

// serializing and de-serializing of html states


// low level url encoding decoding of Strings

encodeString :: String -> String
//encodeString s = urlEncode s
encodeString s = string_to_string52 s	// using the whole alphabet 

decodeString :: String -> *String
//decodeString s = urlDecode s
decodeString s = string52_to_string s	// using the whole alphabet

// utility functions based on low level encoding - decoding

encodeInfo :: a -> String | gPrint{|*|} a
encodeInfo inp = encodeString (printToString inp)

decodeChars :: [Char] -> *String
decodeChars cs = decodeString (mkString cs)

// compact John van Groningen encoding-decoding to lower and uppercase alpabeth

string_to_string52 :: !String -> *String
string_to_string52 s
# n		=	size s
# n3d2	=	3*(n>>1)
| n bitand 1==0
= fill_string52 0 0 n s (createArray n3d2 '\0')
# a = fill_string52 0 0 (n-1) s (createArray (n3d2+2) '\0')
  i=toInt s.[n-1]
  i1=i/52
  r0=i-i1*52
= {a & [n3d2]=int52_to_alpha_char i1,[n3d2+1]=int52_to_alpha_char r0} 
where
	fill_string52 :: !Int !Int !Int !String !*String -> *String
	fill_string52 si ai l s a
	| si<l
	# i=toInt s.[si]<<8+toInt s.[si+1]
	  i1=i/52
	  i2=i1/52
	  r0=i-i1*52
	  r1=i1-i2*52
	  a={a & [ai]=int52_to_alpha_char i2,[ai+1]=int52_to_alpha_char r1,[ai+2]=int52_to_alpha_char r0}
	= fill_string52 (si+2) (ai+3) l s a
	= a

int52_to_alpha_char i :== toChar (i-(((i-26)>>8) bitand 6)+71)

string52_to_string :: !String -> *String
string52_to_string s
# n		=	size s
# nd3	=	n/3
# r3	=	n-nd3*3
# n2d3	=	nd3<<1
| r3==0	= fill_string 0 0 n s (createArray n2d3 '\0')
| r3==2
# a = fill_string 0 0 (n-2) s (createArray (n2d3+1) '\0')
= {a & [n2d3]=toChar (alpha_to_int52 s.[n-2]*52+alpha_to_int52 s.[n-1])}
where
	fill_string :: !Int !Int !Int !String !*String -> *String
	fill_string si ai l s a
	| si<l
	# i=(alpha_to_int52 s.[si]*52+alpha_to_int52 s.[si+1])*52+alpha_to_int52 s.[si+2]
	# a={a & [ai]=toChar (i>>8),[ai+1]=toChar i}
	= fill_string (si+3) (ai+2) l s a
	= a

alpha_to_int52 c
:== let i=toInt c in i+(((i-97)>>8) bitand 6)-71

// small parsing utility functions

mscan :: Char [Char] -> ([Char],[Char])
mscan c list = case (span ((<>) c) list) of  // scan like span but it removes character
				(x,[])	= (x,[])
				(x,y)	= (x,tl y)

skipping [c:cs] list=:[x:xs]
| c == x 		= skipping cs xs
| otherwise 	= list
skipping any    list = list

// The following code is not used anymore...

// encoding - decoding to hexadecimal code

urlEncode :: String -> String
urlEncode s = mkString (urlEncode` (mkList s))
where
	urlEncode` :: [Char] -> [Char]
	urlEncode` [] = []
	urlEncode` [x:xs] 
	| isAlphanum x = [x  : urlEncode` xs]
	| otherwise    = urlEncodeChar x ++ urlEncode` xs
	where
		urlEncodeChar x 
		# (c1,c2) = charToHex x
		= ['%', c1 ,c2]
	
		charToHex :: !Char -> (!Char, !Char)
		charToHex c = (toChar (digitToHex (i >> 4)), toChar (digitToHex (i bitand 15)))
		where
		        i = toInt c
		        digitToHex :: !Int -> Int
		        digitToHex d
		                | d <= 9 = d + toInt '0'
		                = d + (toInt 'A' - 10)

urlDecode :: String -> *String
urlDecode s = mkString (urlDecode` (mkList s))
where
	urlDecode` :: [Char] -> [Char]
	urlDecode` [] 				= []
	urlDecode` ['%',hex1,hex2:xs]= [hexToChar(hex1, hex2):urlDecode` xs]
	where
		hexToChar :: !(!Char, !Char) -> Char
		hexToChar (a, b) = toChar (hexToDigit (toInt a) << 4 + hexToDigit (toInt b))
		where
		        hexToDigit :: !Int -> Int
		        hexToDigit i
		                | i <= toInt '9' = i - toInt '0'
		                = i - (toInt 'A' - 10)
	urlDecode` [x:xs] 	= [x:urlDecode` xs]

