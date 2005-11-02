 implementation module htmlEncodeDecode

// encoding and decoding of information
// (c) 2005 MJP

import StdEnv, ArgEnv, StdMaybe, Directory
import htmlDataDef, htmlTrivial, htmlFormData
import GenPrint, GenParse
import dynamic_string

derive gParse UpdValue
derive gParse (,), (,,)

// This module controls the handling of state forms and the communication with the browser
// Internally, a binairy tree is being used to store the form states
// Externaly,  states are stored either in the Html form in a list, or a state of form id is stored in a file named id
// The are currently two storage formats: 
// 1. a string which can be generically generated for any first order type.
// A generic parser is needed to convert the string back to a value of the required type.
// 2. a static (= no dynamic linker needed) dynamic which works for any type, including higher order types.
// But watch it: only the application which wrotes the dynamic, can read it back: no plug ins! recompiling means: all information lost!
// A distinction is made between old states (states retrieved from the html form)
// and new states (states of newly created forms and updated forms)

:: *FormStates 										// collection of states of all forms
	= 	{ fstates 	:: *FStates						// internal tree of states
		, triplet	:: String						// indicates what has changed: which form, which postion, which value
		, update	:: String						// what is the new value created by the end user
		, updateid	:: String						// which form has changed
		, server	:: ServerKind					// is an external server required
		}		

:: FStates		:== Tree_ (String,FormState)		// each form needs a different string id

:: Tree_ a 		= Node_ (Tree_ a) a (Tree_ a) | Leaf_
:: FormState 	= OldState !FState					// Old states are the states from the previous calculation
				| NewState !FState					// New states are newly created states or old states that have been inspected and updated
:: FState		= { encoding :: !EncodingMethod		// Encoding method used for serialization
				  , life	 :: !Lifespan			// Its life span
				  }
:: EncodingMethod
				= PlainStr 	!String					// Either a string is used for serialization
				| StatDyn	!Dynamic				// Or a dynamic which enables serialization of functions defined in the application (no plug ins yet)


// used for reading and writing of states in html format

:: HtmlState  :== (!String,!Lifespan,!StorageFormat,!String)	// Format just before writing to the page format

// functions defined on the FormStates abstract data type

instance < FormState
where
	(<) _ _ = True

emptyFormStates :: *FormStates
emptyFormStates = { fstates = Leaf_ , triplet = "", update = "", updateid = "", server = Internal}

getTriplet :: *FormStates -> (!Maybe a, !Maybe b,*FormStates) | gParse{|*|} a & gParse{|*|} b 
getTriplet formstates=:{triplet,update}
= (parseString triplet, parseString update, formstates)

getUpdateId ::  *FormStates -> (String,*FormStates)
getUpdateId formStates=:{updateid} = (updateid,formStates)

getUpdate ::  *FormStates -> (String,*FormStates)
getUpdate formStates=:{update} = (update,formStates)

findState :: !FormId *FormStates *NWorld -> (Bool,Maybe a,*FormStates,*NWorld)	| gParse{|*|} a & TC a
findState formid formstates=:{fstates,server} world
# (bool,ma,fstates,world) = findState` formid fstates world
= (bool,ma,{formstates & fstates = fstates},world)
where
	findState` :: !FormId *FStates *NWorld -> (Bool,Maybe a,*FStates,*NWorld)| gParse{|*|} a & TC a
	findState` formid formstate=:(Node_ left (fid,info) right) world
	| formid.id == fid = case info of
					(OldState state) = (True, fetchFState state,formstate,world)
					(NewState state) = (False,fetchFState state,formstate,world)
	with
		fetchFState :: FState -> Maybe a | TC a & gParse{|*|} a
		fetchFState {encoding = PlainStr string} = parseString string
		fetchFState {encoding = StatDyn (v::a^)} = Just v    
		fetchFState _ = Nothing
	| formid.id  < fid 	= (bool,parsed, Node_ leftformstates (fid,info) right,nworld)
					with
						(bool,parsed,leftformstates,nworld) = findState` formid left world
	| otherwise	= (bool,parsed, Node_  left (fid,info) rightformstates,nworld)
					with
						(bool,parsed,rightformstates,nworld) = findState` formid right world

	findState` {id,lifespan = Persistent,storage = PlainString} Leaf_ world 
	# (string,world) = readStringState (MyDir server) id world
	= case parseString string of
		Just a 	-> (True,Just a,Node_ Leaf_ (id,NewState {encoding = PlainStr string, life = Persistent}) Leaf_,world)
		Nothing	-> (False,Nothing,Leaf_,world)

	findState` {id,lifespan = Persistent,storage = StaticDynamic} Leaf_ world 
	# (string,world) = readDynamicState (MyDir server) id world
	= case string of 
		"" = (False,Nothing,Leaf_,world)
		_  = case string_to_dynamic` string of
				dyn=:(dynval::a^) 	-> (True,Just dynval,Node_ Leaf_ (id,NewState {encoding = StatDyn dyn, life = Persistent}) Leaf_,world)
				else				-> (False,Nothing,Leaf_,world)
	findState` _ Leaf_ world = (False,Nothing,Leaf_,world)
	findState` _ _ world = (False,Nothing,Leaf_,world)

replaceState :: !FormId a *FormStates *NWorld -> (*FormStates,*NWorld)	| gPrint{|*|} a & TC a
replaceState formid val formstates=:{fstates} world
# (fstates,world) = replaceState` formid val fstates world
= ({formstates & fstates = fstates},world)
where
	replaceState` :: !FormId a *FStates *NWorld -> (*FStates,*NWorld)	| gPrint{|*|} a & TC a
	replaceState` formid val Leaf_ world = (Node_ Leaf_ (formid.id,NewState (initNewState formid.lifespan formid.storage val)) Leaf_,world)
	replaceState` formid val (Node_ left a=:(fid,_) right) world
	| formid.id == fid 	= (Node_ left (fid,NewState (initNewState formid.lifespan formid.storage val)) right,world)
	| formid.id < fid 	= (Node_ nleft a right,nworld)
							with
								(nleft,nworld) = replaceState` formid val left world
	| otherwise			 = (Node_ left a nright,nworld)
							with
								(nright,nworld) = replaceState` formid val right world

	// NewState Handling routines 

	initNewState :: !Lifespan !StorageFormat !a  -> FState | TC a &  gPrint{|*|} a 
	initNewState lifespan PlainString   nv = {encoding = PlainStr (printToString nv), life = lifespan}
	initNewState lifespan StaticDynamic nv = {encoding = StatDyn  (dynamic nv), life = lifespan}// convert the hidden state information stored in the html page

// Serialization and De-Serialization of states
//
// De-serialize information from server to the internally used form states

retrieveFormStates 	:: ServerKind (Maybe String) *NWorld -> (*FormStates,*NWorld) 					// retrieves all form states hidden in the html page
retrieveFormStates serverkind args world 
	= ({ fstates = retrieveFStates, triplet = triplet, update = update, 
		updateid = calc_updateid , server = serverkind},world)
where
	retrieveFStates 
		= Balance (sort [(sid,OldState {encoding = toExistval storageformat state, life = lifespan}) 
						\\ (sid,lifespan,storageformat,state) <- retrieveHtmlState serverkind args
						|  sid <> ""
						])
	where
		toExistval PlainString string 		= PlainStr string	// string that has to be parsed in the context where the type is known
		toExistval StaticDynamic string 	= StatDyn (hex_string_to_dynamic string) // here it crashes
//		toExistval StaticDynamic string 	= StatDyn (dynamic 1) // no crash

	(_,triplet,update,_) = DecodeArguments serverkind args

	Balance [] = Leaf_
	Balance [x] = Node_ Leaf_ x Leaf_
	Balance xs
		= case splitAt (length xs/2) xs of
			(a,[b:bs]) = Node_ (Balance a) b (Balance bs)
			(as,[]) = Node_   (Balance (init as)) (last as) Leaf_
	
	calc_updateid 		
	= case parseString triplet of
		Just ("",0,UpdI 0) = ""
		Just (id,_,_)   = id 
		else = ""

	// reconstruct HtmlState out of the information obtained from browser

	retrieveHtmlState :: ServerKind (Maybe String) -> [HtmlState]
	retrieveHtmlState serverkind args
	# (_,_,_,state) = DecodeArguments serverkind args
	= [states \\states=:(id,_,_,nstate) <- StringToHtmlState state | id <> "" || nstate <> ""] // to be sure that no rubisch is passed on

// Parse and decode low level information obtained from server 
// In case of using a php script and external server:

DecodeArguments External _ = DecodePhpArguments
where
//	DecodePhpArguments :: (!String,!String,!String,!String) // executable, id + update , new , state
	DecodePhpArguments
	# input 			= mkList GetArgs
	# (thisexe,input) 	= mscan '#' input 		// get rid of garbage
	# input				= skipping ['#UD=']  input
	# (update, input)	= mscan '=' input
	# (new,    input)	= mscan ';' input
	# input				= skipping ['GS=']  input
	# (state, input)	= mscan ';' input
	=: case (decodeChars thisexe, decodeChars update, decodeChars new, decodeChars state) of
			(thisexe,"CS",new,state) -> (thisexe,new,"",state)
		    else		    -> else

	GetArgs :: String 
	GetArgs =: foldl (+++) "" [strings \\ strings <-: getCommandLine]

// In case of using the internal server written in Clean:

DecodeArguments Internal (Just args) = DecodeCleanServerArguments args
where
	DecodeCleanServerArguments :: String -> (!String,!String,!String,!String) // executable, id + update , new , state
	DecodeCleanServerArguments args
	# input 			= mkList args
	# (thisexe,input) 	= mscan '\"' input 				// get rid of garbage
	# input				= skipping ['UD\"']  input
	# (update, input)	= mscan '=' input // should give triplet
	# (new,    input)	= mscan '-' input // should give update value <<< *** Bug for negative integers??? ***
	# (_,input)			= mscan '=' input
	# input				= skipping ['\"GS\"']  input
	# (found,index) 	= FindSubstr ['---'] input
	# state				= if found (take index input) ['']
	= case decodeChars update of
			"CS" -> ("clean", decodeChars new, "", decodeChars state)
			else -> ("clean", decodeChars update, decodeChars new, decodeChars state)

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
=	BodyTag	[	Txt "this executable    : " , B [] (ThisExe serverkind), Txt ";", Br 
			, 	Txt "my php script      : " , B [] (MyPhP serverkind), Txt ";", Br 
			, 	Txt "update				: " , B [] update, Txt ";", Br 
			, 	Txt "new value		  	: " , B [] new, Txt ";", Br 
			, 	Txt "state			  	: " , BodyTag (showstate (mkList state)), Txt ";", Br 
//			, 	Txt "input			  	: " , case args of 
//													(Just x) -> Txt x
//													_ -> Br 
			]
where
	(executable,update,new,state) = DecodeArguments serverkind args

	showstate :: [Char] -> [BodyTag]
	showstate [] 			= []
	showstate listofchar	= [Br, B [] (mkString first)] ++ showstate second // temp fix
	where
		(first,second) = mscan '$' listofchar

// Serialize all states in FormStates that have to be remembered to either hidden encoded Html Code
// or store them in a persistent file, all depending on the kind of states

storeFormStates :: !FormStates *NWorld -> (BodyTag,*NWorld)
storeFormStates {fstates = allFormStates,server} world
#	world = writeAllPersistentStates allFormStates world // first write all persistens states
=	(BodyTag
	[ submitscript    globalFormName updateInpName
	, globalstateform globalFormName updateInpName globalInpName (SV encodedglobalstate) 
	],world)
where
	encodedglobalstate = HtmlStateToString (FStateToHtmlState allFormStates)

//	FStateToHtmlState :: !FormStates -> HtmlState	// remaining states as hidden encoded html
	FStateToHtmlState formstates = toHtmlState` formstates []
	where
		toHtmlState` Leaf_ accu = accu

		// old states which have not been used this time, but with lifespan session, are stored again in the page

		toHtmlState` (Node_ left (fid,OldState {life=Session,encoding=PlainStr stringval}) right) accu 
			= toHtmlState` left [(fid,Session,PlainString,stringval):toHtmlState` right accu]
		toHtmlState` (Node_ left (fid,OldState {life=Session,encoding=StatDyn dynval}) right) accu 
			= toHtmlState` left [(fid,Session,StaticDynamic,dynamic_to_hex_string dynval):toHtmlState` right accu]

		// other old states will have lifespan page or persistent; they need not to be stored in the page

		toHtmlState` (Node_ left (fid,OldState s) right) accu 
			= toHtmlState` left (toHtmlState` right accu)

		// persistent stores (either old or new) have already been stored in files and can be skipped here

		toHtmlState` (Node_ left (fid,NewState {life=Persistent}) right) accu 
			= toHtmlState` left (toHtmlState` right accu)

		// the state of all other new forms created will be stored in the page 

		toHtmlState` (Node_ left (fid,NewState {encoding = PlainStr string,life}) right) accu 
			= toHtmlState` left [(fid,life,PlainString,string): toHtmlState` right accu]
		toHtmlState` (Node_ left (fid,NewState {encoding = StatDyn dynval,life}) right) accu 
			= toHtmlState` left [(fid,life,StaticDynamic,dynamic_to_hex_string dynval): toHtmlState` right accu]
 
	submitscript :: !String !String -> BodyTag
	submitscript formname updatename
	=	Script [] (SScript
		(	" function toclean(inp)" +++
			" { document." +++
				formname  +++ "." +++
				updatename +++ ".value=inp.name+\"=\"+inp.value;" +++
				"document." +++ formname +++ ".submit(); }"
		))

	// form that contains global state and empty input form for storing updated input
		
	globalstateform :: !String !String !String !Value -> BodyTag
	globalstateform formname updatename globalname globalstate
	=	Form 	[ Frm_Name formname
				, Frm_Action (MyPhP server)
				, Frm_Method Post
				, Frm_Enctype "multipart/form-data"			// what to do to enable large data ??
				]
				[ Input [ Inp_Name updatename
						, Inp_Type Inp_Hidden
						] ""
				, Input [ Inp_Name globalname
						, Inp_Type Inp_Hidden
						, Inp_Value globalstate
						] ""
				]		 

	globalFormName :: String
	globalFormName =: "CleanForm"
	
	updateInpName :: String
	updateInpName =: "UD"
	
	globalInpName :: String
	globalInpName =: "GS"
	
	selectorInpName :: String
	selectorInpName =: "CS"

//	writeAllPersistentStates:: FormStates *NWorld -> *NWorld 
	writeAllPersistentStates Leaf_ world = world
	writeAllPersistentStates (Node_ left (sid,NewState {encoding,life = Persistent}) right) world
	= writeAllPersistentStates right (writeAllPersistentStates left (serializeAndStoreState sid encoding world))
	writeAllPersistentStates (Node_ left (sid,OldState {encoding,life = Persistent}) right) world
	= writeAllPersistentStates right (writeAllPersistentStates left (serializeAndStoreState sid encoding world))
	writeAllPersistentStates (Node_ left _ right) world
	= writeAllPersistentStates right (writeAllPersistentStates left world)

	serializeAndStoreState sid (PlainStr string) world 
		= writeState (MyDir server) sid string world
	serializeAndStoreState sid (StatDyn dynval) world 
		= writeState (MyDir server) sid (dynamic_to_string dynval) world

// script for transmitting name and value of changed input 

callClean :: Script
callClean =: SScript "toclean(this)"

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

HtmlStateToString :: ![HtmlState] -> String
HtmlStateToString [] = encodeString "$"
HtmlStateToString [(id,lifespan,storageformat,state):xsys] 
	= encodeString "(\"" +++ fromLivetime lifespan storageformat +++ encodeString id +++ 
	  encodeString "\"," +++ encodeString  state +++ 
	  encodeString ")$" +++ HtmlStateToString xsys
where
	fromLivetime Page 		PlainString		= "N"	// encode Lifespan & StorageFormat in first character
	fromLivetime Session 	PlainString		= "S"
	fromLivetime Persistent PlainString		= "P"
	fromLivetime Page 		StaticDynamic	= "n"
	fromLivetime Session 	StaticDynamic	= "s"
	fromLivetime Persistent StaticDynamic	= "p"

StringToHtmlState :: String -> [HtmlState]
StringToHtmlState state = toHtmlState` (mkList state)
where
	toHtmlState` :: [Char] -> [HtmlState]
	toHtmlState` [] 			= []
	toHtmlState` listofchar	= [mkHtmlState first : toHtmlState` second]
	where
		(first,second) = mscan '$' listofchar

		mkHtmlState :: [Char] -> HtmlState
		mkHtmlState	elem = (mkString (stl fid),toLivetime fid,toStorageFormat fid,mkString (stl (reverse (stl (reverse formvalue)))))
		where
			(fid,formvalue) = mscan '"' (stl (stl elem)) // skip '("'
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

// low level url encoding decoding

encodeString :: String -> String
encodeString s = mkString (urlEncode (mkList s))

encodeInfo :: a -> String | gPrint{|*|} a
encodeInfo inp = encoding  
where
	encoding = encodeString (printToString inp)

urlEncode :: [Char] -> [Char]
urlEncode [] = []
urlEncode [x:xs] 
| isAlphanum x = [x  : urlEncode xs]
| otherwise    = urlEncodeChar x ++ urlEncode xs
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

decodeChars :: [Char] -> *String
decodeChars n = mkString (urlDecode n)

decodeString :: String -> *String
decodeString s = decodeChars (mkList s)

decodeInfo :: String -> Maybe a | gParse{|*|} a
decodeInfo s = parseString (decodeString s)

urlDecode :: [Char] -> [Char]
urlDecode [] 				= []
urlDecode ['%',hex1,hex2:xs]= [hexToChar(hex1, hex2):urlDecode xs]
where
	hexToChar :: !(!Char, !Char) -> Char
	hexToChar (a, b) = toChar (hexToDigit (toInt a) << 4 + hexToDigit (toInt b))
	where
	        hexToDigit :: !Int -> Int
	        hexToDigit i
	                | i <= toInt '9' = i - toInt '0'
	                = i - (toInt 'A' - 10)
urlDecode [x:xs] 			= [x:urlDecode xs]

dynamic_to_hex_string:: Dynamic -> *String
dynamic_to_hex_string s
        = to_hex (dynamic_to_string s);

to_hex :: !String -> *String
to_hex s = {to_hex_digit i s \\ i<-[0..(size s<<1)-1]}
where
	to_hex_digit i s
    # c=s.[i>>1]
    # i=((toInt c)>>((1-(i bitand 1))<<2)) bitand 15
    | i<10 = toChar (48+i)
    = toChar (55+i)

hex_string_to_dynamic :: {#Char} -> Dynamic
hex_string_to_dynamic s
        = string_to_dynamic (from_hex s)

from_hex :: !String -> *String
from_hex s
| (size s) bitand 1==0
= { toChar (hex_digit_to_int s.[i+i]<<4+hex_digit_to_int s.[i+i+1])\\ i<-[0..size s>>1-1]}
where        
	hex_digit_to_int d
	# i=toInt d
	| i>=65 = i-55
	= i-48

// small general utility functions

string_to_dynamic` :: {#Char} -> Dynamic	// just to make a unique copy as requested by string_to_dynamic
string_to_dynamic` s = string_to_dynamic {s` \\ s` <-: s}

mscan c list = case (span ((<>) c) list) of  // scan like span but it removes character
				(x,[])	= (x,[])
				(x,y)	= (x,tl y)

skipping [c:cs] list=:[x:xs]
| c == x 		= skipping cs xs
| otherwise 	= list
skipping any    list = list

// interfaces added for testing:

initTestFormStates 	::  *NWorld -> (*FormStates,*NWorld) 					// retrieves all form states hidden in the html page
initTestFormStates world 
	= ({ fstates = Leaf_, triplet = "", update = "", updateid = "" , server = JustTesting},world)

setTestFormStates 	::  String String String *FormStates *NWorld -> (*FormStates,*NWorld) 					// retrieves all form states hidden in the html page
setTestFormStates triplet updateid update states world 
	= ({ fstates = makeoldstates , triplet = triplet, update = update, updateid = updateid, server = JustTesting},world)
	where
		makeoldstates = toOldStates states.fstates 
		where
			toOldStates Leaf_ 	= Leaf_
			toOldStates (Node_ left (s,NewState fstate) right)  
								= Node_ (toOldStates left) (s,OldState fstate) (toOldStates right)
			toOldStates else	= else
