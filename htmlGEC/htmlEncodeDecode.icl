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
:: FormState 	= OldState !OldState				// Old states will become garbage when the final states are reached
				| NewState !NewState				// New states that will be saved
:: OldState		= { ostrval :: !ExistVal			// Internal encoding
				  , olive	:: !Lifespan			// Its life span
				  }
:: NewState 	= { nstrval :: !ExistVal			// A new state is stored in a dynamic
				  , live	:: !Lifespan			// Its life span
				  }
:: HtmlStates :== [HtmlState]						// For convenience, the state is stored in html as a list and not as a tree
:: HtmlState  :== (!String,!Lifespan,!StorageFormat,!String)		// Format just before writing to the page format
:: ExistVal		= PlainStr 	!String					// Internal encoding: either a string
				| StatDyn	!Dynamic				// or a dynamic

// reconstruct HtmlState out of the information obtained from browser

retrieveHtmlState :: ServerKind (Maybe String) -> HtmlStates
retrieveHtmlState serverkind args
# (_,_,_,state) = DecodeArguments serverkind args
= splitString (mkList state)
where
	splitString [] 			= []
	splitString listofchar	= [mkHtmlState first : splitString second]
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
			toLivetime _   		= Page

			toStorageFormat ['N':_] = PlainString
			toStorageFormat ['n':_] = StaticDynamic
			toStorageFormat ['S':_] = PlainString
			toStorageFormat ['s':_] = StaticDynamic
			toStorageFormat ['P':_] = PlainString
			toStorageFormat ['p':_] = StaticDynamic
			toStorageFormat _   	= PlainString

// convert this HtmlState into FormStates which are used internally

initTestFormStates 	::  *NWorld -> (*FormStates,*NWorld) 					// retrieves all form states hidden in the html page
initTestFormStates world 
	= ({ fstates = Leaf_, triplet = "", update = "", updateid = "" , server = JustTesting},world)

initFormStates 	:: ServerKind (Maybe String) *NWorld -> (*FormStates,*NWorld) 					// retrieves all form states hidden in the html page
initFormStates serverkind args world 
	= ({ fstates = readStatesFromArgs, triplet = triplet, update = update, updateid = updateid , server = serverkind},world)
where
	readStatesFromArgs = Balance (sort [(sid,OldState {ostrval = toExistval storageformat state, olive = lifespan}) 
										\\ (sid,lifespan,storageformat,state) <- retrieveHtmlState serverkind args| sid <> ""
										])
	(_,triplet,update,_) = DecodeArguments serverkind args

	toExistval PlainString state 	= PlainStr state	// string that has to be parsed in the context where the type is known
	toExistval StaticDynamic state 	= StatDyn (string_to_dynamic` state) // turn string into dynamic

	Balance [] = Leaf_
	Balance [x] = Node_ Leaf_ x Leaf_
	Balance xs
		= case splitAt (length xs/2) xs of
			(a,[b:bs]) = Node_ (Balance a) b (Balance bs)
			(as,[]) = Node_   (Balance (init as)) (last as) Leaf_
	
	updateid 		
	= case parseString triplet of
		Just ("",0,UpdI 0) = ""
		Just (id,_,_)   = id 
		else = ""

DecodeArguments External _ = DecodePhpArguments
DecodeArguments Internal (Just args) = DecodeCleanServerArguments args

DecodePhpArguments :: (!String,!String,!String,!String) // executable, id + update , new , state
DecodePhpArguments
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
= case decode update of
		"CS" -> ("clean", decode new, "", decode state)
		else -> ("clean", decode update, decode new, decode state)

Tl [] = []
Tl list = tl list

skipping [c:cs] list=:[x:xs]
| c == x 		= skipping cs xs
| otherwise 	= list
skipping any    list = list

decode n = mkString (urlDecode n)

traceHtmlInput :: ServerKind (Maybe String) -> BodyTag
traceHtmlInput serverkind args
=	BodyTag	[	Txt "this executable    : " , B [] (ThisExe serverkind), Txt ";", Br 
			, 	Txt "my php script      : " , B [] (MyPhP serverkind), Txt ";", Br 
			, 	Txt "update				: " , B [] update, Txt ";", Br 
			, 	Txt "new value		  	: " , B [] new, Txt ";", Br 
			, 	Txt "state			  	: " , BodyTag (showstate (mkList state)), Txt ";", Br 
			, 	Txt "input			  	: " , case args of 
													(Just x) -> Txt x
													_ -> Br 
			]
where
	(executable,update,new,state) = DecodeArguments serverkind args

	showstate :: [Char] -> [BodyTag]
	showstate [] 			= []
	showstate listofchar	= [Br, B [] (mkString first)] ++ showstate second
	where
		(first,second) = mscan '$' listofchar

// FormStates abstract data handling routines

emptyFormStates :: *FormStates
emptyFormStates = { fstates = Leaf_ , triplet = "", update = "", updateid = "", server = Internal}

instance < FormState
where
	(<) _ _ = True

force :: !a *b -> *b
force a b = b

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
					(OldState state) = case state.ostrval of
										PlainStr stringstate = (True, parseString stringstate,formstate,world)
										StatDyn (dynval::a^) = (True, Just dynval,formstate,world)
										StatDyn (dynval::b)  = (True, Nothing,formstate,world)
					(NewState state) = (False,retrieveNewState state,formstate,world)
	| formid.id  < fid 	= (bool,parsed, Node_ leftformstates (fid,info) right,nworld)
					with
						(bool,parsed,leftformstates,nworld) = findState` formid left world
	| otherwise	= (bool,parsed, Node_  left (fid,info) rightformstates,nworld)
					with
						(bool,parsed,rightformstates,nworld) = findState` formid right world
	findState` {id,lifespan = Persistent,storage = PlainString} Leaf_ world 
	# (string,world) = readState id world
	= case parseString string of
		Just a 	-> (True,Just a,Node_ Leaf_ (id,NewState {nstrval = PlainStr string, live = Persistent}) Leaf_,world)
		Nothing	-> (False,Nothing,Leaf_,world)
	findState` {id,lifespan = Persistent,storage = StaticDynamic} Leaf_ world 
	# (string,world) = readState id world
	= case string_to_dynamic` string of
		dyn=:(dynval::a^) 	-> (True,Just dynval,Node_ Leaf_ (id,NewState {nstrval = StatDyn dyn, live = Persistent}) Leaf_,world)
		else				-> (False,Nothing,Leaf_,world)
	findState` _ Leaf_ world = (False,Nothing,Leaf_,world)
	findState` _ _ world = (False,Nothing,Leaf_,world)

	readState :: String *NWorld -> (String,*NWorld) 
	readState filename env
	#(_,env) = case getFileInfo mydir env of
				((DoesntExist,fileinfo),env) -> createDirectory mydir env
				(_,env) -> (NoDirError,env)
	# (ok,file,env)	= fopen (MyDir server +++ "/" +++ filename) FReadData env
	| not ok 		= ("",env)
	# (string,file)	= freads file big
	| not ok 		= ("",env)
	# (ok,env)		= fclose file env
	# string 		= string%(1,size string - 2)
	# string		= mkString (removeBackslashQuote (mkList string))
	= (string,env) 
	where
		big = 100000
		mydir = RelativePath [PathDown (MyDir server)]

		removeBackslashQuote [] 			= []
		removeBackslashQuote ['\\\"':xs] 	= ['\"':removeBackslashQuote xs]
		removeBackslashQuote [x:xs] 		= [x:removeBackslashQuote xs]

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

initNewState :: !Lifespan !StorageFormat !a  -> NewState | TC a &  gPrint{|*|} a 
initNewState lifespan PlainString   nv = {nstrval = PlainStr (printToString nv), live = lifespan}
initNewState lifespan StaticDynamic nv = {nstrval = StatDyn  (dynamic nv), live = lifespan}

retrieveNewState :: NewState -> Maybe a | TC a & gParse{|*|} a
retrieveNewState {nstrval = PlainStr string} = parseString string
retrieveNewState {nstrval = StatDyn (v::a^)} = Just v    
retrieveNewState _ = Nothing

// Convert newly created FormStates to Html Code

convStates :: !FormStates *NWorld -> (BodyTag,*NWorld)
convStates {fstates = allFormStates,server} world
#	world = writeAllPersistentStates allFormStates world 
=	(BodyTag
	[ submitscript    globalFormName updateInpName
	, globalstateform globalFormName updateInpName globalInpName (SV encodedglobalstate) 
	],world)
where
	encodedglobalstate = urlEncodeState (toHtmlState allFormStates)

//	toHtmlState :: !FormStates -> HtmlState
	toHtmlState formstates = toHtmlState` formstates []
	where
		toHtmlState` Leaf_ tl = tl

		// old states have not been used this time, with lifespan session are stored again in the page

		toHtmlState` (Node_ left (fid,OldState {olive,ostrval=PlainStr stringval}) right) tl 
			= toHtmlState` left [(fid,olive,PlainString,stringval):toHtmlState` right tl]
		toHtmlState` (Node_ left (fid,OldState {olive,ostrval=StatDyn dynval}) right) tl 
			= toHtmlState` left [(fid,olive,StaticDynamic,dynamic_to_string dynval):toHtmlState` right tl]

		// other old states will ahve lifespan page; they become garbage and are no longer stored in the page

		toHtmlState` (Node_ left (fid,OldState s) right) tl 
			= toHtmlState` left (toHtmlState` right tl)

		// persistent stores have already been stored in files

		toHtmlState` (Node_ left (fid,NewState {live=Persistent}) right) tl 
			= toHtmlState` left (toHtmlState` right tl)

		// the state of all other new forms created will be stored in the page 

		toHtmlState` (Node_ left (fid,NewState {nstrval = PlainStr string,live}) right) tl 
			= toHtmlState` left [(fid,live,PlainString,string): toHtmlState` right tl]
		toHtmlState` (Node_ left (fid,NewState {nstrval = StatDyn dynval,live}) right) tl 
			= toHtmlState` left [(fid,live,StaticDynamic,dynamic_to_string dynval): toHtmlState` right tl]

	urlEncodeState :: !HtmlStates -> String
	urlEncodeState [] = urlEncodeS "$"
	urlEncodeState [(id,lifespan,storageformat,state):xsys] 
		= urlEncodeS "(\"" +++ fromLivetime lifespan storageformat +++ urlEncodeS id +++ 
		  urlEncodeS "\"," +++ urlEncodeS state +++ 
		  urlEncodeS ")$" +++ urlEncodeState xsys
	where
		fromLivetime Page 		PlainString		= "N"	// encode Lifespan & StorageFormat in first character
		fromLivetime Session 	PlainString		= "S"
		fromLivetime Persistent PlainString		= "P"
		fromLivetime Page 		StaticDynamic	= "n"
		fromLivetime Session 	StaticDynamic	= "s"
		fromLivetime Persistent StaticDynamic	= "p"
	 
	urlEncodeS :: !String -> String
	urlEncodeS s = (mkString o urlEncode o mkList) s
	
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

	// all persistent stores are stored in files

//	writeAllPersistentStates:: FormStates *NWorld -> *NWorld 
	writeAllPersistentStates Leaf_ world = world
	writeAllPersistentStates (Node_ left (sid,NewState {nstrval,live = Persistent}) right) world
	# stringToWrite = case nstrval of
							PlainStr string -> string
							StatDyn dynval	-> dynamic_to_string dynval
	# world = writeState sid stringToWrite world 
	# world = writeAllPersistentStates left world
	= writeAllPersistentStates right world
	writeAllPersistentStates (Node_ left (sid,NewState _) right) world
	# world = writeAllPersistentStates left world
	= writeAllPersistentStates right world
	writeAllPersistentStates (Node_ left (sid,OldState s) right) world
	# world = writeAllPersistentStates left world
	= writeAllPersistentStates right world

	writeState :: String a *NWorld -> *NWorld | gPrint {|*|} a 
	writeState filename val env
	#(_,env) = case getFileInfo mydir env of
				((DoesntExist,fileinfo),env) -> createDirectory mydir env
				(_,env) -> (NoDirError,env)
	# (ok,file,env)	= fopen (MyDir server +++ "/" +++ filename) FWriteData env
	| not ok 		= env
	# file			= fwrites (printToString val) file
	# (ok,env)		= fclose file env
	= env
	where
		mydir = RelativePath [PathDown (MyDir server)]

// script for transmitting name and value of changed input 

callClean :: Script
callClean =: SScript "toclean(this)"

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


// low level url encoding decoding

mscan c list = case (span ((<>) c) list) of  // scan like span but it removes character
				(x,[])	= (x,[])
				(x,y)	= (x,tl y)

FindSubstr substr list = FindSubstr` list 0 
where
	FindSubstr` list=:[] index = (False,0)
	FindSubstr` list=:[x:xs] index
	| substr == take lsubstr list = (True,index)
	| otherwise = FindSubstr` xs (index + 1)

	lsubstr = length substr

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

urlDecodeS :: String -> String
urlDecodeS s = (mkString o urlDecode o mkList) s

string_to_dynamic` :: {#Char} -> Dynamic
string_to_dynamic` s = string_to_dynamic {s` \\ s` <-: s}
