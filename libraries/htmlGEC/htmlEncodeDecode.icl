 implementation module htmlEncodeDecode

// encoding and decoding of information
// (c) 2005 MJP

import StdEnv, ArgEnv, StdMaybe, Directory
import htmlDataDef, htmlTrivial, htmlFormData
import GenPrint, GenParse

derive gParse UpdValue
derive gParse (,), (,,)

// This module controls the handling of state forms and the communication with the browser
// Internally, a Tree is used to store the form states
// Externaly, these states are stored in the Html form in a list
// A distinction is made between old states (states retrieved from the html form)
// and new states (states of newly created forms and updated forms)

:: *FormStates 										// State of forms is internally stored in a tree
	= 	{ fstates 	:: *FStates
		, triplet	:: String
		, update	:: String
		, updateid	:: String
		, server	:: ServerKind
		}		

:: FStates		:== Tree_ (String,FormState)

:: Tree_ a 		= Node_ (Tree_ a) a (Tree_ a) | Leaf_
:: FormState 	= OldState !OldState				// Old states will become garbage when the final states are reached
				| NewState !NewState				// New states that will be saved in the html form
:: OldState		= { ostrval :: !String				// String representation of the view value
				  , olive	:: !Lifespan			// Its life span
				  }
:: NewState 	= { dynval 	::!Dynamic				// A new state is stored in a dynamic
				  , strval 	:: String				// together with its string representation
				  , live	:: !Lifespan			// Its life span
				  }
:: HtmlStates :== [HtmlState]						// For convenience, the state is stored in html as a list and not as a tree
:: HtmlState  :== (!String,!Lifespan,!String)		// Format just before writing to the page format

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
		mkHtmlState	elem = (mkString (stl fid),toLivetime fid,mkString (stl (reverse (stl (reverse formvalue)))))
		where
			(fid,formvalue) = mscan '"' (stl (stl elem)) // skip '("'
			stl [] = []
			stl [x:xs] = xs 
			
			toLivetime ['N':_] = Page
			toLivetime ['S':_] = Session
			toLivetime ['P':_] = Persistent
			toLivetime _   		= Page


// convert this HtmlState into FormStates which are used internally

initFormStates 	:: ServerKind (Maybe String) *NWorld -> (*FormStates,*NWorld) 					// retrieves all form states hidden in the html page
initFormStates serverkind args world 
	= ({ fstates = readStatesFromArgs, triplet = triplet, update = update, updateid = updateid , server = serverkind},world)
where
	readStatesFromArgs = Balance (sort [(sid,OldState {ostrval = state, olive = lifespan}) 
										\\ (sid,lifespan,state) <- retrieveHtmlState serverkind args| sid <> ""
										])
	(_,triplet,update,_) = DecodeArguments serverkind args

	Balance [] = Leaf_
	Balance [x] = Node_ Leaf_ x Leaf_
	Balance xs
		= case splitAt (length xs/2) xs of
			(a,[b:bs]) = Node_ (Balance a) b (Balance bs)
			(as,[]) = Node_   (Balance (init as)) (last as) Leaf_
	
	updateid 		
//	# (_,upd,_,_) = DecodeArguments serverkind args
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
# (new,    input)	= mscan '-' input // should give update value
# (_,input)			= mscan '=' input
# input				= skipping ['\"GS\"']  input
//# (state, input)	= mscan '-' input
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
//		(first,second) = span ((<>) '$') listofchar
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
					(OldState state) = (True, parseString state.ostrval,formstate,world)
					(NewState state) = (False,retrieveNewState state,formstate,world)
	| formid.id  < fid 	= (bool,parsed, Node_ leftformstates (fid,info) right,nworld)
					with
						(bool,parsed,leftformstates,nworld) = findState` formid left world
	| otherwise	= (bool,parsed, Node_  left (fid,info) rightformstates,nworld)
					with
						(bool,parsed,rightformstates,nworld) = findState` formid right world
	findState` {id,lifespan = Persistent} Leaf_ world 
	# (ma,string,world) = readState id world
	= case ma of
		Just a 	-> (True,ma,Node_ Leaf_ (id,NewState {dynval = dynamic a, strval = string, live = Persistent}) Leaf_,world)
		Nothing	-> (False,Nothing,Leaf_,world)
	findState` _ Leaf_ world = (False,Nothing,Leaf_,world)

	readState :: String *NWorld -> (Maybe a,String,*NWorld) | gParse {|*|} a 
	readState filename env
	#(_,env) = case getFileInfo mydir env of
				((DoesntExist,fileinfo),env) -> createDirectory mydir env
				(_,env) -> (NoDirError,env)
	# (ok,file,env)	= fopen (MyDir server +++ "/" +++ filename) FReadData env
	| not ok 		= (Nothing,"",env)
	# (string,file)	= freads file big
	| not ok 		= (Nothing,"",env)
	# (ok,env)		= fclose file env
	# string 		= string%(1,size string - 2)
	# string		= mkString (removeBackslashQuote (mkList string))
	= (parseString  string,string,env) 
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
	replaceState` formid val Leaf_ world = (Node_ Leaf_ (formid.id,NewState (initNewState formid.lifespan val)) Leaf_,world)
	replaceState` formid val (Node_ left a=:(fid,_) right) world
	| formid.id == fid 	= (Node_ left (fid,NewState (initNewState formid.lifespan val)) right,world)
	| formid.id < fid 	= (Node_ nleft a right,nworld)
							with
								(nleft,nworld) = replaceState` formid val left world
	| otherwise			 = (Node_ left a nright,nworld)
							with
								(nright,nworld) = replaceState` formid val right world

// NewState Handling routines 

initNewState :: !Lifespan !a  -> NewState | TC a &  gPrint{|*|} a 
initNewState lifespan nv = {dynval = dynamic nv,strval = printToString nv, live = lifespan}

/*
storeNewState :: a NewState -> NewState | TC a &  gPrint{|*|} a 
storeNewState nv {dynval = (val::a^)} = {dynval = dynamic nv, strval = printToString nv}
storeNewState nv old = old
*/

retrieveNewState :: NewState -> Maybe a | TC a & gParse{|*|} a
retrieveNewState {strval} = parseString strval
//retrieveNewState {dynval = (v::a^)} = Just v    // causes a run-time crash sometimes ??????? bug ????
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

		// old states have not been used this time, with lifespan session they are stored again in the page

		toHtmlState` (Node_ left (fid,OldState {olive,ostrval}) right) tl = toHtmlState` left [(fid,olive,ostrval):toHtmlState` right tl]

		// other old states will ahve lifespan page; they become garbage and are no longer stored in the page

		toHtmlState` (Node_ left (fid,OldState s) right) tl = toHtmlState` left (toHtmlState` right tl)

		// persistent stores have already been stored in files

		toHtmlState` (Node_ left (fid,NewState {strval,live=Persistent}) right) tl = toHtmlState` left (toHtmlState` right tl)

		// the state of all other new forms created will be stored in the page 

		toHtmlState` (Node_ left (fid,NewState {strval,live}) right) tl = toHtmlState` left [(fid,live,strval): toHtmlState` right tl]

	urlEncodeState :: !HtmlStates -> String
	urlEncodeState [] = urlEncodeS "$"
	urlEncodeState [(id,lifespan,state):xsys] 
		= urlEncodeS "(\"" +++ fromLivetime lifespan +++ urlEncodeS id +++ 
		  urlEncodeS "\"," +++ urlEncodeS state +++ 
		  urlEncodeS ")$" +++ urlEncodeState xsys
	where
		fromLivetime Page 			= "N"
		fromLivetime Session 		= "S"
		fromLivetime Persistent 	= "P"
	 
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
	writeAllPersistentStates (Node_ left (sid,NewState {strval,live = Persistent}) right) world
	# world = writeState sid strval world 
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
