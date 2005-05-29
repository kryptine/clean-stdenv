 implementation module htmlEncodeDecode

// encoding and decoding of information
// (c) 2005 MJP

import StdEnv, ArgEnv, StdMaybe, Directory
import htmlDataDef, htmlTrivial, htmlFormData
import GenPrint, GenParse

// This module controls the handling of state forms and the communication with the browser
// Internally, a Tree is used to store the form states
// Externaly, these states are stored in the Html form in a list
// A distinction is made between old states (states retrieved from the html form)
// and new states (states of newly created forms and updated forms)

:: *FormStates 	:== Tree_ (String,FormState)		// State of forms is internally stored in a tree
:: Tree_ a 		= Node_ (Tree_ a) a (Tree_ a) | Leaf_
:: FormState 	= OldState !String					// Old states will become garbage when the final states are reached
				| NewState !NewState				// New states that will be saved in the html form
:: NewState 	= { dynval 	::!Dynamic				// A new state is stored in a dynamic
				  , strval 	:: String				// together with its string representation
				  , live	:: !Livetime
				  }
:: HtmlState :== [(String,String)]					// For convenience, the state is stored in html as a list and not as a tree

// reconstruct HtmlState out of the information obtained from browser

retrieveHtmlState :: HtmlState
retrieveHtmlState 
# (_,_,_,state) = DecodedStateFormBrowser
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

// convert this HtmlState into FormStates which are used internally

initFormStates :: *FormStates
initFormStates = Balance (sort [(formid,OldState state) \\ (formid,state) <- retrieveHtmlState | formid <> ""])
where
	Balance [] = Leaf_
	Balance [x] = Node_ Leaf_ x Leaf_
	Balance xs
		= case splitAt (length xs/2) xs of
			(a,[b:bs]) = Node_ (Balance a) b (Balance bs)
			(as,[]) = Node_   (Balance (init as)) (last as) Leaf_

// FormStates abstract data handling routines

emptyFormStates :: *FormStates
emptyFormStates = Leaf_

instance < FormState
where
	(<) _ _ = True

force :: !a *b -> *b
force a b = b

findState :: !FormId *FormStates *World -> (Bool,Maybe a,*FormStates,*World)	| gParse{|*|} a & TC a
findState formid states world = findState` formid states world
where
	findState` :: !FormId *FormStates *World -> (Bool,Maybe a,*FormStates,*World)| gParse{|*|} a & TC a
	findState` formid formstate=:(Node_ left (fid,info) right) world
	| formid.id == fid = case info of
					(OldState state) = (True, parseString state,formstate,world)
					(NewState state) = (False,retrieveNewState state,formstate,world)
	| formid.id  < fid 	= (bool,parsed, Node_ leftformstates (fid,info) right,nworld)
					with
						(bool,parsed,leftformstates,nworld) = findState` formid left world
	| otherwise	= (bool,parsed, Node_  left (fid,info) rightformstates,nworld)
					with
						(bool,parsed,rightformstates,nworld) = findState` formid right world
	findState` {id,livetime = Persistent} Leaf_ world 
	# (ma,string,world) = readState id world
	= case ma of
		Just a 	-> (True,ma,Node_ Leaf_ (id,NewState {dynval = dynamic a, strval = string, live = Persistent}) Leaf_,world)
		Nothing	-> (False,Nothing,Leaf_,world)
	findState` _ Leaf_ world = (False,Nothing,Leaf_,world)

	readState :: String *World -> (Maybe a,String,*World) | gParse {|*|} a 
	readState filename env
	#(_,env) = case getFileInfo mydir env of
				((DoesntExist,fileinfo),env) -> createDirectory mydir env
				(_,env) -> (NoDirError,env)
	# (ok,file,env)	= fopen (MyDir +++ "/" +++ filename) FReadData env
	| not ok 		= (Nothing,"",env)
	# (string,file)	= freads file big
	| not ok 		= (Nothing,"",env)
	# (ok,env)		= fclose file env
	# string 		= string%(1,size string - 2)
	# string		= mkString (removeBackslashQuote (mkList string))
	= (parseString  string,string,env) 
	where
		big = 100000
		mydir = RelativePath [PathDown MyDir]

		removeBackslashQuote [] 			= []
		removeBackslashQuote ['\\\"':xs] 	= ['\"':removeBackslashQuote xs]
		removeBackslashQuote [x:xs] 		= [x:removeBackslashQuote xs]

replaceState :: !FormId a *FormStates *World -> (*FormStates,*World)	| gPrint{|*|} a & TC a
replaceState formid val Leaf_ world = (Node_ Leaf_ (formid.id,NewState (initNewState formid.livetime val)) Leaf_,world)
replaceState formid val (Node_ left a=:(fid,_) right) world
| formid.id == fid 	= (Node_ left (fid,NewState (initNewState formid.livetime val)) right,world)
| formid.id < fid 	= (Node_ nleft a right,nworld)
						with
							(nleft,nworld) = replaceState formid val left world
| otherwise			 = (Node_ left a nright,nworld)
						with
							(nright,nworld) = replaceState formid val right world

// NewState Handling routines 

initNewState :: !Livetime !a  -> NewState | TC a &  gPrint{|*|} a 
initNewState livetime nv = {dynval = dynamic nv,strval = printToString nv, live = livetime}

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

convStates :: !FormStates *World -> (BodyTag,*World)
convStates allFormStates world
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
		toHtmlState` (Node_ left (formid,OldState s) right) tl = toHtmlState` left (toHtmlState` right tl) // old states are garbage
		toHtmlState` (Node_ left (formid,NewState {strval,live=Persistent}) right) tl = toHtmlState` left (toHtmlState` right tl) // persistent stores are stored in files
		toHtmlState` (Node_ left (formid,NewState {strval}) right) tl = toHtmlState` left [(formid,strval): toHtmlState` right tl] // only remember new states for next round

	urlEncodeState :: !HtmlState -> String
	urlEncodeState [] = urlEncodeS "$"
	urlEncodeState [(x,y):xsys] = urlEncodeS "(\"" +++ urlEncodeS x +++ 
								  urlEncodeS "\"," +++ urlEncodeS y +++ 
								  urlEncodeS ")$" +++ urlEncodeState xsys 
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
				, Frm_Action MyPhP
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

//	writeAllPersistentStates:: FormStates *World -> *World 
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

writeState :: String a *World -> *World | gPrint {|*|} a 
writeState filename val env
#(_,env) = case getFileInfo mydir env of
			((DoesntExist,fileinfo),env) -> createDirectory mydir env
			(_,env) -> (NoDirError,env)
# (ok,file,env)	= fopen (MyDir +++ "/" +++ filename) FWriteData env
| not ok 		= env
# file			= fwrites (printToString val) file
# (ok,env)		= fclose file env
= env
where
	mydir = RelativePath [PathDown MyDir]

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

// determining the update information

:: UpdValue 	= UpdI Int					// new integer value
				| UpdR Real					// new real value
				| UpdB Bool					// new boolean value
				| UpdC String				// choose indicated constructor 
				| UpdS String				// new piece of text

CheckUpdateId :: String
CheckUpdateId 		
# (_,upd,_,_) = DecodedStateFormBrowser
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
# (_,_,new,_) = DecodedStateFormBrowser
=: new

CheckUpdate :: (!Maybe a, !Maybe b) | gParse{|*|} a & gParse{|*|} b 
CheckUpdate 
# (_,upd,new,_) = DecodedStateFormBrowser
= (parseString upd, parseString new)

derive gParse (,), (,,)

// all input information from the browser is obtained once via the arguments passed to this executable
// defined as CAFs such that they are calculated only once

GetArgs :: String 
GetArgs =: foldl (+++) "" [strings \\ strings <-: getCommandLine]

ThisExe :: String
ThisExe 
# (thisexe,_,_,_) = DecodedStateFormBrowser
=: thisexe

MyPhP :: String
MyPhP =: (mkString (takeWhile ((<>) '.') (mkList ThisExe))) +++ ".php"

MyDir :: String
MyDir =: (mkString (takeWhile ((<>) '.') (mkList ThisExe)))

DecodedStateFormBrowser :: (!String,!String,!String,!String) // executable, id + update , new , state
DecodedStateFormBrowser
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
//			,	Txt "decoded input  	: " , B [] (convert GetArgs), Br
//			,	Txt "encoded input      : " , B [] GetArgs, Br 
			]
where
	(executable,update,new,state) = DecodedStateFormBrowser
	convert s = mkString (urlDecode (mkList s))

	showstate :: [Char] -> [BodyTag]
	showstate [] 			= []
	showstate listofchar	= [Br, B [] (mkString first)] ++ showstate (tl second)
	where
		(first,second) = span ((<>) '$') listofchar

// low level url encoding decoding

mscan c list = case (span ((<>) c) list) of  // scan like span but it removes character
				(x,[])	= (x,[])
				(x,y)	= (x,tl y)

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


// storing and retrieving forms from files
// all form data is stored in a directory with the same name as the application

/*
readInitState :: String a *env -> (a,*World) | gParse {|*|} a 
readInitState filename init env
= case readState filename env of
	(Nothing,s,env) -> (init,env)
	(Just a,s,env) -> (a,env)

*/



