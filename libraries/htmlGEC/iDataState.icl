 implementation module iDataState

// encoding and decoding of information
// (c) 2005 MJP

import StdEnv, ArgEnv, StdMaybe, Directory
import htmlDataDef, htmlTrivial, htmlFormData, EncodeDecode
import GenPrint, GenParse
import dynamic_string

derive gParse UpdValue, (,,)
derive gPrint UpdValue, (,,)

// This module controls the handling of state forms and the communication with the browser
// iData states are maintained in a binairy tree

// The are currently two storage formats for iData values: 
// 1. a string format, generically generated; works for any first order type.
// A generic parser is needed to convert the string back to a value of the required type.
// 2. a static (= no dynamic linker needed) dynamic; works for any type, including higher order types.
// But watch it: only the application which wrotes the dynamic, can read it back: no plug ins! recompiling means: all information lost!
// A distinction is made between old states (states retrieved from the html form)
// and new states (states of newly created forms and updated forms)

:: *FormStates 	=										// collection of states of all forms
				{ fstates 	:: *FStates					// internal tree of states
				, triplet	:: Maybe Triplet			// indicates what has changed: which form, which postion, which value
				, update	:: String					// what is the new value created by the end user
				, updateid	:: String					// which form has changed
				, server	:: ServerKind				// is an external server required
				}		

:: FStates		:== Tree_ (String,FormState)			// each form needs a different string id
:: Tree_ a 		= Node_ (Tree_ a) a (Tree_ a) | Leaf_
:: FormState 	= OldState !FState						// Old states are the states from the previous calculation
				| NewState !FState 						// New states are newly created states or old states that have been inspected and updated
:: FState		= { format	 :: !Format					// Encoding method used for serialization
				  , life	 :: !Lifespan				// Its life span
				  }
:: Format		= PlainStr 	!.String 					// Either a string is used for serialization
				| StatDyn	!Dynamic 					// Or a dynamic which enables serialization of functions defined in the application (no plug ins yet)
				| DBStr		!.String (*Gerda -> *Gerda)	// In case a new value has to bestored in the database 
				
// functions defined on the FormStates abstract data type

instance < FormState
where
	(<) _ _ = True

emptyFormStates :: *FormStates
emptyFormStates = { fstates = Leaf_ , triplet = Nothing, update = "", updateid = "", server = Internal}

getTriplet :: *FormStates -> (!Maybe Triplet, !Maybe b,*FormStates) | gParse{|*|} b 
getTriplet formstates=:{triplet,update}
= (triplet, parseString update, formstates)
//= (parseString triplet, parseString update, formstates)

getUpdateId ::  *FormStates -> (String,*FormStates)
getUpdateId formStates=:{updateid} = (updateid,formStates)

getUpdate ::  *FormStates -> (String,*FormStates)
getUpdate formStates=:{update} = (update,formStates)

findState :: !(FormId a) *FormStates *NWorld -> (Bool,Maybe a,*FormStates,*NWorld)	| gPrint {|*|}, gParse{|*|}, gerda{|*|}, TC a
findState formid formstates=:{fstates,server} world
# (bool,ma,fstates,world) = findState` formid fstates world
= (bool,ma,{formstates & fstates = fstates},world)
where
	findState` :: !(FormId a) *FStates *NWorld -> (Bool,Maybe a,*FStates,*NWorld)| gPrint {|*|}, gParse{|*|}, gerda{|*|}, TC a
	findState` formid formstate=:(Node_ left (fid,info) right) world
	| formid.id == fid = case info of
					(OldState state) = (True, fetchFState state,formstate,world)
					(NewState state) = (False,fetchFState state,formstate,world)
	with
		fetchFState :: FState -> Maybe a | TC a & gParse{|*|} a
		fetchFState {format = PlainStr string} = parseString string
		fetchFState {format = DBStr string _}  = parseString string
		fetchFState {format = StatDyn (v::a^)} = Just v    
		fetchFState _ = Nothing
	| formid.id  < fid 	= (bool,parsed, Node_ leftformstates (fid,info) right,nworld)
					with
						(bool,parsed,leftformstates,nworld) = findState` formid left world
	| otherwise	= (bool,parsed, Node_  left (fid,info) rightformstates,nworld)
					with
						(bool,parsed,rightformstates,nworld) = findState` formid right world

	// value is not yet available in the tree storage...
	// all stuf read out from persistent store are now marked as OldState (was NewState)	
	// read out database and store as string 

	findState` {id,lifespan = Database,storage = PlainString} Leaf_ world=:{gerda} 
	# (value,gerda) = readGerda id	gerda
	# world = {world & gerda = gerda}
	= case value of
		Just a 	-> (True,Just a,Node_ Leaf_ (id,OldState {format = PlainStr (printToString a), life = Database}) Leaf_,world)
		Nothing	-> (False,Nothing,Leaf_,world)

	// read out database and store as dynamic

	findState` {id,lifespan = Database,storage = StaticDynamic} Leaf_ world=:{gerda} 
	# (value,gerda) = readGerda id	gerda
	# world = {world & gerda = gerda}
	= case value of 
		Nothing 		= (False,Nothing,Leaf_,world)
		(Just string)	= case string_to_dynamic` string of
							dyn=:(dynval::a^) 	-> (True,Just dynval,Node_ Leaf_ (id,OldState {format = StatDyn dyn, life = Persistent}) Leaf_,world)
							else				-> (False,Nothing,Leaf_,world)

	// read out file and store as string

	findState` {id,lifespan = Persistent,storage = PlainString} Leaf_ world 
	# (string,world) = readStringState (MyDir server) id world
	= case parseString string of
		Just a 	-> (True,Just a,Node_ Leaf_ (id,OldState {format = PlainStr string, life = Persistent}) Leaf_,world)
		Nothing	-> (False,Nothing,Leaf_,world)

	findState` {id,lifespan = PersistentRO,storage = PlainString} Leaf_ world 
	# (string,world) = readStringState (MyDir server) id world
	= case parseString string of
		Just a 	-> (True,Just a,Node_ Leaf_ (id,OldState {format = PlainStr string, life = PersistentRO}) Leaf_,world)
		Nothing	-> (False,Nothing,Leaf_,world)

	// read out file and store as dynamic

	findState` {id,lifespan = Persistent,storage = StaticDynamic} Leaf_ world 
	# (string,world) = readDynamicState (MyDir server) id world
	= case string of 
		"" = (False,Nothing,Leaf_,world)
		_  = case string_to_dynamic` string of
				dyn=:(dynval::a^) 	-> (True,Just dynval,Node_ Leaf_ (id,OldState {format = StatDyn dyn, life = Persistent}) Leaf_,world)
				else				-> (False,Nothing,Leaf_,world)

	findState` {id,lifespan = PersistentRO,storage = StaticDynamic} Leaf_ world 
	# (string,world) = readDynamicState (MyDir server) id world
	= case string of 
		"" = (False,Nothing,Leaf_,world)
		_  = case string_to_dynamic` string of
				dyn=:(dynval::a^) 	-> (True,Just dynval,Node_ Leaf_ (id,OldState {format = StatDyn dyn, life = PersistentRO}) Leaf_,world)
				else				-> (False,Nothing,Leaf_,world)

	// cannot find the value at all

	findState` _ Leaf_ world 	= (False,Nothing,Leaf_,world)
	findState` _ _ world 		= (False,Nothing,Leaf_,world)

string_to_dynamic` :: {#Char} -> Dynamic	// just to make a unique copy as requested by string_to_dynamic
string_to_dynamic` s = string_to_dynamic {s` \\ s` <-: s}

replaceState ::  !(FormId a) a *FormStates *NWorld -> (*FormStates,*NWorld)	| gPrint{|*|}, gerda{|*|}, TC a
replaceState formid val formstates=:{fstates} world
# (fstates,world) = replaceState` formid val fstates world
= ({formstates & fstates = fstates},world)
where
	replaceState` ::  !(FormId a) a *FStates *NWorld -> (*FStates,*NWorld)	| gPrint{|*|}, gerda{|*|}, TC a
	replaceState` formid val Leaf_ world 									// id not part of tree yet
						= (Node_ Leaf_ (formid.id,NewState (initNewState formid.id (adjustlife formid.lifespan) Temp formid.storage val)) Leaf_,world)
	replaceState` formid val (Node_ left a=:(fid,fstate) right) world
	| formid.id == fid 	= (Node_ left (fid,NewState (initNewState formid.id formid.lifespan (detlifespan fstate) formid.storage val)) right,world)
	| formid.id < fid 	= (Node_ nleft a right,nworld)
							with
								(nleft,nworld) = replaceState` formid val left world
	| otherwise			= (Node_ left a nright,nworld)
							with
								(nright,nworld) = replaceState` formid val right world

	// NewState Handling routines 

	initNewState :: !String !Lifespan !Lifespan !StorageFormat !a  -> FState | gPrint{|*|}, gerda{|*|}, TC a 
	initNewState id Database olifespan PlainString   nv = {format = DBStr    (printToString nv) (writeGerda id nv), life = order Database olifespan}
	initNewState id lifespan olifespan PlainString   nv = {format = PlainStr (printToString nv), life = order lifespan olifespan}
	initNewState id lifespan olifespan StaticDynamic nv = {format = StatDyn  (dynamic nv), life = order lifespan olifespan}// convert the hidden state information stored in the html page

	adjustlife PersistentRO = Persistent // to enforce that a read only persistent file is written once
	adjustlife life = life

	detlifespan (OldState formstate) = formstate.life
	detlifespan (NewState formstate) = formstate.life

	order l1 l2 = if (l1 < l2) l2 l1	// longest lifetime chosen will be the final setting Database > Persistent > Session > Page > temp

// Serialization and De-Serialization of states
//
// De-serialize information from server to the internally used form states

retrieveFormStates 	:: ServerKind (Maybe String) *NWorld -> (*FormStates,*NWorld) 					// retrieves all form states hidden in the html page
retrieveFormStates serverkind args world 
	= ({ fstates = retrieveFStates, triplet = parsed_triplet, update = update, 
		updateid = calc_updateid , server = serverkind},world)
where
	retrieveFStates 
		= Balance (sort [(sid,OldState {format = toExistval storageformat state, life = lifespan}) 
						\\ (sid,lifespan,storageformat,state) <- htmlStates
						|  sid <> ""
						])
	where
		toExistval PlainString string 		= PlainStr string	// string that has to be parsed in the context where the type is known
		toExistval StaticDynamic string 	= StatDyn (string_to_dynamic` string) // recover the dynamic

	(htmlStates,triplet,update) = DecodeHtmlStatesAndUpdate serverkind args
	parsed_triplet  = parseString triplet

	Balance [] = Leaf_
	Balance [x] = Node_ Leaf_ x Leaf_
	Balance xs
		= case splitAt (length xs/2) xs of
			(a,[b:bs]) = Node_ (Balance a) b (Balance bs)
			(as,[]) = Node_   (Balance (init as)) (last as) Leaf_
	
	calc_updateid 		
	= case parsed_triplet of
		Just ("",0,UpdI 0) = ""
		Just (id,_,_)   = id 
		else = ""

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
	encodedglobalstate = EncodeHtmlStates (FStateToHtmlState allFormStates)

	FStateToHtmlState :: (Tree_ (String,.FormState)) -> *[HtmlState]
	FStateToHtmlState formstates = toHtmlState` formstates []
	where
		toHtmlState` Leaf_ accu = accu

		// old states which have not been used this time, but with lifespan session, are stored again in the page

		toHtmlState` (Node_ left (fid,OldState {life=Session,format=PlainStr stringval}) right) accu 
			= toHtmlState` left [(fid,Session,PlainString,stringval):toHtmlState` right accu]
		toHtmlState` (Node_ left (fid,OldState {life=Session,format=StatDyn dynval}) right) accu 
			= toHtmlState` left [(fid,Session,StaticDynamic,dynamic_to_string dynval):toHtmlState` right accu]

		// other old states will have lifespan page or persistent; they need not to be stored in the page

		toHtmlState` (Node_ left (fid,OldState s) right) accu 
			= toHtmlState` left (toHtmlState` right accu)

		// persistent stores (either old or new) have already been stored in files and can be skipped here

		toHtmlState` (Node_ left (fid,NewState {life=Database}) right) accu 
			= toHtmlState` left (toHtmlState` right accu)
		toHtmlState` (Node_ left (fid,NewState {life=Persistent}) right) accu 
			= toHtmlState` left (toHtmlState` right accu)
		toHtmlState` (Node_ left (fid,NewState {life=PersistentRO}) right) accu 
			= toHtmlState` left (toHtmlState` right accu)

		// temperal form don't need to be stored and can be skipped as well

		toHtmlState` (Node_ left (fid,NewState {life=Temp}) right) accu 
			= toHtmlState` left (toHtmlState` right accu)

		// the state of all other new forms created will be stored in the page 

		toHtmlState` (Node_ left (fid,NewState {format = PlainStr string,life}) right) accu 
			= toHtmlState` left [(fid,life,PlainString,string): toHtmlState` right accu]
		toHtmlState` (Node_ left (fid,NewState {format = StatDyn dynval,life}) right) accu 
			= toHtmlState` left [(fid,life,StaticDynamic,dynamic_to_string dynval): toHtmlState` right accu]
 
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

	writeAllPersistentStates:: FStates *NWorld -> *NWorld 	// store states in persistent stores
	writeAllPersistentStates Leaf_ nworld = nworld

	// only new states need to be stored, since old states have not been changed (assertion)

//	writeAllPersistentStates (Node_ left (sid,OldState {format,life = Persistent}) right) nworld
//	= writeAllPersistentStates right (writeAllPersistentStates left (serializeAndStoreState sid format nworld))
//	= writeAllPersistentStates right (writeAllPersistentStates left nworld)

	writeAllPersistentStates (Node_ left (sid,NewState {format,life = Database}) right) nworld
	= writeAllPersistentStates right (writeAllPersistentStates left (storeInDatabase sid format nworld))
	writeAllPersistentStates (Node_ left (sid,NewState {format,life = Persistent}) right) nworld
	= writeAllPersistentStates right (writeAllPersistentStates left (serializeAndStoreState sid format nworld))
	writeAllPersistentStates (Node_ left _ right) nworld
	= writeAllPersistentStates right (writeAllPersistentStates left nworld)

	storeInDatabase sid (DBStr string gerdafun) nworld=:{gerda}	//last value stored in tree hidden in curried function
	# gerda = gerdafun gerda
	= {nworld & gerda = gerda}
	storeInDatabase sid (StatDyn dynval) nworld=:{gerda}		//write the dynamic as a string to the database
	# gerda = writeGerda sid (dynamic_to_string dynval) gerda
	= {nworld & gerda = gerda}

	serializeAndStoreState sid (PlainStr string) nworld 	= writeState (MyDir server) sid string nworld
	serializeAndStoreState sid (StatDyn dynval)  nworld 	= writeState (MyDir server) sid (dynamic_to_string dynval) nworld

// to encode triplets in htmlpages

encodeTriplet	:: Triplet -> String				// encoding of triplets
encodeTriplet triplet = encodeInfo triplet

decodeTriplet	:: String -> Maybe Triplet				// encoding of triplets
decodeTriplet triplet = decodeInfo triplet

// script for transmitting name and value of changed input 

callClean :: Script
callClean =: SScript "toclean(this)"

// interfaces added for testing:

initTestFormStates 	::  *NWorld -> (*FormStates,*NWorld) 					// retrieves all form states hidden in the html page
initTestFormStates world 
	= ({ fstates = Leaf_, triplet = Nothing, update = "", updateid = "" , server = JustTesting},world)

setTestFormStates 	::  (Maybe Triplet) String String *FormStates *NWorld -> (*FormStates,*NWorld) 					// retrieves all form states hidden in the html page
setTestFormStates triplet updateid update states world 
	= ({ fstates = makeoldstates , triplet = triplet, update = update, updateid = updateid, server = JustTesting},world)
	where
		makeoldstates = toOldStates states.fstates 
		where
			toOldStates Leaf_ 	= Leaf_
			toOldStates (Node_ left (s,NewState fstate) right)  
								= Node_ (toOldStates left) (s,OldState fstate) (toOldStates right)
			toOldStates else	= else
