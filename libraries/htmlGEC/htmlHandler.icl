implementation module htmlHandler

import StdEnv, ArgEnv, StdMaybe
import htmlDataDef, htmlTrivial, htmlSettings
import StdGeneric
import iDataState, htmlStylelib
import GenParse, GenPrint
import httpServer
import Gerda

derive gPrint (,), (,,), (,,,), UpdValue
derive gParse (,), (,,), (,,,), UpdValue
derive gHpr   (,), (,,), (,,,)
derive gUpd		   (,,), (,,,)
derive bimap Form, [], FormId
 
gParse {|(->)|} gArg gRes _ 	= Nothing 
gPrint{|(->)|} gArg gRes  _ _	= abort "functions can only be used with dynamic storage option!\n" 

:: *HSt 		= 	{ cntr 		:: Int 			// counts position in expression
					, states 	:: *FormStates  // all form states are collected here ... 	
					, world		:: *NWorld		// to enable all other kinds of I/O
					}	

:: InputId	 	:== Int						// unique id for every constructor and basic value appearing in the state

:: FormUpdate	:== (InputId,UpdValue)		// info obtained when form is updated

// OPTIONS

openGerda` database world
:== IF_GERDA (openGerda database world) (abort "Trying to open database while options is switched off",world)
closeGerda` gerda world
:== IF_GERDA (closeGerda gerda world) world


// top level function given to end user
// it collects all the html forms to display, adds clean styles and hidden forms, ands prints the html code to stdout
// assumes that the application is used by a server

doHtml :: .(*HSt -> (Html,!*HSt)) *World -> *World
doHtml userpage world 
# inout					= [|]
# (gerda,world)			= openGerda` MyDataBase world	
# nworld 				= { worldC = world, inout = inout, gerda = gerda}	
# (initforms,nworld) 	= retrieveFormStates External Nothing nworld
# (Html (Head headattr headtags) (Body attr bodytags),{states,world}) 
						= userpage {cntr = 0, states = initforms, world = nworld}
# (allformbodies,world) = storeFormStates states world
# {worldC,gerda}		= print_to_stdout 
								(Html (Head headattr [extra_style:headtags]) 
								(Body (extra_body_attr ++ attr) [debugInput,allformbodies:bodytags]))
								world
# worldC				= closeGerda` gerda worldC
= worldC
where
	stuf = "Hello world"

	extra_body_attr = [Batt_background "back35.jpg",`Batt_Std [CleanStyle]]
	extra_style = Hd_Style [] CleanStyles	

	debugInput = if TraceInput (traceHtmlInput External Nothing) EmptyBody

// same as doHtml, but now a Clean Server will be included

//doHtmlServer :: .(*HSt -> (Html,!*HSt)) *World -> *World   // this one is rejected !!
doHtmlServer :: (*HSt -> (Html,!*HSt)) *World -> *World
doHtmlServer userpage world
	= StartServer 80 (map (\(id,_,f) -> (id,f)) pages) world
where
	pages
		=	[("clean", "CleanExample", \_ _ a  -> doHtmlServer2 (conv a) userpage)
			]
	conv args =  foldl (+++) "" (map (\(x,y) -> y) args)

	doHtmlServer2 :: String .(*HSt -> (Html,!*HSt)) *World -> ([String],String,*World)
	doHtmlServer2 args userpage world 
	# temp 					= [|]
	# (gerda,world)			= openGerda` MyDataBase world
	# nworld 				= { worldC = world, inout = temp, gerda = gerda }	
	# (initforms,nworld) 	= retrieveFormStates Internal (Just args) nworld
	# (Html (Head headattr headtags) (Body attr bodytags),{states,world}) 
							= userpage {cntr = 0, states = initforms, world = nworld}
	# (allformbodies,world) = storeFormStates states world
	# {worldC,gerda,inout}	= print_to_stdout 
										(Html (Head headattr [extra_style:headtags]) 
										(Body (extra_body_attr ++ attr) [debugInput,allformbodies:bodytags])) 
										world
	# world					= worldC
	# world					= closeGerda` gerda world
	# reversed_strings = inout
	# n_chars = count_chars reversed_strings 0
		with
			count_chars [|] n = n
			count_chars [|s:l] n = count_chars l (n+size s)
	# allhtmlcode = copy_strings reversed_strings n_chars (createArray n_chars '\0')
		with
			copy_strings [|e:l] i s
				# size_e = size e
				# i=i-size_e
				= copy_strings l i (copy_chars e 0 i size_e s)
			copy_strings [|] 0 s
				= s

			copy_chars :: !{#Char} !Int !Int !Int !*{#Char} -> !*{#Char}
			copy_chars s_s s_i d_i n d_s
				| s_i<n
					# d_s = {d_s & [d_i]=s_s.[s_i]}
					= copy_chars s_s (s_i+1) (d_i+1) n d_s
					= d_s
	= ([],allhtmlcode,world)
	where
		extra_body_attr = [Batt_background "back35.jpg",`Batt_Std [CleanStyle]]
		extra_style = Hd_Style [] CleanStyles	
		debugInput = if TraceInput (traceHtmlInput Internal (Just args)) EmptyBody

// swiss army nife editor that makes coffee too ...

mkViewForm :: !(InIDataId d) !(HBimap d v) !*HSt -> (Form d,!*HSt) | iData v
mkViewForm (init,formid) bm=:{toForm, updForm, fromForm, resetForm}  hst=:{states,world} 
| init == Const	&& formid.lifespan <> Temp
= mkViewForm (init,{formid & lifespan = Temp}) bm hst				// constant i-data are never stored
| init == Const														// constant i-data, no look up of previous value
= calcnextView False Nothing states world				
# (isupdated,view,states,world) = findFormInfo vformid states world // determine current view value in the state store
= calcnextView isupdated view states world							// and calculate new i-data
where
	vformid = (reuseFormId formid (toForm init formid.ival Nothing))

	calcnextView isupdated view states world
	# (changedid,states) = getUpdateId states
	# view 		= toForm  init formid.ival view		// map value to view domain, given previous view value
	# view		= updForm  {isChanged = isupdated, changedId = changedid} view			// apply update function telling user if an update has taken place
	# newval	= fromForm {isChanged = isupdated, changedId = changedid} view			// convert back to data domain	 
	# view		= case resetForm of					// optionally reset the view hereafter for next time
						Nothing 	-> view		 
						Just reset 	-> reset view

	| formid.mode == NoForm							// don't make a form at all
		# (states,world) = replaceState` vformid view states world	// store new value into the store of states
		= (	{changed	= False
			, value		= newval
			, form		= []
			},{cntr = 0, states = states, world = world})

	# (viewform,{states,world})						// make a form for it
					= gForm{|*|} (init,if (init == Const) vformid (reuseFormId formid view)) {cntr = 0,states = states, world = world}

	| viewform.changed && not isupdated 			// trick: redo it all to handle the case that a user defined specialisation is updated !!
		= calcnextView True (Just viewform.value) states world

	# (states,world) = replaceState` vformid (viewform.value) states world	// store new value into the store of states

	= (	{changed	= isupdated
		, value		= newval
		, form		= viewform.form
		},{cntr = 0, states = states, world = world})

	replaceState` vformid view states world = if (init <> Const) (replaceState vformid view states world) (states,world)

//	findFormInfo :: FormId *FormStates *NWorld -> (Bool,Maybe a,*FormStates,*NWorld) | gUpd{|*|} a & gParse{|*|} a & TC a
	findFormInfo formid formStates world
		= case (decodeInput1 formid formStates world) of

			// an update for this form is detected

			(Just (pos,updval), (True,Just currentState,formStates,world)) 		//state as received from browser 
					-> (True, Just (snd (gUpd{|*|} (UpdSearch updval pos) currentState)),formStates,world)
			(Just (pos,updval), (False,Just currentState,formStates,world)) 	// state has been updated already
					-> (False, Just currentState,formStates,world)   			// to indicate the update already has taken place, used in arrows

			// no update found, determine the current stored state

			(_, (_,Just currentState, formStates,world))	
					-> (False, Just currentState,formStates,world)

			// no update, no state stored, the current value is taken as (new) state

			(_,(_,_,formStates,world))	-> (False, Nothing,formStates,world)	
	where
//		decodeInput1 :: (FormId b) *FormStates *NWorld-> (Maybe FormUpdate, (Bool,Maybe b, *FormStates,*NWorld)) | gParse{|*|} b & TC b
		decodeInput1 formid fs world
		# (updateid,fs) = getUpdateId fs
		# (anyInput,fs) = getUpdate fs 
		| updateid == formid.id	// this state is updated
		= case getTriplet fs of
			(Just (sid,pos,UpdC s), Just "",fs) 							= (Just (pos,UpdC s)  ,findState (nformid sid) fs world)
			(Just (sid,pos,UpdC s), _,fs) 									= (Just (pos,UpdC s)  ,findState (nformid sid) fs world)
			(_,_,fs)= case getTriplet fs of
					(Just (sid,pos,UpdI i), Just ni,fs) 					= (Just (pos,UpdI ni) ,findState (nformid sid) fs world) 
					(Just (sid,pos,UpdI i), _,fs) 							= (Just (pos,UpdI i)  ,findState (nformid sid) fs world) 
					(_,_,fs) = case getTriplet fs of
							(Just (sid,pos,UpdR r), Just nr,fs) 			= (Just (pos,UpdR nr) ,findState (nformid sid) fs world) 
							(Just (sid,pos,UpdR r), _,fs) 					= (Just (pos,UpdR r) ,findState (nformid sid) fs world) 
							(_,_,fs) = case getTriplet fs of
									(Just (sid,pos,UpdB b), Just nb,fs) 	= (Just (pos,UpdB nb) ,findState (nformid sid) fs world) 
									(Just (sid,pos,UpdB b), _,fs) 			= (Just (pos,UpdB b) ,findState (nformid sid) fs world) 
									(_,_,fs) = case getTriplet fs of
										(Just (sid,pos,UpdS s),	Just ns,fs)	= (Just (pos,UpdS ns) ,findState (nformid sid) fs world) 
										(Just (sid,pos,UpdS s),	_,fs)		= (Just (pos,UpdS anyInput)  ,findState (nformid sid) fs world) 
										(upd,new,fs) 						= (Nothing, findState formid fs world)
		| otherwise = (Nothing, findState formid fs world)

		nformid sid = {formid & id = sid}

// specialize has to be used if a programmer want to specialize gForm
// it remembers the current value of the index in the expression and creates an editor to show this value
// the value might have been changed with this editor, so the value returned might differ form the value you started with !

specialize :: !(!(InIDataId a) !*HSt -> (!Form a,!*HSt)) !(InIDataId a) !*HSt -> (!Form a,!*HSt) | gUpd {|*|} a
specialize editor (init,formid) hst=:{cntr = inidx,states = formStates,world}
# nextidx = incrIndex inidx formid.ival		// this value will be repesented differently, so increment counter 
# (nv,{states,world}) 	= editor (init,nformid) {cntr = 0,states = formStates,world = world}
= (nv,{cntr=nextidx,states = states,world = world})
where
	nformid = {formid & id = formid.id +++ "_specialize_" +++ toString inidx +++ "_"}

	incrIndex :: Int v -> Int | gUpd {|*|} v
	incrIndex i v
	# (UpdSearch _ cnt,v) = gUpd {|*|} (UpdSearch (UpdI 0) -1) v
	= i + (-1 - cnt)

// gForm: automatic derives an Html Form for any Clean type

generic gForm a :: !(InIDataId a) !*HSt -> *(Form a, !*HSt)	

gForm{|Int|} (init,formid) hst 	
# (body,hst) = mkInput defsize (init,formid) (IV i) (UpdI i) hst
= ({changed=False, value=i, form=[body]},hst)
where
	i = formid.ival 
	
gForm{|Real|} (init,formid) hst 	
# (body,hst) = mkInput defsize (init,formid) (RV r) (UpdR r) hst
= ({changed=False, value=r, form=[body]},hst)
where
	r = formid.ival 

gForm{|Bool|} (init,formid) hst 	
# (body,hst) = mkInput defsize (init,formid) (BV b) (UpdB b) hst
= ({changed=False, value=b, form=[body]},hst)
where
	b = formid.ival 

gForm{|String|} (init,formid) hst 	
# (body,hst) = mkInput defsize (init,formid) (SV s) (UpdS s) hst
= ({changed=False, value=s, form=[body]},hst)
where
	s = formid.ival 

gForm{|UNIT|}  _ hst 
= ({changed=False, value=UNIT, form=[EmptyBody]},hst)

gForm{|PAIR|} gHa gHb (init,formid)  hst 
# (na,hst) = gHa (init,reuseFormId formid a) hst
# (nb,hst) = gHb (init,reuseFormId formid b) hst
= (	{changed=na.changed || nb.changed
	,value	=PAIR na.value nb.value
	,form	=[STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [na.form,nb.form]]
	},hst)
where
	(PAIR a b) = formid.ival 

gForm{|EITHER|} gHa gHb (init,formid)  hst 
= case formid.ival of
	(LEFT a)
	# (na,hst) = gHa (init,reuseFormId formid a) hst
	= ({changed=na.changed, value=LEFT na.value, form=na.form},hst)
	(RIGHT b)
	# (nb,hst) = gHb (init,reuseFormId formid b) hst
	= ({changed=nb.changed, value=RIGHT nb.value, form=nb.form},hst)

gForm{|OBJECT|} gHo (init,formid) hst
# (no,hst) = gHo (init,reuseFormId formid o) hst
= ({changed=no.changed, value=OBJECT no.value, form=no.form},hst)
where
	(OBJECT o) = formid.ival

gForm{|CONS of t|} gHc (init,formid) hst=:{cntr}
| not (isEmpty t.gcd_fields) 		 
	# (nc,hst) = gHc (init,reuseFormId formid c) (setCntr (cntr+1) hst) // don't display record constructor
	= ({changed=nc.changed, value=CONS nc.value, form=nc.form},hst)
| t.gcd_type_def.gtd_num_conses == 1 
	# (nc,hst) = gHc (init,reuseFormId formid c) (setCntr (cntr+1) hst) // don't display constructors that have no alternative
	= ({changed=nc.changed, value=CONS nc.value, form=nc.form},hst)
| t.gcd_name.[(size t.gcd_name) - 1] == '_' // don't display constructor names which end with an underscore
	# (nc,hst) = gHc (init,reuseFormId formid c) (setCntr (cntr+1) hst) 
	= ({changed=nc.changed, value=CONS nc.value, form=nc.form},hst)
# (selector,hst)= mkConsSelector formid t hst
# (nc,hst) = gHc (init,reuseFormId formid c) hst
= ({changed=nc.changed
	,value=CONS nc.value
	,form=[STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[selector,BodyTag nc.form]]]
	},hst)
where
	(CONS c) = formid.ival

	mkConsSelector formid thiscons hst=:{cntr} 
						= (mkConsSel cntr allnames myindex formid, (setCntr (cntr+1) hst))
	where
		myname   = thiscons.gcd_name
		allnames = map (\n -> n.gcd_name) thiscons.gcd_type_def.gtd_conses
		myindex = find myname allnames 0
		where
			find :: String [String] Int -> Int
			find x [y:ys] idx
			| x == y = idx
			| otherwise = find x ys (idx+1)
			find x [] idx = abort ("cannot find index " +++ x )

	mkConsSel:: Int [String] Int (FormId x) -> BodyTag
	mkConsSel cntr list nr formid
		= Select 	[ Sel_Name ("CS")
					: styles
					]
					[Option  
						[Opt_Value (encodeTriplet (formid.id,cntr,UpdC elem))
						: if (j == nr) [Opt_Selected Selected:optionstyle] optionstyle 
						]
						elem
						\\ elem <- list & j <- [0..]
					] 
		where
			styles = case formid.mode of
						Edit	-> 	[ `Sel_Std	[Std_Style width, EditBoxStyle]
									, `Sel_Events [OnChange callClean]
									]
						_		 ->	[ `Sel_Std	[Std_Style width, DisplayBoxStyle]
									,  Sel_Disabled Disabled
									]
			optionstyle	= case formid.mode of
							Edit	-> []
							_	 	-> [`Opt_Std [DisplayBoxStyle]]

			width = "width:" +++ (toString defpixel) +++ "px"

gForm{|FIELD of d |} gHx (init,formid) hst 
# (nx,hst) = gHx (init,reuseFormId formid x) hst
= ({changed=nx.changed
	,value=FIELD nx.value
	,form=[STable [Tbl_CellPadding (Pixels 1), Tbl_CellSpacing (Pixels 1)] [[fieldname,BodyTag nx.form]]]
	},hst)
where
	(FIELD x) = formid.ival
						
	fieldname =Input 	[	Inp_Type Inp_Text
						, 	Inp_Value (SV (prettyfy d.gfd_name +++ ": "))
						,	Inp_ReadOnly ReadOnly
						, 	`Inp_Std [DisplayBoxStyle]
						,	Inp_Size maxsize`
						] ""

	prettyfy name = mkString [toUpper lname : addspace lnames]
	where
		[lname:lnames] = mkList name
		addspace [] = []
		addspace [c:cs]
		| isUpper c	= [' ',toLower c:addspace cs]
		| otherwise = [c:addspace cs]
		
	maxsize` = ndefsize maxsize defsize
	maxsize = takemax defsize [toInt (toReal (size (prettyfy gfd_name)) * 0.8)  \\ {gfd_name} <- d.gfd_cons.gcd_fields]
	
	takemax i [] = i
	takemax i [j:js] 
	| i > j = takemax i js
	| otherwise = takemax j js

	ndefsize max def
	| max - def <= 0 = def
	| otherwise = ndefsize max (def + defsize)

gForm{|(->)|} garg gres (init,formid) hst 	
= ({changed=False, value=formid.ival, form=[]},hst)

// gUpd calculates a new value given the current value and a change in the value
// if necessary it invents new default value (e.g. when switching from Nil to Cons ...
// and leaves the rest untouched

:: UpdMode	= UpdSearch UpdValue Int		// search for indicated postion and update it
			| UpdCreate [ConsPos]			// create new values if necessary
			| UpdDone						// and just copy the remaining stuff

generic gUpd t :: UpdMode t -> (UpdMode,t)

gUpd{|Int|} (UpdSearch (UpdI ni) 0) 	_ = (UpdDone,ni)					// update integer value
gUpd{|Int|} (UpdSearch val cnt)     	i = (UpdSearch val (dec cnt),i)		// continue search, don't change
gUpd{|Int|} (UpdCreate l)				_ = (UpdCreate l,0)					// create default value
gUpd{|Int|} mode 			  	    	i = (mode,i)						// don't change

gUpd{|Real|} (UpdSearch (UpdR nr) 0) 	_ = (UpdDone,nr)					// update real value
gUpd{|Real|} (UpdSearch val cnt)     	r = (UpdSearch val (dec cnt),r)		// continue search, don't change
gUpd{|Real|} (UpdCreate l)			 	_ = (UpdCreate l,0.0)				// create default value
gUpd{|Real|} mode 			  	     	r = (mode,r)						// don't change

gUpd{|Bool|} (UpdSearch (UpdB nb) 0) 	_ = (UpdDone,nb)					// update boolean value
gUpd{|Bool|} (UpdSearch val cnt)     	b = (UpdSearch val (dec cnt),b)		// continue search, don't change
gUpd{|Bool|} (UpdCreate l)			 	_ = (UpdCreate l,False)				// create default value
gUpd{|Bool|} mode 			  	     	b = (mode,b)						// don't change

gUpd{|String|} (UpdSearch (UpdS ns) 0) 	_ = (UpdDone,ns)					// update string value
gUpd{|String|} (UpdSearch val cnt)     	s = (UpdSearch val (dec cnt),s)		// continue search, don't change
gUpd{|String|} (UpdCreate l)		   	_ = (UpdCreate l,"")				// create default value
gUpd{|String|} mode 			  	 	s = (mode,s)						// don't change

gUpd{|UNIT|} mode _  = (mode,UNIT)

gUpd{|PAIR|} gUpda gUpdb mode=:(UpdCreate l) _		// invent a pair
# (mode,a) = gUpda mode (abort "PAIR a evaluated")
# (mode,b) = gUpdb mode (abort "PAIR b evaluated")
= (mode,PAIR a b)
gUpd{|PAIR|} gUpda gUpdb mode (PAIR a b)			// pass mode to a and b in all other cases
# (mode,a) = gUpda mode a
# (mode,b) = gUpdb mode b
= (mode,PAIR a b)

gUpd{|EITHER|} gUpda gUpdb (UpdCreate [ConsLeft:cl]) _
# (mode,a) = gUpda (UpdCreate cl) (abort "Left a evaluated")
= (mode,LEFT a)
gUpd{|EITHER|} gUpda gUpdb (UpdCreate [ConsRight:cl]) _
# (mode,b) = gUpdb (UpdCreate cl) (abort "Right b evaluated")
= (mode,RIGHT b)
gUpd{|EITHER|} gUpda gUpdb (UpdCreate []) _ 
# (mode,b) = gUpdb (UpdCreate []) (abort "emty either evaluated")
= (mode,RIGHT b)
gUpd{|EITHER|} gUpda gUpdb mode (LEFT a)
# (mode,a) = gUpda mode a
= (mode,LEFT a)
gUpd{|EITHER|} gUpda gUpdb mode (RIGHT b)
# (mode,b) = gUpdb mode b
= (mode,RIGHT b)

gUpd{|OBJECT|} gUpdo (UpdCreate l) _ 		// invent new type
# (mode,o) = gUpdo (UpdCreate l) (abort "Object evaluated")
= (mode,OBJECT o)
gUpd{|OBJECT of typedes|} gUpdo (UpdSearch (UpdC cname) 0) (OBJECT o) // constructor changed of this type
# (mode,o) = gUpdo (UpdCreate path) o
= (UpdDone,OBJECT o)
where
	path = getConsPath (hd [cons \\ cons <- typedes.gtd_conses | cons.gcd_name == cname])
gUpd{|OBJECT|} gUpdo (UpdSearch val cnt) (OBJECT o) // search further
# (mode,o) = gUpdo (UpdSearch val (dec cnt)) o
= (mode,OBJECT o)
gUpd{|OBJECT|} gUpdo mode (OBJECT o)		// other cases
# (mode,o) = gUpdo mode o
= (mode,OBJECT o)

gUpd{|CONS|} gUpdo (UpdCreate l) _ 			// invent new constructor ??
# (mode,c) = gUpdo (UpdCreate l) (abort "Cons evaluated")
= (mode,CONS c)
gUpd{|CONS|} gUpdo mode (CONS c) 			// other cases
# (mode,c) = gUpdo mode c
= (mode,CONS c)

gUpd{|FIELD|} gUpdx (UpdCreate l) _  // invent new type 
# (mode,x) = gUpdx (UpdCreate l) (abort "value of field evaluated")
= (mode,FIELD x)
gUpd{|FIELD|} gUpdx mode (FIELD x)  // other casesr
# (mode,x) = gUpdx mode x
= (mode,FIELD x)

gUpd{|(->)|} gUpdArg gUpdRes mode f = (mode,f)	

// small utility functions

mkInput :: !Int !(InIDataId d) Value UpdValue *HSt -> (BodyTag,*HSt) 
mkInput size (init,formid=:{mode = Edit}) val updval hst=:{cntr} 
	= ( Input 	[	Inp_Type Inp_Text
				, 	Inp_Value val
				,	Inp_Name (encodeTriplet (formid.id,cntr,updval))
				,	Inp_Size size
				, 	`Inp_Std [EditBoxStyle, Std_Title (showType val)]
				,	`Inp_Events	[OnChange callClean]
				] ""
		,setCntr (cntr+1) hst)
where
	showType (SV str) 	= "::String"
	showType (NQV str)	= "::String"
	showType (IV i)		= "::Int"
	showType (RV r) 	= "::Real"
	showType (BV b) 	= "::Bool"
mkInput size (init,{mode = Display}) val _ hst=:{cntr} 
	= ( Input 	[	Inp_Type Inp_Text
				, 	Inp_Value val
				,	Inp_ReadOnly ReadOnly
				, 	`Inp_Std [DisplayBoxStyle]
				,	Inp_Size size
				] ""
		,setCntr (cntr+1) hst)
mkInput size (init,_) val _ hst=:{cntr} 
	= ( EmptyBody,setCntr (cntr+1) hst)
		
toHtml :: a -> BodyTag | gForm {|*|} a
toHtml a 
# (na,_) = gForm{|*|} (Set,{id = "__toHtml", lifespan = Page, mode = Display, storage = PlainString, ival = a}) {cntr = 0, states = emptyFormStates, world = undef}
= BodyTag na.form

toHtmlForm :: (*HSt -> *(Form a,*HSt)) -> [BodyTag]
												| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
toHtmlForm anyform 
# (na,hst) = anyform {cntr = 0, states = emptyFormStates, world = undef}
=  na.form

toBody :: (Form a) -> BodyTag
toBody form = BodyTag form.form

createDefault :: a | gUpd{|*|} a 
createDefault = fromJust (snd (gUpd {|*|} (UpdSearch (UpdC "Just") 0) Nothing))
derive gUpd Maybe

setCntr :: InputId *HSt -> *HSt
setCntr i hst = {hst & cntr = i}

incrHSt :: Int *HSt -> *HSt
incrHSt i hst = {hst & cntr = hst.cntr + i} // BUG ??????

CntrHSt :: *HSt -> (Int,*HSt)
CntrHSt hst=:{cntr} = (cntr,hst)

getChangedId	:: !*HSt -> (String,!*HSt)	// id of form that has been changed by user
getChangedId hst=:{states}
# (id,states) = getUpdateId states
= (id,{hst & states = states })

// Enabling file IO on HSt

instance FileSystem HSt
where
	fopen :: !{#Char} !Int !*HSt -> (!Bool,!*File,!*HSt)
	fopen string int hst=:{world}
		# (bool,file,world) = fopen string int world
		= (bool,file,{hst & world = world})

	fclose :: !*File !*HSt -> (!Bool,!*HSt)
	fclose file hst=:{world}
		# (bool,world) = fclose file world
		= (bool,{hst & world = world})

	stdio :: !*HSt -> (!*File,!*HSt)
	stdio hst=:{world}
		# (file,world) = stdio world
		= (file,{hst & world = world})

	sfopen :: !{#Char} !Int !*HSt -> (!Bool,!File,!*HSt)
	sfopen string int hst=:{world}
		# (bool,file,world) = sfopen string int world
		= (bool,file,{hst & world = world})

// General access to the World environment on HSt:

appWorldHSt :: !.(*World -> *World) !*HSt -> *HSt
appWorldHSt f hst=:{world}
	= {hst & world=appWorldNWorld f world}

accWorldHSt :: !.(*World -> *(.a,*World)) !*HSt -> (.a,!*HSt)
accWorldHSt f hst=:{world}
	# (a,world)	= accWorldNWorld f world
	= (a,{hst & world=world})

// test interface

runUserApplication :: .(*HSt -> *(.a,*HSt)) *FormStates *NWorld -> *(.a,*FormStates,*NWorld)
runUserApplication userpage states nworld
# (html,{states,world}) 
	= userpage {cntr = 0, states = states, world = nworld}
= (html,states,world)

