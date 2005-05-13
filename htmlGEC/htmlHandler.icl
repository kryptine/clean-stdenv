implementation module htmlHandler

import StdEnv, ArgEnv, StdMaybe
import htmlDataDef, htmlTrivial
import StdGeneric
import htmlEncodeDecode
import GenParse, GenPrint

derive gPrint (,), (,,), (,,,), UpdValue
derive gParse (,), (,,), (,,,), UpdValue
derive gHpr   (,), (,,), (,,,)
derive gUpd		   (,,), (,,,)
 
:: HSt 			:== (InputId,FormStates)	// all form states are collected here ... 	
:: FormId	 	:== String					// unique id identifying the form
:: FormValue 	:== String					// current Clean value to remember encoded in a String
:: InputId	 	:== Int						// unique id for every constructor and basic value appearing in the state

:: FormUpdate	:== (InputId,UpdValue)		// info obtained when form is updated

:: UpdValue 	= UpdI Int					// new integer value
				| UpdR Real					// new real value
				| UpdB Bool					// new boolean value
				| UpdC String				// choose indicated constructor 
				| UpdS String				// new piece of text

derive bimap Form, []

toHtml :: a -> BodyTag | gForm {|*|} a
toHtml a 
# (na,_) = gForm{|*|} "__toHtml" Display a mkHSt
= BodyTag na.body

toBody :: (Form a) -> BodyTag
toBody form = BodyTag form.body

mkHSt ::  *HSt
mkHSt = (0,initFormStates)

ifEdit :: Mode a a -> a
ifEdit Edit 	then else = then
ifEdit Display  then else = else 

// top level function given to end user
// it collects the html page to display, and returns the contents of all Clean GEC's / Forms created

doHtml :: (*HSt -> (Html,!*HSt)) *World -> *World
doHtml pagehandler world 
= print_to_stdout (Html header (Body [extra_att:attr] [/*debugstate, */addScript formStates:bodytags])) world
where
	(Html header (Body attr bodytags),(_,formStates)) = pagehandler mkHSt
	extra_att = Batt_background "back35.jpg "

/* for debugging
	debugstate = toHtml formStates

derive gForm FormState, Tree_
*/

// experimental function:
/*
mkEditForm2:: !FormId !Mode d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkEditForm2 formid  mode data (inidx,formStates)
=	case findInLocalStore formid formStates 0 formStates of
		(Just id, Just state,formStates)	-> mkSetForm formid mode state (inidx,formStates)
		(_,_,				 formStates)	-> mkSetForm formid mode data (inidx,formStates)
where
	findInLocalStore :: String [(FormId,FormValue)] Int [(FormId,FormValue)] -> (Maybe Int, Maybe d,[(FormId,FormValue)]) | gParse{|*|} d
	findInLocalStore s [] 			 _ formStates	 = (Nothing,Nothing,formStates)
	findInLocalStore s [(id,st):lst] n formStates
	| formid == id = (Just n, parseString st, removeAt n formStates) 
	| otherwise 	 = findInLocalStore s lst (n+1) formStates
*/
// simple editor for either editing or showing a simple value

mkEditForm:: !FormId !Mode d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkEditForm formid  Edit data hst
= mkViewForm formid Edit 
	{toForm = toFormid , updForm = \_ v -> v , fromForm = \_ v -> v , resetForm = Nothing} data hst
mkEditForm formid  mode data hst
= mkSetForm formid mode data hst

mkSetForm:: !FormId !Mode d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkSetForm formid  mode data hst
= mkViewForm formid mode 
	{toForm = toFormid , updForm = \_ _ -> data , fromForm = \_ v -> v , resetForm = Nothing} data hst

// editor with feedback to its self

mkSelfForm  :: !FormId 	!(d -> d) d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkSelfForm formid cbf initdata hst
= mkViewForm formid Edit 
	{toForm = toFormid , updForm = update , fromForm = \_ v -> v , resetForm = Nothing} initdata hst
where
	update True newval = cbf newval
	update _ val = val
	
// editor which applies the function to its argument

mkApplyForm :: !FormId !(d -> d) d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkApplyForm formid cbf data hst
= mkViewForm formid Display 
	{toForm = toFormid , updForm = \_ _ = cbf data , fromForm = \_ v -> v, resetForm = Nothing} data hst

// editor which applies the function to its argument

mkStoreForm :: !FormId 	!(d -> d) d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkStoreForm formid cbf data hst
= mkViewForm formid Display 
	{toForm = toFormid , updForm = \_ v = cbf v , fromForm = \_ v -> v, resetForm = Nothing} data hst

mkApplyEditForm	:: !FormId !d d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkApplyEditForm formid inputval initval hst
= mkViewForm formid Edit 
	{toForm =  toFormid , updForm = update , fromForm = \_ v -> v, resetForm = Nothing} initval hst
where
	update True  newval = newval
	update False val    = inputval

mkBimapEditor :: !FormId !Mode !(Bimap d v) d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} v
mkBimapEditor formid mode {map_to,map_from} d hst
= mkViewForm formid mode 	{ toForm = \d _ -> map_to d
						, updForm = \b v -> map_to (map_from v)
						, fromForm = \b v -> map_from v
						, resetForm = Nothing
						} d hst 

toFormid d Nothing = d
toFormid d (Just v) = v

// swiss army nife editor that makes coffee too ...

//# (nnextview,(nr,[(uniqueid,mystore):formStates])) = gForm{|*|} formid mode nextview (0,[(formid,nextview):formStates])

mkViewForm :: !FormId !Mode !(HBimap d v) d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} v
mkViewForm formid mode {toForm, updForm, fromForm, resetForm} initdata (inidx,formStates) 
# (isupdated,prevview,formStates) = findFormInfo formid formStates // determine current value in the state store
# newview 		= toForm   initdata prevview		// map value to view domain, given previous view value
# updateview	= updForm  isupdated newview		// apply update function telling user if an update has taken place
# newval		= fromForm isupdated updateview		// convert back to data domain	 
# nextview		= case resetForm of					// optionally reset the view
					Nothing 	-> updateview		 
					Just reset 	-> reset updateview
# formStates	= replaceState formid nextview formStates	// store new view into the store of states
# (viewform,(nr,formStates))								// and make a form for it
				= gForm{|*|} formid mode nextview (0,formStates)
# formStates	= replaceState formid (viewform.value) formStates	// store new value into the store of states
= (	{changed	= viewform.changed || isupdated
	, value		= newval //fromForm isupdated viewform.value
	, body		= viewform.body
	},(0,formStates))
//	},(0,replaceState formid (nnextview.value) formStates))

//	, value		= fromForm2 isupdated newval nnextview.value
//	},(0,replaceNState formid (encodeInfo2 nextview nnextview.value) formStates))
//	},(0,[(formid,encodeInfo2 nextview nnextview.value):formStates]))
//	},(0,replaceState formid (nnextview.value) formStates))
where

	fromForm2 isupdated nnextview nextview 
		= case resetForm of
			Nothing 	-> nnextview 
			(Just reset)-> nextview

	findFormInfo :: FormId FormStates -> (Bool,Maybe a,FormStates) | gUpd{|*|} a & gParse{|*|} a
	findFormInfo formid formStates
		= case (decodeInput1 formid formStates) of

			// an update for this form is detected

			(Just (pos,updval), (True,Just currentState,formStates)) //state as received from browser 
					-> (True, Just (snd (gUpd{|*|} (UpdSearch updval pos) currentState)),formStates)
			(Just (pos,updval), (False,Just currentState,formStates)) // state has been updated already
					-> (True, Just currentState,formStates)

			// no update found, determine the current stored state

			(_, (_,Just currentState, formStates))	
					-> (False, Just currentState,formStates)

			// no update, no state stored, the current value is taken as (new) state

			(_,(_,_,formStates))	-> (False, Nothing,formStates)	
	where
		decodeInput1 :: FormId FormStates -> (Maybe FormUpdate, (Bool,Maybe a, FormStates)) | gParse{|*|} a
		decodeInput1 formid formStates
		| CheckUpdateId == formid	// this state is updated
		= case CheckUpdate of
			(Just (sid,pos,UpdC s), Just "") 						= (Just (pos,UpdC s)  ,findState sid formStates)
			(Just (sid,pos,UpdC s), _) 								= (Just (pos,UpdC s)  ,findState sid formStates)
			else = case CheckUpdate of
					(Just (sid,pos,UpdI i), Just ni) 				= (Just (pos,UpdI ni) ,findState sid formStates) 
					else = case CheckUpdate of
							(Just (sid,pos,UpdR r), Just nr) 		= (Just (pos,UpdR nr) ,findState sid formStates) 
							else = case CheckUpdate of
								(Just (sid,pos,UpdS s),	Just ns)	= (Just (pos,UpdS ns) ,findState sid formStates) 
								(Just (sid,pos,UpdS s),	_)			= (Just (pos,UpdS AnyInput)  ,findState sid formStates) 
								(upd,new) 							= (Nothing, findState formid formStates)
		| otherwise = (Nothing, findState formid formStates)

// automatic tranformation of any Clean type to html body
// the formStates on the head of the hst is the formStates for the form we create here

generic gForm a :: !FormId !Mode a !*HSt -> *(Form a, !*HSt)	

gForm{|Int|} formid mode i hst 	
# (body,hst) = mkInput defsize formid mode (IV i) (UpdI i) hst
= ({changed=False, value=i, body=[body]},hst)

gForm{|Real|} formid mode r hst 	
# (body,hst) = mkInput defsize formid mode (RV r) (UpdR r) hst
= ({changed=False, value=r, body=[body]},hst)

gForm{|Bool|} formid mode b hst 	
# (body,hst) = mkInput defsize formid mode (BV b) (UpdB b) hst
= ({changed=False, value=b, body=[body]},hst)

gForm{|String|} formid mode s hst 	
# (body,hst) = mkInput defsize formid mode (SV s) (UpdS s) hst
= ({changed=False, value=s, body=[body]},hst)

mkInput :: !Int !FormId !Mode Value UpdValue *HSt -> (BodyTag,*HSt) 
mkInput size formid Edit val updval (inidx,hst) 
	= ( Input 	[	Inp_Type Inp_Text
				, 	Inp_Value val
				,	Inp_Name (encodeInfo (formid,inidx,updval))
				,	Inp_Size size
				,	`Inp_Events	[OnChange callClean]
				] ""
		,(inidx+1,hst))
mkInput size formid Display val _ (inidx,hst) 
	= ( Input 	[	Inp_Type Inp_Text
				, 	Inp_Value val
				,	Inp_ReadOnly ReadOnly
				, 	`Inp_Std [Std_Style color]
				,	Inp_Size size
				] ""
		,(inidx+1,hst))
where
	color = "background-color:" +++ backcolor

gForm{|UNIT|}  _ _ _ hst 
= ({changed=False, value=UNIT, body=[EmptyBody]},hst)

gForm{|PAIR|} gHa gHb formid mode (PAIR a b) hst 
# (na,hst) = gHa formid mode a hst
# (nb,hst) = gHb formid mode b hst
= (	{changed=False
	,value	=PAIR na.value nb.value
	,body	=[STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [na.body,nb.body]]
	},hst)

gForm{|EITHER|} gHa gHb formid mode (LEFT a)   hst 
# (na,hst) = gHa formid mode a hst
= ({changed=False, value=LEFT na.value, body=na.body},hst)
gForm{|EITHER|} gHa gHb formid mode (RIGHT b)  hst
# (nb,hst) = gHb formid mode b hst
= ({changed=False, value=RIGHT nb.value, body=nb.body},hst)
gForm{|OBJECT|} gHo formid mode (OBJECT o) hst
# (no,hst) = gHo formid mode o hst
= ({changed=False, value=OBJECT no.value, body=no.body},hst)

gForm{|CONS of t|} gHc formid mode (CONS c) hst=:(inidx,formStates)
| not (isEmpty t.gcd_fields) 		 
# (nc,hst) = gHc formid mode c (inidx+1,formStates) // don't display record constructor
= ({changed=False, value=CONS nc.value, body=nc.body},hst)
| t.gcd_type_def.gtd_num_conses == 1 
# (nc,hst) = gHc formid mode c (inidx+1,formStates) // don't display constructors that have no alternative
= ({changed=False, value=CONS nc.value, body=nc.body},hst)
| t.gcd_name.[(size t.gcd_name) - 1] == '_' // don't display constructor names which end with an underscore
# (nc,hst) = gHc formid mode c (inidx+1,formStates) 
= ({changed=False, value=CONS nc.value, body=nc.body},hst)
# (selector,hst)= mkConsSelector formid t hst
# (nc,hst) = gHc formid mode c hst
= ({changed=False
	,value=CONS nc.value
	,body=[STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[selector,BodyTag nc.body]]]
	},hst)


where
	mkConsSelector formid thiscons (inidx,formStates) 
						= (mkConsSel inidx allnames myindex formid, (inidx+1,formStates))
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

	mkConsSel:: Int [String] Int String -> BodyTag
	mkConsSel inidx list nr formid
		= Select 	[ Sel_Name ("CS")
					, setstyle
					, changeable
					]
					[Option  
						[Opt_Value (encodeInfo (formid,inidx,UpdC elem))
						: if (j == nr) [Opt_Selected Selected:optionstyle] optionstyle 
						]
						elem
						\\ elem <- list & j <- [0..]
					] 
		where
			setstyle	= case mode of
							Edit 		-> `Sel_Std	[Std_Style width]
							Display 	-> `Sel_Std	[Std_Style (width +++ ";" +++ color)]
			changeable = case mode of
							Edit 		-> `Sel_Events [OnChange callClean]
							Display 	-> Sel_Disabled Disabled
			optionstyle	= case mode of
							Edit 		-> []
							Display 	-> [`Opt_Std [Std_Style color]]

			width = "width:" +++ (toString defpixel) +++ "px"
			color = "background-color:" +++ backcolor

gForm{|FIELD of d |} gHx formid mode (FIELD x) hst 
# (nx,hst) = gHx formid mode x hst
= ({changed=False
	,value=FIELD nx.value
	,body=[STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[fieldname,BodyTag nx.body]]]
	},hst)
where
	fieldname =Input 	[	Inp_Type Inp_Text
						, 	Inp_Value (SV (prettyfy d.gfd_name +++ ": "))
						,	Inp_ReadOnly ReadOnly
						, 	`Inp_Std [Std_Style color]
						,	Inp_Size maxsize
						] ""

	color = "background-color:" +++ backcolor

	prettyfy name = mkString [toUpper lname : addspace lnames]
	where
		[lname:lnames] = mkList name
		addspace [] = []
		addspace [c:cs]
		| isUpper c	= [' ',toLower c:addspace cs]
		| otherwise = [c:addspace cs]
		
	maxsize = takemax defsize [size (prettyfy gfd_name)  \\ {gfd_name} <- d.gfd_cons.gcd_fields]
	
	takemax i [] = i
	takemax i [j:js] 
	| i > j = takemax i js
	| otherwise = takemax j js

// generic function to update any type, great miracle function
// will look for an input object with a certain id, updates it
// if required it invents new default value (e.g. when switching from Nil to Cons ...
// and leaves the rest untouched
// gUpd can update any type with indicated value 

:: UpdMode	= UpdSearch UpdValue Int		// search for indicated postion and update it
			| UpdCreate [ConsPos]			// create new values if necessary
			| UpdDone						// and just copy the remaining stuf

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

gUpd{|OBJECT|} gUpdo (UpdCreate l) _ 		// invent new type ???
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

gUpd{|FIELD|} gUpdx mode (FIELD x)  // to be done !!!
# (mode,x) = gUpdx mode x
= (mode,FIELD x)


derive gUpd (,)


// Clean types with a special representation ...

// special representations for lay-out

// tuples are placed next to each other, pairs below each other ...

gForm{|(,)|} gHa gHb formid mode  (a,b) (inidx,formStates)
# (na,hst) = gHa formid mode a (inidx+1,formStates)   // one more for the now invisable (,) constructor 
# (nb,hst) = gHb formid mode b hst
= (	{changed= False
	,value	= (na.value,nb.value)
	,body	= [STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[BodyTag na.body, BodyTag nb.body]]]
	},hst)

gForm{|(,,)|} gHa gHb gHc formid mode (a,b,c) (inidx,formStates)
# (na,hst) = gHa formid mode a (inidx+1,formStates)   // one more for the now invisable (,,) constructor 
# (nb,hst) = gHb formid mode b hst
# (nc,hst) = gHc formid mode c hst
= (	{changed= False
	,value	= (na.value,nb.value,nc.value)
	,body	= [STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[BodyTag na.body,BodyTag nb.body,BodyTag nc.body]]]
	},hst)

gForm{|(,,,)|} gHa gHb gHc gHd formid mode (a,b,c,d) (inidx,formStates)
# (na,hst) = gHa formid mode a (inidx+1,formStates)   // one more for the now invisable (,,) constructor 
# (nb,hst) = gHb formid mode b hst
# (nc,hst) = gHc formid mode c hst
# (nd,hst) = gHd formid mode d hst
= (	{changed= False
	,value	= (na.value,nb.value,nc.value,nd.value)
	,body	= [STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] 
				[[BodyTag na.body,BodyTag nb.body,BodyTag nc.body, BodyTag nd.body]]]
	},hst)

// <-> works exactly the same as (,) and places its arguments next to each other, for compatibility with GEC's

gForm{|(<->)|} gHa gHb formid mode  (a <-> b) (inidx,formStates)
# (na,hst) = gHa formid mode a (inidx+1,formStates)   // one more for the now invisable <-> constructor 
# (nb,hst) = gHb formid mode b hst
= (	{changed= False 
	,value	= na.value <-> nb.value
	,body	= [STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[BodyTag na.body, BodyTag nb.body]]]
	},hst)
derive gUpd   <->
derive gParse <->
derive gPrint <->

// <|> works exactly the same as PAIR and places its arguments below each other, for compatibility with GEC's

gForm{|(<|>)|} gHa gHb formid mode (a <|> b) (inidx,formStates) 
# (na,hst) = gHa formid mode a (inidx+1,formStates) // one more for the now invisable <|> constructor
# (nb,hst) = gHb formid mode b hst
= (	{changed= False 
	,value	= na.value <|> nb.value
	,body	= [STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [na.body, nb.body]]
	},hst)
derive gUpd   <|>
derive gParse <|>
derive gPrint <|>

// to switch between modes within a type ...

gForm{|DisplayMode|} gHa formid mode (HideMode a) (inidx,formStates) 	
# (na,hst) = gHa formid Display a (inidx+1,formStates)
= (	{changed= False 
	,value	= HideMode na.value
	,body	= [EmptyBody]
	},(inidx+1,formStates))
gForm{|DisplayMode|} gHa formid mode (DisplayMode a) (inidx,formStates)  
# (na,hst) = gHa formid Display a (inidx+1,formStates)
= (	{changed= False
	,value	= DisplayMode na.value
	,body	= na.body
	},(inidx+1,formStates))
gForm{|DisplayMode|} gHa formid mode (EditMode a) (inidx,formStates)  
# (na,hst) = gHa formid Edit a (inidx+1,formStates)
= (	{changed= False
	,value	= EditMode na.value
	,body	= na.body
	},(inidx+1,formStates))
gForm{|DisplayMode|} gHa formid mode EmptyMode (inidx,formStates)
= (	{changed= False
	,value	= EmptyMode
	,body	= [EmptyBody]
	},(inidx+1,formStates))

derive gUpd DisplayMode
derive gParse DisplayMode
derive gPrint DisplayMode

// Buttons to press

gForm{|Button|} formid mode v=:(LButton size bname) (inidx,formStates) 
= (	{changed= False
	,value	= v
	,body	= [Input (ifEdit mode [] [Inp_Disabled Disabled] ++
				[ Inp_Type Inp_Button
				, Inp_Value (SV bname)
				, Inp_Name (encodeInfo (formid,inidx,UpdS bname))
				, `Inp_Std [Std_Style ("width:" +++ toString size)]
				, `Inp_Events [OnClick callClean]
				]) ""]
	},(inidx+1,formStates))
gForm{|Button|} formid mode v=:(PButton (height,width) ref) (inidx,formStates) 
= (	{changed= False
	,value	= v
	,body	= [Input	(ifEdit mode [] [Inp_Disabled Disabled] ++
				[ Inp_Type Inp_Image
				, Inp_Value (SV ref)
				, Inp_Src ref
				, Inp_Name (encodeInfo (formid,inidx,UpdS ref))
				, `Inp_Std [Std_Style ("width:" +++ toString width +++ " height:" +++ toString height)]
				, `Inp_Events [OnClick callClean]
				]) ""]
	},(inidx+1,formStates))
gForm{|Button|} formid mode Pressed hst = gForm {|*|} formid mode (LButton defpixel "??") hst // end user should reset button

gUpd{|Button|} (UpdSearch (UpdS name) 0) 	_ = (UpdDone,Pressed)					// update integer value
gUpd{|Button|} (UpdSearch val cnt)      	b = (UpdSearch val (cnt - 1),b)			// continue search, don't change
gUpd{|Button|} (UpdCreate l)				_ = (UpdCreate l,(LButton defsize "Press"))					// create default value
gUpd{|Button|} mode 			  	    	b = (mode,b)							// don't change
derive gParse Button
derive gPrint Button

gForm{|CheckBox|} formid mode v=:(CBChecked name) (inidx,formStates) 
= (	{changed= False
	,value	= v
	,body	= [Input (ifEdit mode [] [Inp_Disabled Disabled] ++
				[ Inp_Type Inp_Checkbox
				, Inp_Value (SV name)
				, Inp_Name (encodeInfo (formid,inidx,UpdS name))
				, Inp_Checked Checked
				, `Inp_Events [OnClick callClean]
				]) ""]
	},(inidx+1,formStates))


gForm{|CheckBox|} formid mode v=:(CBNotChecked name) (inidx,formStates) 
= (	{changed= False
	,value	= v
	,body	= [Input (ifEdit mode [] [Inp_Disabled Disabled] ++
				[ Inp_Type Inp_Checkbox
				, Inp_Value (SV name)
				, Inp_Name (encodeInfo (formid,inidx,UpdS name))
				, `Inp_Events [OnClick callClean]
				]) ""]
	},(inidx+1,formStates))

derive gUpd CheckBox
derive gParse CheckBox
derive gPrint CheckBox

gForm{|RadioButton|} formid mode v=:(RBChecked name) (inidx,formStates) 
= (	{changed= False
	,value	= v
	,body	= [Input (ifEdit mode [] [Inp_Disabled Disabled] ++
				[ Inp_Type Inp_Radio
				, Inp_Value (SV name)
				, Inp_Name (encodeInfo (formid,inidx,UpdS name))
				, Inp_Checked Checked
				, `Inp_Events [OnClick callClean]
				]) ""]
	},(inidx+1,formStates))
gForm{|RadioButton|} formid mode v=:(RBNotChecked name) (inidx,formStates) 
= (	{changed= False
	,value	= v
	,body	= [Input (ifEdit mode [] [Inp_Disabled Disabled] ++
				[ Inp_Type Inp_Radio
				, Inp_Value (SV name)
				, Inp_Name (encodeInfo (formid,inidx,UpdS name))
				, `Inp_Events [OnClick callClean]
				]) ""]
	},(inidx+1,formStates))

derive gUpd	  RadioButton
derive gParse RadioButton
derive gPrint RadioButton

gForm{|PullDownMenu|} formid mode v=:(PullDown (size,width) (menuindex,itemlist)) (inidx,formStates) 
= (	{changed= False
	,value	= v
	,body	= [Select 	(ifEdit mode [] [Sel_Disabled Disabled] ++
					[ Sel_Name ("CS")
					, Sel_Size size
					, `Sel_Std [Std_Style ("width:" +++ (toString width) +++ "px")]
					, `Sel_Events [OnChange callClean]
					])
					[ Option  
						[ Opt_Value (encodeInfo (formid,inidx,UpdC (itemlist!!j)))
						: if (j == menuindex) [Opt_Selected Selected] [] 
						]
						elem
						\\ elem <- itemlist & j <- [0..]
					 ]] 	
	},(inidx + 1,formStates))

gUpd{|PullDownMenu|} (UpdSearch (UpdC cname) 0) (PullDown (size,width) (menuindex,itemlist)) 
			= (UpdDone,PullDown (size,width) (nmenuindex 0 cname itemlist,itemlist))					// update integer value
where
	nmenuindex cnt name [itemname:items] 
	| name == itemname = cnt
	| otherwise		   = nmenuindex (cnt+1) name items
	nmenuindex _ _ [] = -1
	
gUpd{|PullDownMenu|} (UpdSearch val cnt) v = (UpdSearch val (cnt - 1),v)			// continue search, don't change
gUpd{|PullDownMenu|} (UpdCreate l)		_ = (UpdCreate l,PullDown (1,defpixel) (0,["error"]))					// create default value
gUpd{|PullDownMenu|} mode 			  	v = (mode,v)							// don't change
derive gParse PullDownMenu
derive gPrint PullDownMenu


:: TextInput	= TI Int Int						// Input box of size Size for Integers
				| TR Int Real						// Input box of size Size for Reals
				| TS Int String						// Input box of size Size for Strings

gForm{|TextInput|} formid mode (TI size i) hst=:(inidx,formStates) 	
# (body,hst) = mkInput size formid mode (IV i) (UpdI i) hst
= ({changed=False, value=TI size i, body=[body]},(inidx+3,formStates))
gForm{|TextInput|} formid mode (TR size r) hst=:(inidx,formStates)  	
# (body,hst) = mkInput size formid mode (RV r) (UpdR r) hst
= ({changed=False, value=TR size r, body=[body]},(inidx+3,formStates))
gForm{|TextInput|} formid mode (TS size s) hst=:(inidx,formStates) 	
# (body,hst) = mkInput size formid mode (SV s) (UpdS s) hst 
= ({changed=False, value=TS size s, body=[body]},(inidx+3,formStates))

gUpd{|TextInput|} (UpdSearch (UpdI ni) 0) 	(TI size i)  = (UpdDone,TI size ni)		// update integer value
gUpd{|TextInput|} (UpdSearch (UpdR nr) 0) 	(TR size r)  = (UpdDone,TR size nr)		// update integer value
gUpd{|TextInput|} (UpdSearch (UpdS ns) 0) 	(TS size s)  = (UpdDone,TS size ns)		// update integer value
gUpd{|TextInput|} (UpdSearch val cnt)     	i = (UpdSearch val (cnt - 3),i)			// continue search, don't change
gUpd{|TextInput|} (UpdCreate l)				_ = (UpdCreate l,TI defsize 0)			// create default value
gUpd{|TextInput|} mode 			  	    	i = (mode,i)							// don't change

derive gParse TextInput
derive gPrint TextInput

instance toBool RadioButton
where	toBool (RBChecked _)= True
		toBool _ 		 = False

instance toBool CheckBox
where	toBool (CBChecked _)= True
		toBool _ 		 = False

instance toBool Button
where	toBool Pressed = True
		toBool _ 		 = False

// specialization

specialize :: (FormId Mode a *HSt -> (Form a,*HSt)) FormId Mode a *HSt -> (Form a,*HSt) | gUpd {|*|} a
specialize editor name mode v hst=:(inidx,formStates)
# nextidx = incrIndex inidx v
# (nv,(_,lsts)) = editor codedname mode v (0,formStates)
= (	{changed= nv.changed
	,value	= nv.value
	,body	= nv.body	
	},(nextidx,formStates))
where
	codedname = name +++ "_" +++ toString inidx

incrIndex :: Int v -> Int | gUpd {|*|} v
incrIndex i v
# (UpdSearch _ cnt,v) = gUpd {|*|} (UpdSearch (UpdI 0) -1) v
= i + (-1 - cnt)



