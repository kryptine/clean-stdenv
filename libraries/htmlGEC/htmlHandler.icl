implementation module htmlHandler

import StdEnv, ArgEnv, StdMaybe
import htmlDataDef, htmlTrivial
import StdGeneric
import htmlEncodeDecode
import GenParse, GenPrint

derive gPrint (,), (,,), UpdValue
derive gParse (,), (,,), UpdValue
derive gHpr   (,), (,,)
derive gUpd		   (,,)
 
:: HSt 			:== (InputId,[FormState])	// all form states are collected here ... 	
:: FormState 	:== (FormId,FormValue)		// state of a form to remember
:: FormId	 	:== String					// unique id identifying the form
:: FormValue 	:== String					// current Clean value to remember encoded in a String
:: InputId	 	:== Int						// unique id for every constructor and basic value appearing in the state

:: FormUpdate	:== (InputId,UpdValue)		// info obtained when form is updated

:: UpdValue 	= UpdI Int					// new integer value
				| UpdR Real					// new real value
				| UpdB Bool					// new boolean value
				| UpdC String				// choose indicated constructor 
				| UpdS String				// new piece of text


toHtml :: a -> BodyTag | gForm {|*|} a
toHtml a 
# ((_,body),_) = (gForm{|*|} "__toHtml" Display a mkHSt)
= body

mkHSt ::  *HSt
mkHSt = (0,[])

ifMode :: Mode a a -> a
ifMode Edit 	then else = then
ifMode Display  then else = else 


// top level function given to end user
// it collects the html page to display, and returns the contents of all Clean GEC's / Forms created

doHtml :: (*HSt -> (Html,!*HSt)) *World -> *World
doHtml pagehandler world 
= print_to_stdout (Html header (Body [extra_att:attr] [addScript lhst:bodytags])) world
where
	(Html header (Body attr bodytags),(_,lhst)) = pagehandler mkHSt
	extra_att = Batt_background "back35.jpg "

// experimental function:

mkEditForm2:: !FormId !Mode d !*HSt -> ((d,BodyTag),!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkEditForm2 formid  mode data (inidx,lhst)
=	case findInLocalStore formid lhst 0 lhst of
		(Just id, Just state,lhst)	-> mkSetForm formid mode state (inidx,lhst)
		(_,_,				 lhst)	-> mkSetForm formid mode data (inidx,lhst)
where
	findInLocalStore :: String [(FormId,FormValue)] Int [(FormId,FormValue)] -> (Maybe Int, Maybe d,[(FormId,FormValue)]) | gParse{|*|} d
	findInLocalStore s [] 			 _ lhst	 = (Nothing,Nothing,lhst)
	findInLocalStore s [(id,st):lst] n lhst
	| formid == id = (Just n, parseString st, removeAt n lhst) 
	| otherwise 	 = findInLocalStore s lst (n+1) lhst

// simple editor for either editing or showing a simple value

mkEditForm:: !FormId !Mode d !*HSt -> ((d,BodyTag),!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkEditForm formid  Edit data hst
= mkViewForm formid Edit 
	{toForm = toFormid , updForm = \_ v -> v , fromForm = \_ v -> v , resetForm = Nothing} data hst
mkEditForm formid  mode data hst
= mkSetForm formid mode data hst

mkSetForm:: !FormId !Mode d !*HSt -> ((d,BodyTag),!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkSetForm formid  mode data hst
= mkViewForm formid mode 
	{toForm = toFormid , updForm = \_ _ -> data , fromForm = \_ v -> v , resetForm = Nothing} data hst

// editor with feedback to its self

mkSelfForm  :: !FormId 	!(d -> d) d !*HSt -> ((d,BodyTag),!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkSelfForm formid cbf initdata hst
= mkViewForm formid Edit 
	{toForm = toFormid , updForm = update , fromForm = \_ v -> v , resetForm = Nothing} initdata hst
where
	update True newval = cbf newval
	update _ val = val
	
// editor which applies the function to its argument

mkApplyForm :: !FormId !(d -> d) d !*HSt -> ((d,BodyTag),!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkApplyForm formid cbf data hst
= mkViewForm formid Display 
	{toForm = toFormid , updForm = \_ _ = cbf data , fromForm = \_ v -> v, resetForm = Nothing} data hst

// editor which applies the function to its argument

mkStoreForm :: !FormId 	!(d -> d) d !*HSt -> ((d,BodyTag),!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkStoreForm formid cbf data hst
= mkViewForm formid Display 
	{toForm = toFormid , updForm = \_ v = cbf v , fromForm = \_ v -> v, resetForm = Nothing} data hst

mkApplyEditForm	:: !FormId !d d !*HSt -> ((d,BodyTag),!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkApplyEditForm formid inputval initval hst
= mkViewForm formid Edit 
	{toForm =  toFormid , updForm = update , fromForm = \_ v -> v, resetForm = Nothing} initval hst
where
	update True  newval = newval
	update False val    = inputval

mkBimapEditor :: !FormId !Mode !(Bimap d v) d !*HSt -> ((d,BodyTag),!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} v
mkBimapEditor formid mode {map_to,map_from} d hst
= mkViewForm formid mode 	{ toForm = \d _ -> map_to d
						, updForm = \b v -> map_to (map_from v)
						, fromForm = \b v -> map_from v
						, resetForm = Nothing
						} d hst 

toFormid d Nothing = d
toFormid d (Just v) = v


// swiss army nife editor that makes coffee too ...

mkViewForm :: !FormId !Mode !(HBimap d v) d !*HSt -> ((d,BodyTag),!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} v 
mkViewForm formid mode {toForm, updForm, fromForm, resetForm} initdata (inidx,lhsts) 
# ((updview,body),(nr,[(uniqueid,mystore):lhsts])) = gForm{|*|} formid mode nextview (0,[(formid,viewtostore):lhsts])
= ((fromForm2 updview,body),(0,[(formid,encodeInfo2 updview):lhsts]))
where

	(isupdated,newview) = case updateFormInfo formid of
							(False,Nothing) 		= (False,toForm initdata Nothing)
							(False,Just oldview) 	= (False,toForm initdata (Just oldview))
//							(True, Just newview)	= (True, newview) //??  
							(True, Just newview)	= (True, toForm initdata (Just newview)) //??  

	updateview			= updForm  isupdated newview		// apply update function telling user if an update has taken place
	newdata				= fromForm isupdated updateview		// convert back to data domain telling if an update has taken place	 

	nextview			= case resetForm of
								Nothing -> updateview		// adjust view 
								Just upd -> upd updateview
	viewtostore			= encodeInfo  nextview				// in this terrible format

	fromForm2 updview 	= case resetForm of
							Nothing -> fromForm isupdated updview 
							(Just reset)	-> newdata
							
	encodeInfo2 updview = case resetForm of
							Nothing -> encodeInfo updview
							(Just reset)	-> viewtostore

	updateFormInfo :: FormId -> (Bool,Maybe a) | gUpd{|*|} a & gParse{|*|} a
	updateFormInfo formid
		= case (decodeInput1 formid) of

			// an update for this form is detected

			((Just (pos,updval), Just oldstate)) 
					-> (True, Just (snd (gUpd{|*|} (UpdSearch updval pos) oldstate)))

			// no update found, determine the current stored state

			((_, Just oldstate))	
					-> (False, Just oldstate)

			// no update, no state stored, the current value is taken as (new) state

			else	-> (False, Nothing)	
	where
		decodeInput1 :: FormId -> (Maybe FormUpdate, Maybe a) | gParse{|*|} a
		decodeInput1 formid
		| CheckUpdateId == formid// this state is updated
		= case CheckUpdate of
			(Just (sid,pos,UpdC s), Just "") 						= (Just (pos,UpdC s)  ,find sid CheckGlobalState)
			(Just (sid,pos,UpdC s), _) 								= (Just (pos,UpdC s)  ,find sid CheckGlobalState)
			else = case CheckUpdate of
					(Just (sid,pos,UpdI i), Just ni) 				= (Just (pos,UpdI ni) ,find sid CheckGlobalState) 
					else = case CheckUpdate of
							(Just (sid,pos,UpdR r), Just nr) 		= (Just (pos,UpdR nr) ,find sid CheckGlobalState) 
							else = case CheckUpdate of
								(Just (sid,pos,UpdS s),	Just ns)	= (Just (pos,UpdS ns) ,find sid CheckGlobalState) 
								(Just (sid,pos,UpdS s),	_)			= (Just (pos,UpdS AnyInput)  ,find sid CheckGlobalState) 
								(upd,new) 							= (Nothing, find formid CheckGlobalState)
		| otherwise = (Nothing, find formid CheckGlobalState)

		find :: FormId  String -> (Maybe a) | gParse{|*|} a
		find formid   ""	= Nothing
		find formid   input
		# (result,input) = ShiftState input
		= case (result,input) of
			(Just (thisid,a),input) -> if (thisid == formid) (Just a) (find formid input)
			(Nothing, input)		-> find formid input


// automatic tranformation of any Clean type to html body
// the lhst on the head of the hst is the lhst for the form we create here

generic gForm a :: !FormId !Mode a !*HSt -> *((a,BodyTag), !*HSt)	

gForm{|Int|} formid mode i hst 	
# (body,hst) = mkInput formid mode (IV i) (UpdI i) hst
= ((i,body),hst)

gForm{|Real|} formid mode r hst 	
# (body,hst) = mkInput formid mode (RV r) (UpdR r) hst
= ((r,body),hst)

gForm{|Bool|} formid mode b hst 	
# (body,hst) = mkInput formid mode (BV b) (UpdB b) hst
= ((b,body),hst)


gForm{|String|} formid mode s hst 	
# (body,hst) = mkInput formid mode (SV s) (UpdS s) hst
= ((s,body),hst)

mkInput :: !FormId !Mode Value UpdValue *HSt -> (BodyTag,*HSt) 
mkInput formid Edit val updval (inidx,hst) 
	= ( Input 	[	Inp_Type Inp_Text
				, 	Inp_Value val
				,	Inp_Name (encodeInfo (formid,inidx,updval))
				,	Inp_Size defsize
				,	`Inp_Events	[OnChange callClean]
				] ""
		,(inidx+1,hst))
mkInput formid Display val _ (inidx,hst) 
	= ( Input 	[	Inp_Type Inp_Text
				, 	Inp_Value val
				,	Inp_ReadOnly ReadOnly
				, 	`Inp_Std [Std_Style color]
				,	Inp_Size defsize
				] ""
		,(inidx+1,hst))
where
	color = "background-color:" +++ backcolor

gForm{|UNIT|}  _ _ _ hst 	= ((UNIT,EmptyBody),hst)

gForm{|PAIR|} gHa gHb formid mode (PAIR a b) hst 
# ((na,ba),hst) = gHa formid mode a hst
# ((nb,bb),hst) = gHb formid mode b hst
= ((PAIR na nb,STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[ba],[bb]]),hst)

gForm{|EITHER|} gHa gHb formid mode (LEFT a)   hst 
# ((a,ba),hst) = gHa formid mode a hst
= ((LEFT a, ba),hst)
gForm{|EITHER|} gHa gHb formid mode (RIGHT b)  hst
# ((b,bb),hst) = gHb formid mode b hst
= ((RIGHT b, bb),hst)
gForm{|OBJECT|} gHo formid mode (OBJECT o) hst
# ((o,bo),hst) = gHo formid mode o hst
= ((OBJECT o, bo),hst)

gForm{|CONS of t|} gHc formid mode (CONS c) hst=:(inidx,lhst)
| not (isEmpty t.gcd_fields) 		 
# ((c,body),hst) = gHc formid mode c (inidx+1,lhst) // don't display record constructor
= ((CONS c, body),hst) 
| t.gcd_type_def.gtd_num_conses == 1 
# ((c,body),hst) = gHc formid mode c (inidx+1,lhst) // don't display constructors that have no alternative
= ((CONS c, body),hst)
| t.gcd_name.[(size t.gcd_name) - 1] == '_' // don't display constructor names which end with an underscore
# ((c,body),hst) = gHc formid mode c (inidx+1,lhst) 
= ((CONS c, body),hst)
# (selector,hst)= mkConsSelector formid t hst
# ((c,body),hst) = gHc formid mode c hst
= ((CONS c,STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[selector,body]]),hst)
where
	mkConsSelector formid thiscons (inidx,lhst) 
						= (mkConsSel inidx allnames myindex formid, (inidx+1,lhst))
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
# ((nx,bx),hst) = gHx formid mode x hst
= ((FIELD nx, STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[fieldname,bx]]),hst)
where
	fieldname =Input 	[	Inp_Type Inp_Text
						, 	Inp_Value (SV (d.gfd_name +++ ": "))
						,	Inp_ReadOnly ReadOnly
						, 	`Inp_Std [Std_Style color]
						,	Inp_Size defsize
						] ""

	color = "background-color:" +++ backcolor
	


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

gForm{|(,)|} gHa gHb formid mode  (a,b) (inidx,lhsts)
# ((na,ba),hst) = gHa formid mode a (inidx+1,lhsts)   // one more for the now invisable (,) constructor 
# ((nb,bb),hst) = gHb formid mode b hst
= (((na,nb),STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[ba, bb]]),hst)

gForm{|(,,)|} gHa gHb gHc formid mode (a,b,c) (inidx,lhsts)
# ((na,ba),hst) = gHa formid mode a (inidx+1,lhsts)   // one more for the now invisable (,,) constructor 
# ((nb,bb),hst) = gHb formid mode b hst
# ((nc,bc),hst) = gHc formid mode c hst
= (((na,nb,nc),STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[ba, bb, bc]]),hst)

// <-> works exactly the same as (,) and places its arguments next to each other, for compatibility with GEC's

gForm{|(<->)|} gHa gHb formid mode  (a <-> b) (inidx,lhsts)
# ((na,ba),hst) = gHa formid mode a (inidx+1,lhsts)   // one more for the now invisable <-> constructor 
# ((nb,bb),hst) = gHb formid mode b hst
= (((na <-> nb),STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[ba, bb]]),hst)
derive gUpd   <->
derive gParse <->
derive gPrint <->

// <|> works exactly the same as PAIR and places its arguments below each other, for compatibility with GEC's

gForm{|(<|>)|} gHa gHb formid mode (a <|> b) (inidx,lhsts) 
# ((na,ba),hst) = gHa formid mode a (inidx+1,lhsts) // one more for the now invisable <|> constructor
# ((nb,bb),hst) = gHb formid mode b hst
= (((na <|> nb),STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[ba],[bb]]),hst)
derive gUpd   <|>
derive gParse <|>
derive gPrint <|>

// to switch between modes within a type ...

gForm{|DisplayMode|} gHa formid mode (HideMode a) (inidx,lhsts) 	
# ((na,nba),hst) = gHa formid Display a (inidx+1,lhsts)
= ((HideMode a,EmptyBody),hst)
gForm{|DisplayMode|} gHa formid mode (DisplayMode a) (inidx,lhsts)  
# ((na,nba),hst) = gHa formid Display a (inidx+1,lhsts)
= ((DisplayMode na,nba),hst)
gForm{|DisplayMode|} gHa formid mode (EditMode a) (inidx,lhsts)  
# ((na,nba),hst) = gHa formid Edit a (inidx+1,lhsts)
= ((EditMode na,nba),hst)
gForm{|DisplayMode|} gHa formid mode EmptyMode (inidx,lhsts)
= ((EmptyMode,EmptyBody),(inidx+1,lhsts)) 

derive gUpd DisplayMode
derive gParse DisplayMode
derive gPrint DisplayMode

// Buttons to press

gForm{|Button|} formid mode v=:(LButton size bname) (inidx,lhsts=:[(uniqueid,lst):lsts]) 
= ((v	, Input (ifMode mode [] [Inp_Disabled Disabled] ++
				[ Inp_Type Inp_Button
				, Inp_Value (SV bname)
				, Inp_Name (encodeInfo (uniqueid,inidx,UpdS bname))
				, `Inp_Std [Std_Style ("width:" +++ toString size)]
				, `Inp_Events [OnClick callClean]
				]) "")
	, (inidx+1,lhsts))
gForm{|Button|} formid mode v=:(PButton (height,width) ref) (inidx,lhsts=:[(uniqueid,lst):lsts]) 
= ((v	, Input	(ifMode mode [] [Inp_Disabled Disabled] ++
				[ Inp_Type Inp_Image
				, Inp_Value (SV ref)
				, Inp_Src ref
				, Inp_Name (encodeInfo (uniqueid,inidx,UpdS ref))
				, `Inp_Std [Std_Style ("width:" +++ toString width +++ " height:" +++ toString height)]
				, `Inp_Events [OnClick callClean]
				]) "")
	, (inidx+1,lhsts))
gForm{|Button|} formid mode Pressed hst = gForm {|*|} formid mode (LButton defpixel "??") hst // end user should reset button

gUpd{|Button|} (UpdSearch (UpdS name) 0) 	_ = (UpdDone,Pressed)					// update integer value
gUpd{|Button|} (UpdSearch val cnt)      	b = (UpdSearch val (cnt - 1),b)			// continue search, don't change
gUpd{|Button|} (UpdCreate l)				_ = (UpdCreate l,(LButton defsize "Press"))					// create default value
gUpd{|Button|} mode 			  	    	b = (mode,b)							// don't change
derive gParse Button
derive gPrint Button

gForm{|CheckBox|} formid mode v=:(CBChecked name) (inidx,lhsts=:[(uniqueid,lst):lsts]) 
= ((v	, Input (ifMode mode [] [Inp_Disabled Disabled] ++
				[ Inp_Type Inp_Checkbox
				, Inp_Value (SV name)
				, Inp_Name (encodeInfo (uniqueid,inidx,UpdS name))
				, Inp_Checked Checked
				, `Inp_Events [OnClick callClean]
				]) "")
	, (inidx+1,lhsts))
gForm{|CheckBox|} formid mode v=:(CBNotChecked name) (inidx,lhsts=:[(uniqueid,lst):lsts]) 
= ((v	, Input (ifMode mode [] [Inp_Disabled Disabled] ++
				[ Inp_Type Inp_Checkbox
				, Inp_Value (SV name)
				, Inp_Name (encodeInfo (uniqueid,inidx,UpdS name))
				, `Inp_Events [OnClick callClean]
				]) "")
	, (inidx+1,lhsts))

derive gUpd CheckBox
derive gParse CheckBox
derive gPrint CheckBox

gForm{|RadioButton|} formid mode v=:(RBChecked name) (inidx,lhsts=:[(uniqueid,lst):lsts]) 
= ((v	, Input (ifMode mode [] [Inp_Disabled Disabled] ++
				[ Inp_Type Inp_Radio
				, Inp_Value (SV name)
				, Inp_Name (encodeInfo (uniqueid,inidx,UpdS name))
				, Inp_Checked Checked
				, `Inp_Events [OnClick callClean]
				]) "")
	, (inidx+1,lhsts))
gForm{|RadioButton|} formid mode v=:(RBNotChecked name) (inidx,lhsts=:[(uniqueid,lst):lsts]) 
= ((v	, Input (ifMode mode [] [Inp_Disabled Disabled] ++
				[ Inp_Type Inp_Radio
				, Inp_Value (SV name)
				, Inp_Name (encodeInfo (uniqueid,inidx,UpdS name))
				, `Inp_Events [OnClick callClean]
				]) "")
	, (inidx+1,lhsts))

derive gUpd	  RadioButton
derive gParse RadioButton
derive gPrint RadioButton

gForm{|PullDownMenu|} formid mode v=:(PullDown (size,width) (menuindex,itemlist)) (inidx,lhsts=:[(uniqueid,lst):lsts]) 
= ((v	, Select 	(ifMode mode [] [Sel_Disabled Disabled] ++
					[ Sel_Name ("CS")
					, Sel_Size size
					, `Sel_Std [Std_Style ("width:" +++ (toString width) +++ "px")]
					, `Sel_Events [OnChange callClean]
					])
					[ Option  
						[ Opt_Value (encodeInfo (uniqueid,inidx,UpdC (itemlist!!j)))
						: if (j == menuindex) [Opt_Selected Selected] [] 
						]
						elem
						\\ elem <- itemlist & j <- [0..]
					 ]) 				
	, (incrIndex inidx v,lhsts))

gUpd{|PullDownMenu|} (UpdSearch (UpdC cname) 0) (PullDown (size,width) (menuindex,itemlist)) 
			= (UpdDone,PullDown (size,width) (nmenuindex 0 cname itemlist,itemlist))					// update integer value
where
	nmenuindex cnt name [itemname:items] 
	| name == itemname = cnt
	| otherwise		   = nmenuindex (cnt+1) name items
	nmenuindex _ _ [] = -1
	
gUpd{|PullDownMenu|} (UpdSearch val cnt) v = (UpdSearch val (cnt - (incrIndex cnt v)),v)			// continue search, don't change
gUpd{|PullDownMenu|} (UpdCreate l)		_ = (UpdCreate l,PullDown (1,defpixel) (0,["error"]))					// create default value
gUpd{|PullDownMenu|} mode 			  	v = (mode,v)							// don't change
derive gParse PullDownMenu
derive gPrint PullDownMenu


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

specialize :: (FormId Mode a *HSt -> ((a,BodyTag),*HSt)) FormId Mode a *HSt -> ((a,BodyTag),*HSt) | gUpd {|*|} a
specialize editor name mode v hst=:(inidx,[(myid,mylst):lsts])
//# (UpdSearch _ cnt,v) = gUpd {|*|} (UpdSearch (UpdI 0) -1) v
//# nextidx = inidx + (-1 - cnt)
# nextidx = incrIndex inidx v
# ((v,body),(_,lsts)) = editor codedname mode v (0,lsts)
= ((v,body),(nextidx,[(myid,mylst):lsts]))
where
	codedname = myid +++ "_" +++ toString inidx

incrIndex :: Int v -> Int | gUpd {|*|} v
incrIndex i v
# (UpdSearch _ cnt,v) = gUpd {|*|} (UpdSearch (UpdI 0) -1) v
= i + (-1 - cnt)
