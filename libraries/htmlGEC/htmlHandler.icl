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
 
:: HSt 			:== .(InputId,[FormState])	// all form states are collected here ... 	
:: FormState 	:== (FormID,FormValue)		// state of a form to remember
:: FormId	 	:== String					// unique id identifying the form
:: FormValue 	:== String					// current Clean value to remember encoded in a String
:: InputId	 	:== Int						// unique id for every constructor and basic value appearing in the state

:: FormUpdate	:== (InputId,UpdValue)		// info obtained when form is updated

:: UpdValue 	= UpdI Int					// new integer value
				| UpdR Real					// new real value
				| UpdC String				// choose indicated constructor 
				| UpdS String				// new piece of text


mkHSt ::  *HSt
mkHSt = (0,[])

incHSt :: HSt -> HSt
incHSt (inidx,lhst) = (inidx+1,lhst)

// top level function given to end user
// it collects the html page to display, and returns the contents of all Clean GEC's / Forms created

doHtml :: (*HSt -> (Html,!*HSt)) *World -> *World
doHtml pagehandler world 
= print_to_stdout (Head head [addScript lhst:body]) world
where
	(Head head body,(_,lhst)) = pagehandler mkHSt

// experimental function:

mkEditHGEC2:: !FormID !HMode d !*HSt -> ((d,Body),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkEditHGEC2 uniqueid  mode data (inidx,lhst)
=	case findInLocalStore uniqueid lhst 0 lhst of
		(Just id, Just state,lhst)	-> mkSetHGEC uniqueid mode state (inidx,lhst)
		(_,_,				 lhst)	-> mkSetHGEC uniqueid mode data (inidx,lhst)
where
	findInLocalStore :: String [(FormID,FormValue)] Int [(FormID,FormValue)] -> (Maybe Int, Maybe d,[(FormID,FormValue)]) | gParse{|*|} d
	findInLocalStore s [] 			 _ lhst	 = (Nothing,Nothing,lhst)
	findInLocalStore s [(id,st):lst] n lhst
	| uniqueid == id = (Just n, parseString st, removeAt n lhst) 
	| otherwise 	 = findInLocalStore s lst (n+1) lhst

// simple editor for either editing or showing a simple value

mkEditHGEC:: !FormID !HMode d !*HSt -> ((d,Body),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkEditHGEC uniqueid  HEdit data hst
= mkViewHGEC uniqueid HEdit 
	{toHGEC = id , updHGEC = \_ v -> v , fromHGEC = id , resetHGEC = Nothing} data hst
mkEditHGEC uniqueid  mode data hst
= mkSetHGEC uniqueid mode data hst

mkSetHGEC:: !FormID !HMode d !*HSt -> ((d,Body),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkSetHGEC uniqueid  mode data hst
= mkViewHGEC uniqueid mode 
	{toHGEC = id , updHGEC = \_ _ -> data , fromHGEC = id , resetHGEC = Nothing} data hst

// editor with feedback to its self

mkSelfHGEC  :: !FormID 	!(d -> d) d !*HSt -> ((d,Body),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkSelfHGEC uniqueid cbf initdata hst
= mkViewHGEC uniqueid HEdit 
	{toHGEC = id , updHGEC = update , fromHGEC = id , resetHGEC = Nothing} initdata hst
where
	update True newval = cbf newval
	update _ val = val
	
// editor which applies the function to its argument

mkApplyHGEC :: !FormID !(d -> d) d !*HSt -> ((d,Body),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkApplyHGEC uniqueid cbf data hst
= mkViewHGEC uniqueid HDisplay 
	{toHGEC = id , updHGEC = \_ v = cbf data , fromHGEC = id, resetHGEC = Nothing} data hst

// editor which applies the function to its argument

mkStoreHGEC :: !FormID 	!(d -> d) d !*HSt -> ((d,Body),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkStoreHGEC uniqueid cbf data hst
= mkViewHGEC uniqueid HDisplay 
	{toHGEC = id , updHGEC = \_ v = cbf v , fromHGEC = id, resetHGEC = Nothing} data hst

mkApplyEditHGEC	:: !FormID !d d !*HSt -> ((d,Body),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkApplyEditHGEC uniqueid inputval initval hst
= mkViewHGEC uniqueid HEdit 
	{toHGEC = id , updHGEC = update , fromHGEC = id, resetHGEC = Nothing} initval hst
where
	update True  newval = newval
	update False val    = inputval

mkSpecialEditor :: !FormID !HMode !(Bimap d v) d !*HSt -> ((d,Body),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} v
mkSpecialEditor fid mode {map_to,map_from} d hst
= mkViewHGEC fid mode 	{ toHGEC = map_to
						, updHGEC = \b v -> map_to (map_from v)
						, fromHGEC = map_from
						, resetHGEC = Nothing
						} d hst 



// swiss army nife editor that makes coffee too ...

mkViewHGEC :: !FormID !HMode !(HBimap d v) d !*HSt -> ((d,Body),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} v 
mkViewHGEC uniqueid mode {toHGEC, updHGEC, fromHGEC, resetHGEC} data (inidx,lhsts) 
# ((updview,body),(nr,[(uniqueid,mystore):lhsts])) = gHGEC{|*|} mode nextview (0,[(uniqueid,viewtostore):lhsts])
= ((fromHGEC2 updview,body),(0,[(uniqueid,encodeInfo2 updview):lhsts]))
where
	initview 			= toHGEC data						// convert init data to view domain
	(isupdated,newview) = updateFormInfo uniqueid initview	// has to form been updated or is it in the global storage ?	 
	updateview			= updHGEC isupdated newview			// apply update function and tell user if an update has taken place
	newdata				= fromHGEC updateview				// convert back to data domain	 
	nextview			= case resetHGEC of
								Nothing -> updateview		// adjust view 
								Just upd -> upd updateview
	viewtostore			= encodeInfo  nextview				// in this terrible format

	fromHGEC2 updview 	= case resetHGEC of
							Nothing -> fromHGEC updview
							(Just reset)	-> newdata
							
	encodeInfo2 updview = case resetHGEC of
							Nothing -> encodeInfo updview
							(Just reset)	-> viewtostore

	updateFormInfo :: FormID a -> (Bool,a) | gUpd{|*|} a & gParse{|*|} a
	updateFormInfo uniqueid v 
		= case (decodeInput1 uniqueid) of

			// an update for this form is detected

			((Just (pos,updval), Just oldstate)) 
					-> (True, snd (gUpd{|*|} (UpdSearch updval pos) oldstate))

			// no update found, determine the current stored state

			((_, Just oldstate))	
					-> (False, oldstate)

			// no update, no state stored, the current value is taken as (new) state

			else	-> (False, v)	
	where
		decodeInput1 :: String -> (Maybe FormUpdate, Maybe a) | gParse{|*|} a
		decodeInput1 uniqueid
		| CheckUpdateId == uniqueid// this state is updated
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
								(upd,new) 							= (Nothing, find uniqueid CheckGlobalState)
		| otherwise = (Nothing, find uniqueid CheckGlobalState)

		find :: FormId  String -> (Maybe a) | gParse{|*|} a
		find formid   ""	= Nothing
		find formid   input
		# (result,input) = ShiftState input
		= case (result,input) of
			(Just (thisid,a),input) -> if (thisid == formid) (Just a) (find formid input)
			(Nothing, input)		-> find formid input


// automatic tranformation of any Clean type to html body
// the lhst on the head of the hst is the lhst for the form we create here

generic gHGEC a :: HMode a *HSt -> *((a,Body),*HSt)		
gHGEC{|Int|}    mode i hst 	
# (body,hst) = mkInput mode (IV i) (UpdI i) hst
= ((i,body),hst)

gHGEC{|Real|}   mode r hst 	
# (body,hst) = mkInput mode (RV r) (UpdR r) hst
= ((r,body),hst)

gHGEC{|String|} mode s hst 	
# (body,hst) = mkInput mode (SV s) (UpdS s) hst
= ((s,body),hst)

gHGEC{|UNIT|}   _ _ hst 	= ((UNIT,EmptyBody),hst)

gHGEC{|PAIR|} gHa gHb mode (PAIR a b) hst 
# ((na,ba),hst) = gHa mode a hst
# ((nb,bb),hst) = gHb mode b hst
= ((PAIR na nb,Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[ba],[bb]]),hst)

gHGEC{|EITHER|} gHa gHb mode (LEFT a)   hst 
# ((a,ba),hst) = gHa mode a hst
= ((LEFT a, ba),hst)
gHGEC{|EITHER|} gHa gHb mode (RIGHT b)  hst
# ((b,bb),hst) = gHb mode b hst
= ((RIGHT b, bb),hst)
gHGEC{|OBJECT|} gHo     mode (OBJECT o) hst
# ((o,bo),hst) = gHo mode o hst
= ((OBJECT o, bo),hst)

gHGEC{|CONS of t|} gHc mode (CONS c) hst=:(inidx,lhst)
| not (isEmpty t.gcd_fields) 		 
# ((c,body),hst) = gHc mode c (inidx+1,lhst) // don't display record constructor
= ((CONS c, body),hst) 
| t.gcd_type_def.gtd_num_conses == 1 
# ((c,body),hst) = gHc mode c (inidx+1,lhst) // don't display constructors that have no alternative
= ((CONS c, body),hst)
| t.gcd_name.[(size t.gcd_name) - 1] == '_' // don't display constructor names which end with an underscore
# ((c,body),hst) = gHc mode c (inidx+1,lhst) 
= ((CONS c, body),hst)
# (selector,hst)= mkConsSelector t hst
# ((c,body),hst)	= gHc mode c hst
= ((CONS c,Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[selector,body]]),hst)
where
	mkConsSelector thiscons (inidx,lhst=:[(formid,mystate):states]) 
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

	mkConsSel:: Int [String] Int String -> Body
	mkConsSel inidx list nr formid
		= Select 	[ Sel_Name ("CS")
					, setstyle
					, changeable
					]
					[Option elem 
						[	Opt_Value (encodeInfo (formid,inidx,UpdC elem)): if (j == nr) [Opt_Selected Selected:optionstyle] optionstyle ]
						\\ elem <- list & j <- [0..]
					]
		where
			changeable = case mode of
							HEdit 		-> `Sel_ElemEvnts (OnChange callClean)
							HDisplay 	-> Sel_Disabled Disabled
			setstyle	= case mode of
							HEdit 		-> Sel_Style width
							HDisplay 	-> Sel_Style (width +++ ";" +++ color)
			optionstyle	= case mode of
							HEdit 		-> []
							HDisplay 	-> [Opt_Style color]

			width = "width:" +++ (toString defpixel) +++ "px"
			color = "background-color:" +++ backcolor

mkInput :: HMode Value UpdValue *HSt -> (Body,*HSt) 
mkInput HEdit val updval (inidx,lhsts=:[(uniqueid,lst):lsts]) 
	= ( Input [	Inp_Type Text
				, 	Inp_Value val
				,	Inp_Name (encodeInfo (uniqueid,inidx,updval))
				,	Inp_Size defsize
				,	`Inp_ElemEvnts	(OnChange callClean)
				]
		,(inidx+1,lhsts))
mkInput HDisplay val _ (inidx,lhsts=:[(uniqueid,lst):lsts]) 
	= ( Input [	Inp_Type Text
				, 	Inp_Value val
				,	Inp_ReadOnly ReadOnly
				, 	Inp_Style color
				,	Inp_Size defsize
				]
		,(inidx+1,lhsts))
where
	color = "background-color:" +++ backcolor

gHGEC{|FIELD of d |} gHx mode (FIELD x) hst 
# ((nx,bx),hst) = gHx mode x hst
= ((FIELD nx, Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[fieldname,bx]]),hst)
where
//	fieldname = T (d.gfd_name +++ ": ")
	fieldname =Input 	[	Inp_Type Text
						, 	Inp_Value (SV (d.gfd_name +++ ": "))
						,	Inp_ReadOnly ReadOnly
						, 	Inp_Style color
						,	Inp_Size defsize
						]

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

gUpd{|Real|} (UpdSearch (UpdR nr) 0) 	_ = (UpdDone,nr)					// update integer value
gUpd{|Real|} (UpdSearch val cnt)     	r = (UpdSearch val (dec cnt),r)		// continue search, don't change
gUpd{|Real|} (UpdCreate l)			 	_ = (UpdCreate l,0.0)				// create default value
gUpd{|Real|} mode 			  	     	r = (mode,r)						// don't change

gUpd{|String|} (UpdSearch (UpdS ns) 0) 	_ = (UpdDone,ns)					// update integer value
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

gHGEC{|(,)|} gHa gHb mode  (a,b) (inidx,lhsts)
# ((na,ba),hst) = gHa mode a (inidx+1,lhsts)   // one more for the now invisable (,) constructor 
# ((nb,bb),hst) = gHb mode b hst
= (((na,nb),Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[ba, bb]]),hst)

gHGEC{|(,,)|} gHa gHb gHc mode (a,b,c) (inidx,lhsts)
# ((na,ba),hst) = gHa mode a (inidx+1,lhsts)   // one more for the now invisable (,,) constructor 
# ((nb,bb),hst) = gHb mode b hst
# ((nc,bc),hst) = gHc mode c hst
= (((na,nb,nc),Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[ba, bb, bc]]),hst)

// <-> works exactly the same as (,) and places its arguments next to each other, for compatibility with GEC's

gHGEC{|(<->)|} gHa gHb mode  (a <-> b) (inidx,lhsts)
# ((na,ba),hst) = gHa mode a (inidx+1,lhsts)   // one more for the now invisable <-> constructor 
# ((nb,bb),hst) = gHb mode b hst
= (((na <-> nb),Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[ba, bb]]),hst)
derive gUpd   <->
derive gParse <->
derive gPrint <->

// <|> works exactly the same as PAIR and places its arguments below each other, for compatibility with GEC's

gHGEC{|(<|>)|} gHa gHb mode (a <|> b) (inidx,lhsts) 
# ((na,ba),hst) = gHa mode a (inidx+1,lhsts) // one more for the now invisable <|> constructor
# ((nb,bb),hst) = gHb mode b hst
= (((na <|> nb),Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[ba],[bb]]),hst)
derive gUpd   <|>
derive gParse <|>
derive gPrint <|>


// to hide a state ::

gHGEC{|CHHidden|} gHa mode (CHHidden a) hst = ((CHHidden a,EmptyBody),hst)

gUpd{|CHHidden|} gHa (UpdSearch any cnt) val	= ((UpdSearch any cnt),val)	// skip hidden stuf, never updated
gUpd{|CHHidden|} gHa (UpdCreate l)		  _ 	= (UpdCreate l,abort "creation of new hidden values not implemeneted")					// create default value
gUpd{|CHHidden|} gHa umode 			     any	= (umode,any)				// don't change

derive gParse CHHidden
derive gPrint CHHidden

gHGEC{|Mode|} gHa mode (Hide a) (inidx,lhsts) 	
# ((na,nba),hst) = gHa HDisplay a (inidx+1,lhsts)
= ((Hide a,EmptyBody),hst)
gHGEC{|Mode|} gHa mode (Display a) (inidx,lhsts)  
# ((na,nba),hst) = gHa HDisplay a (inidx+1,lhsts)
= ((Display na,nba),hst)
gHGEC{|Mode|} gHa mode (Edit a) (inidx,lhsts)  
# ((na,nba),hst) = gHa HEdit a (inidx+1,lhsts)
= ((Edit na,nba),hst)
gHGEC{|Mode|} gHa mode EmptyMode (inidx,lhsts)
= ((EmptyMode,EmptyBody),(inidx+1,lhsts)) 

derive gUpd Mode
derive gParse Mode
derive gPrint Mode


// Button to press

gHGEC{|CHButton|} mode (CHButton size bname) (inidx,lhsts=:[(uniqueid,lst):lsts]) 
= ((CHButton size bname, Input [	Inp_Type Button
			, 	Inp_Value (SV bname)
			,	Inp_Name (encodeInfo (uniqueid,inidx,UpdS bname))
			,	Inp_Style ("width:" +++ toString size)
			, 	`Inp_MouseEvnts (OnClick callClean)
			])
	, (inidx+1,lhsts))
gHGEC{|CHButton|} mode CHPressed hst = gHGEC {|*|} mode (CHButton defsize "??") hst // end user should reset button

gUpd{|CHButton|} (UpdSearch (UpdS name) 0) 	_ = (UpdDone,CHPressed)					// update integer value
gUpd{|CHButton|} (UpdSearch val cnt)      	b = (UpdSearch val (cnt - 1),b)			// continue search, don't change
gUpd{|CHButton|} (UpdCreate l)				_ = (UpdCreate l,(CHButton defsize "Press"))					// create default value
gUpd{|CHButton|} mode 			  	    	b = (mode,b)							// don't change
derive gParse CHButton
derive gPrint CHButton

// specialize attempt ...

specialize :: (FormID HMode a *HSt -> ((a,Body),*HSt)) FormID HMode a *HSt -> ((a,Body),*HSt) | gUpd {|*|} a
specialize editor name mode v hst=:(inidx,[(myid,mylst):lsts])
# (UpdSearch _ cnt,v) = gUpd {|*|} (UpdSearch (UpdI 0) -1) v
# nextidx = inidx + (-1 - cnt)
# ((v,body),(_,lsts)) = editor codedname mode v (0,lsts)
= ((v,body),(nextidx,[(myid,mylst):lsts]))
where
	codedname = myid +++ "_" +++ toString inidx

