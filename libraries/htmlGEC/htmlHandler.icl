implementation module htmlHandler

import StdEnv, ArgEnv, StdMaybe
import StdHtml
import StdGeneric
import htmlEncodeDecode
import GenParse, GenPrint

derive gPrint (,), UpdValue
derive gParse (,), UpdValue
derive gHpr (,)

:: HSt 			:== ([FormState],InputId)	// all form sates are collected here ... 	
:: FormState 	:== (FormID,FormValue)		// state of a form to remember
:: FormID	 	:== String					// unique id identifying the form
:: FormValue 	:== String					// current Clean value to remember encoded in a String
:: InputId	 	:== Int						// unique id for every constructor and basic value appearing in the state


mkHSt :: HSt
mkHSt = ([],0)

// top level function given to end user
// it collects the html page to display, and returns the contents of all Clean GEC's / Forms created

doHtml :: (HSt -> (Html,HSt)) *World -> *World
doHtml pagehandler world 
# (html,hst) = pagehandler mkHSt
// find out how store the global hst in the page somehow ...
= print_to_stdout html world

//= print_to_stdout [(id,urlDecodeS state) \\ (id,state) <- fst hst ] world   // just testing state collected

mkHGEC :: FormID (a -> a) a HSt -> (a,(Body,HSt)) | gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
mkHGEC uniqueid update v (states,inidx) 
= (nv,gHGEC{|*|} nv ([(uniqueid,state):states],0))
where
	state 	= encodeInfo nv
	
	nv = updClean uniqueid update v	 

	updClean :: FormID (a -> a) a -> a | gUpd{|*|} a & gParse{|*|} a
	updClean uniqueid update v 
		= case decodeInput1 of
			// update of this form detected
			(Just (pos,updval), Just formid, Just state) 	
				-> if (formid == uniqueid)
						(update (snd (gUpd{|*|} (UpdSearch updval pos) state)))
						v //state
			// no update, but I can find the state
			(_, Just formid, Just state) 		
					-> if (formid == uniqueid)
							state
							v 
			else	-> v	
	where
		decodeInput1 :: (Maybe (Int,UpdValue), Maybe String, Maybe a) | gParse{|*|} a
		decodeInput1
		= case CheckUpdateInfo of
			(Just (n,UpdC s),	Just "" ,formid,state) = (Just (n,UpdC s),formid,state)
			else = case CheckUpdateInfo of
					(Just (n,UpdI i),	Just ni,formid,state) = (Just (n,UpdI ni),formid,state) 
					else = case CheckUpdateInfo of
							(Just (n,UpdR r),	Just nr,formid,state) = (Just (n,UpdR nr),formid,state) 
							else = case CheckUpdateInfo of
								(Just (n,UpdS s),	Just ns,formid,state)	= (Just (n,UpdS ns),formid,state) 
								(upd,new,formid,state) = (upd,formid,state)

// automatic tranformation of any Clean type to html body
// the state on the head of the hst is the state for the form we create here

generic gHGEC a :: a HSt -> (Body,HSt)		
gHGEC{|Int|}    i hst 	= toBody i hst
gHGEC{|Real|}   r hst 	= toBody r hst
gHGEC{|String|} s hst 	= toBody s hst
gHGEC{|UNIT|}   _ hst 	= (EmptyBody,hst)

gHGEC{|PAIR|} gHa gHb (PAIR a b) hst 
# (ba,hst) = gHa a hst
# (bb,hst) = gHb b hst
= (Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[ba],[bb]],hst)

gHGEC{|EITHER|} gHa gHb (LEFT a)   hst = gHa a hst
gHGEC{|EITHER|} gHa gHb (RIGHT b)  hst = gHb b hst
gHGEC{|OBJECT|} gHo     (OBJECT o) hst = gHo o hst

gHGEC{|CONS of t|} gHc (CONS c) hst
# (selector,hst)= mkConsSelector t hst
# (body,hst)	= gHc c hst
= (Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[selector,body]],hst)
where
	mkConsSelector thiscons (states,inidx) 
						= (mkConsSel inidx allnames myindex (hd states), (states,inidx+1))
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

	mkConsSel:: Int [String] Int FormState -> Body
	mkConsSel id list nr formstate=:(formid,state)
		= (Form	[ Frm_Action MyPhP
				, Frm_Name formid
				, Frm_Method Post
				, Frm_Style "margin:0"] 
				[ 	Select 	[ Sel_Name ("ConsSelector")
							, `Sel_ElemEvnts (OnChange changescript)
							]
							[Option elem [	Opt_Value (encodeUpdate (id,UpdC elem)): if (j == nr) [Opt_Selected Selected] [] ]
							\\ elem <- list & j <- [0..]
							]
 				,	storeHidden formstate 
				] )
		where
				changescript = "\"javascript: form.submit()\""  


class toBody a :: a HSt -> (Body,HSt) 	

instance toBody Int
where
	toBody i (states,inidx) 
			= (Form 	[Frm_Action MyPhP, Frm_Name formname, Frm_Method Post, Frm_Style "margin:0"] 
						 [	Input 	[	Inp_Type Text
									, 	Inp_Value (IV i)
									,	Inp_Name (encodeUpdate (inidx,UpdI i))
									,	Inp_Size defsize
									]
						 ,	storeHidden (hd states) 
						 ]
						,(states,inidx+1))
	where
		formname = fst (hd states)

instance toBody Real
where
	toBody r (states,inidx) 
		= (Form 	[Frm_Action MyPhP, Frm_Name formname, Frm_Method Post, Frm_Style "margin:0"] 
						 [	Input 	[	Inp_Type Text
									, 	Inp_Value (RV r)
									,	Inp_Name (encodeUpdate (inidx,UpdR r))
									,	Inp_Size defsize
									]
						 ,	storeHidden (hd states) 
						 ]
						,(states,inidx+1))
	where
		formname = fst (hd states)

instance toBody String
where
	toBody s (states,inidx)
			 = (Form 	[Frm_Action MyPhP, Frm_Name formname, Frm_Method Post, Frm_Style "margin:0"] 
						 [	Input 	[	Inp_Type Text
									, 	Inp_Value (SV s)
									,	Inp_Name (encodeUpdate (inidx,UpdS s))
									,	Inp_Size defsize
									]
						 ,	storeHidden (hd states) 
						 ]
						,(states,inidx + 1))
	where
		formname = fst (hd states)

storeHidden ::  FormState -> Body
storeHidden (idform,state)
	 =	Input	[	Inp_Type Hidden
				,	Inp_Value (SV (encodeHidden (idform,state)))
				,	Inp_Name idform
				]

// special representations, buutons and the like

:: CHButton = CHPressed | CHButton Int String

gHGEC{|CHButton|} (CHButton size bname) (states,inidx) 
= (Form [Frm_Action MyPhP, Frm_Name "thisbutton", Frm_Method Post, Frm_Style "margin:0"] 
						 [	Input 	[	Inp_Type Button
									, 	Inp_Value (SV bname)
									,	Inp_Name (encodeUpdate (inidx,UpdS bname))
									,	Inp_Size size
									, 	`Inp_MouseEvnts (OnClick submitscript)
									]
						 ,	storeHidden2 ((inidx,UpdS bname),hd states) 
						 ]
						, (states,inidx+1))
where
	submitscript = "\"javascript: var y = document.forms.thisbutton + document.forms.globalform; y.submit()\""  
//	submitscript = "\"javascript: form.submit() ; document.forms.globalform.submit()\""  
//	submitscript = "\"javascript: form.submit()\""    // works fine for global state 
//	submitscript = "\"javascript: form.submit(document.Global.State.value)\""  

	storeHidden2 (upd,fstate=:(id,state)) 
		=	Input	[	Inp_Type Hidden
		 			,	Inp_Value (SV ("; " +++ 
		 								"=;i;" +++ encodeInfo id +++
		 								";s;" +++ state )) //id
//		 			,	Inp_Value (SV ("; " +++ encodeInfo fstate +++ ";=")) //id
		 			,	Inp_Name (encodeUpdate upd +++ 
		 							";i;" +++ id +++
		 							";s;" +++ state +++
		 								encodeHidden fstate ) //state //name
					]

/*	storeHidden2 (id,state,upd) =	Input	[	Inp_Type Hidden
							 			,	Inp_Value (SV ("; " +++ encodeInfo state +++ ";=")) //id
							 			,	Inp_Name (encodeUpdate upd +++ ";"

*/
gHGEC{|CHButton|} CHPressed hst = gHGEC {|*|} CHPressed hst

gUpd{|CHButton|} (UpdSearch (UpdS name) 0) 	_ = (UpdDone,CHPressed)					// update integer value
gUpd{|CHButton|} (UpdSearch val cnt)      	b = (UpdSearch val (cnt - 1),b)			// continue search, don't change
gUpd{|CHButton|} (UpdCreate l)				_ = (UpdCreate l,(CHButton defsize "Press"))					// create default value
gUpd{|CHButton|} mode 			  	    	b = (mode,b)							// don't change
derive gPrint CHButton
derive gHpr CHButton
derive gParse CHButton

// special representations for lay-out

// tuples are placed next to each other, pairs below each other ...

gHGEC{|(,)|} gHa gHb (a,b) (states,inidx)
# (ba,hst) = gHa a (states,inidx+1)   // one more for the now invisable (,) constructor 
# (bb,hst) = gHb b hst
= (Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[ba, bb]],hst)

gHGEC{|(,,)|} gHa gHb gHc (a,b,c) (states,inidx)
# (ba,hst) = gHa a (states,inidx+1)   // one more for the now invisable (,,) constructor 
# (bb,hst) = gHb b hst
# (bc,hst) = gHc c hst
= (Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[ba, bb, bc]],hst)


// generic function to update any type, great miracle function
// will look for an input object with a certain id
// updates it
// if required invents new default value (e.g. when switching from Nil to Cons ...
// and leaves the rest untouched
// it makes coffee too 

// gUpd can any type with indicated value 

:: UpdMode	= UpdSearch UpdValue Int
			| UpdCreate [ConsPos]
			| UpdDone

:: UpdValue = UpdI Int			// new integer value
			| UpdR Real			// new real value
			| UpdC String		// choose indicated constructor 
			| UpdS String		// new piece of text

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

derive gUpd (,)


// garbage ???

/*

decodeInput :: (Maybe (Int,UpdValue),Maybe a) | gParse{|*|} a
decodeInput
=	case UpdateInfo of
	  (update, "", state, id) 		= (parseString update, parseString state)
	  (update, newvalue, state, id) 
	  			= case (parseString update) of
	  					Just (id,UpdI i) = (Just (id, UpdateI (UpdI i) (parseString newvalue)), parseString state) 
	  					else = (Nothing,Nothing)	

UpdateI :: UpdValue (Maybe Int) -> UpdValue
UpdateI (UpdI i) (Just ni) = UpdI ni
UpdateI val else = val

UpdateR :: UpdValue (Maybe Real) -> UpdValue
UpdateR (UpdR r) (Just nr) = UpdR nr
UpdateR val else = val

//			changescript = "\"javascript: window.document.location.href = this.options[this.selectedIndex].value\""

*/
