implementation module htmlHandler

import StdEnv, ArgEnv, StdMaybe
import StdHtml
import StdGeneric
import htmlEncodeDecode
import GenParse, GenPrint

derive gPrint (,), (,,), UpdValue
derive gParse (,), (,,), UpdValue
derive gHpr   (,), (,,)
derive gUpd		   (,,)
 
:: HSt 			:== (InputId,[FormState],String)	// all form sates are collected here ... 	
:: FormState 	:== (FormID,FormValue)				// state of a form to remember
:: FormId	 	:== String							// unique id identifying the form
:: FormValue 	:== String							// current Clean value to remember encoded in a String
:: InputId	 	:== Int								// unique id for every constructor and basic value appearing in the state

:: FormUpdate	:== (InputId,UpdValue)		// info obtained when form is updated

:: UpdValue 	= UpdI Int							// new integer value
				| UpdR Real							// new real value
				| UpdC String						// choose indicated constructor 
				| UpdS String						// new piece of text


mkHSt :: String -> HSt
//mkHSt gbst :: HSt
mkHSt gbst = (0,[],gbst)

// top level function given to end user
// it collects the html page to display, and returns the contents of all Clean GEC's / Forms created

doHtml :: (HSt -> (Html,HSt)) *World -> *World
doHtml pagehandler world 
= print_to_stdout html world
where
	(html,(_,lhst,_))  // the collection of local states is saved in the global state: easy thanks to lazy evaluation !
		= pagehandler (mkHSt (urlEncodeState (reverse lhst))) 

mkHGEC :: FormID (HMode a)  a HSt -> (a,(Body,HSt)) | gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
mkHGEC uniqueid mode v (inidx,lhsts,ghst) 
= (nv,gHGEC{|*|} nv (0,[(uniqueid,lhst):lhsts],ghst))
where
	lhst 	= encodeInfo  nv
	
	nv = updClean uniqueid mode v	 

	updClean :: FormID (HMode a) a -> a | gUpd{|*|} a & gParse{|*|} a
	updClean uniqueid mode v 
		= case (decodeInput1 uniqueid,mode) of
			// an update is for this form is detected
			((Just (pos,updval), Just oldstate),Edit update) -> (update (snd (gUpd{|*|} (UpdSearch updval pos) oldstate)))
			// no update, look for it previous state
			((Nothing, Just oldstate),Edit update)			-> oldstate
			// no update, look for it previous state
			((Nothing, Just oldstate),Set)					-> v
			// no update, no previous state, so take the initial value as state
			else											-> v	
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
								(Just (sid,pos,UpdS s),	_)			= (Just (pos,UpdS s)  ,find sid CheckGlobalState) 
								(upd,new) 							= abort "cannot find global state" //(Nothing,Nothing)
		| otherwise = (Nothing, find uniqueid CheckGlobalState)

		find :: FormId  String -> (Maybe a) | gParse{|*|} a
		find formid   ""	= Nothing
		find formid   input
		# (result,input) = ShiftState input
		= case (result,input) of
			(Just (thisid,a),input) -> if (thisid == formid) (Just a) (find formid input)
			(Nothing, input)		-> find formid input


	
//			find formid  [(stateid,state):states]
//			| True	= abort "id gevonden" //parseString state
//			| formid == stateid	= abort "id gevonden" //parseString state
//			| otherwise			= abort "formstate gevonden" //find formid (Just states)
//			find formid  else = Nothing

// automatic tranformation of any Clean type to html body
// the lhst on the head of the hst is the lhst for the form we create here

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
	mkConsSelector thiscons (inidx,lhst=:[(formid,mystate):states],ghst) 
//						= (mkConsSel inidx allnames myindex (hd lhsts), (inidx+1,lhsts))
						= (mkConsSel inidx allnames myindex formid ghst, (inidx+1,lhst,ghst))
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

	mkConsSel:: Int [String] Int String String -> Body
	mkConsSel inidx list nr formid ghst
		= (Form	[ Frm_Action MyPhP
				, Frm_Name formid
				, Frm_Method Post
				, Frm_Style "margin:0"] 
				[ 	Select 	[ Sel_Name ("ConsSelector")
							, `Sel_ElemEvnts (OnChange changescript)
							]
							[Option elem [	Opt_Value (encodeUpdate (formid,inidx,UpdC elem)): if (j == nr) [Opt_Selected Selected] [] ]
							\\ elem <- list & j <- [0..]
							]
 				,	storeHidden ghst 
				] )
		where
				changescript = "\"javascript: form.submit()\""  


class toBody a :: a HSt -> (Body,HSt) 	

instance toBody Int
where
	toBody i (inidx,lhsts=:[(uniqueid,lst):lsts],ghst) 
			= (Form 	[Frm_Action MyPhP, Frm_Name formname, Frm_Method Post, Frm_Style "margin:0"] 
						 [	Input 	[	Inp_Type Text
									, 	Inp_Value (IV i)
									,	Inp_Name (encodeUpdate (uniqueid,inidx,UpdI i))
									,	Inp_Size defsize
									]
						 ,	storeHidden ghst // !! (hd lhsts) 
						 ]
						,(inidx+1,lhsts,ghst))
	where
		formname = fst (hd lhsts)

instance toBody Real
where
	toBody r (inidx,lhsts=:[(uniqueid,lst):lsts],ghst) 
		= (Form 	[Frm_Action MyPhP, Frm_Name formname, Frm_Method Post, Frm_Style "margin:0"] 
						 [	Input 	[	Inp_Type Text
									, 	Inp_Value (RV r)
									,	Inp_Name (encodeUpdate (uniqueid,inidx,UpdR r))
									,	Inp_Size defsize
									]
						 ,	storeHidden ghst //(hd lhsts) 
						 ]
						,(inidx+1,lhsts,ghst))
	where
		formname = fst (hd lhsts)

instance toBody String
where
	toBody s (inidx,lhsts=:[(uniqueid,lst):lsts],ghst)
			 = (Form 	[Frm_Action MyPhP, Frm_Name formname, Frm_Method Post, Frm_Style "margin:0"] 
						 [	Input 	[	Inp_Type Text
									, 	Inp_Value (SV s)
									,	Inp_Name (encodeUpdate (uniqueid,inidx,UpdS s))
									,	Inp_Size defsize
									]
						 ,	storeHidden ghst // (hd lhsts) 
						 ]
						,(inidx+1,lhsts,ghst))
	where
		formname = fst (hd lhsts)

storeHidden ::  String -> Body
storeHidden ghst
	 =	Input	[	Inp_Type Hidden
				,	Inp_Value (SV  ghst) // global lhst
//				,	Inp_Value (SV (encodeHidden (idform,lhst))) // global lhst
				,	Inp_Name "hidden"									
				]

// special representations, buutons and the like

:: CHButton = CHPressed | CHButton Int String

gHGEC{|CHButton|} (CHButton size bname) (inidx,lhsts=:[(uniqueid,lst):lsts],ghst) 
= (Form [Frm_Action MyPhP, Frm_Name "thisbutton", Frm_Method Post, Frm_Style "margin:0"] 
						 [	Input 	[	Inp_Type Button
									, 	Inp_Value (SV bname)
									,	Inp_Name (encodeUpdate (uniqueid,inidx,UpdS bname))
									,	Inp_Size size
									, 	`Inp_MouseEvnts (OnClick submitscript)
									]
						 ,	storeHidden2 
						 ]
						, (inidx+1,lhsts,ghst))
where
	submitscript = "\"javascript: form.submit()\""    // works fine for global lhst 

	storeHidden2
		=	Input	[	Inp_Type Hidden
			 			,	Inp_Value (SV ("; =;i;" +++ //encodeInfo uniqueid +++
			 								ghst ))
		 			,	Inp_Name (encodeUpdate (uniqueid,inidx, UpdS  bname) 
		 										+++
		 										";i;" 	+++ ghst 
		 						 )
		 							
					]

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

gHGEC{|(,)|} gHa gHb (a,b) (inidx,lhsts,ghst)
# (ba,hst) = gHa a (inidx+1,lhsts,ghst)   // one more for the now invisable (,) constructor 
# (bb,hst) = gHb b hst
= (Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[ba, bb]],hst)

gHGEC{|(,,)|} gHa gHb gHc (a,b,c) (inidx,lhsts,ghst)
# (ba,hst) = gHa a (inidx+1,lhsts,ghst)   // one more for the now invisable (,,) constructor 
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


/* garbage: 

//# (Head headattr body,hst) = pagehandler mkHSt
//= print_to_stdout (Head headattr (body ++ [globalstore hst body])) world
//= print_to_stdout [(id,urlDecodeS state) \\ (id,state) <- fst hst ] world   // just testing state collected
//where
	// storing the global state in the hidden field of form "globalform" 

	// we still have to find out how to submit this info with every input action !!!
	
//		globalstore globalstate body
//			=  Form	[Frm_Action MyPhP, Frm_Name "globalform", Frm_Method Post, Frm_Style "margin:0"] 
//					[	Input	[	Inp_Type Hidden
//								,	Inp_Value (SV (decode (fst globalstate)))
//								,	Inp_Name "GlobalName"
//								]
//				 	]
//	decode globalstate = urlEncodeState (reverse globalstate)

*/