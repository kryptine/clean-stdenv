implementation module htmlHandler

import StdEnv, ArgEnv, StdMaybe
import StdHtml
import StdGeneric
import htmlEncodeDecode
import GenParse, htmlPrint

derive gPrint (,), UpdValue
derive gParse (,), UpdValue

// some constants

//defsize = 10															// size of inputfield

// gUpd can any type with indicated value 

:: State :== (String,String) // (id,global state)


:: UpdMode	= UpdSearch UpdValue Int
			| UpdCreate [ConsPos]
			| UpdDone

:: UpdValue = UpdI Int			// new integer value
			| UpdR Real			// new real value
			| UpdC String		// choose indicated constructor 
			| UpdS String		// new piece of text

// high level functions intended for end user ...

doHtml ::  (a -> Html) a *World -> *World | gHpr{|*|} a & gUpd{|*|}  a  & gParse{|*|} a 
doHtml pagehandler initval world = print_to_stdout (pagehandler (updClean (initval))) world


showClean :: a -> Body | gHGEC{|*|} a & gPrint{|*|} a
showClean v = fst (gHGEC{|*|} (id,state) v mkHSt)
where
	id 		= "myState"
	state 	= "\"" +++ encodeHidden v +++ "\"" 	 

updClean  :: a -> a	| gUpd{|*|}  a & gParse{|*|} a					// kindles version of gUpd
updClean v = case decodeInput1 of
				(Just (pos,updval), Just nv,_) 	-> snd (gUpd{|*|} (UpdSearch updval pos) nv)
				(Just (pos,updval), Nothing,_) 	-> snd (gUpd{|*|} (UpdSearch updval pos) v)
				(Nothing, Just nv,_) 				-> nv
				(Nothing, Nothing,_)				-> v
where
	decodeInput1 :: (Maybe (Int,UpdValue),Maybe a,Maybe String) | gParse{|*|} a
	decodeInput1
	= case CheckUpdateInfo of
		(Just (n,UpdC s),	Just "",	state,ss) 	= (Just (n,UpdC s),	state,ss)
		else = case CheckUpdateInfo of
				(Just (n,UpdI i),	Just ni,	state,ss)	= (Just (n,UpdI ni),state,ss) 
				else = case CheckUpdateInfo of
						(Just (n,UpdR r),	Just nr,	state,ss)	= (Just (n,UpdR nr),state,ss) 
						else = case CheckUpdateInfo of
							(Just (n,UpdS s),	Just ns,	state,ss)	= (Just (n,UpdS ns),state,ss) 
							(upd,new,state,ss) = (upd,state,ss)


// id is used to give a unique id to every object,
// starts counting with 0, should be made unique sometime...

:: HSt = HSt Int		

mkHSt :: HSt
mkHSt = (HSt 0) 

// automatic tranformation of any type to html body

generic gHGEC a :: State a HSt -> (Body,HSt)		
gHGEC{|Int|}    st i hst 	= toBody st i hst
gHGEC{|Real|}   st r hst 	= toBody st r hst
gHGEC{|String|} st s hst 	= toBody st s hst
gHGEC{|UNIT|}   _  _ hst 	= (EmptyBody,hst)

gHGEC{|PAIR|} gHa gHb st (PAIR a b) hst 
# (ba,hst) = gHa st a hst
# (bb,hst) = gHb st b hst
= (Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[ba],[bb]],hst)


gHGEC{|EITHER|} gHa gHb st (LEFT a)   hst = gHa st a hst
gHGEC{|EITHER|} gHa gHb st (RIGHT b)  hst = gHb st b hst
gHGEC{|OBJECT|} gHo     st (OBJECT o) hst = gHo st o hst

gHGEC{|CONS of t|} gHc st (CONS c) hst
# (selector,hst)= mkConsSelector t hst
# (body,hst)	= gHc st c hst
= (Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[selector,body]],hst)
where
	mkConsSelector thiscons (HSt id) = (mkConsSel id allnames myindex, HSt (id + 1))
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

	mkConsSel:: Int [String] Int -> Body
	mkConsSel id list nr 
		= (Form	[ Frm_Action MyPhP
				, Frm_Name "myform"
				, Frm_Method Post
				, Frm_Style "margin:0"] 
				[ 	Select 	[ Sel_Name ("ConsSelector")
							, `Sel_ElemEvnts (OnChange changescript)
							]
							[Option elem [	Opt_Value (encodeUpdate (id,UpdC elem)): if (j == nr) [Opt_Selected Selected] [] ]
							\\ elem <- list & j <- [0..]
							]
 				,	storeHidden st 
				] )
		where
			changescript = "\"javascript: form.submit()\""  

// not quite certain why a class is needed here ..

class toBody a ::  State a HSt -> (Body,HSt) 	

instance toBody Int
where
	toBody st i (HSt id) = (Form 	[Frm_Action MyPhP, Frm_Name "myform", Frm_Method Post, Frm_Style "margin:0"] 
						 [	Input 	[	Inp_Type Text
									, 	Inp_Value (IV i)
									,	Inp_Name (encodeUpdate (id,UpdI i))
									,	Inp_Size defsize
									]
						 ,	storeHidden st 
						 ]
						,HSt (id+1))

instance toBody Real
where
	toBody st r (HSt id) = (Form 	[Frm_Action MyPhP, Frm_Name "myform", Frm_Method Post, Frm_Style "margin:0"] 
						 [	Input 	[	Inp_Type Text
									, 	Inp_Value (RV r)
									,	Inp_Name (encodeUpdate (id,UpdR r))
									,	Inp_Size defsize
									]
						 ,	storeHidden st 
						 ]
						,HSt (id+1))

instance toBody String
where
	toBody st s (HSt id) = (Form 	[Frm_Action MyPhP, Frm_Name "myform", Frm_Method Post, Frm_Style "margin:0"] 
						 [	Input 	[	Inp_Type Text
									, 	Inp_Value (SV s)
									,	Inp_Name (encodeUpdate (id,UpdS s))
									,	Inp_Size defsize
									]
						 ,	storeHidden st 
						 ]
						,HSt (id+1))

storeHidden (id,state) =	Input	[	Inp_Type Hidden
						 			,	Inp_Value (SV ("; " +++ encodeHidden state +++ ";=")) //id
						 			,	Inp_Name state
						 		    ]
//state name
// special representation

:: CHButton = CHPressed | CHButton Int String

gHGEC{|CHButton|} st=:(fid,state) (CHButton size bname) (HSt id) 
= (Form [Frm_Action MyPhP, Frm_Name "myform2", Frm_Method Post, Frm_Style "margin:0"] 
						 [	Input 	[	Inp_Type Button
									, 	Inp_Value (SV bname)
									,	Inp_Name (encodeUpdate (id,UpdS bname))
									,	Inp_Size size
									, 	`Inp_MouseEvnts (OnClick submitscript)
									]
						 ,	storeHidden2 (fid,state,(id,UpdS bname)) 
						 ]
						,HSt (id+1))
where
	submitscript = "\"javascript: form.submit()\""  

	storeHidden2 (id,state,upd) =	Input	[	Inp_Type Hidden
							 			,	Inp_Value (SV ("; " +++ encodeInfo state +++ ";=")) //id
							 			,	Inp_Name (encodeUpdate upd +++ ";"
							 			
							 						  ) //state //name
							 		    ]

gHGEC{|CHButton|} st (CHPressed) hst = gHGEC {|*|} st (CHButton defsize "Press") hst

gUpd{|CHButton|} (UpdSearch (UpdS name) 0) 	_ = (UpdDone,CHPressed)					// update integer value
gUpd{|CHButton|} (UpdSearch val cnt)      	b = (UpdSearch val (dec cnt),b)			// continue search, don't change
gUpd{|CHButton|} (UpdCreate l)				_ = (UpdCreate l,(CHButton defsize "Press"))					// create default value
gUpd{|CHButton|} mode 			  	    	b = (mode,b)							// don't change
derive gPrint CHButton
derive gHpr CHButton
derive gParse CHButton


// generic function to update any type, great miracle function
// will look for an input object with a certain id
// updates it
// if required invents new default value (e.g. when switching from Nil to Cons ...
// and leaves the rest untouched
// it makes coffee too 

generic gUpd t :: UpdMode t -> (UpdMode,t)

gUpd{|Int|} (UpdSearch (UpdI ni) 0) _ = (UpdDone,ni)					// update integer value
gUpd{|Int|} (UpdSearch val cnt)     i = (UpdSearch val (dec cnt),i)		// continue search, don't change
gUpd{|Int|} (UpdCreate l)			_ = (UpdCreate l,0)					// create default value
gUpd{|Int|} mode 			  	    i = (mode,i)						// don't change

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
