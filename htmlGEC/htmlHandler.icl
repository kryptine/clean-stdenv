implementation module htmlHandler

import StdEnv, ArgEnv, StdMaybe
import StdHtml
import StdGeneric
import htmlEncodeDecode
import GenParse, htmlPrint

derive gPrint (,), UpdValue
derive gParse (,), UpdValue

// some constants

defsize = 10															// size of inputfield

// gUpd can update any type with indicated value 


:: UpdMode	= UpdSearch UpdValue Int
			| UpdCreate [ConsPos]
			| UpdDone

:: UpdValue = UpdI Int			// new integer value
			| UpdR Real			// new real value
			| UpdC String		// choose indicated constructor 
			| UpdS String		// new piece of text


doHtml ::  (a -> Html) a *World -> *World | gHpr{|*|} a & gUpd{|*|}  a  & gParse{|*|} a 
doHtml pagehandler initval world = print_to_stdout (pagehandler (updClean (initval))) world


showClean :: a -> Body | gHGEC{|*|} a & gPrint{|*|} a
showClean v = fst (gHGEC{|*|} (id,state) v mkHSt)
where
	id 		= "myState"
	state 	= "\"" +++ encodeHidden v +++ "\"" 	 

updClean  :: a -> a	| gUpd{|*|}  a & gParse{|*|} a					// kindles version of gUpd
updClean v = case decodeInput of
				(Just (pos,updval), Just nv) 	-> snd (gUpd{|*|} (UpdSearch updval pos) nv)
				(Just (pos,updval), Nothing) 	-> snd (gUpd{|*|} (UpdSearch updval pos) v)
				(Nothing, Just nv) 				-> nv
				(Nothing, Nothing)				-> v

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


// automatic tranformation of any type to html body

:: HSt = HSt Int		// id to be used for next object, starts counting with 0, should be unique

class toBody a ::  State a HSt -> (Body,HSt) 	


mkHSt :: HSt
mkHSt = (HSt 0) 

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
							, `Sel_ElemEvnts (OnChange changescript) //(Script [] changescript))
							]
							[Option elem [	Opt_Value (encodeUpdate (id,UpdC elem)): if (j == nr) [Opt_Selected Selected] [] ]
							\\ elem <- list & j <- [0..]
							]
 				,	storeHidden st 
				] )
		where
			changescript = "\"javascript: form.submit()\""  //this.options[this.selectedIndex].value\""
//			changescript = "\"javascript: window.document.location.href = this.options[this.selectedIndex].value\""


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

storeHidden (state,name) =	Input	[	Inp_Type Hidden
						 			,	Inp_Value (SV state)
						 			,	Inp_Name name
						 		    ]

// updating any type 

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

