module Algebra

import StdEnv, StdHtml

// representation of a simple process algebra:

:: PA 	= E String						// atom, the string will be used as the label of the button
		| (.+.) infixl 6 PA PA			// equivalent of process algebra +
		| (.>.) infixl 5 PA PA			// equivalent of proces  algebra "followed by"
		
// some example expressions (they have to be finite)

//expr =  E "a" .+. E "b" 
//expr =  E "a" .+. E "b" .+. E "c"
//expr =  (E "a" .>. E "b") .+. (E "c" .>. E "d" ).+. E "f" 
//expr =  (E "a" .+. E "b" .>. E "c" .+. E "d" ).+. E "f" 
//expr =  E "a" .+. E "b" .+. E "c" .>. E "d" .+. E "f" .>. E "g" .>. E "h"
//expr =  E "a" .>. E "b" .>. E "c"
expr =  E "a" .+. (E "b1" .>. E "b2") .>. E "c" 

Start world  = doHtml MyPage  world

// the program is still quite complicated:

// first the current set of buttons is read from store "butstore"
// this is used to re-create these "buttons"; funchosen is the function corresponding to the button pressed 
// in "store" the process algebra expression is stored: the new expression is calculated depending on the button pressed and stored 
// from the new expression the new set of buttons is calculated (nbutset); and stored in "butstore" for the next time
// this is used to calculate the new "buttons" to display
// the next time we start from the beginning

MyPage hst
# ((butset,_),hst)		= mkStoreForm 		"butstore"  id initbuttonset hst
# ((funchosen,_),hst) 	= ListFuncBut False  "buttons"  Edit (calcbuttons butset) hst
# (((_,nbutset),_),hst)	= mkStoreForm 		"store"     funchosen initstore hst
# (_,hst)				= mkSetForm 		"butstore"  Display nbutset hst
# ((_,buttons),hst) 	= ListFuncBut True	"buttons"   Edit (calcbuttons nbutset) hst
= mkHtml "Process Algebra Experiment"
	[ H1 [] "Process Algebra Experiment"
	, Br, Br
	, BodyTag buttons
	, Br
	] hst
where
	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags

	initstore 		= calcnext expr
	initbuttonset 	= snd initstore
	
// calculate buttons out of the set of next event that can occur

calcbuttons :: [PAE] -> [(Button, (PAE,[PAE]) -> (PAE,[PAE]))]
calcbuttons paes = reverse [(LButton size label,\pae -> nextstate number pae) \\ (PAE label number) <- paes]
where
	size = defpixel

	nextstate :: Int (PAE,[PAE]) -> (PAE,[PAE])
	nextstate eventnr (pae,_) 
	# (b,more) = calcnextevent pae eventnr
	| b = case more of
			Just npae = npae
			Nothing = done
	= done

	done = (PAE "Done!" 0,[])

// internal representation: a unique number is added to each possible next event that can occur 

:: PAE 	= PAE String Int				// possible event labeled with a unique number
		| (.+) infixl 6 PAE PAE			// process algebra +
		| (.>) infixl 5 PAE PA			// proces algebra "followed by"

derive gForm  PAE, PA, []
derive gUpd   PAE, PA, []
derive gPrint PAE, PA
derive gParse PAE, PA

calcnext :: PA -> (PAE,[PAE])			// numbers all possible events with a unique number (>= 0)
										// returns these events in a list to make buttons generating easier
calcnext pa 
# (pai,list,_) = (next` pa [] 0)
= (pai,list)
where
	next` :: PA [PAE]Int -> (PAE,[PAE],Int)
	next` (E s) list n = (ne, [ne:list],n+1) where ne = PAE s n
	next` (left .+. right) list n
	# (left,list,n)		= next` left list n
	# (right,list,n) 	= next` right list n
	= (left .+ right,list,n)  
	next` (left .>. right) list n
	# (left,list,n) 	= next` left list n
	= (left .> right,list,n)

calcnextevent :: PAE Int -> (Bool,Maybe (PAE,[PAE])) // calculates new expression given event i
calcnextevent pai i = search pai i
where
	search	(PAE s j) i = (i == j,Nothing)	// event found if i == j, no more to do here 
	search	(left .> right) i
	# (b,moreleft) = search left i
	| b = case moreleft of 
				Nothing 			-> (b, Just (calcnext right)) 			// continue with right
				Just (leftmore,set) -> (b, Just (leftmore .> right,set))	// not quite finished left  
	= (b,Nothing)	// event not found here
	search	(left .+ right) i
	# (b,moreleft) = search left i
	| b = (b,moreleft)
	= search right i
	


