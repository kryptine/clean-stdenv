module Algebra

import StdEnv, StdHtml

Start world  = doHtml MyPage2  world


// representation of a simple process algebra:

:: PA 	= (.+.) infixl 6 PA PA			// equivalent of process algebra +
		| (.>.) infixl 5 PA PA			// equivalent of proces  algebra "followed by"
		| E String						// atom, the string will be used as the label of the button
		
// some example expressions (they have to be finite)

//expr =  E "a" .+. (E "b1" .>. E "b2") .>. E "c" 
//expr =  E "a" .+. (E "a" .>. E "b") .>. E "c" 
expr = E "koffie" .+. E "thee" .+. E "chocmelk" .>. E "melk" .+. E "suiker" .>. E "klaar"

MyPage2 hst
# ((expr,exprbody),hst) 	 = mkEditForm  "expr" Edit (E "Init") hst
# ((pressed,donebutton),hst) = ListFuncBut False "but" Edit [(LButton defpixel "Done!",\b -> not b)] hst
# ((bool,_),hst)			 = mkStoreForm "store" pressed False hst
# (buttons,hst)				 = if bool (calcprocess expr hst) ([EmptyBody],hst)
= mkHtml "Process Algebra Experiment"
	[ H1 [] "Process Algebra Experiment"
	, Br, Br
	, exprbody
	, Br
	, BodyTag donebutton
	, Br , Br
	, BodyTag buttons
	] hst

MyPage hst
# (buttons,hst) = calcprocess expr hst
= mkHtml "Process Algebra Experiment"
	[ H1 [] "Process Algebra Experiment"
	, Br, Br
	, BodyTag buttons
	, Br
	] hst

// small utility stuf

mkHtml s tags hst 	= (Html (header s) (body tags),hst)
header s 			= Head [`Hd_Std [Std_Title s]] [] 
body tags 			= Body [] tags

// the program is still quite complicated:

// first the current set of buttons is read from store "butstore"
// this is used to re-create these "buttons"; funchosen is the function corresponding to the button pressed 
// in "store" the process algebra expression is stored: the new expression is calculated depending on the button pressed and stored 
// from the new expression the new set of buttons is calculated (nbutset); and stored in "butstore" for the next time
// this is used to calculate the new "buttons" to display
// the next time we start from the beginning

// internal representation: a unique number is added to each possible next event that can occur 

:: PAE 	= PAE String Int				// possible event labeled with a unique number
		| (.+) infixl 6 PAE PAE			// process algebra +
		| (.>) infixl 5 PAE PA			// proces algebra "followed by"

derive gForm  PAE, PA, []
derive gUpd   PAE, PA, []
derive gPrint PAE, PA
derive gParse PAE, PA
	
calcprocess :: PA *HSt -> *([BodyTag],*HSt)
calcprocess expr hst 
# ((butset,_),hst)		= mkStoreForm 		"butstore"  id initbuttonset hst
# ((funchosen,_),hst) 	= ListFuncBut False  "buttons"  Edit (calcbuttons butset) hst
# (((_,nbutset),_),hst)	= mkStoreForm 		"store"     funchosen initstore hst
# (_,hst)				= mkSetForm 		"butstore"  Display nbutset hst
# ((_,buttons),hst) 	= ListFuncBut True	"buttons"   Edit (calcbuttons nbutset) hst
= 	(buttons,hst)
where
	initstore 		= calcnext expr
	initbuttonset 	= snd initstore

	// calculate buttons out of the set of next event that can occur
	
	calcbuttons :: [PAE] -> [(Button, (PAE,[PAE]) -> (PAE,[PAE]))]
	calcbuttons paes = reverse [(LButton size (label +++ toString number),\pae -> nextstate number pae) \\ (PAE label number) <- paes]
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
		
	

