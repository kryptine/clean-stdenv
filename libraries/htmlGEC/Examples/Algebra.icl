module Algebra

import StdEnv, StdHtml

Start world  = doHtml MyPage3  world

derive gForm  PAI, PAExpr, []
derive gUpd   PAI, PAExpr, []
derive gPrint PAI, PAExpr
derive gParse PAI, PAExpr

// representation of a simple process algebra:

:: PAProgr 
	= PAProgr [PADef] PAExpr			// collection of process algebra definitions with initial expression to evaluate

:: PADef 
	= (.=.) infixl 4  String PAExpr		// assigns a name to a definition

:: PAExpr
	= (.+.) infixl 6 PAExpr PAExpr		// equivalent of process algebra +
	| (.>.) infixl 5 PAExpr PAExpr		// equivalent of proces  algebra "followed by"
	| Do  String						// search for definition with indicated name and apply it 
	| But String						// make a button with indicated name

// some example expressions (they have to be finite)

myprogram 
	= PAProgr 
		["products" 	.=. But "coffee" .+. But "thee" .+. But "chocmilk"
		,"ingrediants" 	.=. Do "wantmilk" .>. Do "wantsugar" 
		,"wantmilk" 	.=. But "milk" .+. But "nomilk"
		,"wantsugar" 	.=. But "sugar" .+. But "nosugar"
		,"start" 		.=. Do "products" .>. Do "ingrediants" .>. Do "start"] (Do "start")

MyPage3 hst
# (buttons,hst) = calcprogramme myprogram hst
= mkHtml "Process Algebra Experiment"
	[ H1 [] "Process Algebra Experiment"
	, Br, Br
	, BodyTag buttons
	, Br
	] hst


		
// internal representation: a unique number is added to each possible next event that can occur 

:: PAI 	= PABut String Int				// possible event labeled with a unique number
		| (.+) infixl 6 PAI PAI			// process algebra +
		| (.>) infixl 5 PAI PAExpr		// proces algebra "followed by"

calcprogramme :: PAProgr *HSt -> *([BodyTag],*HSt)
calcprogramme (PAProgr paDefs initialExpr) hst  = calcExpression paDefs initialExpr hst



calcExpression :: [PADef] PAExpr *HSt -> *([BodyTag],*HSt)
calcExpression exprstore initialExpr hst 
# (nbutset,hst)		= expressionStore id hst								// retrieve the expression and the set of buttons to generate  
# (funchosen,hst) 	= createButtons (calcbuttons (snd nbutset.value)) hst 	// create the buttons to see which is pressed
# (nbutset,hst)		= expressionStore funchosen.value hst					// calculate the new expressions and the new set of buttons 
# (buttons,hst) 	= createButtons (calcbuttons (snd nbutset.value)) hst 	// create the buttons for the next round
= 	(buttons.form,hst)
where
	initstore		= calcnext initialExpr
	initbuttonset 	= snd initstore

	createButtons :: [(Button, (PAI,[PAI]) -> (PAI,[PAI]))] *HSt -> (Form ((PAI,[PAI]) -> (PAI,[PAI])),*HSt)
	createButtons butdef hst = ListFuncBut False (nFormId "PAbuttons") butdef hst

	expressionStore :: ((PAI,[PAI]) -> (PAI,[PAI])) *HSt -> (Form (PAI,[PAI]),*HSt)
	expressionStore f hst = mkStoreForm (nFormId "PAexprstore") initstore f hst

	// calculate buttons out of the set of next event that can occur
	
	calcbuttons :: [PAI] -> [(Button, (PAI,[PAI]) -> (PAI,[PAI]))]
	calcbuttons paes = reverse [(LButton size label,\pae -> nextstate number pae) \\ (PABut label number) <- paes]
	where
		size = defpixel
	
		nextstate :: Int (PAI,[PAI]) -> (PAI,[PAI])
		nextstate eventnr (pae,_) 
		# (b,more) = calcnextevent pae eventnr
		| b = case more of
				Just npae = npae
				Nothing = done
		= done
	
		done = (PABut "Done!" 0,[])
	
	calcnext :: PAExpr -> (PAI,[PAI])		// numbers all possible events with a unique number (>= 0)
											// returns these events in a list to make buttons generating easier
	calcnext pa 
	# (pai,list,_) = (next` pa [] 0)
	= (pai,list)
	where
		next` :: PAExpr [PAI] Int -> (PAI,[PAI],Int)
		next` (But s) list n = (ne, [ne:list],n+1) where ne = PABut s n
		next` (left .+. right) list n
		# (left,list,n)		= next` left list n
		# (right,list,n) 	= next` right list n
		= (left .+ right,list,n)  
		next` (left .>. right) list n
		# (left,list,n) 	= next` left list n
		= (left .> right,list,n)
		next` (Do searchname) list n
		# def = [expr \\ foundname .=. expr <- exprstore | foundname == searchname]
		| isEmpty def = (ne, [ne:list],n+1) with ne = PABut "DefNotFounf" n
		| otherwise = next` (hd def) list n
	
	calcnextevent :: PAI Int -> (Bool,Maybe (PAI,[PAI])) // calculates new expression given event i
	calcnextevent pai i = search pai i
	where
		search	(PABut s j) i = (i == j,Nothing)	// event found if i == j, no more to do here 
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

expr = But "koffie" .+. But "thee" .+. But "chocmelk" .>. But "melk" .+. But "suiker" .>. But "klaar"

MyPage2 hst
# (exprf,hst) 	 	= mkEditForm  (nFormId "expr") (But "Init") hst
# (donebut,hst) 	= ListFuncBut False (nFormId "but") [(LButton defpixel "Done!",\b -> not b)] hst
# (boolstore,hst)	= mkStoreForm (nFormId "boolstore") False donebut.value  hst
# (buttons,hst)		= if boolstore.value (calcExpression [] exprf.value hst) ([EmptyBody],hst)
= mkHtml "Process Algebra Experiment"
	[ H1 [] "Process Algebra Experiment"
	, Br, Br
	, if boolstore.value (toHtml exprf.value) (toBody exprf)
	, Br
	, toBody donebut
	, Br , Br
	, BodyTag buttons
	] hst

MyPage hst
# (buttons,hst) = calcExpression [] expr hst
= mkHtml "Process Algebra Experiment"
	[ H1 [] "Process Algebra Experiment"
	, Br, Br
	, BodyTag buttons
	, Br
	] hst

