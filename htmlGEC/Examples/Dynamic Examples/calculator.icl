module calculator

// simple spreadsheet example
// (c) MJP 2005

import StdEnv
import StdHtml

//Start world  = doHtmlServer arrowcalculator world
Start world  = doHtmlServer simple world
//Start world  = doHtmlServer temp world

simple hst
//# (fun,hst)		= mkEditForm (pDFormId "fun") ((+) 1) hst	// calculates new values	
# (display,hst)	= mkEditForm (pDFormId "xdisplay") 44 hst	// calculates new values	
= mkHtml "Calculator"
	[ H1 [] "Calculator Example: "
	, toBody display 
	] hst

import dynamic_string

temp hst
# (butfun,hst) 	= TableFuncBut (nFormId "calcbut") incbut hst		// shows buttons
# (display,hst)	= mkStoreForm (nFormId "display") 0 butfun.value  hst	// calculates new values	
= mkHtml "Calculator"
	[ H1 [] "Calculator Example: "
	, toBody display 
	, toBody butfun 
	] hst
where
	incbut = [[(LButton (defpixel / 3) "+1",inc)]]

calculator hst
# (calcfun,hst) 	= TableFuncBut (nFormId "calcbut") calcbuttons hst		// shows buttons
# (display,hst) 	= mkStoreForm (nFormId "display") initcalc calcfun.value  hst	// calculates new values	
= mkHtml "Calculator"
	[ H1 [] "Calculator Example: "
	, toBody display 
	, toBody calcfun
	] hst

arrowcalculator hst
# (calcfun,hst) 	= TableFuncBut (nFormId "calcfun") calcbuttons hst		// shows buttons
# (display,hst) 	= startCircuit circuit calcfun.value  hst	// calculates new values	
= mkHtml "Calculator" 
	[ H1 [] "Calculator Example: "
	, toBody display
	, toBody calcfun
	] hst
where
	circuit  =  store (nFormId "display") initcalc 
	
initcalc = (0 <|> 0)

calcbuttons = 	[	[(but "7",set 7),	(but "8",set 8),	(but "9",set 9)	]
				,	[(but "4",set 4),	(but "5",set 5),	(but "6",set 6)	]
				,	[(but "1",set 1),	(but "2",set 2),	(but "3",set 3)	]
				,	[(but "0",set 0),	(but "C",clear),    (but "CA",cla) ]	
				,	[(but "+",app (+)),	(but "-",app (-)),	(but "*",app (*))]
				,   [(but "^2",app2 (*))]
				]
where
	set 	i 	(t <|> b) = (t 		 <|> b*10 + i)
	clear 		(t <|> b) = (t 		 <|> 0)
	cla 		(t <|> b) = (0 		 <|> 0)
	app		fun (t <|> b) = (fun t b <|> 0)
	app2    fun (t <|> b) = (fun t t <|> 0)
	but i = LButton (defpixel / 3) i

