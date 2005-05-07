module calculator

// simple spreadsheet example
// (c) MJP 2005

import StdEnv
import StdHtml

Start world  = doHtml arrowcalculator world
//Start world  = doHtml calculator world

calculator hst
# (calcfun,hst) 	= TableFuncBut "calcbut" Edit calcbuttons hst		// shows buttons
# (display,hst) 	= mkStoreForm "display" calcfun.value initcalc hst	// calculates new values	
= mkHtml "Calculator"
	[ H1 [] "Calculator Example: "
	, toBody display 
	, toBody calcfun
	] hst
where
	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags


arrowcalculator hst
# (calcfun,hst) 	= TableFuncBut "calcfun" Edit calcbuttons hst		// shows buttons
# (display,hst) 	= startCircuit circuit calcfun.value  hst	// calculates new values	
= mkHtml "Calculator" 
	[ H1 [] "Calculator Example: "
	, toBody display
	, toBody calcfun
	] hst
where
	circuit  =  store "display" initcalc 
	
	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags

initcalc = (0 <|> 0)

calcbuttons = 	[	[(but "7",set 7),	(but "8",set 8),	(but "9",set 9)	]
				,	[(but "4",set 4),	(but "5",set 5),	(but "6",set 6)	]
				,	[(but "1",set 1),	(but "2",set 2),	(but "3",set 3)	]
				,	[(but "0",set 0),	(but "C",clear) 				]
				,	[(but "+",app (+)),	(but "-",app (-)),	(but "*",app (*))]
				]
where
	set 	i 	(t <|> b) = (t 		 <|> b*10 + i)
	clear 		(t <|> b) = (t 		 <|> 0)
	app		fun (t <|> b) = (fun t b <|> 0)

but i = LButton (defpixel / 3) i
