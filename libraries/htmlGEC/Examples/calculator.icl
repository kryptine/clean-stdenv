module calculator

// simple spreadsheet example
// (c) MJP 2005

import StdEnv
import StdHtml

Start world  = doHtml arrowcalculator world

calculator hst
# ((fun,calcb),hst) 	= assignTableFuncBut calcbuttons hst		// shows buttons
# ((_,display),hst) 	= mkStoreHGEC "display" fun initcalc hst	// calculates new values	
= (Head [ Hd_Title "Calculator"
		] 
		[ H1 "Calculator Example: "
		, display 
		, calcb
		] ,hst)
where
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

	but i = CHButton 10 i
	
arrowcalculator hst
# ((fun,calcb),hst) 	= assignTableFuncBut calcbuttons hst		// shows buttons
# ((_,display),hst) 	= startCircuit circuit fun  hst	// calculates new values	
= (Head [ Hd_Title "Calculator"
		] 
		[ H1 "Calculator Example: "
		, Body display
		, calcb
		] ,hst)
where
	circuit  =  store "display" initcalc 

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

	but i = CHButton (defpixel / 3) i
