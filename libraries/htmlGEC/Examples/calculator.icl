module calculator

// simple spreadsheet example
// (c) MJP 2005

import StdEnv
import StdHtml

Start world  = doHtml calculator world

calculator hst
# (fun,(calcb,hst)) 	= assignTableFuncBut calcbuttons hst		// shows buttons
# (_,(display,hst)) 	= mkStoreHGEC "display" fun initcalc hst	// calculates new values	
= (Head [ Hd_Title "Calculator"
		] 
		[ H1 "Calculator Example: "
		, display <||> calcb
		] ,hst)
where
	initcalc = (0 <|> 0)
	
	calcbuttons = 	[	[("7",set 7),	("8",set 8),	("9",set 9)	]
					,	[("4",set 4),	("5",set 5),	("6",set 6)	]
					,	[("1",set 1),	("2",set 2),	("3",set 3)	]
					,	[("0",set 0),	("C",clear) 				]
					,	[("+",app (+)),	("-",app (-)),	("*",app (*))]
					]
	where
		set 	i 	(t <|> b) = (t 		 <|> b*10 + i)
		clear 		(t <|> b) = (t 		 <|> 0)
		app		fun (t <|> b) = (fun t b <|> 0)
	
