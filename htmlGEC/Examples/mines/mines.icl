module mines

// simple spreadsheet example
// (c) MJP 2005

import StdEnv
import StdHtml

derive gForm [],Mode
derive gUpd [], Mode
derive gParse Mode
derive gPrint Mode

//Start world  = doHtml mines world
Start world  = doHtmlServer mines world

:: MineField :== [[(Mode,Bool)]]

nrow 	= 10
ncol 	= 5
rows n 	= [0..n-1]

mines hst
# (mineField,hst)		= mineFieldStore id hst						// fetch mineField state from store
# (click,hst) 			= mineFieldButtons mineField.value hst		// determine which field is pressed
# (hit,nmines)			= click.value (False,mineField.value)		// calculate new mineField state
# (mineField,hst)		= mineFieldStore (const nmines) hst 		// store new state
# (buttons,hst) 		= mineFieldButtons mineField.value hst		// calculate new buttons
= mkHtml "Mines"
	[ H1 [] "Mines Example: "
	, toBody buttons
	, Br
	, Txt (if hit "You have hitted a mine" "You lucky bastard")
	, Br
	] hst

mineFieldStore f hst 		= mkStoreForm (nFormId "minestore") (initmines nrow ncol) f hst 
where
	initmines :: Int Int -> MineField // minefield clicked, mine present
	initmines nrow ncol = 	[	[(Edit,i rem 5 == 0) \\	j <- rows ncol] 
							\\  i <- rows nrow
							]

mineFieldButtons mines hst	= TableFuncBut2 False (nFormId "calcbut") (calcbuttons nrow ncol mines) hst
where
	calcbuttons :: Int Int MineField -> [[(Mode,Button, (Bool,MineField) -> (Bool,MineField))]]
	calcbuttons nrow ncol mines = 	[ [ let (mode,isMine) = mines!!i!!j in
										(mode, mbut mode isMine, click i j) \\ j <- rows ncol ]
									\\  i <- rows nrow	
									]					
	where
		click :: Int Int (Bool,MineField) -> (Bool,MineField)
		click i j (_,mines) = (snd (mines!!i!!j), [[nmines i j \\ j <- rows ncol] \\ i <- rows nrow])
		where
			nmines p q 
			| p==i && j==q = (Display,snd (mines!!p!!q))
			|otherwise	= mines!!p!!q
			
		mbut Edit _ 		= LButton (defpixel / 3) "?"
		mbut Display  False = LButton (defpixel / 3) ""
		mbut Display  True 	= LButton (defpixel / 3) "x"
		
		
