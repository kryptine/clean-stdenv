module calculatorGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import StdGecComb, basicAGEC, buttonGEC, calcAGEC

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	example_calc
 	world  

example_calc	= CGEC (selfGEC "Calculator" update_calc) calculator
where
	calculator	= 	zero  	   <|> 
					calc zero  <|> 
					horlistGEC buttons

	update_calc (mem <|> i <|> pressed) = (nmem <|> calc ni <|> horlistGEC buttons)
	where
		(nmem,ni)	= case whichopper (^^ pressed) operators of
							[] 		= (mem,^^ i)
							[f:_]	= (f mem (^^ i),zero)

	calc		= realcalcGEC			// to obtain a real calculator
//	calc		= intcalcGEC 			// to obtain an int calculator
	buttons		= [Button "+", Button "-", Button "*"]
	operators 	= [(+),(-),(*)]
	whichopper buttons operators = [x \\ (Pressed,x) <- (zip2 buttons operators)]

