module calculatorGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import GecArrow, basicAGEC, buttonAGEC, calcAGEC, dynamicAGEC, tupleAGEC

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	example_calc3
 	world  

example_calc	= startCircuit (feedback (edit "Calculator" >>@ update_calc)) calculator
where
	calculator	= 	zero  	   <|> 
					calc zero  <|> 
					horlistAGEC buttons

	update_calc (mem <|> i <|> pressed) = (nmem <|> calc ni <|> horlistAGEC buttons)
	where
		(nmem,ni)	= case whichopper (^^ pressed) operators of
							[] 		= (mem,^^ i)
							[f:_]	= (f mem (^^ i),zero)

	calc		= realcalcAGEC			// to obtain a real calculator
//	calc		= intcalcAGEC 			// to obtain an int calculator
	buttons		= [Button buttonWidth "+", Button buttonWidth "-", Button buttonWidth "*"]
	operators 	= [(+),(-),(*)]
	whichopper buttons operators = [x \\ (Pressed,x) <- (zip2 buttons operators)]

:: ButtonEditor 	:== [(String,AGEC (Int Int -> Int))]
:: MyButtonFuns 	:== ([Button],[Int Int -> Int])

:: MoreOrLess = AddOneMore | DeleteOneMore | EndOfList

derive gGEC MoreOrLess

derive generate Button, MoreOrLess

calcEditor	= startCircuit (designButtons >>@ convert >>> myCalculator) init
where
	init:: ButtonEditor
	init = [("+",dynamicAGEC (+))]

	designButtons ::  GecCircuit ButtonEditor ButtonEditor
	designButtons =  feedback (toDesignButtons
								 @>> edit "design buttons" 
								 >>@ fromDesignButtons)
	
	toDesignButtons :: ButtonEditor -> (<|> (AGEC ButtonEditor) MoreOrLess)							 
	toDesignButtons list = vertlistAGEC list <|> EndOfList 							 

	fromDesignButtons :: (<|> (AGEC ButtonEditor) MoreOrLess) -> ButtonEditor 							 
	fromDesignButtons (list <|> AddOneMore) = (^^ list) ++ init							 
	fromDesignButtons (list <|> DeleteOneMore) = (^^ list) % (0,length (^^ list) - 2)							 
	fromDesignButtons (list <|> _) = (^^ list)							 


	convert :: ButtonEditor -> MyCalculatorType 
	convert editbuttons =  initCalculator 0 0 mybuttons
	where
		mybuttons :: MyButtonFuns
		mybuttons = unzip [(Button buttonWidth string,^^ fun)\\ (string,fun) <- editbuttons]

:: MyCalculatorType :== ((<|> Int (<|> (AGEC Int) (AGEC [Button]))),AGEC MyButtonFuns)

initCalculator :: Int Int MyButtonFuns -> MyCalculatorType
initCalculator mem ival (mybuttons,myfunctions) 
	= (mem <|> 
	   intcalcAGEC ival <|>
	   horlistAGEC mybuttons, hidAGEC (mybuttons,myfunctions))		

//myCalculator :: GecCircuit myCalculatorType myCalculatorType // BUG
myCalculator :: GecCircuit ((<|> Int (<|> (AGEC Int) (AGEC [Button]))),AGEC MyButtonFuns) ((<|> Int (<|> (AGEC Int) (AGEC [Button]))),AGEC MyButtonFuns)
myCalculator =  feedback (edit "calculator" >>@ updateCalculator)
							 
//updateCalculator :: myCalculatorType -> myCalculatorType // BUG 
//updateCalculator :: ((<|> Int (<|> (AGEC Int) (AGEC [Button]))),AGEC MyButtonFuns) -> ((<|> Int (<|> (AGEC Int) (AGEC [Button]))),AGEC MyButtonFuns)
updateCalculator((mem <|> i <|> buttons),butsfun) = initCalculator nmem ni (^^ butsfun)
where
	(nmem,ni)	= case whichopper (^^ buttons) fun of
						[] 		= (mem,^^ i)
						[f:_]	= (f mem (^^ i),0)
	fun =  snd (^^ butsfun)

	whichopper buttons operators = [x \\ (Pressed,x) <- (zip2 buttons operators)]

example_calc3	= startCircuit (feedback (edit "Calculator" >>@ updateCalculator)) mybuttons
where
	mybuttons = initCalculator 0 0 (buttons,operators)

	buttons		= [Button buttonWidth "+", Button buttonWidth "-", Button buttonWidth "*"]
	operators 	= [(+),(-),(*)]

buttonWidth	:== defCellWidth / 3	// Place three buttons in a single cell
