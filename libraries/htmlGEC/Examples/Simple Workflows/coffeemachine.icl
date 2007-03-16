module coffeemachine

// (c) MJP 2007
//
// This is just a demo of a coffeemachine programmed with iTasks combinators
// The persistent variant will remember the state in which the coffee machine was left
// Garbage collection of unused tasks will be done automatically

import StdEnv, StdHtml

//Start world = doHtmlServer (singleUserTask (repeatTask_GC singleStepCoffeeMachine)) world
Start world = doHtmlServer (singleUserTask singleStepCoffeeMachine) world
//Start world = doHtmlServer (singleUserTask (repeatTask_GC CoffeeMachine)) world
//Start world = doHtmlServer (singleUserTask (repeatTask_GC CoffeeMachine <@ Persistent)) world

CoffeeMachine :: Task (String,Int)
CoffeeMachine  
=	[Txt "Choose product:",Br,Br] ?>>
	chooseTask
		[("Coffee: 100",    return_V (100,"Coffee"))
		,("Cappucino: 150", return_V (150,"Cappucino"))
		,("Tea: 50",        return_V (50, "Tea"))
		,("Chocolate: 100", return_V (100,"Chocolate"))
		] =>> \(toPay,product) ->	
	[Txt ("Chosen product: " <+++ product),Br,Br] ?>>
	getCoins (toPay,0) =>> \(cancel,returnMoney) ->	
	let nproduct = if cancel "Cancelled" product 
	in
	[Txt ("product = " <+++ nproduct <+++ ", returned money = " <+++ returnMoney),Br,Br] ?>>
	buttonTask "Thanks" 
	(return_V (nproduct,returnMoney))

getCoins :: (Int,Int) -> Task (Bool,Int)
getCoins (cost,paid) = newTask "getCoins" getCoins`
where
	coins			= [5,10,20,50,100,200]
	getCoins`		= [Txt ("To pay: " <+++ cost),Br,Br] ?>>
					  orTask
						( chooseTask [(c +++> " cents", return_V (False,c)) \\ c <- coins]
						, buttonTask "Cancel" (return_V (True,0))
						) =>> \(cancel,coin) -> 
					  if cancel        (return_V (cancel,   paid))
					 (if (cost > coin) (getCoins (cost-coin,paid+coin))
					                   (return_V (cancel,   coin-cost)))

singleStepCoffeeMachine :: Task (String,Int)
singleStepCoffeeMachine
=	[Txt "Choose product:",Br,Br] ?>>
	chooseTask
		[(p<+++": "<+++c, return_V prod) \\ prod=:(p,c)<-products]
	=>> \prod=:(p,c) -> 
	[Txt ("Chosen product: "<+++p),Br,Br] ?>>
	pay prod (buttonTask "Thanks" (return_V prod))
where
	products	= [("Coffee",100),("Tea",50)]
	
//	pay (p,c) t	= buttonTask ("Pay "<+++c<+++ " cents") t
	pay (p,c) t	= getCoins (c,0) =>> \(cancel,returnMoney) ->
				  let np = if cancel "cancelled" p
				  in  [Txt ("Product = "<+++np<+++". Returned money = "<+++returnMoney),Br,Br] ?>> t
