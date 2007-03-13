module coffeemachine

// (c) MJP 2007
//
// This is just a demo of a coffeemachine programmed with iTasks combinators
// The persistent variant will remember the state in which the coffee machine was left
// Garbage collection of unused tasks will be done automatically

import StdEnv, StdHtml

Start world = doHtmlServer (singleUserTask (repeatTask_GC CoffeeMachine)) world
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
	seqTask "Thanks" (return_V Void) #>>
	return_V (nproduct,returnMoney)
where
	getCoins :: (Int,Int) -> Task (Bool,Int)
	getCoins (toPay,paid) = newTask "getCoins" getCoins`
	where
		getCoins` = [Txt ("To pay: " <+++ toPay),Br,Br] ?>>
					orTask
						( chooseTask [(c +++> " cts", return_V (False,c)) \\ c <- coins]
						, seqTask "Cancel" (return_V (True,0))
						) =>> \(cancel,coin) ->
					handleCoin (cancel,coin)
		coins     = [5,10,20,50,100,200]
		handleCoin (cancel,coin)
		| cancel			= return_V (cancel,paid)
		| toPay - coin > 0 	= getCoins (toPay - coin,paid + coin)
		| otherwise			= return_V (cancel,coin - toPay)
