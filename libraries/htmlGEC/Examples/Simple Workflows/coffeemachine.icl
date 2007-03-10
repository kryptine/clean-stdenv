module coffeemachine

import StdEnv, StdHtml

Start world = doHtmlServer (singleUserTask (repeatTaskGC CoffeeMachine)) world

CoffeeMachine :: Task (String,Int)
CoffeeMachine  
=	 							[Txt "Choose product:",Br,Br] 
								?>>	ChooseTask
								 	[	("Coffee: 100", 	returnV (100,"Coffee"))
									,	("Cappucino: 150",	returnV (150,"Cappucino"))
									,	("Tee: 50",			returnV (50, "Tee"))
									,	("Choclate: 100",	returnV (100,"Choclate"))
									] 
	=>> \(toPay,product)	->	[Txt ("Chosen product: " <+++ product),Br,Br] 
								?>> getCoins (toPay,0)
	=>> \(cancel,returnMoney)->	let nproduct = if cancel "Cancelled" product 
								in
								[Txt ("product = " <+++ nproduct <+++ ", returned money = " <+++ returnMoney),Br,Br] 
								?>>	SeqTask "Thanks" (returnV Void)
	#>>							returnV (nproduct,returnMoney) 
where
	getCoins :: (Int,Int) -> Task (Bool,Int)
	getCoins (toPay,paid) = newTask "getCoins" getCoins`
	where
		getCoins` = [Txt ("To pay: " <+++ toPay),Br,Br] 
								?>>	OrTask	
									( 	ChooseTask [(toString i <+++ " cts", returnV (False,i)) \\ i <- [5,10,20,50,100,200]]
									, 	SeqTask "Cancel" (returnV (True,0))
									)
					=>> \(cancel,coin) ->	handleCoin (cancel,coin)

		handleCoin (cancel,coin)
		| cancel			= returnV (cancel,paid)
		| toPay - coin > 0 	= getCoins (toPay - coin,paid + coin)
		= returnV (cancel,coin - toPay)




