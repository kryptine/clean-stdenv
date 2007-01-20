module coffeemachine

import StdEnv, StdHtml

Start world = doHtmlServer (singleUserTask (repeatTask test12aPCs)) world

derive gForm []
derive gUpd []

test12orm = MCTask_ckbox [("oneMC",test12orp),("twoMC",test12orb)]

test12PC = PCTask2 (test12l 21, PCTask2 (test12l 31,test12l 41))
test12PCs = PCTasks [("21",test12l 21),("31", test12l 31),("41",test12orb)]
test12aPCs = PTask2 (test12l 21, PTask2 (test12l 31,test12l 41))
test12orp = CTask_pdmenu [("onePD",test12l 1),("twoPD",test12l 5)]
test12orb = CTask_button [("oneB",test12l 9),("twoB",test12l 13)]

test12l i = STasks [("oneS",test12 i),("twoS",test12 (i+2))]

test12 i = 				simpleTask i
				#>> 	simpleTask (i+1)

simpleTask i = STask "OK"  i


//Start world = doHtmlServer (singleUserTask (repeatTask CoffeeMachine)) world

CoffeeMachine :: Task (String,Int)
CoffeeMachine  
=	 							[Txt "Choose product:",Br,Br] 
								?>>	CTask_button
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
								?>>	STask_button "Thanks" (returnV Void)
	#>>							returnV (nproduct,returnMoney) 
where
	getCoins :: (Int,Int) -> Task (Bool,Int)
	getCoins (toPay,paid)
	= 							[Txt ("To pay: " <+++ toPay),Br,Br] 
								?>>	PCTask2	
									( 	CTask_button [(toString i <+++ " cts", returnV (False,i)) \\ i <- [5,10,20,50,100,200]]
									, 	STask_button "Cancel" (returnV (True,0))
									)
		=>> \(cancel,coin) ->	handleCoin (cancel,coin)
	where
		handleCoin (cancel,coin)
		| cancel			= returnV (cancel,paid)
		| toPay - coin > 0 	= mkTask (getCoins (toPay - coin,paid + coin))
		= returnV (cancel,coin - toPay)




