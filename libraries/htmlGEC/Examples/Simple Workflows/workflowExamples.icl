module workflowExamples

import StdEnv, StdHtml

import htmlTask

derive gForm []
derive gUpd []



//Start world = doHtmlServer (mkflow Coffeemachine) world
//Start world = doHtmlServer (mkflow (requestTask 100)) world
// Start world = doHtmlServer (mkflow (RecordSongs ["song 1","song 2","song 3"])) world
//Start world = doHtmlServer (mkflow CreateMusic) world
//Start world = doHtmlServer (mkflow (Quotation myQuotation)) world
//Start world = doHtmlServer (mkflow travel) world
Start world = doHtmlServer (mkflow test2) world
where
	mkflow tasks hst 
	# (html,hst) = startTask tasks hst
	= mkHtml "test" html hst

test2 tst
# (tb,ta,tst)	= LazyTask "test" (mkTask test) tst
//# (a,tst) 		= ta tst
# (b,tst)		= PCTask2 ( STask "Klaar1" True, STask "Klaar2" True) tst
# (b,tst)		= PCTask2 ( STask "Klaar1" True, STask "Klaar2" True) tst
= (b,tst)

test tst
# (a,tst) = PTasks	[ ("travel",travel)
					, ("keuze2",STask "Gereed" "")
					]  tst
= STask "Klaar" a tst

:: EenOfAnder = Naam | Woonplaats 

AnalyseForm tst  
# (((_,naam),(_,woonplaats),geslacht),tst) 
			= STask "Vul in" ((Dsp "Naam:",""),(Dsp "Woonplaats",""),False) tst
# ((_,either),tst) = STask "Kies" (Dsp geslacht,Naam) tst
= returnTask (either,if (either == Naam) naam woonplaats) tst

derive gForm EenOfAnder
derive gUpd EenOfAnder
derive gParse EenOfAnder
derive gPrint EenOfAnder

instance == EenOfAnder
where
	(==) Naam Naam = True
	(==) Woonplaats Woonplaats = True
	(==) _ _ = False

// travel request

travel tst
# (booked,tst)= PCTask2
					( STasks 
						[	( "Choose Booking options"
							, MCTask_ckbox	[ ("Book_Flight",BookFlight)
											, ("Book_Hotel", BookHotel)
											, ("Book_Car",   BookCar)
											]
							)
						, 	( "Booking confirmation"
							, STask_button "Confirm" (returnV [])
							)
						]
					, STask_button "Cancel" (returnV [])
					) tst
| isNil	booked	= returnTask "Cancelled" tst
# (_,tst)		= STask "Pay" (Dsp (calcCosts booked)) tst
= returnTask "Paid" tst
where
	BookFlight tst 	= STask "BookFlight" 	(Dsp "Flight Number","",Dsp "Costs",0) tst
	BookHotel tst 	= STask "BookHotel" 	(Dsp "Hotel Name","",Dsp "Costs",0) tst
	BookCar tst 	= STask "BookCar" 		(Dsp "Car Brand","",Dsp "Costs",0) tst

	Pay booked bookings tst		= returnTask "OK" tst	

	calcCosts booked = sum [cost \\ (_,_,_,cost) <- hd booked]

// quotation example

:: QForm = { fromComp :: String
			, toComp :: String
			, startDate :: HtmlDate
			, endDate :: HtmlDate
			, estHours :: Int
			}
:: QState =  Submitted | Approved | Cancelled | Rework | Draft
			
derive gForm QForm, QState
derive gUpd QForm, QState
derive gParse QForm, QState
derive gPrint QForm, QState

myQuotation :: (QState,QForm)
myQuotation = createDefault

Quotation (state,form) tst
# ((_,form),tst) = STask "Submit" (Dsp state, form) tst
# ((_,form),tst) = STask "Review" (Dsp Submitted,form) tst
= CTask_button
	[ ("Rework",Quotation (Rework,form))
	, ("Approved",returnTask Approved)
	, ("Cancel",returnTask Cancelled)
	] tst

// coffee machine

Coffeemachine tst
# (_,tst) 				= returnTask "Choose Product" tst
# ((toPay,product),tst) = CTask_button
								 	[	("Coffee", returnTask (100,"Coffee"))
									,	("Cappucino",returnTask (150,"Cappucino"))
									,	("Thee",returnTask (50,"Thee"))
									,	("Chocolate",returnTask (100,"Chocolate"))
									] tst
# (returnMoney,tst) 	= getCoins (toPay,0) tst
= returnTask (product,returnMoney) tst
where
	getCoins (toPay,paid) tst
	# (coin,tst)		= CTask_button [(toString i <+++ " cts", returnTask i) \\ i <- [5,10,20,50,100]] tst
	| toPay - coin > 0 	= mkTask (getCoins (toPay - coin,paid + coin)) tst
	= returnV (coin - toPay) tst

// coffee machine, monadic style

Coffeemachine2 
= 	returnTask "Choose Product" `bind`
	\_ 					-> CTask_button 	
									[	("Coffee", returnTask (100,"Coffee"))
									,	("Cappucino",returnTask (150,"Cappucino"))
									,	("Thee",returnTask (50,"Thee"))
									,	("Chocolate",returnTask (100,"Chocolate"))
									] `bind`
	\(toPay,product) 	-> getCoins (toPay,0) `bind`
	\returnMoney 		-> returnTask (product,returnMoney) 
where
	getCoins (toPay,paid) 
	=	CTask_button [(toString i <+++ " cts", returnTask i) \\ i <- [5,10,20,50,100]] `bind`
		\coin			-> if (toPay - coin > 0)
								(mkTask (getCoins (toPay - coin,paid + coin)))
								(returnV (coin - toPay))

// Coffeemachine, more explicite control over html code

Coffeemachine3 tst
# tst 					= returnF [Txt "Choose Product:", Br] tst
# ((toPay,product),tst) = CTask_pdmenu 	[	("Coffee", returnTask (100,"Coffee"))
									,	("Cappucino",returnTask (150,"Cappucino"))
									,	("Thee",returnTask (50,"Thee"))
									,	("Chocolate",returnTask (100,"Chocolate"))
									] tst
# (returnMoney,tst) 	= getCoins (toPay,0) tst
= returnTask (product,returnMoney) tst
where
	getCoins (toPay,paid) tst
	# (coin,tst)		= CTask_pdmenu [(toString i <+++ " cts", returnTask i) \\ i <- [5,10,20,50,100]] tst
	| toPay - coin > 0 	= mkTask (getCoins (toPay - coin,paid + coin)) tst
	= returnTask (coin - toPay) tst

// microsoft order request example

requestTask budget tst
# (_,tst) 				= return "Start Request" budget tst
= requestTask` budget tst
where
	requestTask` budget tst
	# ((_,_,budget),tst) 	= handleRequest budget tst
	= CTask_pdmenu 	[	("More Requests?", requestTask` budget)
				,	("Stop",returnTask "End Request")
				] tst

	handleRequest budget tst
	# (n,tst) 		= STask "request" 0 tst
	| n > budget	= return "Not Authorized" budget tst
	= CTask_pdmenu
		[	("Approved", placeOrder budget n)
		,	("Not Approved",return "Not Approved" budget)
		] tst

	placeOrder budget n tst 
	# (_,tst) = STask "Submit" (Dsp ("Submit Order","price = ",n)) tst
	= return "Order Placed" (budget - n) tst

	return s b tst = returnTask (s,"budget = ",b)tst
	
// create music example

CreateMusic tst
# (_,tst) 	= returnTask "In Music" tst
# (_,tst) 	= STask "Decide" (Dsp "Make music") tst
# (_,tst) 	= PTask2 (audition,learn) tst
= returnTask "Out Music" tst
where
	audition tst = CTask_pdmenu [("Audition passed",returnTask True),("Audition failed",returnTask False)] tst
	learn tst = STask "Skill" 0 tst

// record songs example (vd aalst)

RecordSongs songs tst
# (_,tst) 	= returnTask "In Make Record" tst
= RecordSongs` songs [] tst
where
	RecordSongs` songs rsongs tst
	# (rsongs,tst) = ChooseSongs songs rsongs tst
	# (_,tst) 		= STask "Done" (Dsp "Record Songs") tst
	= CTask_pdmenu [ ("More recordings?", RecordSongs` songs rsongs)
			  , ("No More",Market rsongs)
			  ] tst

	ChooseSongs songs rsongs tst
	= CTask_pdmenu 
		([(s,ChooseSongs songs [s:rsongs]) \\ s <- songs] ++ [("Stop",returnTask rsongs)]) tst

	Market rsongs tst
	# (_,tst)	= STask "Send" (Dsp "to Market Dept") tst
	= returnTask ("Out Make Record",rsongs) tst



Dsp = DisplayMode 




