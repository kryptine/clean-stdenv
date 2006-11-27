module workflowExamples

import StdEnv, StdHtml

derive gForm []
derive gUpd []

//Start world = doHtmlServer (multiUser (Quotation myQuotation)) world
//Start world = doHtmlServer (multiUser twotasks3) world


Start world = doHtmlServer (multiUser twotasks3) world
where
	singleUser tasks hst 
	# (_,html,hst) = startTask 0 tasks hst
	= mkHtml "stest" html hst

	multiUser tasks hst 
	# (idform,hst) 	= FuncMenu (Init,nFormId "pdm_chooseWorker" 
							(0,[("User " +++ toString i,\_ -> i) \\ i<-[0..5] ])) hst
	# currentWorker	= snd idform.value
	# (_,html,hst) 	= startTask currentWorker (persistent tasks) hst
	= mkHtml "mtest" [idform.form <=> html] hst
	where
		persistent tasks tst
		# tst	= setTaskAttribute Persistent tst
//		# tst	= setTaskAttribute StaticDynamic tst
//		# tst	= setTaskAttribute Database tst
		= tasks tst


list tst
# (a,tst) = appIData (vertlistFormButs 1 True (Init,pFormId "list0" [0])) tst
# (a,tst) = ((1,"Control List)") @: appIData (vertlistFormButs 1 True (Init,pFormId "list1" a))) tst
# (a,tst) = returnTask a tst
= (a,tst)

testEenTwee tst
# (v,tst) = STasks  [ ("een", (1,"number1") @: simple 1 =>> \t -> returnTask t)
					, ("twee",(2,"number2") @: simple 2 =>> \t -> returnTask t)
					] tst
= STask "Klaar"  (sum v) tst


simple n  = STask "OK" n 

infTask a tst
# (_,tst) = STask "Update" Void tst
| False = returnV a tst
= mkTask (infTask a) tst

testTime tst
# (time,tst) = STask "SetTimer" (Time 0 0 0) tst
# ((ok,estimation),tst) = PCTask2	( waitForTimeTask time #>> returnV (False,0)
									, (1,"Estimation") @: returnTask time #>> (STask "Confirm" 0 =>> \t -> returnV (True,t)) 
									) tst
| ok	= (estimation,returnF [Txt ("Received estimation is " <+++ estimation)] tst)
= mkTask testTime tst

:: Situation = Difficult Int | Easy

twotasks3 tst
# ((forSecr,fromBoss),tst) 		= mkRDynTaskCall "boss-secr"  0 tst		// split name task
# ((forAssist,fromSecr),tst) 	= mkRDynTaskCall "secr-assist" 0 tst		// split name task
= PTasks
	 [( "boss", STask "Choose" Easy `bind` 
	 			\situation ->  forSecr ((1,"Do Job ") @: taskForSecr situation forAssist) `bind`
	 			\result ->  STask "accept" result
	  )
	 ,("secretary", fromBoss)
	 ,("assistent", fromSecr)							
	 ] tst
where
	taskForSecr Easy forAssist tst
	# tst = returnF [Txt ("Handle easy case")] tst
	= forAssist ((2,"Specify damage") @: STask "Damage" 0) tst
	taskForSecr (Difficult upperbound) _ tst
	# tst = returnF [Txt ("Handle difficult case with limit " +++ (toString upperbound) +++ " Euro's")] tst
	= checktask upperbound tst
	where
		checktask limit  tst
		# (amount,tst) = STask "Damage" 0 tst
		| amount > limit
			# tst = returnF [Txt ("amount " +++ toString amount +++ " exceeds limit set")] tst
			= mkTask (checktask limit) tst
		= returnTask amount tst



twotasks2 tst
# ((tboss,tsecr),tst) 		= mkRDynTaskCall "name" 0 tst		// split name task
= PTasks
	 [( "secretary", tsecr)							// assign name task
	 ,( "boss", STask "Choose" Easy `bind` 
	 			\situation ->  tboss (handle situation) `bind`
	 			\result ->  STask "accept" result
	  )
	 ,( "boss2", STask "Choose" Easy `bind` 
	 			\situation ->  tboss (handle situation) `bind`
	 			\result ->  STask "accept" result
	  )
	 ] tst
where
	handle Easy tst
	# tst = returnF [Txt ("Handle easy case")] tst
	= STask "Damage" 0 tst
	handle (Difficult upperbound) tst
	# tst = returnF [Txt ("Handle difficult case with limit " +++ (toString upperbound) +++ " Euro's")] tst
	= checktask upperbound tst
	where
		checktask limit  tst
		# (amount,tst) = STask "Damage" 0 tst
		| amount > limit
			# tst = returnF [Txt ("amount " +++ toString amount +++ " exceeds limit set")] tst
			= mkTask (checktask limit) tst
		= returnTask amount tst
	

derive gForm Situation
derive gUpd Situation
derive gParse Situation
derive gPrint Situation
derive gerda Situation

:: RecForm = {name :: String, number:: Int}


twotasks tst
# ((tbname,tname),tst) 		= mkRTask "name"   ((1,"give name") @: STask "name" "") tst		// split name task
# ((tbnumber,tnumber),tst)	= mkRTask "number" ((2,"geive number") @: STask "number" 0) tst		// split number task
= PTasks
	 [( "employee1", tname `bind` void)							// assign name task
	 ,( "employee2", tnumber `bind` void)						// assign number task
	 ,( "boss"
	  , (PTask2
	  		(tbname,			// ask for name and
	  		 tbnumber))			// ask for number in any order
		`bind` 										// construct record form
			\(name,number) -> returnTask {name = name, number = number}
		`bind` \_ -> STask_button "klaar" (returnV Void)  
	  )
	 ] tst
where
	void _ tst = returnV Void tst


optelTaak tst
# (a,tst) = STask "waarde1" 0 tst
# (b,tst) = STask "waarde2" 0 tst
# (c,tst) = returnTask (a+b) tst
| c > 1000	= returnTask c tst
= mkTask optelTaak tst

:: Single a = Single a
derive gForm Single
derive gUpd Single
derive gParse Single
derive gPrint Single
derive gerda Single

instance toString (a,b) | toString a & toString b
where
	toString (a,b) = "(" <+++ a <+++ "," <+++ b <+++ ")"

agenda2 = \tst -> agenda` 0 (Date 0 0 0,Time 0 0 0) tst
where
	agenda` who daytime tst
	# (daytime,tst) 	= askDateTime daytime tst
	# (whoPd,tst)		= STask "AskPerson" (PullDown (1,100) (who,[toString i \\ i <- [0..10]])) tst
	# ((ok,daytime),tst)= ((toInt (toString whoPd),"Meeting Request") @: handle daytime) tst
	| ok				
		# tst			= returnF [Txt "Accepted",Br] tst
		= returnTask daytime tst
	# tst				= promptDateTime daytime tst
	# (ok,tst)			= CTask_button [("Accept",returnV True),("Sorry",returnV False)] tst
	| ok				= returnV daytime tst
	= mkTask (agenda` (toInt(toString whoPd)) daytime) tst
	where
		handle daytime tst
		# tst 			= promptDateTime daytime tst
		# (ok,tst)		= CTask_button [("Accept",returnV True),("Sorry",returnV False)] tst
		| ok			= returnV (ok,daytime) tst
		# (daytime,tst) = askDateTime daytime tst
		= returnV (ok,daytime) tst

		askDateTime (date,time) tst
		# input = (showHtml [Txt "Meeting Date: "], date, showHtml [Txt "Meeting Time: "], time)
		# ((_,date,_,time),tst) = STask "Set" input tst
		= ((date,time),tst)
		
		promptDateTime (date,time) tst
		= returnF [Txt ("Can we meet on the " <+++ date <+++ " at " <+++ time <+++ "?"),Br]  tst	
		
		
//agenda :: (Task Bool)
agenda = \tst -> agenda` (PullDown (1,300) (0,[toString i \\ i <- [0..10]]) ) tst
//agenda = \tst -> agenda` (PullDown (1,30) (0,[toString i \\ i <- [0..10]]) ) tst
where
	agenda` date tst
	# ((voorstel,acceptatie),tst) = mkRTaskCall "agenda" date datumbrief tst
	# (afspraak,tst)			= PTasks
								 [( "antwoorder"
								  ,	acceptatie `bind` 
								  		 \t -> returnTask t
								  )
								 ,( "vrager"
								  , STask "kiesDatum" date `bind` 
								  	\t -> voorstel t `bind`
								  	afhandelen								  	
								  )
								 ] tst
	# [(_,date):_]	= afspraak
	# ok = or (map fst afspraak)
	| ok 	= returnTask date tst 
	= mkTask (agenda` date) tst
	where
		afhandelen (True,datum) tst = returnTask (True,datum) tst
		afhandelen (False,datum) tst
		# (_,tst) = returnTask datum tst
		# (ok,tst) = CTask_button [("Accoord",returnV True),("NietAccoord",returnV False)] tst
		= returnTask (ok,datum) tst


		datumbrief date tst
		# tst = returnF [Txt "voorgestelde datum:",Br]  tst	
		# (_,tst) = returnTask date tst						// laat voorgestelde datum zien
		= CTask_button 	[ ("geaccepteerd", returnTask (True,date))
						 , ("afgewezen",   STask "kiesDatum" date `bind` \date -> returnTask (False,date))
						 ] tst
test2 tst
# ((tboss,tsecr),tst)	= mkRTask "travel" travel tst
# (result,tst)			= PTasks
							 [( "secretary"
							  ,	PTask2 (tsecr `bind` \t -> returnTask t,  tsecr `bind` \t -> returnTask t)
							  )
							 ,( "boss"
							  , PTask2 (tboss `bind` \t -> returnTask t, tsecr `bind` \t -> returnTask t)
							  )
							 ] tst
= returnTask result tst 
where
	working s tst = CTask_button [(s+++"Working",working s),(s+++"Done",returnTask "done")] tst
	
:: EenOfAnder = Naam | Woonplaats 

AnalyseForm tst  
# (((_,naam),(_,woonplaats),geslacht),tst) 
			= STask "Vul in" ((Dsp "Naam:",""),(Dsp "Woonplaats",""),False) tst
# ((_,either),tst) = STask "Kies" (Dsp geslacht,Naam) tst
= returnTask (either,if (either == Naam) naam woonplaats) tst

derive gForm EenOfAnder, RecForm
derive gUpd EenOfAnder, RecForm
derive gParse EenOfAnder, RecForm
derive gPrint EenOfAnder, RecForm
derive gerda EenOfAnder, RecForm

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

	isNil [] = True
	isNil _ = False

// quotation example

:: QForm = { fromComp 	:: String
			, toComp 	:: String
			, startDate :: HtmlDate
			, endDate 	:: HtmlDate
			, estHours 	:: Int
			}
:: QState =  Submitted | Approved | Cancelled | Rework | Draft
			
derive gForm QForm, QState
derive gUpd QForm, QState
derive gParse QForm, QState
derive gPrint QForm, QState
derive gerda QForm, QState

myQuotation :: (QState,QForm)
myQuotation = createDefault

Quotation (state,form) tst
# ((_,form),tst) = ((1,"Quotation") @: STask "Submit" (Dsp state, form))    tst
# ((_,form),tst) = ((2,"Review")    @: STask "Review" (Dsp Submitted,form)) tst
# (_,tst) = returnTask form tst
= CTask_button
	[ ("Rework",Quotation (Rework,form))
	, ("Approved",returnTask Approved)
	, ("Cancel",returnTask Cancelled)
	] tst

// coffee machine

CoffeeMachineInf :: *TSt -> (Int,*TSt)
CoffeeMachineInf tst
# (_,tst) = Coffeemachine tst
= mkTask CoffeeMachineInf tst

Coffeemachine tst
# (_,tst) 				= returnTask "Choose Product" tst
# ((toPay,product),tst) = CTask_button
								 	[	("Coffee", returnTask (100,"Coffee"))
									,	("Cappucino",returnTask (150,"Cappucino"))
									,	("Thee",returnTask (50,"Thee"))
									,	("Chocolate",returnTask (100,"Chocolate"))
									] tst
# ((cancel,returnMoney),tst) = getCoins (toPay,0) tst
| cancel				= returnTask ("Cancelled",returnMoney) tst
= returnTask (product,returnMoney) tst
where
	getCoins (toPay,paid) tst
	# ((cancel,coin),tst)= PCTask2 	( CTask_button [(toString i <+++ " cts", returnTask (False,i)) \\ i <- [5,10,20,50,100,200]]
									, STask_button "Cancel" (returnV (True,0))
									) tst
	| cancel			= returnV (cancel,paid) tst
	| toPay - coin > 0 	= mkTask (getCoins (toPay - coin,paid + coin)) tst
	= returnV (cancel,coin - toPay) tst

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




