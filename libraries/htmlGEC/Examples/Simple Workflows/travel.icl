module travel

import StdEnv, StdHtml

derive gForm []
derive gUpd []

Start world = doHtmlServer (singleUserTask travel) world

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

Dsp = DisplayMode 
