module travel

import StdEnv, StdHtml

derive gForm []
derive gUpd []

Start world = doHtmlServer (singleUserTask ( travel)) world

travel :: (Task Void)
travel 
= 			[Txt "Book your journey:",Br,Br]
			?>>	PCTask2
					( STasks 
						[	( "Choose Booking options:"
							, MCTask_ckbox	[ ("Book_Flight",BookFlight)
											, ("Book_Hotel", BookHotel)
											, ("Book_Car",   BookCar)
											]
							)
						, 	( "Confirm Booking:"
							, STask_button "Confirm" (returnV [])
							)
						]
					, STask_button "Cancel" (returnV [])
					)
	=>> \booking -> [Txt "Handling bookings:",Br,Br]
					?>> handleBookings booking
where
	handleBookings booking
	| isNil	booking	= 		STask "Cancelled" Void
	| otherwise		= 		STask "Pay" (Dsp (calcCosts booking))
					  #>>	STask "Paid" Void
	where
		calcCosts booked = sum [cost \\ (_,_,_,cost) <- hd booked]

		isNil [] = True
		isNil _ = False

	BookFlight  = STask "BookFlight" 	(Dsp "Flight Number","",Dsp "Costs",0) 	<<@ Submit
	BookHotel  	= STask "BookHotel" 	(Dsp "Hotel Name","",Dsp "Costs",0)		<<@ Submit
	BookCar  	= STask "BookCar" 		(Dsp "Car Brand","",Dsp "Costs",0)		<<@ Submit


Dsp = DisplayMode 
