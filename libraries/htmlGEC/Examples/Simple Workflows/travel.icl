module travel

import StdEnv, StdHtml

derive gForm []
derive gUpd []

Start world = doHtmlServer (singleUserTask (repeatTask travel)) world

travel :: (Task Void)
travel 
= 			[Txt "Book your journey:",Br,Br]
			?>>	orTask
					( seqTasks 
						[	( "Choose Booking options:"
							, mchoiceTasks	[ ("Book_Flight",BookFlight)
											, ("Book_Hotel", BookHotel)
											, ("Book_Car",   BookCar)
											]
							)
						, 	( "Confirm Booking:"
							, seqTask "Confirm" (returnV [])
							)
						]
					, seqTask "Cancel" (returnV [])
					)
	=>> \booking -> [Txt "Handling bookings:",Br,Br]
					?>> handleBookings booking
where
	handleBookings booking
	| isNil	booking	= 		editTask "Cancelled" Void
	| otherwise		= 		editTask "Pay" (Dsp (calcCosts booking))
					  #>>	editTask "Paid" Void
	where
		calcCosts booked = sum [cost \\ (_,_,_,cost) <- hd booked]

		isNil [] = True
		isNil _ = False

	BookFlight  = editTask "BookFlight" 	(Dsp "Flight Number","",Dsp "Costs",0) 	<<@ Submit
	BookHotel  	= editTask "BookHotel" 	(Dsp "Hotel Name","",Dsp "Costs",0)		<<@ Submit
	BookCar  	= editTask "BookCar" 		(Dsp "Car Brand","",Dsp "Costs",0)		<<@ Submit


Dsp = DisplayMode 
