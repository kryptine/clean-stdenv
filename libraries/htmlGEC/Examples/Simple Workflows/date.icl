module date

import StdEnv, StdHtml

// findDate will settle a date and time between two persons that want to meet
// first a person is chosen by the person taken the initiative
// then a date is settled by the two persons by repeatedly asking each other for a convenient date
// if such a date is found both have to confirm the date and the task is finished

npersons = 5

Start world = doHtmlServer (multiUserTask npersons [] findDate) world

findDate tst
# (whomPD,tst)		= 	( 	[Txt "Choose person you want to date:",Br] 
						?>>	STask "Set" (PullDown (1,100) (0,[toString i \\ i <- [1..npersons]]))
					  	) 	tst
# whom				=	toInt(toString whomPD)
# (datetime,tst) 	= 	(	[Txt "Determining date:",Br,Br] 
						?>> findDate` whom (Date 1 1 2007,Time 9 0 0)
						) 	tst
# (me,tst)			=	myId tst
# (_,tst) 			= 	(	[]
						?>> PTask2 (confirm me whom datetime,confirm whom me datetime) 
						)	tst
= (datetime,tst)
where
	findDate` whom daytime tst
	# (daytime,tst) 	= proposeDateTime daytime tst
	# ((ok,daytime),tst)= ((whom,"Meeting Request") @: determineDateTime daytime) tst
	| ok				= returnV daytime tst
	# (ok,tst)			= askDateTime daytime tst
	| ok				= returnV daytime tst
	= mkTask (findDate` whom daytime) tst
	where
		proposeDateTime (date,time) tst
		# input 				= (showHtml [Txt "Propose date: "], date, showHtml [Txt "Propose time: "], time)
		# ((_,date,_,time),tst)	= STask "Set" input tst
		= ((date,time),tst)

		determineDateTime daytime tst
		# (ok,tst)		= askDateTime daytime tst
		| ok			= returnV (ok,daytime) tst
		# (daytime,tst) = proposeDateTime daytime tst
		= returnV (ok,daytime) tst

		askDateTime (date,time) tst
		= ( [Txt ("Can we meet on the " <+++ date <+++ " at " <+++ time <+++ "?"),Br] ?>>
			CTask_button [("Accept",returnV True),("Sorry",returnV False)]
		  ) tst

	confirm me you (date,time) tst
	= 	(	me @::
			(	[Txt ("Person " <+++ me <+++ " and person " <+++ you <+++ " will meet on " <+++ date <+++ " at " <+++ time),Br,Br] 
				?>>  STask "OK" Void
			)
		) 	tst
		
		
		




