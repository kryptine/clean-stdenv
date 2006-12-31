module doTasks

import StdEnv, StdHtml

derive gForm 	Situation, []
derive gUpd 	Situation, []
derive gParse 	Situation
derive gPrint 	Situation
derive gerda 	Situation

Start world = doHtmlServer (multiUserTask 5 [] (repeatTask doTasks)) world

:: Situation = `Limit Int

doTasks
=								 	mkRDynTaskCall "boss-secr"   createDefault
	*>> \(forSecr,fromBoss)   	->	mkRDynTaskCall "secr-assist" createDefault 
	*>> \(forAssist,fromSecr) 	->	PmuTasks
										 [(0, boss forSecr)
										 ,(1, secretary fromBoss forAssist)
										 ,(2, assistent fromSecr)							
										 ]
	=>> \v						->	[Txt ("Result: " <+++ printToString v)]
									?>> STask "OK" Void
where
	boss forSecr
 	= 			[Txt "Define Limit!",Br,Br]
 				?>> STask "OK" 0
 		=>> \v ->	forSecr ( 	[Txt ("Limit = " <+++ v)]
 								?>> (STask "Set" 0) <| (\nv -> nv <= v && nv > 0, \_ -> "Value should not exceed " <+++ v))
 	secretary fromBoss forAssist
	= 			[Txt "Doing my own work as secretary:",Br,Br]
				?>> STask "Done" Void
		#>> 	[Txt "Time for doing some work for the boss:",Br,Br]
				?>> forAssist fromBoss 
	assistent fromSecr
	= 			[Txt "Doing my own work as assistent:",Br,Br]
				?>> STask "Done" Void
		#>>		[Txt "Time for doing some work for the secretary:",Br,Br]
				?>> fromSecr

