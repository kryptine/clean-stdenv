module doTasks

import StdEnv, StdHtml

derive gForm 	Situation, []
derive gUpd 	Situation, []
derive gParse 	Situation
derive gPrint 	Situation
derive gerda 	Situation

Start world = doHtmlServer (multiUserTask 5 (repeatTask doTasks)) world

:: Situation = `Limit Int

simpleTask 	= STask "Done" 0
Boss 		= 0
Secretary	= 1
Assistent	= 2

doTasks
=								 	mkRDynTaskCall "boss-secr"   0
	*>> \(forSecr,fromBoss)   	->	mkRDynTaskCall "secr-assist" 0 
	*>> \(forAssist,fromSecr) 	->	PmuTasks
										 [(Boss, 		bossWork forSecr)								
										 ,(Secretary, 	doWork simpleTask (forAssist fromBoss))
										 ,(Assistent, 	doWork simpleTask fromSecr)							
										 ]
	=>> \v						->	[Txt ("Result: " <+++ printToString v)]
									?>> STask "OK" Void
where
	bossWork forSecr
 	= 				[Txt "Define Limit!",Br,Br]
 					?>> STask "OK" 0
 		=>> \v ->	forSecr (taskToDelegate v)

 	taskToDelegate v
 	= 	[Txt ("Limit = " <+++ v)]
 		?>> (STask "Set" 0) <| (\nv -> nv <= v && nv > 0, \_ -> "Value should not exceed " <+++ v)

 	doWork task delegatedtask
	= 	PCTasks	[	("MyOwnWork", repeatTask task)
				,	("Delegated", delegatedtask)
				]



