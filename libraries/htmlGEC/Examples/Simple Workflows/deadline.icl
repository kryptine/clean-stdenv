module deadline

import StdEnv, StdHtml

derive gForm []
derive gUpd []

npersons = 5

Start world = doHtmlServer (multiUserTask npersons (repeatTask (deadline mytask) )) world

mytask = STask "Press" 0

deadline :: (Task a) -> (Task a) | iData a
deadline task
=						[Txt "Choose person you want to delegate work to:",Br,Br] 
						?>>	STask "Set" (PullDown (1,100) (0,[toString i \\ i <- [1..npersons]]))
	=>> \whomPD		->	[Txt "Until what time do you want to wait today?",Br,Br] 
						?>>	STask "SetTime" (Time 0 0 0)
	=>> \time		->	[Txt "Cancel delegated work if you are getting impatient:",Br,Br]
						?>> PCTask2
								(	delegateTask (toInt(toString whomPD)) time task
								, 	STask_button "Cancel" (returnV (False,createDefault))
								)
	=>> \(ok,value) ->	if ok 	(	[Txt ("Result of task: " +++ printToString value),Br,Br] 
									?>> STask_button "OK" (returnV value)
								)
								(	[Txt "Task expired or canceled, you have to do it yourself!",Br,Br]
									?>>	STask_button "OK" task
								)
where
	delegateTask who time task
		= 	(who,"Timed Task") 	
				@: 	PCTask2	
					(	waitForTimeTask time 								// wait for deadline
						#>> returnV (False,createDefault)					// return default value
					, 	[Txt ("Please finish task before" <+++ time),Br,Br]	// tell deadline
						?>> (task =>> \v -> returnV (True,v))				// do task and return its result
					) 


