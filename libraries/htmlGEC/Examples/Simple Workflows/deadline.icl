module deadline

import StdEnv, StdHtml

derive gForm []
derive gUpd []

npersons = 5

Start world = doHtmlServer (multiUserTask npersons [] (deadline mytask)) world

mytask = STask "Press" 0

deadline :: (Task a) -> (Task a) | iData a
deadline task
=						[Txt "Choose person you want to shift work to:",Br,Br] 
						?>>	STask "Set" (PullDown (1,100) (0,[toString i \\ i <- [1..npersons]]))
	=>> \whomPD		->	[Txt "Until what time do you want to wait today?",Br,Br] 
						?>>	STask "SetTimer" (Time 0 0 0)
	=>> \time		->	[]
						?>> shifttask (toInt(toString whomPD)) time task
	=>> \(ok,value) ->	if ok [Txt ("Result of task: " +++ printToString value),Br,Br] [Txt "Task Expired, default value chosen !",Br,Br]
						?>> STask "OK" value

where
	shifttask who time task
		= 	(who,"Timed Task") 	
				@: 	PCTask2	
					(	waitForTimeTask time 								// wait for deadline
						#>> returnV (False,createDefault)					// return default value
					, 	[Txt ("Please finish task before" <+++ time),Br,Br]	// tell deadline
						?>> (task =>> \v -> returnV (True,v))				// do task and return its result
					) 


