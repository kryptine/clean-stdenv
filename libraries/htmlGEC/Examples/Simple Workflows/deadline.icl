module deadline

import StdEnv, StdHtml

derive gForm []
derive gUpd []

npersons = 5

Start world = doHtmlServer (multiUserTask npersons [] (deadline mytask)) world

mytask = STask "Press" 0

deadline task tst
# (whomPD,tst)		= 	( 	[Txt "Choose person you want to shift work to:",Br,Br] 
						?>>	STask "Set" (PullDown (1,100) (0,[toString i \\ i <- [1..npersons]]))
					  	) 	tst
# who				=	toInt(toString whomPD)
# (time,tst)		= 	( 	[Txt "Until what time do you want to wait today?",Br,Br] 
						?>>	STask "SetTimer" (Time 0 0 0)
					  	) 	tst
# ((ok,value),tst) = 	(	[]
						?>> shifttask who time task 
						)	tst
# (_,tst) 			= 	(	if ok [Txt ("Result of task: " <+++ value),Br,Br] [Txt "Task Expired !",Br,Br]
						?>> STask "OK" Void
						)	tst
= (value,tst)
where
	shifttask who time task tst
		= ((who,"Timed Task") 	
				@: 	PCTask2	
					(	waitForTimeTask time 								// wait for deadline
						#>> returnV (False,createDefault)					// return default
					, 	[Txt ("Please finish task before" <+++ time),Br,Br]	// tell deadline
						?>> (task =>> \v -> returnV (True,v))				// do task and return its result
					) 
			) tst



