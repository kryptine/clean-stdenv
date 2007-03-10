module delegate

import StdEnv, StdHtml

derive gForm [], Maybe
derive gUpd [], Maybe
derive gPrint Maybe
derive gParse Maybe
//derive gerda Maybe

npersons = 5

Start world = doHtmlServer (multiUserTask npersons (delegate mytask (Time 0 0 15))) world

mytask = editTask "Done" 0


delegate taskToDelegate time 
=						[Txt "Choose persons you want to delegate work to:",Br,Br] 
						?>>	determineSet [] =>> \set -> delegateToSet set
where
	delegateToSet set = newTask "delegateToSet" delegateToSet`
	where 
		delegateToSet`						
		  =					OrTasks [("Waiting", who @:: editTask "I Will Do It" Void #>> returnV who) \\ who <- set]
			=>> \who 	->	who @:: (timedTask time taskToDelegate)	
			=>> \(b,work)->	if b (returnV work) (delegateToSet set )

	determineSet set = newTask "determineSet" determineSet`
	where
		determineSet`	
		= 					[Txt ("Current set:" +++ print set)] 
							?>> ChooseTask	[("Add Person", cancelTask choosePerson =>> \nr  -> returnV nr)
											,("Finished",	returnV Nothing)
											]
			=>> \result -> case result of
							(Just new)  -> determineSet (sort (removeDup [new:set])) 
							Nothing		-> returnV set

		choosePerson =	editTask "Set" (PullDown (1,100) (0,[toString i \\ i <- [1..npersons]]))
						=>> \whomPD  -> returnV (Just (toInt(toString whomPD)))

		cancelTask task = OrTask (task,editTask "Cancel" Void #>> returnV createDefault)
		
		print [] = ""
		print [x:xs] = toString x +++ " " +++ print xs

	timedTask time task	= OrTask  	( OrTasks 	[ ("TimedTask",task =>> \value -> returnV (True,value))
												, ("Return", returnV (False,createDefault))
												] 
						 			, waitForTimerTask time #>> returnV (False,createDefault)
						  			)
