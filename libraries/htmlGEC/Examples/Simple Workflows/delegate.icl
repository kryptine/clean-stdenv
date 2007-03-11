module delegate

import StdEnv, StdHtml

derive gForm [], Maybe
derive gUpd [], Maybe
derive gPrint Maybe
derive gParse Maybe
//derive gerda Maybe

npersons = 5

Start world = doHtmlServer (multiUserTask npersons (delegate mytask (Time 0 0 15))) world
//Start world = doHtmlServer (multiUserTask npersons strange) world


strange = 					chooseTask [("een",returnV (TClosure mytask)),("twee",returnV (TClosure mytask))]
			=>> \(TClosure task)  ->	task   


mytask = editTask "Done" 0
mytask2 =			editTask "Done" 0
		 =>> \v1 ->	editTask "Done" 0
		 =>> \v2 -> returnV (v1 + v2)

delegate taskToDelegate time 
=						[Txt "Choose persons you want to delegate work to:",Br,Br] 
						?>>	determineSet [] 
			=>> \set -> delegateToSet set
where
	delegateToSet set = newTask "delegateToSet" delegateToSet`
	where 
		delegateToSet`						
		  =					orTasks [("Waiting", who @:: editTask "I Will Do It" Void #>> returnV who) \\ who <- set]
			=>> \who 	->	who @:: (timedTask time taskToDelegate)	
			=>> \(b,work)->	if b (returnV work) (delegateToSet set )

	determineSet set = newTask "determineSet" determineSet`
	where
		determineSet`	
		= 					[Txt ("Current set:" +++ print set)] 
							?>> chooseTask	[("Add Person", cancelTask choosePerson =>> \nr  -> returnV nr)
											,("Finished",	returnV Nothing)
											]
			=>> \result -> case result of
							(Just new)  -> determineSet (sort (removeDup [new:set])) 
							Nothing		-> returnV set

		choosePerson =	editTask "Set" (PullDown (1,100) (0,[toString i \\ i <- [1..npersons]]))
						=>> \whomPD  -> returnV (Just (toInt(toString whomPD)))

		cancelTask task = orTask (task,editTask "Cancel" Void #>> returnV createDefault)
		
		print [] = ""
		print [x:xs] = toString x +++ " " +++ print xs

	timedTask time task	= orTask  	( orTasks 	[ ("TimedTask",task =>> \value -> returnV (True,value))
												, ("Return", returnV (False,createDefault))
												] 
						 			, waitForTimerTask time #>> returnV (False,createDefault)
						  			)
