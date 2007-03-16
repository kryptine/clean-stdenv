module delegate

import StdEnv, htmlTask, htmlTrivial


// (c) 2007 MJP

// Quite a difficult workflow exercise given to me by Erik Zuurbier.
// First a set of person id's is made to which a task can be delegated
// The task is actually shipped to the first person who accepts the task
// That person can stop the task whenever he wants
// Now again everybody in the set is asked again to accept the task
// The one who accepts can *continue* the work already done so far
// This process can be repeated as many times one likes until finally the task is finished

// When the timer goes, the whole process is repeated from scratch and the task performed so far is lossed.
// To make this work an additional combinator is needed. Work to do.

derive gForm [], Maybe
derive gUpd [], Maybe
derive gPrint Maybe

npersons = 5

//Start world = doHtmlServer (multiUserTask npersons (delegate mytask2 (Time 0 0 30))) world
Start world = doHtmlServer (multiUserTask npersons (test mytask2)) world

test :: (Task Int) -> Task Int
test task 		= newTask "test" (doit task)
where 
	
	doit t		= stop  task =>>  \t -> ifStopped t (\t -> orTask (test t, test t))

	doit2 task	= stop  task =>>  \t -> ifStopped t test

	userStop	= buttonTask "Stop" (return_V True) 				  			

	stop task	= sharedTask userStop task 

	ifStopped (True,TClosure task) alttask 	= alttask task
	ifStopped (_,   TClosure task) _ 		= task

mytask = editTask "Done" 0
mytask2 =			editTask "Done1" 0
		 =>> \v1 ->	editTask "Done2" 0
		 =>> \v2 ->	editTask "Done3" 0
		 =>> \v3 -> return_D (v1 + v2 + v3)

delegate :: (Task a) HtmlTime -> (Task a) | iData a
delegate taskToDelegate time 
=						[Txt "Choose persons you want to delegate work to:",Br,Br] 
						?>>	determineSet [] 
			=>> \set -> delegateToSomeone taskToDelegate set
			=>> \result -> return_D result
where
	delegateToSomeone :: (Task a) [Int] -> (Task a) | iData a
	delegateToSomeone task set = newTask "delegateToSet" doDelegate
	where 
		doDelegate						
		  =									orTasks [("Waiting for " <+++ who, who @:: buttonTask "I Will Do It" (return_V who)) \\ who <- set]
			=>> \who 						->	who @:: sharedTask stopIt task	
			=>> \(stopped,TClosure task)	->	if stopped (delegateToSomeone task set) task 
	
	userStop 		= buttonTask "Stop" (return_V True)					  			
	timerStop time	= waitForTimerTask time #>> return_V True
	
	stopIt = orTask (timerStop time,userStop)				  			
						  			
determineSet set = newTask "determineSet" determineSet`
where
	determineSet`	
	= 					[Txt ("Current set:" +++ print set)] 
						?>> chooseTask	[("Add Person", cancelTask choosePerson =>> \nr  -> return_V nr)
										,("Finished",	return_V Nothing)
										]
		=>> \result -> case result of
						(Just new)  -> determineSet (sort (removeDup [new:set])) 
						Nothing		-> return_V set

	choosePerson =	editTask "Set" (PullDown (1,100) (0,[toString i \\ i <- [1..npersons]]))
					=>> \whomPD  -> return_V (Just (toInt(toString whomPD)))

	cancelTask task = orTask (task,editTask "Cancel" Void #>> return_V createDefault)
	
	print [] = ""
	print [x:xs] = toString x +++ " " +++ print xs
						  			
