module combinatorTest

import StdEnv, StdHtml

// (c) MJP 2007

// Just a scratch file to test the different combinators

// Known bugs 
// -> an andTask should skip to add html code of the task being selected by the user

Start world = doHtmlServer (multiUserTask 9 (/*repeatTask*/ simpleMile)) world

simpleMile = show( andTasks_mstone [("task " <+++ i,simple) \\ i <- [0..2]])

// the following obscure tasks have been tested succesfully

// milestones

// andTask tests
simpleAnd 	= show( andTasks [("task " <+++ i,simple) \\ i <- [0..3]])
myAndTasks2 = andTasks [("MyTask " <+++ i,
						("W" <+++ i,i) @: (editTask ("OK " <+++ i) i =>> \v -> 
						("B1",0) @: editTask ("OK " <+++ v) v))
						\\ i <- [0..3]]
			=>> \val -> return_D val
myAndTasks = andTasks ([("MyTask " <+++ i,editTask ("OK " <+++ i) i) \\ i <- [0..3]] ++ 
					 [("Special",1 @:: orTasks [("Temp",editTask "SpecOK" 4),("Temp2",2 @:: editTask "SpecOK" 4)])])
			=>> \val -> return_D val

// orTasks tests
simpleOr 	= show( orTasks [("task " <+++ i,simple) \\ i <- [0..3]])
myOrTasks = orTasks ([("MyTask " <+++ i,editTask ("OK " <+++ i) i) \\ i <- [0..3]] ++ 
					 [("Special",1 @:: orTasks [("Temp",editTask "SpecOK" 4),("Temp2",2 @:: editTask "SpecOK" 4)])])
			=>> \val -> return_D val
myOrTasks2 = orTasks [("MyTask " <+++ i,
						("W" <+++ i,i) @: (editTask ("OK " <+++ i) i =>> \v -> 
						("B1",0) @: editTask ("OK " <+++ v) v))
						\\ i <- [0..3]]
			=>> \val -> return_D val

// multi user tests
mysingletest 		= simple_mu 0 1 (simple_mu 1 0 simple)
myduotest 			= duo 0 (duo 1 simple)
duo i task 			= show (simple_mu 0 i task) #>> show (simple_mu 1 i task)
simple_mu n i task 	= ("MyTask " <+++ n, i) @: task

// super simple editors
show task			= task =>> \v -> return_D v
simple  			= editTask "OK" 0
simple2 n 			= [Txt "Fill in integer value:"] ?>> editTask "OK" n

// stop Editors test
myStop 				= stopMe simple
stopMe v 			= newTask "Oeps" (stopTask v =>> \(stopped,TClosure v) -> if stopped (stopMe v) v)




derive gForm []
derive gUpd []

test 
= 	
	andTask
			(	("number 1",1) @: (editTask "Set" 1 =>> \v -> editTask "Set" (v,0))
			, 	("number 2",1) @: (editTask "Set" 2 =>> \v -> editTask "Set" (v,0))
			)
	=>> \((a1,a2),(b1,b2)) -> return_V (a1+b1,a2+b2)
