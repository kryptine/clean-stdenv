module loopGEC

import StdEnv
import StdGEC, StdGECExt, StdAGEC
import GecArrow

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world = goGui feedbackTest5 world  

loopTest1 = startCircuit (edit "edit" >>> loop (arr \(x, y) -> (x + 1, y + 1)) >>> display "display") 42

loopTest2 = startCircuit (edit "edit" >>> loop (first (edit "loop")) >>> display "display") 42

:: LoopTest3 = Reset | Higher | Not_Higher
derive gGEC LoopTest3

loopTest3 = startCircuit (edit "enter number (0 resets)" >>> loop (arr f) >>> display "number higher than all before?") 0
where
	f (x, xs)
		| x == 0 = (Reset, [])
		= (if (all ((>) x) xs) Higher Not_Higher, [x:xs])

loopTest4 = startCircuit (edit "reset" >>> counter >>> display "output") False
where
	counter = fix (second (arr inc >>> delay 0) >>> arr cond)
	cond (True, n) = 0
	cond (False, n) = n

loopTest5 = startCircuit (edit "reset" >>> counter >>> display "output") False
where
	counter // converted automatically from Paterson arrow notation
	  = (loop
	       (arr (\ (reset, out) -> (out, reset)) >>>
		  (first (arr (\ out -> out + 1) >>> delay 0) >>>
		     arr
		       (\ (next, reset) ->
			  case if reset /*then*/ 0 /*else*/ next of
			      out -> (out, out)))))

feedbackTest1 = startCircuit (self (edit "") ((+) 1)) 1
where
	self g f = feedback (g >>> arr f)

feedbackTest2 = startCircuit (self2 ((+) 100) (edit "") ((+) 1)) 0
where
	self2 f1 g f2 = feedback (arr f1 >>> g >>> arr f2)

feedbackTest3 = startCircuit (edit "input" >>> feedback (arr ((+) 1)) >>> display "output") 0

derive gGEC (,)

feedbackTest4 = startCircuit (edit "input" >>> feedback (first (arr ((+) 1))) >>> display "output") (0, 0)

feedbackTest5 = startCircuit (feedback ((edit "+1" >>> arr ((+) 1)) *** (edit "+100" >>> arr ((+) 100)))) (0, 0)
