module loopGEC

import StdEnv
import StdGEC, StdGECExt, StdAGEC
import GecArrow

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world = goGui loopTest3 world  

loopTest1 = startCircuit (edit "edit" >>> loop (first (edit "loop")) >>> display "display") 42

loopTest2 = startCircuit (edit "edit" >>> loop (arr \(x, y) -> (x + 1, y + 1)) >>> display "display") 42

loopTest3 = startCircuit (edit "enter number (0 resets)" >>> loop (arr f) >>> display "number higher than all before?") 0
where
	f (x, xs) 
		| x == 0 = (True, [])
		= (all ((>) x) xs, [x:xs])

derive gGEC (,)
