module timersGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import StdGecComb

// TO TEST JUST REPLACE THE EXAMPLE NAME IN THE START RULE WITH ANY OF THE EXAMPLES BELOW
// ALL EXAMPLES HAVE TO BE OF FORM pst -> pst

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	example_timer1
 	world  

example_timer1 = CGEC (selfGEC "TickTack" clock) (myclock,0<->0<->0)
where
	clock (tick,min<->59<->9) 		= clock (tick,min+1<->0<->0)
	clock (tick,min<->secs<->9) 	= clock (tick,min<->secs+1<->0)
	clock (tick,min<->secs<->msecs) = (tick,min<->secs<->msecs+1)

myclock = Timed (\i -> 100) 100 

example_timer2 = CGEC (selfGEC "TickTack" thisone) (myclock,(2,Hide allprimes))
where
	thisone (tick,(p,Hide [x:xs])) = (tick,(x,Hide xs))

allprimes = sieve [2..]
where
	sieve [x:xs] = [x : sieve  (filter x xs)]
	where
		filter x [y:ys] | y rem x == 0 = filter x ys
		| otherwise = [y: filter x ys]
