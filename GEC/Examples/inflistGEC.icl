module inflistGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import StdGecComb, basicAGEC

// TO TEST JUST REPLACE THE EXAMPLE NAME IN THE START RULE WITH ANY OF THE EXAMPLES BELOW
// ALL EXAMPLES HAVE TO BE OF FORM pst -> pst

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	example_lists1
 	world  

example_lists1	= CGEC (mkGEC "InfiniteListDisplay") (listGEC False allprimes) 
where
	allprimes = sieve [2..]
	where
		sieve [x:xs] = [x : sieve  (filter x xs)]
		where
			filter x [y:ys] | y rem x == 0 = filter x ys
			| otherwise = [y: filter x ys]
	
example_lists2 = CGEC (applyGEC "Henks demo" (\n -> listGEC True (calcnum n))) 1 
where
	calcnum n | n <= 0 = [0]
	calcnum 1  	= 	[1]
	calcnum n
	| n rem 2 == 0 	= [n2 : calcnum n2 ]	with n2 = n / 2
	| otherwise 	= [n31: calcnum n31]	with n31 = (3 * n) + 1	

