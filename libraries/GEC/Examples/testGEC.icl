module testGEC

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
 	test4
 	world  



test1 = CGEC mycgec 23
test2 = CGEC (AGECtoCGEC "test" (CGECtoAGEC mycgec 27)) 23
test3 = CGEC (gecEdit "test") (CGECtoAGEC (selfGEC "self" inc) 22)
test4 = CGEC (gecEdit "test") { inout = (Edit 1,Display 1) , gec = gecDisplay "circuit"}

mycgec = (AGECtoCGEC "test" (counterGEC 0))
