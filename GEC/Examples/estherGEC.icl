module estherGEC

import StdEnv
import StdGEC, StdGECExt, StdAGEC, StdDynamicGEC
import StdGecComb
import StdDynamic

// TO TEST JUST REPLACE THE EXAMPLE NAME IN THE START RULE WITH ANY OF THE EXAMPLES BELOW
// ALL EXAMPLES HAVE TO BE OF FORM pst -> pst

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	test1
 	world  

testX = CGEC (gecEdit "test")  (dynamicGEC 23)

derive gGEC MyRecord, MyRecord2
				
:: MyRecord a =  { arg:: AGEC a
				 , fun:: AGEC (a -> a)
				 , fun_arg:: AGEC a 
				 }

test1 = CGEC (selfGEC "self" calcsum) 	{ arg	  = dynamicGEC 0
							  			, fun	  = dynamicGEC id
							  			, fun_arg = modeGEC (Display 0)}
where
	calcsum  rec = {rec & MyRecord.fun_arg = rec.MyRecord.fun_arg ^= (^^ rec.fun) (^^ rec.arg)}

:: MyRecord2 a =  { field1:: AGEC a
				  , field2:: AGEC a
				  , sum:: AGEC a 
				  }

test2 = CGEC (selfGEC "self" calcsum) 	{ field1	= counterGEC 0
							  			, field2	= dynamicGEC 0
							  			, sum 		= modeGEC (Display 0)}
where
	calcsum rec = {rec & MyRecord2.sum = rec.MyRecord2.sum ^= (^^ rec.field1 + ^^ rec.field2)}

			
				
