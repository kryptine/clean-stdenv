module estherGEC

import StdEnv
import StdGEC, StdGECExt, StdAGEC, StdDynamicGEC
import StdGecComb
import StdDynamic
import EstherInterFace, basicAGEC

// TO TEST JUST REPLACE THE EXAMPLE NAME IN THE START RULE WITH ANY OF THE EXAMPLES BELOW
// ALL EXAMPLES HAVE TO BE OF FORM pst -> pst

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	test4
 	world  

//testX = CGEC (gecEdit "test")  (dynamicGEC 23)

derive gGEC MyRecord, MyRecord2, T
				
:: MyRecord a =  { arg:: AGEC a
				 , fun:: AGEC (a -> a)
				 , fun_arg:: AGEC a 
				 }

/*test1 = CGEC (selfGEC "self" calcsum) 	{ arg	  = dynamicGEC 0
							  			, fun	  = dynamicGEC id
							  			, fun_arg = modeGEC (Display 0)}
where
	calcsum  rec = {rec & MyRecord.fun_arg = rec.MyRecord.fun_arg ^= (^^ rec.fun) (^^ rec.arg)}
*/
:: MyRecord2 a =  { field1:: AGEC a
				  , field2:: AGEC a
				  , sum:: AGEC a 
				  }

/*test2 = CGEC (selfGEC "self" calcsum) 	{ field1	= counterGEC 0
							  			, field2	= dynamicGEC 0
							  			, sum 		= modeGEC (Display 0)}
where
	calcsum rec = {rec & MyRecord2.sum = rec.MyRecord2.sum ^= (^^ rec.field1 + ^^ rec.field2)}
*/
:: T a b = C a | B Int Real b a
/* PA: uit arren moede in commentaar gezet omdat de compiler anders crasht...
:: Tdyn :== T DynString DynString

fromDynString :: a DynString -> a | TC a
fromDynString _ (DynStr (v::a^) _) = v
fromDynString v _ = v

toDynString :: a  -> DynString | TC a & toString a
toDynString v = DynStr (dynamic v) (toString v)

mapFDS dynstr i r = gMap {|* -> * -> *|} (fromDynString i) (fromDynString r) dynstr
mapTDS tab = gMap {|* -> * -> *|} toDynString toDynString tab

mapFDS2 dynstr  = gMap {|* -> * |}  (^^)  dynstr
mapTDS2 tab = gMap {|* -> * |} counterGEC  tab

//	mapFDS dynstr i r = gMap {|* -> * -> *|} (^^) (fromDynString r)  dynstr
//	mapTDS tab = gMap {|* -> * -> *|} counterGEC toDynString  tab

//	mapFDS dynstr i r = gMap {|* -> * -> *|} (^^) (^^)  dynstr
//	mapTDS tab = gMap {|* -> * -> *|} counterGEC dynamicGEC  tab
*/
import GenMap
derive gMap T
/*
test3 = CGEC (selfGEC "self" calc) (mapTDS init,init)	
where
	init = B 3 4.5 22 5
	
//	calc :: (T (AGEC Int) DynString, T Int Bool) -> (T (AGEC Int) DynString, T Int Bool)
	calc (dyn,val) =   (ndyn, nval) 
	where
		ndyn = mapTDS nval
		nval = mapFDS dyn 5 22
*/

:: MyRecord3 a b c =  	{ val1::  a
				  		, val2::  b
				  		, res ::  c 
				  		}
				  		
derive gMap MyRecord3
derive gGEC MyRecord3

test4 = CGEC (selfGEC "self" convert) (mapTo init)	
where
	init = 	{ val1	= 0.0
			, val2	= 0.0
			, res 	= 0.0}
	
	calcsum rec = {rec & MyRecord3.res = rec.val1 + rec.val2}
		
	convert = mapTo o calcsum o mapFrom

	mapFrom agec  	= gMap {|* -> * -> * -> *|} (^^) (^^) (^^) agec
	mapTo val 		= gMap {|* -> * -> * -> *|} counterGEC dynamicGEC (modeGEC o Display) val


:: Counter a = Counter (AGEC a)

test5 = CGEC (selfGEC "self" id) (mkAGECs (counterGEC) (dynamicGEC) init)	
where
	init :: (T Int Real)
	init = 	C 19
	
	convert = mapTo o id o mapFrom

	mapFrom agec  	= gMap {|* -> * -> *|} (^^) (^^) agec
	mapTo val 		= gMap {|* -> * -> *|} counterGEC dynamicGEC  val
	
//mkAGECs :: (AGEC a,a) (AGEC b,b) (t a b) ->  (t a b)
mkAGECs  ageca agecb tab
=	(mkAGEC (mkBimapGEC  
					(\a _ -> mapTo a) 
			    	(mapTo o mapFrom) 
			    	mapFrom 
			     	tab
			     ) "mkAGECs")
where
	mapFrom agec  	= gMap {|* -> * -> *|} (^^) (^^) agec
	mapTo val 		= gMap {|* -> * -> *|} ageca agecb  val


:: X a = X a
derive gGEC X

//test7 = CGEC (selfGEC "self" convert2) (mapto2 init) //(mapTo init)	
test7 = CGEC (selfGEC "self" mapto2) init //(mapTo init)	
where
	init = 	X (3,(idGEC [1..3]))

	mapto2 (X (n,list`)) = if (isEven (length list)) (X (n,(idGEC  (mytest list)))) 
												 (X (n,(horlistGEC (mytest list))))
	where
		list = ^^ list`											

	convert2 list = list
	
	mytest [x:xs] = [x+1,x:xs]
	mytest [] = [1]
		
	convert = mapTo o mytest o mapFrom

	mapFrom agec  	= gMap {|* -> * |} (^^) agec
	mapTo val 		= gMap {|* -> * |} horlistGEC val
