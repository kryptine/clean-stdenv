implementation module support

import StdHtml, StdGeneric

derive gForm 	[]
derive gUpd 	[]

// generated stuf

generic gCollect a :: a [String] -> [String]

gCollect{|Int|} x accu						= accu
gCollect{|Real|} x accu						= accu
gCollect{|Char|} x accu						= accu
gCollect{|Bool|} x accu						= accu
gCollect{|String|} x accu					= accu
gCollect{|PAIR|} fx fy (PAIR x y) accu 		= (fy y (fx x accu)) 
gCollect{|EITHER|} fl fr (LEFT x) accu 		= fl x accu
gCollect{|EITHER|} fl fr (RIGHT x) accu		= fr x accu
gCollect{|CONS of t|} f (CONS x) accu		
| t.gcd_type_def.gtd_name%(0,1) == "X_"		= f x [t.gcd_name:accu]
= f x accu
gCollect{|FIELD|} f (FIELD x) accu			= f x accu
gCollect{|OBJECT|} f (OBJECT x) accu		= f x accu
gCollect{|UNIT|} x accu						= accu
gCollect{|{}|} f s accu						= accu

testUnique:: [String] -> (String,Bool)
testUnique [] = ("",True) 
testUnique [x:xs]
| isMember x xs = ("Chosen value " +++ x +++ " must be unique but has already been used",False) 
= testUnique xs

Alert :: ((Bool,String) -> (Bool,String)) -> (*HSt -> *((Form (Bool,String)),*HSt))
Alert fun = mkStoreForm (Init,sdFormId "alert" (False,"")) fun


	

