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
 	test5
 	world  

// paper stuf

derive gGEC MyRecord, MyRecord2, MyEditRecord, MyEditRecord2

// basic example

:: MCounter a :== (a,UpDown)

updateCounter:: (MCounter a) -> (MCounter a) | IncDec a
updateCounter (n,UpPressed) 	= (n+one,UpPressed)
updateCounter (n,DownPressed) 	= (n-one,DownPressed)
updateCounter (n,any)	 	 	= (n,any)

:: MyRecord2 a = { rfield1::  a
				 , rfield2::  a
				 , rsum::  a
				 }

initRecord2 a b = { rfield1 =  a
				 , rfield2 =  b
				 , rsum =  a+b
				 }				 

updRecord :: (MyRecord2 a) -> (MyRecord2 a) | IncDec a
updRecord rec = { rec
			 	& rsum  = rec.rfield1 +  rec.rfield2
			 	}

test23 = CGEC (selfGEC "self" updRecord) (initRecord2 0 0)

// basic example with counters

:: MyEditRecord a = { efield1	:: MCounter a
					, efield2	:: MCounter a
					, esum		:: Mode a
					}

toMyEditRecord :: (MyRecord2 a) -> (MyEditRecord a)
toMyEditRecord edrec = 	{ efield1	= toCounter edrec.rfield1
						, efield2	= toCounter edrec.rfield2
						, esum		= toDisplay edrec.rsum
						}

fromMyEditRecord :: (MyEditRecord a) -> (MyRecord2 a) | IncDec a
fromMyEditRecord rec = 	{ rfield1	= fromCounter (updateCounter rec.efield1)
						, rfield2	= fromCounter (updateCounter rec.efield2)
						, rsum		= fromDisplay rec.esum
						}

test24 = CGEC (selfGEC "self" (toMyEditRecord o updRecord o fromMyEditRecord)) (toMyEditRecord (initRecord2 0 0))


toDisplay :: a -> Mode a
toDisplay a = Display a
fromDisplay :: (Mode a) -> a
fromDisplay (Display a) = a
toCounter :: a -> MCounter a
toCounter n = (n,Neutral)
fromCounter :: (MCounter a) -> a
fromCounter (n,_) = n

	
	
// basic example with AGECs
						
:: MyEditRecord2 a = { afield1	:: AGEC a
					, afield2	:: AGEC a
					, asum		:: AGEC a
					}

toMyEditRecord2 :: (MyRecord2 a) -> (MyEditRecord2 a) | IncDec a & gGEC {|*|} a 
toMyEditRecord2 edrec = 	{ afield1	= counterGEC edrec.rfield1
							, afield2	= counterGEC edrec.rfield2
							, asum		= idGEC edrec.rsum
							}

fromMyEditRecord2 :: (MyEditRecord2 a) -> (MyRecord2 a)
fromMyEditRecord2 rec = 	{ rfield1	= ^^ rec.afield1
							, rfield2	= ^^ rec.afield2
							, rsum		= ^^ rec.asum
							}

test25 = CGEC (selfGEC "self" (toMyEditRecord2 o updRecord o fromMyEditRecord2)) (toMyEditRecord2 (initRecord2 0 0))









//test5 = CGEC (gecEdit "test") { inout2 = In 1 , gec2 = gecDisplay "circuit"}

test0 = CGEC (gecEdit "test") mylist
where
	mylist :: AGEC [[Int]]
	mylist = tableGEC [list\\j <- list] where list = []
test1 = CGEC mycgec 23
test2 = CGEC (AGECtoCGEC "test" (CGECtoAGEC mycgec 27)) 23
test3 = CGEC (gecEdit "test") (CGECtoAGEC (selfGEC "self" inc) 22)
test4 = CGEC (gecEdit "test") { inout = (Edit 1,Display 1) , gec = gecDisplay "circuit"}

mycgec = (AGECtoCGEC "test" (counterGEC 0))


				
test6 = CGEC (selfGEC "self" incsum) 	{ field1	= counterGEC 0
							  			, field2	= counterGEC 0
							  			, sum 		= idGEC 0}
where
	incsum rec = {rec & sum = rec.sum ^= (^^ rec.field1 + ^^ rec.field2)}

			
test5 = CGEC (gecEdit "test") (counterCGEC 23)

:: MyRecord a =  { field1:: AGEC a
				 , field2:: AGEC a
				 , sum:: AGEC a }

counterCGEC n = CGECtoAGEC (%| ((\n -> (n,Neutral)) @| gecEdit "test" |@ updateCounter)) n
where
	updateCounter (n,UpPressed) 	= n+1
	updateCounter (n,DownPressed) 	= n-1
	updateCounter (n,any)	 	 	= n
			
				
