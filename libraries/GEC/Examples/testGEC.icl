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
 	test25
 	world  

testbetsy = CGEC (selfGEC "self" mytestje) (mydata 5)
where
	mydata i = i <|> horlistGEC (repeatn i i)
	mytestje (i <|> _) = mydata i


test12 = CGEC (gecEdit "Editor") (hidGEC 1)

// paper stuf

derive gGEC MyRecord, MyRecord2, MyEditRecord, MyEditRecord2

// basic example

:: MCounter a :== (a,UpDown)

updateCounter:: (MCounter a) -> (MCounter a) | IncDec a
updateCounter (n,UpPressed) 	= (n+one,UpPressed)
updateCounter (n,DownPressed) 	= (n-one,DownPressed)
updateCounter (n,any)	 	 	= (n,any)

:: MyRecord2 a = { value1::  a
				 , value2::  a
				 , sum   ::  a
				 }

initRecord2 a b = { value1 =  a
				  , value2 =  b
				  , sum    =  a+b
				  }				 

updRecord :: (MyRecord2 a) -> (MyRecord2 a) | IncDec a
updRecord rec = { rec
			 	& MyRecord2.sum  = rec.value1 +  rec.value2
			 	}

test23 = CGEC (selfGEC "self" updRecord) (initRecord2 0 0)

// basic example with counters

:: MyEditRecord a = { edvalue1	:: MCounter a
					, edvalue2	:: MCounter a
					, edsum		:: Mode a
					}

toMyEditRecord :: (MyRecord2 a) -> (MyEditRecord a)
toMyEditRecord edrec = 	{ MyEditRecord |
						  edvalue1	= toCounter edrec.MyRecord2.value1
						, edvalue2	= toCounter edrec.MyRecord2.value2
						, edsum		= toDisplay edrec.MyRecord2.sum
						}

fromMyEditRecord :: (MyEditRecord a) -> (MyRecord2 a) | IncDec a
fromMyEditRecord rec = 	{ value1	= fromCounter (updateCounter rec.MyEditRecord.edvalue1)
						, value2	= fromCounter (updateCounter rec.MyEditRecord.edvalue2)
						, sum		= fromDisplay rec.MyEditRecord.edsum
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
						
:: MyEditRecord2 a = { edvalue1	:: AGEC a
					 , edvalue2	:: AGEC a
					 , edsum	:: AGEC a
					 }

toMyEditRecord2 :: (MyRecord2 a) -> (MyEditRecord2 a) | IncDec a & gGEC {|*|} a 
toMyEditRecord2 edrec = 	{ MyEditRecord2 |
							  edvalue1	= idGEC edrec.MyRecord2.value1
							, edvalue2	= idGEC edrec.MyRecord2.value2
//							, edvalue2	= doubleCounterGEC edrec.MyRecord2.value2
//							, edvalue2	= idGEC edrec.MyRecord2.value2
							, edsum		= modeGEC (Display edrec.MyRecord2.sum)
							}


fromMyEditRecord2 :: (MyEditRecord2 a) -> (MyRecord2 a)
fromMyEditRecord2 rec = 	{ value1	= ^^ rec.MyEditRecord2.edvalue1
							, value2	= ^^ rec.MyEditRecord2.edvalue2
							, sum		= ^^ rec.MyEditRecord2.edsum
							}

test25 = CGEC (selfGEC "self" (toMyEditRecord2 o updRecord o fromMyEditRecord2)) 
												(toMyEditRecord2 (initRecord2 22 23))


doubleCounterGEC :: a -> AGEC a | IncDec a & gGEC {|*|} a 
doubleCounterGEC a = mkAGEC    { toGEC   = toEditRec //\arg _ -> toMyEditRecord21 (toMyData arg)
							, fromGEC = fromMyData o fromMyEditRecord2
							, updGEC  = toMyEditRecord21 o updRecord o fromMyEditRecord2
							, value   = a
							}
							where 
							 toMyData n 	= initRecord2 zero n
							 toEditRec arg Undefined = toMyEditRecord21 (toMyData arg)
							 toEditRec arg (Defined oarg) = oarg
							 fromMyData rec = rec.MyRecord2.sum

toMyEditRecord21 :: (MyRecord2 a) -> (MyEditRecord2 a) | IncDec a & gGEC {|*|} a 
toMyEditRecord21 edrec = 	{ MyEditRecord2 |
							  edvalue1	= counterGEC edrec.MyRecord2.value1
							, edvalue2	= counterGEC edrec.MyRecord2.value2
							, edsum		= modeGEC (Display edrec.MyRecord2.sum)
							}




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
	incsum rec = {rec & MyRecord.sum = rec.MyRecord.sum ^= (^^ rec.field1 + ^^ rec.field2)}

			
test5 = CGEC (gecEdit "test") (counterCGEC 23)

:: MyRecord a =  { field1:: AGEC a
				 , field2:: AGEC a
				 , sum:: AGEC a }

counterCGEC n = CGECtoAGEC (%| ((\n -> (n,Neutral)) @| gecEdit "test" |@ updateCounter)) n
where
	updateCounter (n,UpPressed) 	= n+1
	updateCounter (n,DownPressed) 	= n-1
	updateCounter (n,any)	 	 	= n
			
				
