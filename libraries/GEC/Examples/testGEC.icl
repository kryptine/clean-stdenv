module testGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import GecArrow, basicAGEC

// TO TEST JUST REPLACE THE EXAMPLE NAME IN THE START RULE WITH ANY OF THE EXAMPLES BELOW
// ALL EXAMPLES HAVE TO BE OF FORM pst -> pst

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	test5
 	world  

:: T = C1 (AGEC Int)
	 | C2 (AGEC Real)
	 
derive gGEC T

testX = startCircuit (feedback (edit "self" >>@ test)) (C1 (counterAGEC 0))
where
	 test (C1 igec) = C1 (counterAGEC ((^^ igec) + 5))
//	 test (C1 igec) = C2 (counterAGEC (toReal (^^ igec)))
	 test (C2 rgec) = C1 (counterAGEC (toInt (^^ rgec)))

:: T` = C1` Int
	  | C2` Real
	 
derive gGEC T`

testX` = startCircuit (feedback (edit "self" >>@ test)) (C1` 0)
where
//	 test (C1` i) = C1` (i + 5)
	 test (C1` i) = C2` (toReal i)
	 test (C2` r) = C1` (toInt r)

testbetsy = startCircuit (feedback (edit "self" >>@ mytestje)) (mydata 5)
where
	mydata i = i <|> horlistAGEC (repeatn i i)
	mytestje (i <|> _) = mydata i


test12 = startCircuit (edit "Editor") (hidAGEC 1)

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

test23 = startCircuit (feedback (edit "self" >>@ updRecord)) (initRecord2 0 0)

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

test24 = startCircuit (feedback (edit "self" >>@ toMyEditRecord o updRecord o fromMyEditRecord)) (toMyEditRecord (initRecord2 0 0))


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
							  edvalue1	= idAGEC edrec.MyRecord2.value1
							, edvalue2	= idAGEC edrec.MyRecord2.value2
//							, edvalue2	= doubleCounterAGEC edrec.MyRecord2.value2
//							, edvalue2	= idAGEC edrec.MyRecord2.value2
							, edsum		= modeAGEC (Display edrec.MyRecord2.sum)
							}


fromMyEditRecord2 :: (MyEditRecord2 a) -> (MyRecord2 a)
fromMyEditRecord2 rec = 	{ value1	= ^^ rec.MyEditRecord2.edvalue1
							, value2	= ^^ rec.MyEditRecord2.edvalue2
							, sum		= ^^ rec.MyEditRecord2.edsum
							}

test25 = startCircuit (feedback (edit "self" >>@ toMyEditRecord2 o updRecord o fromMyEditRecord2)) 
												(toMyEditRecord2 (initRecord2 22 23))


doubleCounterAGEC :: a -> AGEC a | IncDec a & gGEC {|*|} a 
doubleCounterAGEC a = mkAGEC    { toGEC   = toEditRec //\arg _ -> toMyEditRecord21 (toMyData arg)
							, fromGEC = fromMyData o fromMyEditRecord2
							, updGEC  = toMyEditRecord21 o updRecord o fromMyEditRecord2
							, value   = a
							} "doubleCounterAGEC"
							where 
							 toMyData n 	= initRecord2 zero n
							 toEditRec arg Undefined = toMyEditRecord21 (toMyData arg)
							 toEditRec arg (Defined oarg) = oarg
							 fromMyData rec = rec.MyRecord2.sum

toMyEditRecord21 :: (MyRecord2 a) -> (MyEditRecord2 a) | IncDec a & gGEC {|*|} a 
toMyEditRecord21 edrec = 	{ MyEditRecord2 |
							  edvalue1	= counterAGEC edrec.MyRecord2.value1
							, edvalue2	= counterAGEC edrec.MyRecord2.value2
							, edsum		= modeAGEC (Display edrec.MyRecord2.sum)
							}




//test5 = CGEC (gecEdit "test") { inout2 = In 1 , gec2 = gecDisplay "circuit"}

test0 = startCircuit (edit "test") mylist
where
	mylist :: AGEC [[Int]]
	mylist = table_vh_AGEC [list\\j <- list] where list = []
test1 = startCircuit mycgec 23
test2 = startCircuit (AGECtoCGEC "test" (CGECtoAGEC mycgec 27)) 23
test3 = startCircuit (edit "test") (CGECtoAGEC (feedback (edit "self" >>@ inc)) 22)
test4 = startCircuit (edit "test") { inout = (Edit 1,Display 1) , gec = display "circuit"}

mycgec = (AGECtoCGEC "test" (counterAGEC 0))


				
test6 = startCircuit (feedback (edit "self" >>@ incsum))
								 	{ field1	= counterAGEC 0
							  		, field2	= counterAGEC 0
							  		, sum 		= idAGEC 0}
where
	incsum rec = {rec & MyRecord.sum = rec.MyRecord.sum ^= (^^ rec.field1 + ^^ rec.field2)}

			
test5 = startCircuit (feedback (test @>> edit "test" >>@ (^^)) ) 23
where
	test n = if (isEven n) (counterAGEC n) (idAGEC n)

:: MyRecord a =  { field1:: AGEC a
				 , field2:: AGEC a
				 , sum:: AGEC a }

counterCGEC n = CGECtoAGEC (feedback ((\n -> (n,Neutral)) @>> edit "test" >>@ updateCounter)) n
where
	updateCounter (n,UpPressed) 	= n+1
	updateCounter (n,DownPressed) 	= n-1
	updateCounter (n,any)	 	 	= n
			
				
