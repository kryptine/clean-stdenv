module databaseGEC

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
 	example_db3
 	world  

example_db1	=	CGEC (mkGEC "My Database") [MyRecord] 					
example_db2	=	CGEC (predGEC "My Database" checkrecord) [MyRecord]					
where
	checkrecord rs = and (map check rs)
	where
		check r = r.age >= 0 && r.age <= 110 && legal r.zipcode
		legal zipcode = size zipcode >= 6 	&& isDigit zipcode.[0] // 8, string includes CR + LF
											&& isDigit zipcode.[1]
											&& isDigit zipcode.[2]
											&& isDigit zipcode.[3]
											&& isAlpha zipcode.[4]
											&& isAlpha zipcode.[5] //|| zipcode==""
example_db3	= CGEC (mkGEC "ListDisplay") (listAGEC True initrecords) 
where
	initrecords 	= [MyRecord]

::	MyAdminstration 
				= 	{ name		::String
					, street	::String
					, zipcode	::String
					, number	::Int
					, age		::Int
					}
::	ZipCode 	= Number Char Char

derive gGEC   MyAdminstration, ZipCode 

MyRecord = 	{ name = "rinus plasmeijer"
			, street="knollenberg"
			, number=17
			, zipcode="6585WJ"
			, age = 50
			}
