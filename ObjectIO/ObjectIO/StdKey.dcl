definition module StdKey


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	StdKey defines the special keys for the Object I/O library. 
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


from	oskey		import	:: SpecialKey, 
							backSpaceKey, beginKey, 
							clearKey, 
							deleteKey, downKey, 
							endKey, enterKey, escapeKey, 
							f1Key,  f2Key,  f3Key,  f4Key,  f5Key,  
							f6Key,  f7Key,  f8Key,  f9Key,  f10Key,
							f11Key, f12Key, f13Key, f14Key, f15Key, 
							helpKey, 
							leftKey, 
							pgDownKey, pgUpKey, 
							returnKey, rightKey, 
							upKey,
						class ==, instance == SpecialKey,
						class toString, instance toString SpecialKey