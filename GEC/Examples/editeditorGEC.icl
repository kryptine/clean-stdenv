module editeditorGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC, calcAGEC
import StdGecComb, basicAGEC, StdDynamicGEC, StdDynamic//, StdGeneric

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

// editor that can be used to design and test another editor MJP


Start :: *World -> *World
Start world 
= 	goGui editoreditor world  

:: DesignElem 	:== (Type,Editors,Commands)
:: ListIndex	:== Int			

// options of design editor

:: Type 	=	F_I_I  	Int (AGEC (Int -> Int))						// define function :: Int -> Int 
			|	F_R_R  	Int (AGEC (Real -> Real)) 					// define function :: Real -> Real
			|	F_LI_I 	(AGEC [ListIndex]) (AGEC ([Int] -> Int))  	// define function :: [Int] -> Int
			|	F_LR_R 	(AGEC [ListIndex]) (AGEC ([Real] -> Real))  // define function :: [Real] -> Real
			|	String_ String										// define initial string 								
			|	Real_ 	Real										// define initial real value 
			|	Int_ 	Int											// define initial int value (default)
			
:: Editors 	= Calculator											// ad calculator
			| Expression											// allow expressions /function definitions
			| Counter												// ad counter
			| Displayval											// display non editable value
			| Identity												// identity editor (default)

:: Commands	= Insert												// insert element
			| Delete												// delete element
			| Apply													// function can be applied, hack to avoid evaluation of undefined function
			| Choose												// noop (default)

// The Editors and Circuits:

derive gGEC Type, Editors, Commands, ApplicationElem

editoreditor = CGEC (designeditor |@ convert |>>>| applicationeditor) 
			   [myinit,myinit,myinit,myinit]
where
	designeditor 		= %| (vertlistGEC @| gecEdit "design" |@ updateDesign o (^^))

	applicationeditor 	= %| (vertlistGEC o updateApplication @| gecEdit "user" |@ (^^))

// Initial value of design editor

myinit = (Int_ 0, Identity, Choose)

// Update of design editor

updateDesign xs = keepone (update xs)
where
	keepone [] = [myinit]
	keepone xs = xs

	update [(x,e,Insert):xs] 			= [(x,e,Choose),(x,e,Choose):xs] 
	update [(x,e,Delete):xs] 			= xs

	update [(F_I_I  ix f,e,Choose):xs] 	= [(F_I_I 	ix 				 (dynamicGEC2 (const 0))  ,e,Apply): xs]
	update [(F_R_R  ix f,e,Choose):xs]	= [(F_R_R 	ix 				 (dynamicGEC2 (const 0.0)),e,Apply): xs]
	update [(F_LI_I ix f,e,Choose):xs]	= [(F_LI_I  (dynamicGEC2 []) (dynamicGEC2 (const 0))  ,e,Apply): xs]
	update [(F_LR_R ix f,e,Choose):xs]	= [(F_LR_R  (dynamicGEC2 []) (dynamicGEC2 (const 0.0)),e,Apply): xs]

	update [xo:xs]  					= [xo:update xs]
	update []		  					= []

// type of application editor element

:: ApplicationElem											
			= AF_I_I 	(AGEC String)( AGEC (Int->Int,	 ListIndex ))
			| AF_R_R 	(AGEC String) (AGEC (Real->Real, ListIndex ))
			| AF_LI_I 	(AGEC String) (AGEC ([Int]->Int, [ListIndex]))
			| AF_LR_R 	(AGEC String) (AGEC ([Real]->Real,[ListIndex]))
			| AInt_		(AGEC Int)
			| AReal_	(AGEC Real)
			| AString_ 	(AGEC String)

// turn design editor info in working user application editor

convert list = map toT2 list
where
	toT2 (Int_ i,Calculator,_)  	= AInt_		(intcalcGEC i)
	toT2 (Int_ i,agec,_)	 	 	= AInt_ 	(chooseAGEC agec i)
	toT2 (Real_ r,Calculator,_)	 	= AReal_  	(realcalcGEC r)
	toT2 (Real_ r,agec,_)	 	 	= AReal_	(chooseAGEC agec r)
	toT2 (String_ s,Displayval,_)	= AString_ 	(showGEC s)
	toT2 (String_ s,_,_)			= AString_ 	(idGEC s)
	toT2 (F_I_I i f,_,_)	 	= AF_I_I  	(showGEC "") (hidGEC (^^ f, i))
	toT2 (F_R_R r f,_,_)	 	= AF_R_R  	(showGEC "") (hidGEC (^^ f, r))
	toT2 (F_LI_I i f,_,_)	 	= AF_LI_I  	(showGEC "") (hidGEC (^^ f, ^^ i))
	toT2 (F_LR_R r f,_,_)	 	= AF_LR_R  	(showGEC "") (hidGEC (^^ f, ^^ r))
	toT2 _					 		= AString_ 	(showGEC "not implemented")

	chooseAGEC Counter 		= counterGEC
	chooseAGEC Displayval 	= showGEC
	chooseAGEC Expression 	= dynamicGEC2
	chooseAGEC _ 			= idGEC

// the handling of the application editor boils down to applying all defined functions like in a spreadsheet ...

updateApplication list = map doT2 list
where
	doT2 (AF_I_I  _ fi) 	= AF_I_I 	(showGEC  (calcfli (f o hd) [i]))  (hidGEC (f,i)) where (f,i) = ^^ fi
	doT2 (AF_R_R  _ fi) 	= AF_R_R 	(showGEC  (calcfr f i))  (hidGEC (f,i)) where (f,i) = ^^ fi
	doT2 (AF_LI_I _ fi) 	= AF_LI_I 	(showGEC  (calcfli f i)) (hidGEC (f,i)) where (f,i) = ^^ fi
	doT2 (AF_LR_R _ fi) 	= AF_LR_R 	(showGEC  (calcflr f i)) (hidGEC (f,i)) where (f,i) = ^^ fi
	doT2 x 					= x

	calcfi :: (Int -> Int) ListIndex -> String
	calcfi f i
	# (bix,bty,ni) = CheckBoundsIntVal i
	| bix	= "Index out of range "
	| bty	= ":: Int Expected "
	= ToString (f ni)

	calcfr :: (Real -> Real) ListIndex -> String
	calcfr f i
	# (bix,bty,nr) = CheckBoundsRealVal i
	| bix	= "Index out of range "
	| bty	= ":: Real Expected "
	= ToString (f nr)

	calcfli :: ([Int] -> Int) [ListIndex] -> String
	calcfli f indexlist
	# res				= map CheckBoundsIntVal indexlist
	# (bix,bty,lval)	= (or (map fst3 res),or (map snd3 res),map thd3 res) 
	| bix	= "Index out of range "
	| bty	= ":: [Int] Expected "
	= ToString (f lval)
	
	calcflr :: ([Real] -> Real) [ListIndex] -> String
	calcflr f indexlist
	# res				= map CheckBoundsRealVal indexlist
	# (bix,bty,lval)	= (or (map fst3 res),or (map snd3 res),map thd3 res) 
	| bix	= "Index out of range "
	| bty	= ":: [Real] Expected "
	= ToString (f lval)

	CheckBoundsIntVal i 
	| checkBounds i = (True,False,0)
	= fetchIntVal (list!!i)
	where
			fetchIntVal (AInt_ i) 	= (False,False,^^ i)
			fetchIntVal _ 			= (False,True,0)
				 
	CheckBoundsRealVal i 
	| checkBounds i = (True,False,0.0)
	= fetchRealVal (list!!i)
	where
			fetchRealVal (AReal_ j) = (False,False,^^ j)
			fetchRealVal _ 			= (False,True,0.0)
				 
	checkBounds i = i < 0 || i > (length list) - 1

// small auxilery functions

showGEC i = (modeGEC (Display i))
ToString s = toString s +++ " "


