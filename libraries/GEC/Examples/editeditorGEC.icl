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

:: DesignEditor :== ([(Element,Commands)],Element)
:: Element 		:== (Type,Editors)
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
			| Copy													// copy   element
			| Paste													// paste  element
			| Apply													// function can be applied, hack to avoid evaluation of undefined function
			| Choose												// noop (default)

// The Editors and Circuits:

derive gGEC Type, Editors, Commands, ApplicationElem

editoreditor = CGEC (designeditor |@ convert |>>>| applicationeditor) 
			   ([myinit,myinit,myinit,myinit],zeroElem)
where
	designeditor 		= %| (vertlistGEC` @| gecEdit "design" |@ updateDesign o unvert)

	applicationeditor 	= %| (vertlistGEC o updateApplication @| gecEdit "user" |@ (^^))

	vertlistGEC` (list,clipboard) = (vertlistGEC list,hidGEC clipboard)
	unvert (vertlist,clipboard)   = (^^ vertlist,^^ clipboard)

// Initial value of design editor

myinit 		= (zeroElem, Choose)
zeroElem 	= (Int_ 0, Identity)

// Update of design editor

updateDesign :: DesignEditor -> DesignEditor
updateDesign (elems,s) =  (keepone (update (paste elems s)),(copy elems s))
where
	keepone [] = [myinit]
	keepone xs = xs

	copy [(elem,Copy  ):xs] s = elem
	copy [x:xs] s			  = copy xs s
	copy [] s				  = s
	
	paste [(elem, Paste):xs]s = [(s,Apply):xs]
	paste [x:xs] s			  = [x: paste xs s]
	paste [] s				  = []
	
	update [(elem,Insert):xs] = [(elem,Choose),(elem,Choose):xs]
	update [(_   ,Delete):xs] = xs
	
	update [((F_I_I  ix f,e),Choose):xs] = [((F_I_I   ix 			  (dynamicGEC2 (const 0))  ,e),Apply): xs]
	update [((F_R_R  ix f,e),Choose):xs] = [((F_R_R   ix 			  (dynamicGEC2 (const 0.0)),e),Apply): xs]
	update [((F_LI_I ix f,e),Choose):xs] = [((F_LI_I (dynamicGEC2 []) (dynamicGEC2 (const 0))  ,e),Apply): xs]
	update [((F_LR_R ix f,e),Choose):xs] = [((F_LR_R (dynamicGEC2 []) (dynamicGEC2 (const 0.0)),e),Apply): xs]
	
	update [(x,Choose): xs] = [(x,Choose):update xs]
//	update [(x,Copy): xs]  = [(x,Apply):update xs]
	update [(x,_): xs]     = [(x,Apply):update xs]
	update []      = []

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

convert :: DesignEditor -> [ApplicationElem]
convert designeditor = map toAppl (fst designeditor)
where
	toAppl ((Int_ i,Calculator),   _)	= AInt_		(intcalcGEC i)
	toAppl ((Int_ i,agec),         _)	= AInt_ 	(chooseAGEC agec i)
	toAppl ((Real_ r,Calculator),  _)	= AReal_  	(realcalcGEC r)
	toAppl ((Real_ r,agec),        _)	= AReal_	(chooseAGEC agec r)
	toAppl ((String_ s,Displayval),_)	= AString_ 	(showGEC s)
	toAppl ((String_ s,_),         _)	= AString_ 	(idGEC s)
	toAppl ((F_I_I i f,_),         _)	= AF_I_I  	(showGEC "") (hidGEC (^^ f, i))
	toAppl ((F_R_R r f,_),         _)	= AF_R_R  	(showGEC "") (hidGEC (^^ f, r))
	toAppl ((F_LI_I i f,_),        _)	= AF_LI_I  	(showGEC "") (hidGEC (^^ f, ^^ i))
	toAppl ((F_LR_R r f,_),        _)	= AF_LR_R  	(showGEC "") (hidGEC (^^ f, ^^ r))
	toAppl _					 		= AString_ 	(showGEC "not implemented")

	chooseAGEC Counter 		= counterGEC
	chooseAGEC Displayval 	= showGEC
	chooseAGEC Expression 	= dynamicGEC2
	chooseAGEC _ 			= idGEC

// the handling of the application editor boils down to applying all defined functions like in a spreadsheet ...

updateApplication :: [ApplicationElem] -> [ApplicationElem]
updateApplication list = map updatefun list
where
	updatefun (AF_I_I  _ fi) = AF_I_I  (showIFUN (applyfii  fi)) fi
	updatefun (AF_R_R  _ fi) = AF_R_R  (showRFUN (applyfrr  fi)) fi
	updatefun (AF_LI_I _ fi) = AF_LI_I (showIFUN (applyflii fi)) fi
	updatefun (AF_LR_R _ fi) = AF_LR_R (showRFUN (applyflrr fi)) fi
	updatefun x 			 = x

	showIFUN :: (Bool,Bool,Int) -> AGEC String
	showIFUN (bix,bty,ival)
		| bix	= showGEC "Index error "
		| bty	= showGEC "Int arg expected "
		= showGEC (ToString ival)
	
	showRFUN :: (Bool,Bool,Real) -> AGEC String
	showRFUN (bix,bty,rval)
		| bix	= showGEC "Index error "
		| bty	= showGEC "Real arg expected "
		= showGEC (ToString rval)

	applyfii  fi = calcfli (f o hd) [i] where (f,i) = ^^ fi
	applyflii fi = calcfli f i 		    where (f,i) = ^^ fi
	applyfrr  fi = calcflr (f o hd) [i] where (f,i) = ^^ fi
	applyflrr fi = calcflr f i 		    where (f,i) = ^^ fi

	calcfli :: ([Int] -> Int) [ListIndex] -> (Bool,Bool,Int)
	calcfli f indexlist
	# res				= map tryGetIntArgs indexlist
	= (or (map fst3 res),or (map snd3 res),f (map thd3 res)) 
	
	calcflr :: ([Real] -> Real) [ListIndex] -> (Bool,Bool,Real)
	calcflr f indexlist
	# res				= map tryGetRealArgs indexlist
	= (or (map fst3 res),or (map snd3 res),f (map thd3 res)) 

	tryGetIntArgs i 
	| checkBounds i = (True,False,0)
	= fetchIntVal (list!!i)
	where
			fetchIntVal (AInt_ i) 		= (False,False,^^ i)
			fetchIntVal (AF_I_I  _ fi) 	= applyfii fi
			fetchIntVal (AF_LI_I _ fi) 	= applyflii fi
			fetchIntVal _ 				= (False,True,0)
				 
	tryGetRealArgs i 
	| checkBounds i = (True,False,0.0)
	= fetchRealVal (list!!i)
	where
			fetchRealVal (AReal_ r) 	= (False,False,^^ r)
			fetchRealVal (AF_R_R  _ fi) = applyfrr fi
			fetchRealVal (AF_LR_R _ fi) = applyflrr fi
			fetchRealVal _ 				= (False,True,0.0)

	checkBounds i = i < 0 || i >= length list

// small auxilery functions

showGEC i = (modeGEC (Display ( i)))

strip s = { ns \\ ns <-: s | ns >= '\020' && ns <= '\0200'}	

ToString v = toString v +++ " "