module editeditorGEC

// editor that can be used to design and test another editor --MJP

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdGecComb, StdDynamic
import basicAGEC, StdAGEC, calcAGEC, dynamicAGEC

Start :: *World -> *World
Start world = goGui editoreditor world  
//Start world = goGui testval world  

testval = CGEC  (predAGEC isEven @| gecEdit "test" |>>>| gecEdit "output")  34



goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world


// The Editors and Circuits:

derive gGEC TypeVal, Editor, Command, ApplicationElem

editoreditor = CGEC (designeditor |@ convert |>>>| applicationeditor) initvalue
where
	designeditor :: CGEC DesignEditor DesignEditor
	designeditor 		= %| (toDesignEditor @| gecEdit "design" |@ updateDesign o fromDesignEditor)

	applicationeditor :: CGEC ApplicationEditor ApplicationEditor
	applicationeditor 	= %| (toApplicEditor o updateApplication @| gecEdit "application" |@ fromApplicEditor)

	toDesignEditor   (table,clipboard) = (listAGEC True (map vertlistAGEC table),hidAGEC clipboard)
	fromDesignEditor (table,clipboard) = (map (^^) (^^ table),^^ clipboard)

	toApplicEditor		= table_hv_AGEC	
	fromApplicEditor	= ^^
	
// Initial value of design editor

initvalue	= ([[initelem,initelem,initelem],[initelem,initelem]],zeroValue)
initelem	= (zeroValue, Choose)
zeroValue 	= (Int_ 0, Identity)

// the design editor types:

:: DesignEditor :== (DesignTable,Clipboard)				// the table is displayed as col x rows
:: DesignTable	:== [[(Element,Command)]]
:: Clipboard	:== Element
:: Element 		:== (TypeVal,Editor)
:: TableIndex	:== (Int,Int)			

:: TypeVal 	= F_I_I   TableIndex		  (AGEC (Int -> Int))    (AGEC Bool) // define function :: Int -> Int 
			| F_R_R   TableIndex 		  (AGEC (Real -> Real))  (AGEC Bool) // define function :: Real -> Real
			| F_LI_I  (AGEC [TableIndex]) (AGEC ([Int] -> Int))  (AGEC Bool) // define function :: [Int] -> Int
			| F_LR_R  (AGEC [TableIndex]) (AGEC ([Real] -> Real))(AGEC Bool) // define function :: [Real] -> Real
			| String_ String										// define initial string 								
			| Real_   Real											// define initial real value 
			| Int_ 	  Int											// define initial int value (default)
			
:: Editor 	= Counter												// ad counter
			| Displayval											// display non editable value
			| Calculator											// ad Calculator
			| Expression											// allow expressions /function definitions
			| Identity 												// identity editor (default)

:: Command	= Insert												// insert element
			| Delete												// delete element
			| Copy													// copy   element
			| Paste													// paste  element
			| Choose												// noop (default)

// create default functions


// Update of design editor

updateDesign :: DesignEditor -> DesignEditor
updateDesign (table,s) =  (keepone (update (paste newclipboard (initfuns table))),newclipboard)
where
	keepone [] = [[initelem]]		// to ensure that there is at least one element...
	keepone xs = xs

	newclipboard = case [elem \\ col <- table, (elem,Copy) <- col] of // copy to clipboard
					[elem] -> elem
					else   -> s
	
	paste :: Clipboard DesignTable -> DesignTable // paste from clipboard
	paste s table = map (map paste_elem) table 
	where
		paste_elem (_,Paste) = (s,Choose)
		paste_elem elem		  = elem
		
	update :: DesignTable -> DesignTable // all other commands ...
	update table = map update_col table 
	where
		update_col [(elem,Insert):xs] = [(elem,Choose),(elem,Choose):xs]
		update_col [(_   ,Delete):xs] = xs
		update_col [(x,_): xs]     	  = [(x,Choose):update_col xs]
		update_col []      			  = []

	initfuns :: DesignTable -> DesignTable
	initfuns table = map (map initfun) table // fill in proper default functions
	where
		initfun :: (Element,Command) -> (Element,Command)
		initfun elem=:((F_I_I  ix f b, e),c) = if (^^ b) ((F_I_I  ix 			   (dynamicAGEC2 (const 0))   nb,e),c) elem
		initfun elem=:((F_R_R  ix f b, e),c) = if (^^ b) ((F_R_R  ix 			   (dynamicAGEC2 (const 0.0)) nb,e),c) elem
		initfun elem=:((F_LI_I ix f b, e),c) = if (^^ b) ((F_LI_I (dynamicAGEC2 []) (dynamicAGEC2 (const 0))   nb,e),c) elem
		initfun elem=:((F_LR_R ix f b, e),c) = if (^^ b) ((F_LR_R (dynamicAGEC2 []) (dynamicAGEC2 (const 0.0)) nb,e),c) elem
		initfun elem = elem
	
		nb =  hidAGEC False 

// the application editor types:

:: ApplicationEditor :== [[ApplicationElem]]

:: ApplicationElem											
			= AF_I_I 	(AGEC String) (AGEC (Int->Int,	   TableIndex ))
			| AF_R_R 	(AGEC String) (AGEC (Real->Real,   TableIndex ))
			| AF_LI_I 	(AGEC String) (AGEC ([Int]->Int,  [TableIndex]))
			| AF_LR_R 	(AGEC String) (AGEC ([Real]->Real,[TableIndex]))
			| AInt_		(AGEC Int)
			| AReal_	(AGEC Real)
			| AString_ 	(AGEC String)

// turn design editor info in working user application editor

convert :: DesignEditor -> ApplicationEditor
convert (table,clipboard) = map (map toAppl) table
where
	toAppl ((Int_ i,Calculator)   ,_)	= AInt_		(intcalcAGEC i)
	toAppl ((Int_ i,agec)         ,_)	= AInt_ 	(chooseAGEC agec i)
	toAppl ((Real_ r,Calculator)  ,_)	= AReal_  	(realcalcAGEC r)
	toAppl ((Real_ r,agec)        ,_)	= AReal_	(chooseAGEC agec r)
	toAppl ((String_ s,Displayval),_)	= AString_ 	(showAGEC s)
	toAppl ((String_ s,_)         ,_)	= AString_ 	(idAGEC s)
	toAppl ((F_I_I  ix f _,_)     ,_)	= AF_I_I  	(showAGEC "") (hidAGEC (^^ f, ix))
	toAppl ((F_R_R  ix f _,_)     ,_)	= AF_R_R  	(showAGEC "") (hidAGEC (^^ f, ix))
	toAppl ((F_LI_I ix f _,_)     ,_)	= AF_LI_I  	(showAGEC "") (hidAGEC (^^ f, ^^ ix))
	toAppl ((F_LR_R ix f _,_)     ,_)	= AF_LR_R  	(showAGEC "") (hidAGEC (^^ f, ^^ ix))
	toAppl _					 		= AString_ 	(showAGEC "not implemented")

	chooseAGEC Counter 		= counterAGEC
	chooseAGEC Displayval 	= showAGEC
	chooseAGEC Expression 	= dynamicAGEC2
	chooseAGEC _ 			= idAGEC

// the handling of the application editor boils down to applying all defined functions like in a spreadsheet ...

updateApplication :: ApplicationEditor -> ApplicationEditor
updateApplication table = map (map updatefun) table
where
	updatefun (AF_I_I  _ fix) = AF_I_I  (showIFUN (applyfii  fix)) fix
	updatefun (AF_R_R  _ fix) = AF_R_R  (showRFUN (applyfrr  fix)) fix
	updatefun (AF_LI_I _ fix) = AF_LI_I (showIFUN (applyflii fix)) fix
	updatefun (AF_LR_R _ fix) = AF_LR_R (showRFUN (applyflrr fix)) fix
	updatefun x 			 = x

	showIFUN :: (Bool,Bool,Int) -> AGEC String
	showIFUN (bix,bty,ival)
		| bix	= showAGEC "Index error "
		| bty	= showAGEC "Int arg expected "
		= showAGEC (ToString ival)
	
	showRFUN :: (Bool,Bool,Real) -> AGEC String
	showRFUN (bix,bty,rval)
		| bix	= showAGEC "Index error "
		| bty	= showAGEC "Real arg expected "
		= showAGEC (ToString rval)

	applyfii  fix = calcfli (f o hd) [ix] 	where (f,ix) = ^^ fix
	applyflii fix = calcfli f ix			where (f,ix) = ^^ fix
	applyfrr  fix = calcflr (f o hd) [ix] 	where (f,ix) = ^^ fix
	applyflrr fix = calcflr f ix 			where (f,ix) = ^^ fix

	calcfli :: ([Int] -> Int) [TableIndex] -> (Bool,Bool,Int)
	calcfli f indexlist
	# res				= map tryGetIntArgs indexlist
	= (or (map fst3 res),or (map snd3 res),f (map thd3 res)) 
	
	calcflr :: ([Real] -> Real) [TableIndex] -> (Bool,Bool,Real)
	calcflr f indexlist
	# res				= map tryGetRealArgs indexlist
	= (or (map fst3 res),or (map snd3 res),f (map thd3 res)) 

	tryGetIntArgs (r,c) 
	| checkBounds (r,c) = (True,False,0)
	= fetchIntVal (table!!c!!r)
	where
			fetchIntVal (AInt_ i) 		= (False,False,^^ i)
			fetchIntVal (AF_I_I  _ fi) 	= applyfii fi
			fetchIntVal (AF_LI_I _ fi) 	= applyflii fi
			fetchIntVal _ 				= (False,True,0)
				 
	tryGetRealArgs (r,c)
	| checkBounds (r,c) = (True,False,0.0)
	= fetchRealVal (table!!c!!r)
	where
			fetchRealVal (AReal_ r) 	= (False,False,^^ r)
			fetchRealVal (AF_R_R  _ fi) = applyfrr fi
			fetchRealVal (AF_LR_R _ fi) = applyflrr fi
			fetchRealVal _ 				= (False,True,0.0)

	checkBounds (i,j) = j < 0 || j >= length table || i < 0 || i >= length (table!!j)

// small auxilery functions

showAGEC i = (modeAGEC (Display ( i)))

strip s = { ns \\ ns <-: s | ns >= '\020' && ns <= '\0200'}	

ToString v = toString v +++ " "

