module editeditorGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC, calcAGEC
import StdGecComb, basicAGEC, StdDynamicGEC, StdDynamic//, StdGeneric

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	editoreditor
 	world  

:: T 	 	:== (Type,Editors,Commands)
			
:: Type 	=	FI  Int (AGEC (Int -> Int)) 
			|	FR  Int (AGEC (Real -> Real)) 
			|	FLI (AGEC [Int]) (AGEC ([Int] -> Int)) 
			|	S String 
			|	R Real 
			|	I Int 
			
:: Editors 	= Calculator
			| Expression
			| Counter
			| Displayval
			| Identity

:: Commands	= Insert
			| Delete
			| Apply
			| Choose

:: T2  a b c d e f
			= FII a
			| FRR b
			| FLII c
			| INT d
			| REAL e
			| STRING f

derive gGEC Editors, T2, Type, Commands

editoreditor = CGEC (designeditor |@ convert |>>>| applicationeditor) 
			   [myinit,myinit,myinit,myinit]
where
	designeditor 		= %| (vertlistGEC	@|	gecEdit "design" |@	updateDesign o (^^))

	applicationeditor 	= %| (vertlistGEC o updateApplication @| gecEdit "spreadsheet" |@ (^^))

myinit = (I 0, Identity, Choose)

updateDesign xs = keepone (update xs)
where
	keepone [] = [myinit]
	keepone xs = xs

	update [(x,e,Insert):xs] 		= [(x,e,Choose),(x,e,Choose):xs] 
	update [(x,e,Delete):xs] 		= xs
	update [(FI i f,e,Choose):xs] 	= [(FI i (dynamicGEC2 (const 0)),e,Choose): xs]
	update [(FR i f,e,Choose):xs]	= [(FR i (dynamicGEC2 (const 0.0)),e,Choose): xs]
	update [(FLI i f,e,Choose):xs]	= [(FLI (dynamicGEC2 []) (dynamicGEC2 (const 0)),e,Choose): xs]
	update [xo:xs]  				= [xo:update xs]
	update []		  				= []

convert list = map toT2 list
where
	toT2 (I i,Counter,_)  	 = INT		(counterGEC i)
	toT2 (I i,Calculator,_)	 = INT 	 	(intcalcGEC i)
	toT2 (I i,Displayval,_)	 = INT 	 	(showGEC i)
	toT2 (I i,Expression,_)	 = INT 	 	(dynamicGEC2 i)
	toT2 (I i,_,_)			 = INT 	 	(idGEC i)
	toT2 (R r,Counter,_)	 = REAL  	(counterGEC r)
	toT2 (R r,Calculator,_)	 = REAL  	(realcalcGEC r)
	toT2 (R r,Displayval,_)	 = REAL  	(showGEC r)
	toT2 (R r,Expression,_)	 = REAL  	(dynamicGEC2 r)
	toT2 (R r,_,_)			 = REAL  	(idGEC r)
	toT2 (S s,Displayval,_)	 = STRING 	(showGEC s)
	toT2 (S s,_,_)			 = STRING 	(idGEC s)
	toT2 (FR i f,_,Apply)	 = FRR  	((showGEC ""), hidGEC (^^ f, i))
	toT2 (FI i f,_,Apply)	 = FII  	((showGEC ""), hidGEC (^^ f, i))
	toT2 (FLI i f,_,Apply)	 = FLII  	((showGEC ""), hidGEC (^^ f, ^^ i))
	toT2 _					 = FII 	  	((showGEC "Not implemented! "), hidGEC (id, 0))

updateApplication list = map doT2 list
where
	doT2 (FII (_, fi)) = FII 	((showGEC  (calcfi f (list!!i))), hidGEC (f,i)) where (f,i) = ^^ fi
	doT2 (FRR (_, fi)) = FRR 	((showGEC  (calcfr f (list!!i))), hidGEC (f,i)) where (f,i) = ^^ fi
	doT2 (FLII (_, fi)) = FLII 	((showGEC  (calcfli f i)), hidGEC (f,i)) where (f,i) = ^^ fi
	doT2 x 				= x

	calcfi f (INT i)  = toString (f (^^i)) +++ " "
	calcfi f _		 = "Type error "

	calcfr f (REAL r)  = toString (f (^^r)) +++ " "
	calcfr f _		 = "Type error "

	calcfli f indexlist  
	| typeerror listelem  = "Type error "
	= toString (f [^^ i \\ (INT i) <- listelem]) +++ " "
	where
		listelem = [list!!idx\\ idx <- indexlist]
		typeerror [INT i:xs] = typeerror xs 
		typeerror [x:xs] = True
		typeerror [] = False

showGEC i = (modeGEC (Display i)) 