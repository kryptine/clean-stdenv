module spreadsheetGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC, calcAGEC
import StdGecComb, basicAGEC//, StdDynamicGEC, StdDynamic//, StdGeneric

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	spreadsheet5
 	world  

spreadsheet	=	CGEC (selfGEC 	"spreadsheet"			updsheet)	    (mksheet inittable) 
where
		updsheet (table <-> _ <|>
		          _ <-> _ )			= mksheet (^^ table)
		mksheet table				= tableGEC table <-> Display (vertlistGEC rowsum) <|>
									  Display (horlistGEC colsum) <-> Display (sum rowsum)
		where
			rowsum					= map sum table
			colsum 					= map sum transpose
			transpose				= [[table!!i!!j \\ i <- [0..(length table)    - 1]]
												    \\ j <- [0..length (table!!0) - 1]
									  ]
		inittable	  				= [map ((+) i) [1..5] \\ i <- [0,5..25]]	

spreadsheet2	=	CGEC (selfGEC 	"spreadsheet"			updsheet)	    (mksheet inittable) 
where
		updsheet (table <-> _ <|>
		          _ <-> _ )			= mksheet (^^ table)
		mksheet table				= tableGEC table <-> vertlistGEC rowsum <|>
									  horlistGEC colsum <-> sum rowsum
		where
			rowsum					= map sum table
			colsum 					= map sum transpose
			transpose				= [[table!!i!!j \\ i <- [0..(length table)    - 1]]
												    \\ j <- [0..length (table!!0) - 1]
									  ]
		inittable	  				= [map ((+) i) [1..5] \\ i <- [0,5..25]]	

spreadsheet3	=	CGEC (selfGEC "spreadsheet" updsheet) (mksheet initcosts initvat) 
where
		updsheet (_ <-> _ <-> _ <-> _ <|>
				  costs <-> _ <-> _ <-> vat <|>
		          _ <-> _ <-> _ )	= mksheet (^^ costs) vat
		mksheet costs vat			= Display "Result " <-> Display "Tax " <-> Display "Result + Tax " <-> Display "VAT " <|>
									  vertlistGEC costs <-> Display (vertlistGEC tax) <-> Display (vertlistGEC rowsum) <-> vat <|>
									  Display sumcosts  <-> Display sumtax <-> Display (sumcosts + sumtax)
		where
			sumcosts				= sum costs
			sumtax					= sum tax
			rowsum					= [x + y \\ x <- costs & y <- tax]
			tax 					= [x * vat\\ x <- costs]
			
		initcosts	  				= [10.00 .. 16.00]
		initvat						= 0.19	

spreadsheet4 = CGEC ( %| 	(		mapTo
							@|		gecEdit "design"
							|@		mapFrom 
							)
					 ) 
					 init
where
	init = (1,[3])

	mapTo  (l,list)	= vertlistGEC list <-> l 
	mapFrom (edlist <-> l) = (l, calc (^^ edlist) l)
	where
		calc edlist l = take l edlist ++ (repeatn (num edlist) 0)
		num edlist = if ((l - length edlist) <= 0) 0 (l - length edlist)

// ---------

:: T 	 	:== (Type,Editors,Commands)
			
:: Type 	=	FI Int //(Int -> Int) 
			|	S String 
			|	R Real 
			|	I Int 
			
:: Editors 	= Calculator
			| Expression
			| Counter
			| Displayval
			| Identity

:: Commands	= Insert
			| Append
			| Delete
			| Choose

:: T2 a b c	= INT a
			| REAL b
			| STRING c

import GenMap
derive gGEC Editors, T2, Type, Commands

spreadsheet5 = CGEC ( %| 	(		ToEdit
							@|		gecEdit "design"
							|@		update` o FromEdit 
							)
							|@		vertlistGEC o  mapT2
							|>>>| 	gecEdit "spreadsheet"
					 ) 
					 [init,init]
where
	
	init = (I 0, Identity, Choose)

	ToEdit  list	 = (vertlistGEC list) 
	FromEdit edlist  = (^^ edlist)

	update` xs		  			= keepone (update xs)
	where
		keepone [] = [init]
		keepone xs = xs

	update [(x,e,Insert):xs] 		= [(x,e,Choose),(x,e,Choose):update xs] 
	update [(x,e,Append):xs] 		= [(x,e,Choose),(x,e,Choose):update xs] 
	update [(x,e,Delete):xs] 		= update xs
	update [xo:xs]  			= [xo:update xs]
	update []		  			= []

	mapT2 list = map toT2 list
	where
		toT2 (I i,Counter,c)  	= INT 	 (counterGEC i)
		toT2 (I i,Calculator,c)	= INT 	 (intcalcGEC i)
		toT2 (I i,Displayval,c)	= INT 	 (modeGEC (Display i))
//		toT2 (I i,Expression,c)	= INT 	 (dynamicGEC2 i)
		toT2 (I i,_,c)			= INT 	 (idGEC i)
		toT2 (R r,Counter,c)	= REAL 	 (counterGEC r)
		toT2 (R r,Calculator,c)	= REAL 	 (realcalcGEC r)
		toT2 (R r,Displayval,c)	= REAL 	 (modeGEC (Display r))
//		toT2 (R r,Expression,c)	= REAL 	 (dynamicGEC2 r)
		toT2 (R r,_,c)			= REAL 	 (idGEC r)
		toT2 (S s,Displayval,c)	= STRING (modeGEC (Display s))
		toT2 (S s,_,c)			= STRING (idGEC s)
		toT2 (_,_,_)			= INT    (modeGEC (Display ( 43)))

