module spreadsheetGEC

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
 	spreadsheet4
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


:: T 	 	:== (Type,Options)
			
:: Type 	=	FI (Int -> Int) 
			|	S String 
			|	R Real 
			|	I Int 
			
:: Options 	= Insert
			| Delete
			| Calculator
			| Expression
			| Counter
			| Displayval
			| Identity

:: T2 a b c	= INT a
			| REAL b
			| STRING c

import GenMap
derive gGEC Options, T2, Type

spreadsheet4 = CGEC ( %| 	(		ToEdit
							@|		gecEdit "design"
							|@		update` o FromEdit 
							)
//							|@		vertlistGEC o  mapT2
							|>>>| 	gecEdit "spreadsheet"
					 ) 
					 [init,init]
where
	
	init = (I 0, Identity)

	ToEdit  list	 = (idGEC list) // Bug? Crash als idGEC = vertlistGEC, en kies "Insert"
									// Bug? Delete van laatste element geeft crash, zelfs al gebruik je geen abstracte editor
	FromEdit edlist  = (^^ edlist)

	update` xs		  			= keepone (update xs)
	where
		keepone [] = [init]
		keepone xs = xs

	update [(x,Insert):xs] 		= [(x,Identity),(x,Identity):update xs] 
	update [(x,Delete):xs] 		= update xs
	update [xo:xs]  			= [xo:update xs]
	update []		  			= []

//		mapT2 list = list // map toT2 list
//		where
//			toT2 (I i,Counter)  	= INT 	 (counterGEC i)
//			toT2 (I i,Calculator)	= INT 	 (intcalcGEC i)
//			toT2 (I i,Displayval)	= INT 	 (modeGEC (Display i))
//			toT2 (I i,Expression)	= INT 	 (dynamicGEC2 i)
//			toT2 (I i,_)			= INT 	 (idGEC i)
//			toT2 (R r,Counter)	 	= REAL 	 (counterGEC r)
//			toT2 (R r,Calculator)	= REAL 	 (realcalcGEC r)
//			toT2 (R r,Displayval)	= REAL 	 (modeGEC (Display r))
//			toT2 (R r,Expression)	= REAL 	 (dynamicGEC2 r)
//			toT2 (R r,_)			= REAL 	 (idGEC r)
//			toT2 (S s,Displayval)	= STRING (modeGEC (Display s))
//			toT2 (S s,_)			= STRING (idGEC s)
//			toT2 (FI fun,_)			= INT    (modeGEC (Display ( 43)))


spreadsheet5 = CGEC ( %| 	(		mapTo
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

