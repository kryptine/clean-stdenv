module spreadsheetGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC, calcAGEC
import StdGecComb, basicAGEC

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


:: T a b 	= I .(a,IGEC)
			| R .(b,IGEC)
			| Choose
			| Less
			
:: T2 a b 	= INT a
			| REAL b

:: IGEC  	= Counter
			| Calculator
			| Identity
			
import GenMap
derive gMap T
derive gGEC T, IGEC, T2

spreadsheet4 = CGEC ( %| 	(		mapTo
							@|		gecEdit "design"
							|@		update o mapFrom
							)
							|@		vertlistGEC o  mapT2
							|>>>| 	gecEdit "spreadsheet"
					 ) 
					 init
where
	init :: [T Int Real]
	init = [I (3,Identity)]

	mapTo list					= vertlistGEC list <|> Choose
	mapFrom (agecs <|> more)	= (^^ agecs, more)

	update (list,Choose)= list
	update ([],Less) 	= []
	update (list,Less) 	= reverse (tl (reverse list))
	update (list,new)  	= list ++ [new]

	mapT2 list = map toT2 list
	where
		toT2 (I (i,Counter)) 	= INT (counterGEC i)
		toT2 (I (i,Identity))	= INT (idGEC i)
		toT2 (I (i,Calculator))	= INT (intcalcGEC i)
		toT2 (R (r,Counter)) 	= REAL (counterGEC r)
		toT2 (R (r,Identity))	= REAL (idGEC r)
		toT2 (R (r,Calculator))	= REAL (realcalcGEC r)
		 

	mapTo` val	 		=gMap {|* -> * |} (gMap {|* -> *  -> *|} counterGEC counterGEC) val 

	mapFrom` agecs	  	= gMap {|* -> * |} (gMap {|* -> *  -> *|} (^^) (^^)) agecs
		
		
