module spreadsheetGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import StdGecComb

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	spreadsheet3
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

spreadsheet3	=	CGEC (selfGEC 	"spreadsheet"			updsheet)	    (mksheet initcosts) 
where
		updsheet (_ <-> _ <-> _ <|>
				  costs <-> _ <-> _ <|>
		          _ <-> _ <-> _ )	= mksheet (^^ costs)
		mksheet costs				= Display "Result " <-> Display "Tax " <-> Display "Result + Tax " <|>
									  vertlistGEC costs <-> Display (vertlistGEC tax) <-> Display (vertlistGEC rowsum) <|>
									  Display sumcosts  <-> Display sumtax <-> Display (sumcosts + sumtax)
		where
			sumcosts				= sum costs
			sumtax					= sum tax
			rowsum					= [x + y \\ x <- costs & y <- tax]
			tax 					= [x * 0.19 \\ x <- costs]

		initcosts	  				= [10.00 .. 20.00]	
		
		
