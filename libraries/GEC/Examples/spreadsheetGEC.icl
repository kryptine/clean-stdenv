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
 	spreadsheet
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

