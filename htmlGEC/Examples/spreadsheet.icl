module spreadsheet

// simple spreadsheet example
// (c) MJP 2005

import StdEnv
import StdHtml

Start world  = doHtml spreadsheet world

spreadsheet hst
# (table, (tableB,hst)) = table_hv_HGEC "table" inittable    	hst
# (_,    (rowsumB,hst)) = vertlistHGEC  "vsum"  (rowsum table)  hst
# (_,    (colsumB,hst)) = horlistHGEC   "hsum"  (colsum table)  hst
# (_,    (tsumB,  hst)) = mkHGEC        "tsum"  id (sum (rowsum table)) hst
= (Head 
		[ Hd_Title "Spreadsheet"
		] 
		[ H1 "Spreadsheet Example: "
		, Br
		, (tableB  <-> rowsumB)
		, (colsumB <-> tsumB)
		],hst)
where
	rowsum table	= map sum table
	colsum table	= map sum (transpose table)
	transpose table	= [[table!!i!!j \\ i <- [0..(length table)    - 1]]
							    	\\ j <- [0..length (table!!0) - 1]
				  	  ]
	inittable	= [map ((+) i) [1..5] \\ i <- [0,5..15]]	
