module spreadsheet

// simple spreadsheet example
// (c) MJP 2005

import StdEnv
import StdHtml

Start world  = doHtml spreadsheet world

spreadsheet hst
# (table, (tableB,hst)) = table_hv_HGEC "table" HEdit 	 inittable    	hst
# (_,    (rowsumB,hst)) = vertlistHGEC  "vsum"  HDisplay (rowsum table)  hst
# (_,    (colsumB,hst)) = horlistHGEC   "hsum"  HDisplay (colsum table)  hst
# (_,    (tsumB,  hst)) = mkEditHGEC    "tsum"  HDisplay (sum (rowsum table)) hst
= (Head 
		[ Hd_Title "Spreadsheet"
		] 
		[ H1 "Spreadsheet Example: "
		, Br
		, (tableB  <=> rowsumB)
		, (colsumB <=> tsumB)
		, T "so the result of the spreadsheet is :", tsumB
		],hst)
where
	rowsum table	= map sum table
	colsum table	= map sum (transpose table)
	transpose table	= [[table!!i!!j \\ i <- [0..(length table)    - 1]]
							    	\\ j <- [0..length (table!!0) - 1]
				  	  ]
	inittable	= [map ((+) i) [1..5] \\ i <- [0,5..15]]	
