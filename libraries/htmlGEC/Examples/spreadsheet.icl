module spreadsheet

// simple spreadsheet example
// (c) MJP 2005

// two versions, with and without arrows

import StdEnv
import StdHtml

Start world  = doHtml spreadsheet world

spreadsheet hst
# ((table, tableB), hst) = table_hv_Form "table" Edit 	 (inittable 8 12)     hst
# ((_,     rowsumB),hst) = vertlistForm  "rsum"  Display (rowsum table)       hst
# ((_,     colsumB),hst) = horlistForm   "csum"  Display (colsum table)       hst
# ((_,     tsumB),  hst) = mkEditForm    "tsum"  Display (sum (rowsum table)) hst
= mkHtml "Spreadsheet"
	[ H1 [] "Spreadsheet Example: "
	, Br
	, (tableB  <=> rowsumB)
	, (colsumB <=> tsumB)
	,Br, Br
	, Txt "so the result of the spreadsheet is :", tsumB
	,Br ,Br
//	, traceHtmlInput
	] hst

arrowsspreadsheet hst
# ((_, [tableB,rowsumB,colsumB,tsumB:_]), hst) = startCircuit mycircuit (inittable 8 10) hst
= mkHtml "Spreadsheet"
	[ H1 [] "Spreadsheet Example: "
	, Br
	, (tableB  <=> rowsumB)
	, (colsumB <=> tsumB)
	,Br, Br
	] hst
where
	mycircuit =	lift "table" Edit table_hv_Form
				>>>	(	(arr rowsum >>> lift "rsum" Display vertlistForm)	&&&
			    		(arr colsum >>> lift "csum" Display horlistForm) 
			 		)
			 	>>> arr (sum o fst)
			 	>>> display "tsum" 		

mkHtml s tags hst 	= (Html (header s) (body tags),hst)
header s 			= Head [`Hd_Std [Std_Title s]] [] 
body tags 			= Body [] tags

rowsum table	= map sum table
colsum table	= map sum (transpose table)
transpose table	= [[table!!i!!j \\ i <- [0..(length table)    - 1]]
						    	\\ j <- [0..length (table!!0) - 1]
			  	  ]
inittable n m	= [ [i..i+n] \\ i <- [0,n+1 .. n*m+1]]	
