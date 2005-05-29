module spreadsheet

// simple spreadsheet example
// (c) MJP 2005

// two versions, with and without arrows

import StdEnv
import StdHtml

Start world  = doHtml spreadsheet world
//Start world  = doHtml arrowsspreadsheet world

spreadsheet hst
# (tablef, hst) = table_hv_Form (nFormId "table") Edit 	(inittable 8 10)     	    hst
# (rowsumf,hst) = vertlistForm  (nFormId "rsum")  Display (rowsum tablef.value)       hst
# (colsumf,hst) = horlistForm   (nFormId "csum")  Display (colsum tablef.value)       hst
# (totsumf,hst) = mkEditForm    (nFormId "tsum")  (sum (rowsum tablef.value)) Display  hst
= mkHtml "Spreadsheet"
	[ H1 [] "Spreadsheet Example: "
	, Br
	, tablef.body  <=> rowsumf.body
	, colsumf.body <=> totsumf.body
	,Br, Br
	, Txt "so the result of the spreadsheet is :", toHtml totsumf.value
	,Br ,Br
//	, traceHtmlInput
	] hst

arrowsspreadsheet hst
# (circuitf, hst) = startCircuit mycircuit (inittable 8 10) hst
# [tablefbody,rowsumfbody,colsumfbody,totsumfbody:_] = circuitf.body
= mkHtml "Spreadsheet"
	[ H1 [] "Spreadsheet Example: "
	, Br
	, [tablefbody]  <=> [rowsumfbody]
	, [colsumfbody] <=> [totsumfbody]
	,Br, Br
	] hst
where
	mycircuit =	lift (nFormId "table") Edit table_hv_Form
				>>>	(	(arr rowsum >>> lift (nFormId "rsum") Display vertlistForm)	&&&
			    		(arr colsum >>> lift (nFormId "csum") Display horlistForm) 
			 		)
			 	>>> arr (sum o fst)
			 	>>> display (nFormId "tsum") 		

mkHtml s tags hst 	= (Html (header s) (body tags),hst)
header s 			= Head [`Hd_Std [Std_Title s]] [] 
body tags 			= Body [] tags

rowsum table	= map sum table
colsum table	= map sum (transpose table)
transpose table	= [[table!!i!!j \\ i <- [0..(length table)    - 1]]
						    	\\ j <- [0..length (table!!0) - 1]
			  	  ]
inittable n m	= [ [i..i+n] \\ i <- [0,n+1 .. n*m+1]]	
