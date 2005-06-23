module spreadsheet

// simple spreadsheet example
// (c) MJP 2005

// two versions, with and without arrows

import StdEnv
import StdHtml, htmlMonad

Start = 0
//Start world  = doHtml spreadsheetMH world
Start world  = doHtml spreadsheetM world
//Start world  = doHtml spreadsheet world
//Start world  = doHtml arrowsspreadsheet world

spreadsheetMH :: (Form [.[b]]) -> 
                 (.(*HSt -> *((Form [[Int]]),!*HSt)),.((Form u:c) -> (.(*HSt -> *((Form [b]),!*HSt)),v:((Form w:d) -> (.(*HSt -> *((Form [b]),!*HSt)),x:(.(Form a) -> (.(*HSt -> *((Form b),!*HSt)),y:(.e -> *(*HSt -> *(Html,*HSt)))))))))) | TC b & + b & zero b, [y x v <= u,y x <= w]
spreadsheetMH
  = \tablef  -> (table_hv_Form (nFormId "table") (inittable 8 10),
	\rowsumf -> (vertlistForm  (ndFormId "rsum") (rowsum tablef.value),
	\colsumf -> (horlistForm   (ndFormId "csum") (colsum tablef.value),
	\totsumf -> (mkEditForm    (ndFormId "tsum") (sum (rowsum tablef.value)),
	\_ -> (mkHtmlM "Spreadsheet"
	[ H1 [] "Spreadsheet Example: "
	, Br
	, tablef.form  <=> rowsumf.form
	, colsumf.form <=> totsumf.form
	,Br, Br
	, Txt "so the result of the spreadsheet is :", toHtml totsumf.value
	,Br ,Br
	])))))

spreadsheetM
  = table_hv_Form (nFormId "table") (inittable 8 10)            >>= \tablef  ->
	vertlistForm  (ndFormId "rsum") (rowsum tablef.value)       >>= \rowsumf -> 
	horlistForm   (ndFormId "csum") (colsum tablef.value)       >>= \colsumf ->
	mkEditForm    (ndFormId "tsum") (sum (rowsum tablef.value)) >>= \totsumf ->
	mkHtmlM "Spreadsheet"
	[ H1 [] "Spreadsheet Example: "
	, Br
	, tablef.form  <=> rowsumf.form
	, colsumf.form <=> totsumf.form
	,Br, Br
	, Txt "so the result of the spreadsheet is :", toHtml totsumf.value
	,Br ,Br
	]

spreadsheet hst
# (tablef, hst) = table_hv_Form (nFormId "table") (inittable 8 10)     	    hst
# (rowsumf,hst) = vertlistForm  (ndFormId "rsum") (rowsum tablef.value)       hst
# (colsumf,hst) = horlistForm   (ndFormId "csum") (colsum tablef.value)       hst
# (totsumf,hst) = mkEditForm    (ndFormId "tsum") (sum (rowsum tablef.value)) hst
= mkHtml "Spreadsheet"
	[ H1 [] "Spreadsheet Example: "
	, Br
	, tablef.form  <=> rowsumf.form
	, colsumf.form <=> totsumf.form
	,Br, Br
	, Txt "so the result of the spreadsheet is :", toHtml totsumf.value
	,Br ,Br
//	, traceHtmlInput
	] hst

arrowsspreadsheet hst
# (circuitf, hst) = startCircuit mycircuit (inittable 8 10) hst
# [tablefbody,rowsumfbody,colsumfbody,totsumfbody:_] = circuitf.form
= mkHtml "Spreadsheet"
	[ H1 [] "Spreadsheet Example: "
	, Br
	, [tablefbody]  <=> [rowsumfbody]
	, [colsumfbody] <=> [totsumfbody]
	,Br, Br
	] hst
where
	mycircuit =	lift (nFormId "table") table_hv_Form
				>>>	(	(arr rowsum >>> lift (nFormId "rsum") vertlistForm)	&&&
			    		(arr colsum >>> lift (nFormId "csum") horlistForm) 
			 		)
			 	>>> arr (sum o fst)
			 	>>> display (nFormId "tsum") 		

rowsum table	= map sum table
colsum table	= map sum (transpose table)
transpose table	= [[table!!i!!j \\ i <- [0..(length table)    - 1]]
						    	\\ j <- [0..length (table!!0) - 1]
			  	  ]
inittable n m	= [ [i..i+n] \\ i <- [0,n+1 .. n*m+1]]	
