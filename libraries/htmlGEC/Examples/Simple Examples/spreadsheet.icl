module spreadsheet

// simple spreadsheet example
// (c) MJP 2005

import StdEnv
import StdHtml, htmlMonad

derive gUpd []
derive gForm []


// Different ways to define a simple spreadsheet
// Just pick out one of the following Start rules.

//Start world  = doHtml spreadsheet world
//Start world  = doHtml toHtmlFormspreadsheet world
//Start world  = doHtml arrowsspreadsheet world
//Start world  = doHtml spreadsheetM world
Start world  = doHtmlServer spreadsheet world


// Classical way using Cleans # notation

spreadsheet hst
# (tablef, hst) = table_hv_Form (nFormId "table") (inittable 8 10)     	    hst
# (rowsumf,hst) = vertlistForm  (ndFormId "rsum") (rowsum tablef.value)       hst
# (colsumf,hst) = horlistForm   (ndFormId "csum") (colsum tablef.value)       hst
# (totsumf,hst) = mkEditForm    (ndFormId "tsum") (sum (rowsum tablef.value)) hst
= mkHtml "Spreadsheet"
	[ H1 [] "Spreadsheet Example: "
	, tablef.form  <=> rowsumf.form
	, colsumf.form <=> totsumf.form
	] hst
	
// Variant using only editable forms in the # notation, displaying rest using toHtmlForm

toHtmlFormspreadsheet hst
# (table, hst) = table_hv_Form (nFormId "table") (inittable 8 10) hst
= mkHtml "Spreadsheet"
	[ H1 [] "Simple Spreadsheet Example: "
	, table.form  <=> rowsumF table.value
	, colsumF table.value <=> totsumF table.value
	] hst
where
	rowsumF table = toHtmlForm (vertlistForm  (ndFormId "rsum") (rowsum table))       
	colsumF table = toHtmlForm (horlistForm   (ndFormId "csum") (colsum table))       
	totsumF table = toHtmlForm (mkEditForm    (ndFormId "tsum") (sum (rowsum table)))

// Variant using Arrow notation

arrowsspreadsheet hst
# (circuitf, hst) = startCircuit mycircuit (inittable 8 10) hst
# [tablefbody,rowsumfbody,colsumfbody,totsumfbody:_] = circuitf.form
= mkHtml "Spreadsheet"
	[ H1 [] "Spreadsheet Example: "
	, [tablefbody]  <=> [rowsumfbody]
	, [colsumfbody] <=> [totsumfbody]
	] hst
where
	mycircuit =	lift (nFormId "table") table_hv_Form
				>>>	(	(arr rowsum >>> lift (nFormId "rsum") vertlistForm)	&&&
			    		(arr colsum >>> lift (nFormId "csum") horlistForm) 
			 		)
			 	>>> arr (sum o fst)
			 	>>> display (nFormId "tsum") 		

// Variant uding monads

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
	]






// simple utility functions to calculate the sum of the rows, sum of columns, total sum

rowsum table	= map sum table
colsum table	= map sum (transpose table)
transpose table	= [[table!!i!!j \\ i <- [0..(length table)    - 1]]
						    	\\ j <- [0..length (table!!0) - 1]
			  	  ]
inittable n m	= [ [i..i+n] \\ i <- [0,n+1 .. n*m+1]]	



