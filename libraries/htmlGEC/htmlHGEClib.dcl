definition module htmlHGEClib

// Handy collection of HGEC's
// HGEC library similar to the AGEC lib
// (c) MJP 2005

import StdEnv
import StdHtml

// Place two bodies next to each other

(<=>) infixl 5 :: Body Body -> Body

// Place second body below first

(<||>) infixl 4	:: Body Body -> Body	// Place a above b



// handy HGEC's

counterHGEC 		:: String HMode a 		HSt -> ((a		,Body),HSt) 	| +, -, one
																			, gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlistHGEC 		:: String HMode [a] 	HSt -> (([a]	,Body),HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
vertlistHGEC 		:: String HMode [a] 	HSt -> (([a]	,Body),HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
table_hv_HGEC 		:: String HMode [[a]] 	HSt -> (([[a]]	,Body),HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a

assignTableFuncBut 	:: [[(CHButton, a -> a)]] HSt -> ((a -> a,Body) ,HSt)



