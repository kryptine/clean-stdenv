definition module htmlHGEClib

// Handy collection of HGEC's
// HGEC library similar to the AGEC lib
// (c) MJP 2005

import StdEnv
import StdHtml

// Place two bodies next to each other

(<->) infixl 5 :: Body Body -> Body

// Place second body below first

(<|>) infixl 4	:: Body Body -> Body	// Place a above b

// handy HGEC's

horlistHGEC 	:: String [a] 	HSt -> ([a],(Body,HSt)) 	| gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
vertlistHGEC 	:: String [a] 	HSt -> ([a],(Body,HSt)) 	| gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
table_hv_HGEC 	:: String [[a]] HSt -> ([[a]],(Body,HSt)) 	| gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 




