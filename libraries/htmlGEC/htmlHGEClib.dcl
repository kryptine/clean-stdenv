definition module htmlHGEClib

// Handy collection of HGEC's
// HGEC library similar to the AGEC lib
// (c) MJP 2005

import StdEnv
import StdHtml

// Place two bodies next to each other

(<=>) infixl 5 :: BodyTag BodyTag -> BodyTag

// Place second body below first

(<||>) infixl 4	:: BodyTag BodyTag -> BodyTag	// Place a above b

// Simple table of standard cell size

mkCTable :: [[BodyTag]] -> BodyTag


// handy HGEC's

counterHGEC 		:: !String !HMode a 		!*HSt -> ((a	,!BodyTag),!*HSt) 	| +, -, one
																			, gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlistHGEC 		:: !String !HMode ![a] 		!*HSt -> (([a]	,!BodyTag),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlist2HGEC 		:: !String !HMode a ![a] 	!*HSt -> (([a]	,!BodyTag),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
vertlistHGEC 		:: !String !HMode ![a] 		!*HSt -> (([a]	,!BodyTag),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
table_hv_HGEC 		:: !String !HMode ![[a]] 	!*HSt -> (([[a]],!BodyTag),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a

assignTableFuncBut 	:: !String !HMode ![[(CHButton, a -> a)]] 
												!*HSt -> ((a -> a,!BodyTag) ,!*HSt)

listHGEC 			:: !String !HMode ![a] 		!*HSt -> (([a],![BodyTag]),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
assignListFuncBut 	:: !String !HMode [(CHButton, a -> a)] 
												!*HSt -> ((a -> a,![BodyTag]),!*HSt)



