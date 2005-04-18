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

mkSTable :: [[BodyTag]] -> BodyTag


// handy HGEC's

counterHGEC 		:: !String !HMode a 		!*HSt -> ((a	,!BodyTag),!*HSt) 	| +, -, one
																			, gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlistHGEC 		:: !String !HMode ![a] 		!*HSt -> (([a]	,!BodyTag),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlist2HGEC 		:: !String !HMode a ![a] 	!*HSt -> (([a]	,!BodyTag),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
vertlistHGEC 		:: !String !HMode ![a] 		!*HSt -> (([a]	,!BodyTag),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
table_hv_HGEC 		:: !String !HMode ![[a]] 	!*HSt -> (([[a]],!BodyTag),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a

TableFuncBut 		:: !String !HMode ![[(Button, a -> a)]] !*HSt 
													  -> ((a -> a,!BodyTag) ,!*HSt)

listHGEC 			:: !String !HMode ![a] 		!*HSt -> (([a],![BodyTag]),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a

// ListFuncBut		: assign functions to buttons, returns function corresponding to the button pressed, and the buttons body

ListFuncBut 		:: !String !HMode [(Button, a -> a)] !*HSt 
													 -> ((a -> a,![BodyTag]),!*HSt)
// ListFuncCheckBox	: assign functions to checkboxes, returns function corresponding to the checkbox pressed, and the checkbox body
//					: the current setting of the checkboxes is remembered, first argument indicates that the new settings have to be taken over
//					: arguments of callback function: - Bool indicates corresponding box is checked or not
//													  - [Bool] indicates the settings of all (other) checkboxes 

ListFuncCheckBox 	:: !Bool !String !HMode [(CheckBox, Bool [Bool] a -> a)] !*HSt 
													 -> (((a -> a,[Bool]),![BodyTag]),!*HSt)

// ListFuncRadio	: assign functions to radiobuttons, returns function corresponding to the radiobutton chosen, and the radiobuttons body
//					: the current setting of the radiobutoons is remembered, non-negative integer indicates which radiobuttons should be set
//					: arguments of callback function: - Int indicates which button is set

ListFuncRadio 		:: !Int !String !HMode [Int -> a -> a] !*HSt 
													 -> (((a -> a,Int),![BodyTag]),!*HSt)



