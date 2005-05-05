definition module htmlFormlib

// Handy collection of Form's
// Form library similar to the AGEC lib
// (c) MJP 2005

import StdEnv
import StdHtml

// Place two bodies next to each other

(<=>) infixl 5 :: BodyTag BodyTag -> BodyTag

// Place second body below first

(<||>) infixl 4	:: BodyTag BodyTag -> BodyTag	// Place a above b

// Simple table of standard cell size

mkSTable :: [[BodyTag]] -> BodyTag


// handy Form's

counterForm 		:: !FormId !Mode a 		!*HSt -> ((a	,BodyTag),!*HSt) 	| +, -, one
																			, gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlistForm 		:: !FormId !Mode ![a] 		!*HSt -> (([a]	,BodyTag),!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlist2Form 		:: !FormId !Mode a ![a] 	!*HSt -> (([a]	,BodyTag),!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
vertlistForm 		:: !FormId !Mode ![a] 		!*HSt -> (([a]	,BodyTag),!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
table_hv_Form 		:: !FormId !Mode ![[a]] 	!*HSt -> (([[a]],BodyTag),!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a

TableFuncBut 		:: !FormId !Mode ![[(Button, a -> a)]] !*HSt 
													  -> ((a -> a,BodyTag) ,!*HSt)

listForm 			:: !FormId !Mode ![a] 		!*HSt -> (([a],[BodyTag]),!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a

// ListFuncBut		: assign functions to buttons, returns function corresponding to the button pressed, and the buttons body

ListFuncBut 		:: !Bool !FormId !Mode [(Button, a -> a)] !*HSt 
													 -> ((a -> a,[BodyTag]),!*HSt)
// ListFuncCheckBox	: assign functions to checkboxes, returns function corresponding to the checkbox pressed, and the checkbox body
//					: the current setting of the checkboxes is remembered, first argument indicates that the new settings have to be taken over
//					: arguments of callback function: - Bool indicates corresponding box is checked or not
//													  - [Bool] indicates the settings of all (other) checkboxes 

ListFuncCheckBox 	:: !Bool !FormId !Mode [(CheckBox, Bool [Bool] a -> a)] !*HSt 
													 -> (((a -> a,[Bool]),[BodyTag]),!*HSt)

// ListFuncRadio	: assign functions to radiobuttons, returns function corresponding to the radiobutton chosen, and the radiobuttons body
//					: the current setting of the radiobutoons is remembered, pos integer: set radio, otherwise it just indicates the initial setting
//					: arguments of callback function: - Int indicates which button is set

ListFuncRadio 		:: !Int !FormId !Mode [Int -> a -> a] !*HSt 
													 -> (((a -> a,Int),[BodyTag]),!*HSt)

// ListFuncMenu		: assign functions to a pull down menu, returns function corresponding to the menu item chosen, and the pull down menu body
//					: the current setting of the menu is remembered, pos integer: set menu, otherwise it just indicates the initial setting
//					: arguments of callback function: - Int indicates which button is set

FuncMenu 			:: !Int !FormId !Mode [(String, a -> a)] !*HSt 
													 -> (((a -> a,Int),BodyTag),!*HSt)

// scripts
// openWindowScript will open a new browser window displaying the html code
// parameters resp: scriptname() height width toolbar menubar scrollbars resizable location status html
openWindowScript ::  !String     !Int   !Int  !Bool   !Bool   !Bool      !Bool     !Bool    !Bool  !Html -> Script

// openNoticeScript simplified version of openWindowScript
// parameters are resp: scriptname() height width html
openNoticeScript ::  !String !Int !Int !Html -> Script
