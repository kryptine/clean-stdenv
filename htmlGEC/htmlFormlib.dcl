definition module htmlFormlib

// Handy collection of Form's
// Form library similar to the AGEC lib
// (c) MJP 2005

import StdEnv
import StdHtml

// Place two bodies next to each other

(<=>)   infixl 5 :: [BodyTag] [BodyTag] -> BodyTag
(<.=.>) infixl 5 ::  BodyTag   BodyTag  -> BodyTag
mkRowForm 		 :: [BodyTag] 			-> BodyTag

// Place second body below first

(<||>)   infixl 4 :: [BodyTag] [BodyTag] -> BodyTag	// Place a above b
(<.||.>) infixl 4 ::  BodyTag   BodyTag  -> BodyTag	// Place a above b
mkColForm		  :: [BodyTag] 			 -> BodyTag

// Simple table of standard cell size

mkSTable :: [[BodyTag]] -> BodyTag

// special forms:

counterForm 		:: !FormId !Mode a 		!*HSt -> (Form a,!*HSt) 			| +, -, one
																				, gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a

// browseButtons reset curindex step length numberofbuttuns formid mode 
// returns buttons to step through numbers from 1 to length
browseButtons 		:: !Bool !Int !Int !Int !Int !FormId !Mode !*HSt -> (Form Int,!*HSt)

// simple forms for lists and tuples:

horlistForm 		:: !FormId 	 !Mode ![a] 	!*HSt -> (Form [a],!*HSt) 		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlist2Form 		:: !FormId a !Mode ![a] 	!*HSt -> (Form [a],!*HSt) 		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
vertlistForm 		:: !FormId   !Mode ![a] 	!*HSt -> (Form [a],!*HSt) 		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
table_hv_Form 		:: !FormId   !Mode ![[a]] 	!*HSt -> (Form [[a]],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
t2EditForm  		:: !FormId   !Mode !(a,b) 	!*HSt -> ((Form a,Form b),!*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
																				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} b

t3EditForm  		:: !FormId 	!Mode !(a,b,c) 	!*HSt -> ((Form a,Form b,Form c),!*HSt) 
																				| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
																	   			& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} b
																	   			& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} c
t4EditForm  		:: !FormId !Mode !(a,b,c,d) !*HSt -> ((Form a,Form b,Form c,Form d),!*HSt)
																				| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
																	   			& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} b
																	   			& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} c
																	   			& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d

// forms that assign functions to buttons:

TableFuncBut 		:: !FormId !Mode ![[(Button, a -> a)]] !*HSt 
													  -> (Form (a -> a) ,!*HSt)

listForm 			:: !FormId !Mode ![a] 		!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a

// ListFuncBut		: assign functions to buttons, returns function corresponding to the button pressed, and the buttons body

ListFuncBut 		:: !Bool !FormId !Mode [(Button, a -> a)] !*HSt 
													 -> (Form (a -> a),!*HSt)
ListFuncBut2 		:: !Bool !FormId [(Mode,Button, a -> a)] !*HSt  //fine grain, mode of each buttons can be set 
													 -> (Form (a -> a),!*HSt)
// ListFuncCheckBox	: assign functions to checkboxes, returns function corresponding to the checkbox pressed, and the checkbox body
//					: the current setting of the checkboxes is remembered, first argument indicates that the new settings have to be taken over
//					: arguments of callback function: - Bool indicates corresponding box is checked or not
//													  - [Bool] indicates the settings of all (other) checkboxes 

ListFuncCheckBox 	:: !Bool !FormId !Mode [(CheckBox, Bool [Bool] a -> a)] !*HSt 
													 -> (Form (a -> a,[Bool]),!*HSt)

// ListFuncRadio	: assign functions to radiobuttons, returns function corresponding to the radiobutton chosen, and the radiobuttons body
//					: the current setting of the radiobutoons is remembered, pos integer: set radio, otherwise it just indicates the initial setting
//					: arguments of callback function: - Int indicates which button is set

ListFuncRadio 		:: !Int !FormId !Mode [Int -> a -> a] !*HSt 
													 -> (Form(a -> a,Int),!*HSt)

// ListFuncMenu		: assign functions to a pull down menu, returns function corresponding to the menu item chosen, and the pull down menu body
//					: the current setting of the menu is remembered, pos integer: set menu, otherwise it just indicates the initial setting
//					: arguments of callback function: - Int indicates which button is set

FuncMenu 			:: !Int !FormId !Mode [(String, a -> a)] !*HSt 
													 -> (Form(a -> a,Int),!*HSt)

// scripts:

// openWindowScript will open a new browser window displaying the html code
// parameters resp: scriptname() height width toolbar menubar scrollbars resizable location status html
openWindowScript ::  !String !Int !Int !Bool !Bool !Bool !Bool !Bool !Bool !Html -> Script

// openNoticeScript simplified version of openWindowScript
// parameters are resp: scriptname() height width html
openNoticeScript ::  !String !Int !Int !Html -> Script