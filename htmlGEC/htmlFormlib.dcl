definition module htmlFormlib

// Handy collection of Form's
// Form library similar to the AGEC lib
// (c) MJP 2005

import StdEnv, htmlHandler

// **** easy creation of FormId's ****

pFormId			:: String -> FormId					// persitent formid
sFormId			:: String -> FormId					// session formid
nFormId			:: String -> FormId					// page formid

// **** frquently used "mkViewForm" variants ****

// mkBimap			: editor, using Bimaps instead of HBimaps
// mkEdit  		 	: editor, remembers its state, argument is taken as initial state 
// mkSet			: editor, always displays argument regardless of current state of the editor
// mkStore			: applies function to the internal state; second argument is initial state
// mkSelf			: editor, state is updated with function; second argument is taken as initial state   
// mkApply 			: displays application of function to the argument
// mkApplyEdit		: editor, displays its first argument if it is not updated; second argument is initial state

mkBimapEditor 	:: !FormId 	d !Mode !(Bimap d v)	!*HSt -> (Form d,!*HSt)		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC v
mkEditForm 		:: !FormId 	d !Mode					!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkSetForm 		:: !FormId 	d !Mode					!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkStoreForm 	:: !FormId 	d !(d -> d)				!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkSelfForm 		:: !FormId 	d !(d -> d)				!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkApplyForm 	:: !FormId 	d !(d -> d)				!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkApplyEditForm	:: !FormId 	d !d					!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d

// **** forms for lists ****

listForm 			:: !FormId 	 !Mode ![a] 	!*HSt -> (Form [a],!*HSt) 		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
horlistForm 		:: !FormId 	 !Mode ![a] 	!*HSt -> (Form [a],!*HSt) 		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
horlist2Form 		:: !FormId a !Mode ![a] 	!*HSt -> (Form [a],!*HSt) 		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
vertlistForm 		:: !FormId   !Mode ![a] 	!*HSt -> (Form [a],!*HSt) 		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
table_hv_Form 		:: !FormId   !Mode ![[a]] 	!*HSt -> (Form [[a]],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a

// **** forms for tuples ****

t2EditForm  		:: !FormId   !Mode !(a,b) 	!*HSt -> ((Form a,Form b),!*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b

t3EditForm  		:: !FormId 	!Mode !(a,b,c) 	!*HSt -> ((Form a,Form b,Form c),!*HSt) 
																				| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																	   			& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
																	   			& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC c
t4EditForm  		:: !FormId !Mode !(a,b,c,d) !*HSt -> ((Form a,Form b,Form c,Form d),!*HSt)
																				| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																	   			& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
																	   			& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC c
																	   			& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
// **** special buttons ****

counterForm 		:: !FormId !Mode a 		!*HSt -> (Form a,!*HSt) 			| +, -, one
																				,  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
// browseButtons reset curindex step length numberofbuttuns formid mode 
// returns buttons to step through numbers from 1 to length

browseButtons 		:: !Bool !Int !Int !Int !Int !FormId !Mode !*HSt -> (Form Int,!*HSt)

// **** assigning functions to buttons **** yield identity function when nothing pressed

// ordinary buttons with labels 

ListFuncBut 		:: !Bool !FormId !Mode [(Button, a -> a)] !*HSt 
													 -> (Form (a -> a),!*HSt)
ListFuncBut2 		:: !Bool !FormId [(Mode,Button, a -> a)] !*HSt  //fine grain, mode of each buttons can be set 
													 -> (Form (a -> a),!*HSt)

// ordinary buttons with labels displayed in table form

TableFuncBut 		:: !FormId !Mode ![[(Button, a -> a)]] !*HSt 
													  -> (Form (a -> a) ,!*HSt)


// checkboxes		:returns function corresponding to the checkbox pressed, and the checkbox body
//					: the current setting of the checkboxes is remembered, first argument indicates that the new settings have to be taken over
//					: arguments of callback function: - Bool indicates corresponding box is checked or not
//													  - [Bool] indicates the settings of all (other) checkboxes 

ListFuncCheckBox 	:: !Bool !FormId !Mode [(CheckBox, Bool [Bool] a -> a)] !*HSt 
													 -> (Form (a -> a,[Bool]),!*HSt)

// radiobuttons		: assign functions to radiobuttons, returns function corresponding to the radiobutton chosen, and the radiobuttons body
//					: the current setting of the radiobutoons is remembered, pos integer: set radio, otherwise it just indicates the initial setting
//					: arguments of callback function: - Int indicates which button is set

ListFuncRadio 		:: !Int !FormId !Mode [Int -> a -> a] !*HSt 
													 -> (Form(a -> a,Int),!*HSt)

// pull down menu	: assign functions to a pull down menu, returns function corresponding to the menu item chosen, and the pull down menu body
//					: the current setting of the menu is remembered, pos integer: set menu, otherwise it just indicates the initial setting
//					: arguments of callback function: - Int indicates which button is set

FuncMenu 			:: !Int !FormId !Mode [(String, a -> a)] !*HSt 
													 -> (Form(a -> a,Int),!*HSt)

// **** LayOut support ****

(<=>)   infixl 5 	:: [BodyTag] [BodyTag] 	-> BodyTag		// place next to each other on a page
(<.=.>) infixl 5 	::  BodyTag   BodyTag  	-> BodyTag		// place next to each other on a page
mkRowForm 		 	:: [BodyTag] 			-> BodyTag		// place next to each other on a page

(<||>)   infixl 4 	:: [BodyTag] [BodyTag] 	-> BodyTag		// Place second below first
(<.||.>) infixl 4 	::  BodyTag   BodyTag  	-> BodyTag		// Place second below first
mkColForm		  	:: [BodyTag] 			-> BodyTag		// Place second below first

mkSTable 			:: [[BodyTag]] 			-> BodyTag		// Make a table

// **** scripts ****

// openWindowScript will open a new browser window displaying the html code
// parameters resp: scriptname() height width toolbar menubar scrollbars resizable location status html
openWindowScript ::  !String !Int !Int !Bool !Bool !Bool !Bool !Bool !Bool !Html -> Script

// openNoticeScript simplified version of openWindowScript
// parameters are resp: scriptname() height width html
openNoticeScript ::  !String !Int !Int !Html -> Script

// **** special objects ****

mediaPlayer:: (Int,Int) Bool String -> BodyTag	// plays movies, music etc

