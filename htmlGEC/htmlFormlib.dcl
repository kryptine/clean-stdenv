definition module htmlFormlib

// Handy collection of Form's
// Form library similar to the AGEC lib
// (c) MJP 2005

import StdEnv, htmlHandler

// **** easy creation of a simple html page ****

mkHtml		:: String [BodyTag] *HSt -> (Html,*HSt)	// string is used for the title of the page
simpleHtml	:: String [BodyTag]      -> Html		// as above, without HSt

// **** LayOut support ****

(<=>)   infixl 5 	:: [BodyTag] [BodyTag] 	-> BodyTag		// place next to each other on a page
(<.=.>) infixl 5 	::  BodyTag   BodyTag  	-> BodyTag		// place next to each other on a page
mkRowForm 		 	:: [BodyTag] 			-> BodyTag		// place next to each other on a page

(<||>)   infixl 4 	:: [BodyTag] [BodyTag] 	-> BodyTag		// Place second below first
(<.||.>) infixl 4 	::  BodyTag   BodyTag  	-> BodyTag		// Place second below first
mkColForm		  	:: [BodyTag] 			-> BodyTag		// Place second below first

mkSTable 			:: [[BodyTag]] 			-> BodyTag		// Make a table
(<=|>) infixl 4		:: [BodyTag] [BodyTag] 	-> BodyTag			// Make a table by putting elements pairwise below each other

// **** frquently used "mkViewForm" variants ****

// mkBimap			: editor, using the more simple Bimap instead of an HBimap
// mkEdit  		 	: simple editor with view identical to data model
// mkStore			: applies function to the internal state
// mkSelf			: applies function to the internal state only if the idata has been changed
// mkApplyEdit		: sets iData with second value if the idata has not been changed by user

mkBimapEditor 	:: !FormId 	!(Init d) !(Bimap d v) 	!*HSt -> (Form d,!*HSt)		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC v
mkEditForm 		:: !FormId 	!(Init d) 				!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkStoreForm 	:: !FormId 	!(Init d) !(d -> d)		!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkSelfForm 		:: !FormId 	!(Init d) !(d -> d)		!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkApplyEditForm	:: !FormId 	!(Init d) !d			!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkSubStateForm 	:: !FormId !subState state (subState state -> state) !*HSt 
												-> (state,![BodyTag],!*HSt)		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC subState

// **** forms for lists ****

listForm 		:: !FormId 	  !(Init [a])			!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
horlistForm 	:: !FormId 	  !(Init [a]) 	 		!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
vertlistForm 	:: !FormId    !(Init [a]) 	 		!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
table_hv_Form 	:: !FormId    !(Init [[a]]) 		!*HSt -> (Form [[a]],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
layoutListForm	:: !([BodyTag] [BodyTag] -> [BodyTag]) 
                       !(FormId !(Init a)    *HSt -> (Form a,*HSt))
                        !FormId !(Init [a]) !*HSt -> (Form [a],!*HSt)			| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a

// **** forms for tuples ****

t2EditForm  	:: !FormId !(Init (a,b))		!*HSt -> ((Form a,Form b),!*HSt)| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b

t3EditForm  	:: !FormId !(Init (a,b,c))  	!*HSt -> ((Form a,Form b,Form c),!*HSt) 
																				| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																   				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
																   				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC c
t4EditForm  	:: !FormId !(Init (a,b,c,d)) 	!*HSt -> ((Form a,Form b,Form c,Form d),!*HSt)
																				| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																   				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
																   				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC c
																   				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
// **** special buttons ****

counterForm 		:: !FormId !(Init a) 		  	!*HSt -> (Form a,!*HSt) | +, -, one
																			,  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a

// **** to each button below a function is assigned which is returned as iData value when the corresponding button is pressed
// **** an identity function is returned when none of the set of buttons pressed 

FuncBut 			:: !FormId !(Init (Button, a -> a)) 				!*HSt -> (Form (a -> a),!*HSt)
ListFuncBut 		:: !FormId !(Init [(Button, a -> a)]) 				!*HSt -> (Form (a -> a),!*HSt)
TableFuncBut 		:: !FormId !(Init [[(Button, a -> a)]]) 			!*HSt -> (Form (a -> a),!*HSt)

//fine grain variant, mode of each button in list or table can be set

ListFuncBut2 		:: !FormId !(Init [(Mode,Button, a -> a)]) 			!*HSt -> (Form (a -> a),!*HSt)
TableFuncBut2 		:: !FormId !(Init [[(Mode,Button, a -> a)]])		!*HSt -> (Form (a -> a),!*HSt)

// assign function to each check box which gets an integer of the selected box and the settings of all other boxes
// in addition to the chosen function the settings of all check boxes is returned

ListFuncCheckBox 	:: !FormId !(Init [(CheckBox, Bool [Bool] a -> a)]) !*HSt -> (Form (a -> a,[Bool]),!*HSt)

// assign function to each radio button which gets an integer of the selected radio
// in addition to the chosen function an integer indicating the selected radio button is returned

ListFuncRadio 		:: !FormId !(Init (Int,[Int a -> a])) 				!*HSt -> (Form (a -> a,Int),!*HSt)

// assign function to each pull down menu which gets an integer of the selected menu element
// in addition to the chosen function an integer indicating the selected menu item is returned

FuncMenu 			:: !FormId !(Init (Int,[(String, a -> a)])) 		!*HSt -> (Form (a -> a,Int),!*HSt)

// browseButtons initial index, step, length, numberofbuttuns, formid 
// returns buttons to step through numbers from 1 to length

browseButtons :: !(Init !Int) !Int !Int !Int !FormId !*HSt -> (Form Int,!*HSt)

// **** scripts ****

// openWindowScript will open a new browser window displaying the html code
// parameters resp: scriptname() height width toolbar menubar scrollbars resizable location status html
openWindowScript ::  !String !Int !Int !Bool !Bool !Bool !Bool !Bool !Bool !Html -> Script

// openNoticeScript simplified version of openWindowScript
// parameters are resp: scriptname() height width html
openNoticeScript ::  !String !Int !Int !Html -> Script

// **** special objects ****

mediaPlayer:: (Int,Int) Bool String -> BodyTag	// plays movies, music etc; parameters (height,width) autostart filename

