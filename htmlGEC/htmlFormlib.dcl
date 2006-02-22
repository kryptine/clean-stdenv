definition module htmlFormlib

// Handy collection of Form creating functions and layout functions
// (c) MJP 2005

import StdEnv, htmlHandler, htmlButtons

// **** easy creation of a simple html page ****

mkHtml		:: String [BodyTag] *HSt -> (Html,*HSt)			// string is used for the title of the page
mkHtmlB		:: String [BodyAttr] [BodyTag] *HSt -> (Html,*HSt) // same, with bodytags options
simpleHtml	:: String [BodyAttr] [BodyTag]      -> Html		// as above, without HSt

// **** LayOut support ****

(<=>)   infixl 5 	:: [BodyTag] [BodyTag] 	-> BodyTag		// place next to each other on a page
(<.=.>) infixl 5 	::  BodyTag   BodyTag  	-> BodyTag		// place next to each other on a page
mkRowForm 		 	:: [BodyTag] 			-> BodyTag		// place every element in a row next to each other on a page

(<||>)   infixl 4 	:: [BodyTag] [BodyTag] 	-> BodyTag		// Place second below first
(<.||.>) infixl 4 	::  BodyTag   BodyTag  	-> BodyTag		// Place second below first
mkColForm		  	:: [BodyTag] 			-> BodyTag		// Place every element in a column below first

mkSTable 			:: [[BodyTag]] 			-> BodyTag		// Make a table
(<=|>) infixl 4		:: [BodyTag] [BodyTag] 	-> BodyTag		// Make a table by putting elements pairwise below each other

// **** frquently used "mkViewForm" variants ****

// mkBimap			: editor, using the more simple Bimap instead of an HBimap
// mkEdit  		 	: simple editor with view identical to data model
// mkStore			: applies function to the internal state
// mkSelf			: applies function to the internal state only if the idata has been changed
// mkApplyEdit		: sets iData with second value if the idata has not been changed by user

mkBimapEditor 	:: !(InIDataId d) !(Bimap d v) 	!*HSt -> (Form d,!*HSt)		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC v
mkEditForm 		:: !(InIDataId d)  				!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkStoreForm 	:: !(InIDataId d)  !(d -> d)	!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkSelfForm 		:: !(InIDataId d)  !(d -> d)	!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkApplyEditForm	:: !(InIDataId d)  !d			!*HSt -> (Form d,!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d

mkSubStateForm :: !(InIDataId !subState) !state !(subState state -> state) !*HSt -> (Form state,!*HSt)
																				| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC subState

// **** forms for lists ****

listForm 		:: !(InIDataId [a]) 			!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
horlistForm 	:: !(InIDataId [a]) 			!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
vertlistForm 	:: !(InIDataId [a]) 			!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
table_hv_Form 	:: !(InIDataId [[a]])		 	!*HSt -> (Form [[a]],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
layoutListForm	:: !([BodyTag] [BodyTag] -> [BodyTag]) 
                       !(!(InIDataId a)	!*HSt  -> (Form a,*HSt))
                        !(InIDataId [a]) !*HSt -> (Form [a],!*HSt)			| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a

// User controlled number of list elements will be shown, including delete and append buttons; Int indicates max number of browse buttons

vertlistFormButs:: !Int !(InIDataId [a]) 		!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a


// **** forms for tuples ****

t2EditForm  	:: !(InIDataId (a,b))			!*HSt -> ((Form a,Form b),!*HSt)| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b

t3EditForm  	:: !(InIDataId (a,b,c))	 		!*HSt -> ((Form a,Form b,Form c),!*HSt) 
																				| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																   				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
																   				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC c
t4EditForm  	:: !(InIDataId (a,b,c,d))	 	!*HSt -> ((Form a,Form b,Form c,Form d),!*HSt)
																				| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																   				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
																   				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC c
																   				& gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
// **** special buttons ****

counterForm 	:: !(InIDataId a)	  			!*HSt -> (Form a,!*HSt) | +, -, one
																			,  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
// buttons returning index between 1 to n given stepsize, n, maximal numberofbuttuns to show

browseButtons :: !(InIDataId !Int) !Int !Int !Int !*HSt -> (Form Int,!*HSt)

// **** to each button below a function is assigned which is returned as iData value when the corresponding button is pressed
// **** an identity function is returned when none of the set of buttons pressed 

simpleButton 	:: !String 		!(a -> a) 				!*HSt -> (Form (a -> a),!*HSt)
FuncBut 		:: !(InIDataId (Button, a -> a))		!*HSt -> (Form (a -> a),!*HSt)
ListFuncBut 	:: !(InIDataId [(Button, a -> a)])		!*HSt -> (Form (a -> a),!*HSt)
TableFuncBut 	:: !(InIDataId [[(Button, a -> a)]])	!*HSt -> (Form (a -> a),!*HSt)

//fine grain variant, mode of each button in list or table can be set

ListFuncBut2 	:: !(InIDataId [(Mode,Button, a -> a)])		!*HSt -> (Form (a -> a),!*HSt)
TableFuncBut2 	:: !(InIDataId [[(Mode,Button, a -> a)]])	!*HSt -> (Form (a -> a),!*HSt)

// assign function to each check box which gets an integer of the selected box and the settings of all other boxes
// in addition to the chosen function the settings of all check boxes is returned

ListFuncCheckBox:: !(InIDataId [(CheckBox, Bool [Bool] a -> a)]) !*HSt -> (Form (a -> a,[Bool]),!*HSt)

// assign function to each radio button which gets an integer of the selected radio
// in addition to the chosen function an integer indicating the selected radio button is returned

ListFuncRadio 		:: !(InIDataId (Int,[Int a -> a]))			!*HSt -> (Form (a -> a,Int),!*HSt)

// assign function to each pull down menu which gets an integer of the selected menu element
// in addition to the chosen function an integer indicating the selected menu item is returned

FuncMenu 			:: !(InIDataId (Int,[(String, a -> a)]))	!*HSt -> (Form (a -> a,Int),!*HSt)

// **** special objects ****

mediaPlayer			:: (Int,Int) Bool String -> BodyTag		// plays movies, music etc; parameters (height,width) autostart filename
MailForm 			:: String Int Int -> BodyTag 			// mailadddres, row size, col size
MailApplicationLink :: String String String -> BodyTag 		// Link will start mail application: mailadddres, subject, contensbody

// **** scripts ****

// openWindowScript will open a new browser window displaying the html code
// parameters resp: scriptname() height width toolbar menubar scrollbars resizable location status html
openWindowScript 	::  !String !Int !Int !Bool !Bool !Bool !Bool !Bool !Bool !Html -> Script

// openNoticeScript simplified version of openWindowScript
// parameters are resp: scriptname() height width html
openNoticeScript 	::  !String !Int !Int !Html -> Script

OnLoadException		:: (Bool,String) -> [BodyAttr]					// to produce message on opening page
