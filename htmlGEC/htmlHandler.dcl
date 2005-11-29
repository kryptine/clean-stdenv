definition module htmlHandler

// Main entrance for en user
// Generating HTML code, converting Clean types to GEC's, automatic dealing with form's ..
// (c) MJP 2005 *** under construction

import htmlDataDef, htmlFormData
import StdBool
import GenPrint, GenParse

TraceInput :== False		// set it to True if you want to see what kind of information is received from browser

:: *HSt 					// unique state required for creating Html forms

// doHtml main wrapper for generating & handling of a Html form

doHtml 			:: .(*HSt -> (Html,!*HSt)) *World -> *World  	// use this application with some external server and php
doHtmlServer 	:: (*HSt -> (Html,!*HSt))  *World -> *World 	// use this application with the build-in Clean server

// mkViewForm is the swiss army nife function creating stateful interactive forms with a view v of data d
// make shure that all editors have a unique identifier !

mkViewForm 		:: !FormId 	(Init d) !(HBimap d v) !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC v

// gForm converts any Clean type to html code (form) to be used in a body
// gUpd updates a value of type t given any user input in the html form

generic gForm a :: !FormId a !*HSt -> *(Form a, !*HSt)	
generic gUpd a 	:: UpdMode a -> (UpdMode,a)

:: UpdMode

derive bimap Form
		
// utility functions

toHtml 			:: a -> BodyTag | gForm {|*|} a				// toHtml displays any type into a non-editable form
toHtmlForm 		:: (*HSt -> *(Form a,*HSt)) -> [BodyTag] 	// toHtmlForm displays any form one can make with a form function
												| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
toBody 			:: (Form a) -> BodyTag						// just (BodyTag form.body)

getChangedId	:: !*HSt -> (String,!*HSt)					// id of form that has been changed by user

// Clean types that have a special representation

// buttons

:: Button 		= Pressed 							// button pressed
				| LButton Int String				// label   button, size in pixels, label of button
				| PButton (Int,Int) String			// picture button, (height,width), reference to picture
:: CheckBox		= CBChecked String 					// checkbox 	checked
				| CBNotChecked String				// checkbox 	not checked
:: RadioButton	= RBChecked String					// radiobutton 	checked
				| RBNotChecked String				// radiobutton	not checked
:: PullDownMenu	= PullDown (Int,Int) (Int,[String]) // pulldownmenu (number visible,width) (item chosen,menulist)		
:: TextInput	= TI Int Int						// Input box of size Size for Integers
				| TR Int Real						// Input box of size Size for Reals
				| TS Int String						// Input box of size Size for Strings
	
instance toBool   CheckBox, Button, RadioButton		// True if checkbox checked, button pressed
instance toInt    PullDownMenu						// Current index in pull down list
instance toString PullDownMenu						// Corresponding element in pull down list

// lay out

:: <-> a b		= (<->) infixl 5 a b				// place b to the left of a
:: <|> a b		= (<|>) infixl 4 a b				// place b below a

:: DisplayMode a 
				= DisplayMode a						// non-editable display of a
				| EditMode    a						// editable
				| HideMode    a						// hiding a
				| EmptyMode							// nothing to display or hide

derive gForm Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, (,) 
derive gUpd  Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, (,) 

derive gForm 		 (,,), (,,,), (<->), <|>, DisplayMode, Button, CheckBox, RadioButton, PullDownMenu, TextInput, (->) 
derive gUpd  		 (,,), (,,,), (<->), <|>, DisplayMode, Button, CheckBox, RadioButton, PullDownMenu, TextInput, (->)
derive gPrint 	(,), (,,), (,,,), (<->), <|>, DisplayMode, Button, CheckBox, RadioButton, PullDownMenu, TextInput, (->)
derive gParse 	(,), (,,), (,,,), (<->), <|>, DisplayMode, Button, CheckBox, RadioButton, PullDownMenu, TextInput, (->)

// specialize has to be used if one wants to specialize gForm for a user-defined type

specialize :: !(!FormId !(Init a) !*HSt -> (!Form a,!*HSt)) !FormId !(Init a) !*HSt -> (!Form a,!*HSt) | gUpd {|*|} a
//specialize :: (FormId a *HSt -> (Form a,*HSt)) FormId a *HSt -> (Form a,*HSt) | gUpd {|*|} a

// definitions on HSt

instance FileSystem HSt												// enabling file IO on HSt

appWorldHSt		:: !.(*World -> *World)       !*HSt -> *HSt			// enabling World operations on HSt
accWorldHSt		:: !.(*World -> *(.a,*World)) !*HSt -> (.a,!*HSt)	// enabling World operations on HSt

// for testing

import iDataState

runUserApplication :: .(*HSt -> *(.a,*HSt)) *FormStates *NWorld -> *(.a,*FormStates,*NWorld)

