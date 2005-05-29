definition module htmlHandler

// Main entrance for en user
// Generating HTML code, converting Clean types to GEC's, automatic dealing with form's ..
// (c) MJP 2005 *** under construction

import htmlDataDef, htmlEncodeDecode, htmlFormData
import GenPrint
import GenParse
import StdBool, StdFile

// doHtml main wrapper for generating & handling of a Html form

doHtml :: (*HSt -> (Html,!*HSt)) *World -> *World 

:: *HSt 								// unique state required for creating Html forms
instance FileSystem HSt					// enabling file IO on HSt

mkViewForm 		:: !FormId 	d !Mode !(HBimap d v) !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC v

// mkViewForm is the swiss army nife function creating stateful interactive forms with a view v of data d
// make shure that all editors have a unique identifier !

// For convenience all kinds of variants of "mkViewForm" are predefined, simply by chosing certain defaults for the HBimap.

// mkBimap			: editor, using Bimaps instead of HBimaps
// mkEdit  		 	: editor, remembers its state, argument is taken as initial state 
// mkSet			: editor, always displays argument regardless of current state of the editor
// mkStore			: applies function to the internal state; second argument is initial state
// mkSelf			: editor, state is updated with function; second argument is taken as initial state   
// mkApply 			: displays application of function to the argument
// mkApplyEdit		: editor, displays its first argument if it is not updated; second argument is initial state

mkBimapEditor 	:: !FormId 	d !Mode !(Bimap d v)	!*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC v
mkEditForm 		:: !FormId 	d !Mode					!*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkSetForm 		:: !FormId 	d !Mode					!*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkStoreForm 	:: !FormId 	d !(d -> d)				!*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkSelfForm 		:: !FormId 	d !(d -> d)				!*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkApplyForm 	:: !FormId 	d !(d -> d)				!*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkApplyEditForm	:: !FormId 	d !d					!*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d

// utility functions

toHtml 			:: a -> BodyTag | gForm {|*|} a		// toHtml displays any type into a non-editable form
toBody 			:: (Form a) -> BodyTag				// just (BodyTag form.body)

pFormId			:: String -> FormId					// persitent formid
sFormId			:: String -> FormId					// session formid
nFormId			:: String -> FormId					// page formid
	
// Clean types that have a special representation

// lay out

:: <-> a b		= (<->) infixl 5 a b				// place b to the left of a
:: <|> a b		= (<|>) infixl 4 a b				// place b below a

:: DisplayMode a 
				= DisplayMode a						// non-editable display of a
				| EditMode    a						// editable
				| HideMode    a						// hiding a
				| EmptyMode							// nothing to display or hide

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
	
instance toBool CheckBox, Button, RadioButton		// True if checkbox checked, button pressed

// generic functions that do the real work,
// end user only has to derive them when using the functions above

// gForm converts any Clean type to html code (form) to be used in a body
// gUpd updates a value of type t given any user input in the html form

:: UpdMode

generic gForm a :: !FormId a !Mode !*HSt -> *(Form a, !*HSt)	
generic gUpd a 	:: UpdMode a -> (UpdMode,a)

derive bimap Form

derive gForm Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, (,) 
derive gUpd  Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, (,) 

derive gForm 		 (,,), (,,,), (<->), <|>, DisplayMode, Button, CheckBox, RadioButton, PullDownMenu, TextInput 
derive gUpd  		 (,,), (,,,), (<->), <|>, DisplayMode, Button, CheckBox, RadioButton, PullDownMenu, TextInput
derive gPrint 	(,), (,,), (,,,), (<->), <|>, DisplayMode, Button, CheckBox, RadioButton, PullDownMenu, TextInput
derive gParse 	(,), (,,), (,,,), (<->), <|>, DisplayMode, Button, CheckBox, RadioButton, PullDownMenu, TextInput

// specialize has to be used if one wants to specialize gForm for a user-defined type

specialize :: (FormId a Mode *HSt -> (Form a,*HSt)) FormId a Mode *HSt -> (Form a,*HSt) | gUpd {|*|} a

