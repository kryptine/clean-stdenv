definition module htmlHandler

// Main entrance for en user
// Generating HTML code, converting Clean types to GEC's, automatic dealing with form's ..
// (c) MJP 2005 *** under construction

import htmlDataDef
import GenPrint
import GenParse
import StdBool

:: HSt 	// unique state to pass around

// doHtml main wrapper for generating & handling of a Html form

doHtml :: (*HSt -> (Html,!*HSt)) *World -> *World 

// toHtml displays any type into a non-editable form

toHtml :: a -> BodyTag | gForm {|*|} a
toBody :: (Form a) -> BodyTag

// mkViewForm is the swiss army nife function creating stateful interactive forms with a view v of data d
// make shure that all editors have a unique identifier !

:: FormId	 	:== String				// unique id identifying the form
:: Mode			= Edit					// indicates an editable form
				| Display				// indicates a non-editable form
:: HBimap d v =	{ toForm   	:: d (Maybe v) -> v		// converts data to view domain, given current view
				, updForm 	:: Bool v -> v			// update function, True when the form is edited 
				, fromForm 	:: Bool v -> d			// converts view back to data domain, True when form is edited
				, resetForm :: Maybe (v -> v)		// can be used to reset view (eg for buttons)
				}

:: Form a =		{ changed 	:: Bool
				, value		:: a
				, body		:: [BodyTag]
				}

mkViewForm 		:: !FormId 	!Mode !(HBimap d v) 	d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} v 

// For convenience all kinds of variants of "mkViewForm" are predefined, simply by chosing certain defaults for the HBimap.

// mkBimap			: editor, using Bimaps instead of HBimaps
// mkEdit  		 	: editor, remembers its state, argument is taken as initial state 
// mkEdit2    		: used for feedback loop in html arrows
// mkSet			: editor, always displays argument regardless of current state of the editor
// mkSelf			: editor, state is updated with function; second argument is taken as initial state   
// mkApply 			: displays application of function to the argument
// mkStore			: applies function to the internal state; second argument is initial state
// mkApplyEdit		: editor, displays its first argument if it is not updated; second argument is initial state

mkBimapEditor 	:: !FormId 	!Mode !(Bimap d v)  d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} v
mkEditForm 		:: !FormId 	!Mode				d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkEditForm2		:: !FormId  !Mode 				d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkSetForm 		:: !FormId 	!Mode				d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkSelfForm 		:: !FormId 	!(d -> d)			d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkApplyForm 	:: !FormId 	!(d -> d)			d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkStoreForm 	:: !FormId 	!(d -> d)			d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkApplyEditForm	:: !FormId 	!d					d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d

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
:: CheckBox		= CBChecked FormId 					// checkbox 	checked
				| CBNotChecked FormId				// checkbox 	not checked
:: RadioButton	= RBChecked FormId					// radiobutton 	checked
				| RBNotChecked FormId				// radiobutton	not checked
:: PullDownMenu	= PullDown (Int,Int) (Int,[String]) // pulldownmenu (number visible,width) (item chosen,menulist)		
	
instance toBool CheckBox, Button, RadioButton		// True if checkbox checked, button pressed

// Some default constants used for the length of input boxes

defsize  :== 10										// size of inputfield
defpixel :== 83										// size in pixels for buttons, pull-down buttons
backcolor :== "#6699CC"								// background color of non-editable fields

// generic functions that do the real work,
// end user only has to derive them when using the functions above

// gForm converts any Clean type to html code (form) to be used in a body
// gUpd updates a value of type t given any user input in the html form

:: UpdMode

generic gForm a :: !FormId !Mode a !*HSt -> *(Form a, !*HSt)	
generic gUpd a 	:: UpdMode a -> (UpdMode,a)

derive bimap Form

derive gForm Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, (,) 
derive gUpd  Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, (,) 

derive gForm 		 (,,), (<->), <|>, DisplayMode, Button, CheckBox, RadioButton, PullDownMenu 
derive gUpd  		 (,,), (<->), <|>, DisplayMode, Button, CheckBox, RadioButton, PullDownMenu
derive gPrint 	(,), (,,), (<->), <|>, DisplayMode, Button, CheckBox, RadioButton, PullDownMenu
derive gParse 	(,), (,,), (<->), <|>, DisplayMode, Button, CheckBox, RadioButton, PullDownMenu

// specialize should be used if one want to make specialized instantiations of gForm
// it ensures that update positions remain counted correctly

specialize :: (FormId Mode a *HSt -> (Form a,*HSt)) FormId Mode a *HSt -> (Form a,*HSt) | gUpd {|*|} a

