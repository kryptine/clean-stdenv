definition module htmlHandler

// Main entrance for en user
// Generating HTML code, converting Clean types to GEC's, automatic dealing with form's ..
// (c) MJP 2005 *** under construction

import htmlDataDef
import GenPrint
import GenParse

// doHtml main routine for generating & handling of a Html form

:: HSt 	// unique state to pass around

doHtml :: (*HSt -> (Html,!*HSt)) *World -> *World 

// mkHGEC converts any Clean type into a Html GEC

:: FormId	 	:== String				// unique id identifying the form

// the editors below are just versiosn of "mkViewHGEC".
// make shure that all editors have a unique identifier !

// mkEdit  HEdit 	: editor, argument is initial value, delivers contents of (updated) form ("source")
// mkEdit  HDisplay : same as mkSet
// mkSet			: displays its arguments
// mkApply 			: displays application of function to the argument
// mkStore			: displays application of function to the internal state, second argument is initial state
// mkApplyEdit		: editor, displays its first argument, if not updated, second argument is initial state
// mkSpecial		: editor, applies bimaps when evaluated

:: HMode		= HEdit					// indicates an editor
				| HDisplay				// indicates that one just wants to display something

mkEditHGEC 		:: !FormId 	!HMode				d !*HSt -> ((d,BodyTag),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkSetHGEC 		:: !FormId 	!HMode				d !*HSt -> ((d,BodyTag),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkSelfHGEC 		:: !FormId 	!(d -> d)			d !*HSt -> ((d,BodyTag),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkApplyHGEC 	:: !FormId 	!(d -> d)			d !*HSt -> ((d,BodyTag),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkStoreHGEC 	:: !FormId 	!(d -> d)			d !*HSt -> ((d,BodyTag),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkApplyEditHGEC	:: !FormId 	!d					d !*HSt -> ((d,BodyTag),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkSpecialEditor :: !FormId 	!HMode !(Bimap d v) d !*HSt -> ((d,BodyTag),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} v

mkEditHGEC2:: !FormId !HMode d !*HSt -> ((d,BodyTag),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d


// mkViewHGEC is the swiss army nife function creating an editor with a view v of data d

:: HBimap d v =	{ toHGEC   	:: d (Maybe v) -> v		// converts data to view domain, given current view
				, updHGEC 	:: Bool v -> v			// update function, True when the form is edited 
				, fromHGEC 	:: Bool v -> d			// converts view back to data domain, True when form is edited
				, resetHGEC :: Maybe (v -> v)		// can be used to reset view (eg for buttons)
				}

mkViewHGEC 		:: !FormId 	!HMode !(HBimap d v) 	d !*HSt -> ((d,BodyTag),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} v 

// generic functions that do the real work,
// end user just has to derive them for mkHGEC ...

// gHGEC converts any Clean type to html code (form) to be used in a body
// gUpd updates a value of type t given any user input in the html form

:: UpdMode

generic gHGEC a :: HMode a *HSt -> *((a,BodyTag), *HSt)	
generic gUpd a 	:: UpdMode a -> (UpdMode,a)

derive gHGEC Int, Real, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, (,) 
derive gUpd  Int, Real, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, (,) 

// Clean types that have a special representation


:: <-> a b		= (<->) infixl 5 a b				// place b to the left of a
:: <|> a b		= (<|>) infixl 4 a b				// place b below a
:: CHButton 	= CHPressed 
				| CHButton Int String				// button, size in pixels, text of button
				| ChButtonPict (Int,Int) String		// button, (height,width), reference to picture

:: Mode a 		= Display a
				| Hide a
				| Edit a
				| EmptyMode


derive gHGEC 		 (,,), (<->), <|>, CHButton, Mode  
derive gUpd  		 (,,), (<->), <|>, CHButton, Mode
derive gPrint 	(,), (,,), (<->), <|>, CHButton, Mode
derive gParse 	(,), (,,), (<->), <|>, CHButton, Mode

// Some default constants used for the length of input boxes

defsize  :== 10										// size of inputfield
defpixel :== 83										// size in pixels for buttons, pull-down buttons
backcolor :== "#6699CC"								// background color of non-editable fields

specialize :: (FormId HMode a *HSt -> ((a,BodyTag),*HSt)) FormId HMode a *HSt -> ((a,BodyTag),*HSt) | gUpd {|*|} a

