definition module htmlHandler

// Main entrance for en user
// Generating HTML code, converting Clean types to GEC's, automatic dealing with form's ..
// (c) MJP 2005 *** under construction

import htmlDataDef
import GenPrint
import GenParse

// doHtml main routine for generating & handling of a Html form

:: HSt 	// unique state to pass around *** still has to define it unique !!! ***

doHtml :: (HSt -> (Html,HSt)) *World -> *World 

// mkHGEC converts any Clean type into a Html GEC

:: FormID	 	:== String					// unique id identifying the form
:: HMode a		= Edit (a -> a)
				| Set 

mkHGEC :: FormID (HMode a) a HSt -> (a,(Body,HSt)) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a 

// generic functions that do the real work,
// end user just has to derive them for mkHGEC ...

// gHGEC converts any Clean type to html code (form) to be used in a body
// gUpd updates a value of type t given any user input in the html form

:: UpdMode

generic gHGEC a :: a HSt -> (Body, HSt)	
generic gUpd a 	:: UpdMode a -> (UpdMode,a)

derive gHGEC Int, Real, String, UNIT, PAIR, EITHER, OBJECT, CONS, (,) 
derive gUpd  Int, Real, String, UNIT, PAIR, EITHER, OBJECT, CONS, (,) 

// Clean types with a special representation

defsize :== 10															// size of inputfield
:: CHButton = CHPressed | CHButton Int String

derive gHGEC 	(,,), CHButton
derive gUpd  	(,,), CHButton
derive gPrint 	(,,), CHButton
derive gParse 	(,,), CHButton



