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

:: FormID	 	:== String				// unique id identifying the form

:: HBimap d v =	{ toHGEC   	:: d -> v	// converts data to a HGEC for the view
				, updHGEC 	:: v -> v	// update function will only be applied if the View form is changed 
				, fromHGEC 	:: v -> d	// the view (either updated or not) is converted back to data domain
				, resetHGEC :: v -> v	// appearance of the view for the next time
				}

:: HMode		= HEdit					// indicates an editor
				| HDisplay				// indicates that one just wants to display something

mkEditHGEC 		:: FormID 	HMode 			d HSt -> (d,(Body,HSt)) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
mkSelfHGEC 		:: FormID 	(d -> d)		d HSt -> (d,(Body,HSt)) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d

mkViewHGEC 		:: FormID 	(HBimap d v) 	d HSt -> (d,(Body,HSt)) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} v 


//mkHGEC :: FormID (HMode a) a HSt -> (a,(Body,HSt)) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a 

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
:: CHButton 	= CHPressed | CHButton Int String						// button with text
:: CHHidden a 	= CHHidden a 											// hidden state

derive gHGEC 	(,,), CHButton, CHHidden  
derive gUpd  	(,,), CHButton, CHHidden
derive gPrint 	(,,), CHButton, CHHidden
derive gParse 	(,,), CHButton, CHHidden



