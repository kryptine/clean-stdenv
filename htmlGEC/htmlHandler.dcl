definition module htmlHandler

// Main entrance for en user
// Generating HTML code, converting Clean types to GEC's, automatic dealing with form's ..
// (c) MJP 2005 *** under construction

import htmlDataDef
import GenPrint
import GenParse

// doHtml main routine for generating & handling of a Html form

doHtml :: (HSt -> (Html,HSt)) *World -> *World //| gHpr{|*|} a & gUpd{|*|}  a  & gParse{|*|} a 

// mkHGEC converts any Clean type into a Html GEC

:: FormID	 	:== String					// unique id identifying the form

mkHGEC :: FormID (a -> a) a HSt -> (a,(Body,HSt)) | gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 

// generic functions that do the real work

:: HSt // unique state to pass around *** still has to define it unique !!! ***

// gHGEC converts any Clean type to html code (form) to be used in a body

generic gHGEC a :: a HSt -> (Body, HSt)	

derive gHGEC Int, Real, String, UNIT, PAIR, EITHER, OBJECT, CONS 

// gUpd updates a value of type t given any user input in the html form

:: UpdMode

generic gUpd t :: UpdMode t -> (UpdMode,t)

derive gUpd Int, UNIT, PAIR, EITHER, OBJECT, CONS, (,)

// Clean types with a special representation

defsize :== 10															// size of inputfield

:: CHButton = CHPressed | CHButton Int String

derive gHGEC 	CHButton, (,), (,,)
derive gUpd  	CHButton
derive gHpr 	CHButton//, (,)

derive gPrint 	CHButton
derive gParse 	CHButton


