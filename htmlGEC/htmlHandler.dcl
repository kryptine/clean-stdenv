definition module htmlHandler

import htmlDataDef
import htmlPrint
import GenParse

// doHtml handles an update of a page

doHtml 		:: (a -> Html) a *World -> *World | gHpr{|*|} a & gUpd{|*|}  a  & gParse{|*|} a 

// showClean shows any data structure

showClean 	:: a -> Body 			| gHGEC{|*|} a	& gPrint{|*|} a 	

// updClean updates any data structure with the form info

updClean  	:: a -> a				| gUpd{|*|}  a  & gParse{|*|} a	

// generic stuf ...

:: HSt 		// state to pass around

// gHGEC converts any Clean type to html code (form) to be used in a body

generic gHGEC a :: State a HSt -> (Body,HSt)	

derive gHGEC Int, Real, String, UNIT, PAIR, EITHER, OBJECT, CONS 

// gUpd updates a value of type t given any user input in the html form

:: UpdMode

generic gUpd t :: UpdMode t -> (UpdMode,t)

derive gUpd Int, UNIT, PAIR, EITHER, OBJECT, CONS, (,)

:: State :== (String,String) // (id,global state)

