definition module htmlHandler

import htmlDataDef, StdGeneric, StdMaybe
import htmlPrint
import GenParse

showClean :: a -> Body 			| gHGEC{|*|} a	& gPrint{|*|} a // kindles version of gHGEC
updClean  :: a -> a				| gUpd{|*|}  a  & gParse{|*|} a					// kindles version of gUpd


:: HSt 		// state to pass around

// gHGEC converts any type to html code to be used in a body

:: State :== (String,String) // (id,global state)


generic gHGEC a :: State a HSt -> (Body,HSt)	

derive gHGEC Int, Real, String, UNIT, PAIR, EITHER, OBJECT, CONS 

// gUpd updtes any expressions given a user input in the html form

:: UpdMode

generic gUpd t :: UpdMode t -> (UpdMode,t)

derive gUpd Int, UNIT, PAIR, EITHER, OBJECT, CONS

 
//GetArgs :: String		// arguments received from client after removing garbage

