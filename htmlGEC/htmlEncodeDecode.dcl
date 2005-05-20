definition module htmlEncodeDecode

// encoding and decoding of information between browser and myprogram.exe via myprogram.php
// (c) 2005 - MJP

import StdMaybe
import GenParse, GenPrint, htmlDataDef

// constants that maybe useful

ThisExe			:: String		// name of this executable
MyPhP 			:: String		// name of php script interface between server and this executable

traceHtmlInput	:: BodyTag		// for debugging showing the information received from browser

// Decoding of information

:: *FormStates 									// collection of all states of all forms

initFormStates 	:: *FormStates 					// initial state as received from browser
findState 		:: !String *FormStates -> (Bool, Maybe a,*FormStates)		| gParse{|*|} a // true if form has not yet been updated 	
replaceState 	:: !String a *FormStates -> *FormStates	| gPrint{|*|} a // replace state given FormId
addScript 		:: *FormStates -> BodyTag		// script which stores the global state for the next round


// low level encoding of information

encodeInfo 		:: a -> String | gPrint{|*|} a	// serialization to a Mime format 
callClean 		:: Script						// script that will take care of sending the required input to this application

// low level decoding of information

CheckUpdateId 			:: String				// FormId of form previously changed by user
CheckUpdate 			:: (!Maybe a, !Maybe b) | gParse{|*|} a & gParse{|*|} b // value of updated form, value of changed part
StrippedCheckUpdateId 	:: String				// used to determine related formid's e.g. for radio buttons
AnyInput 				:: String				// any input is accepted if a string is required

// utility 

mkString 	:: [Char] -> String
mkList 		:: String -> [Char]
