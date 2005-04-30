definition module htmlEncodeDecode

// encoding and decoding of information between browser and myprogram.exe via myprogram.php
// (c) 2005 - MJP

import StdMaybe
import GenParse, GenPrint, htmlDataDef

// constants that maybe useful

ThisExe			:: String		// name of this executable
MyPhP 			:: String		// name of php script interface between server and this executable

traceHtmlInput	:: BodyTag		// for debugging to show which information is received from browser

// encoding of information

:: GlobalState :== [(String,String)]

encodeInfo 		:: a -> String | gPrint{|*|} a	// format required for storing stuf in html
callClean 		:: String						// call script that will transmit input info to this executable
addScript 		:: GlobalState -> BodyTag		// the corresponding script, stores global state as well					

// decoding of information

CheckGlobalState 		:: GlobalState					// all states of all forms
findState 				:: String GlobalState -> (Maybe a) 
									| gParse{|*|} a  	// returns value of state with indicated FormId 
CheckUpdateId 			:: String						// FormId of previously updated form
CheckUpdate 			:: (!Maybe a, !Maybe b) | gParse{|*|} a & gParse{|*|} b // updated form, updated value
StrippedCheckUpdateId 	:: String						// used to determine related id's e.g. for radio buttons
AnyInput 				:: String						// any input is accepted if a string is required

