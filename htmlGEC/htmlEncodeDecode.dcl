definition module htmlEncodeDecode

// maintains the state of forms
// internally (in a tree containing the state of all forms),
// and externally, either in a html form or in files on disk.
// provides the encoding and decoding of information between browser and myprogram.exe via myprogram.php
// (c) 2005 - MJP

import StdMaybe
import GenParse, GenPrint
import htmlDataDef, htmlFormData

// constants that maybe useful

ThisExe			:: String		// name of this executable
MyPhP 			:: String		// name of php script interface between server and this executable
MyDir 			:: String		// name of directory in which persistent form info is stored

traceHtmlInput	:: BodyTag		// for debugging showing the information received from browser

// Decoding of information

:: *FormStates 									// collection of all states of all forms

emptyFormStates :: *FormStates					// creates emtpy states
initFormStates 	:: *FormStates 					// retrieves all form states hidden in the html page

findState 		:: !FormId *FormStates *World -> (Bool, Maybe a,*FormStates,*World)	// true if form has not yet been previously inspected 	
												| gParse{|*|} a & TC a		
replaceState 	:: !FormId a *FormStates *World -> (*FormStates,*World)				// replace state given FormId
												| gPrint{|*|} a & TC a

convStates 		:: !FormStates *World -> (BodyTag,*World) 	// script which stores the global state for the next round

// low level encoding of information

encodeInfo 		:: a -> String | gPrint{|*|} a	// serialization to a Mime format, used to encode input forms 
callClean 		:: Script						// script that will take care of sending the required input to this application

// low level decoding of information

CheckUpdateId 			:: String				// FormId of form previously changed by user
CheckUpdate 			:: (!Maybe a, !Maybe b) | gParse{|*|} a & gParse{|*|} b // value of updated form, value of changed part
StrippedCheckUpdateId 	:: String				// used to determine related formid's e.g. for radio buttons
AnyInput 				:: String				// any input is accepted if a string is required

