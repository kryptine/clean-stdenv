definition module htmlEncodeDecode

// maintains the state of forms
// internally (in a tree containing the state of all forms),
// and externally, either in a html form or in files on disk.
// provides the encoding and decoding of information between browser and myprogram.exe via myprogram.php
// (c) 2005 - MJP

import StdMaybe
import GenParse, GenPrint
import htmlDataDef, htmlFormData

// Maintaining the internal state of all forms

:: *FormStates 												// collection of all states of all forms

emptyFormStates :: *FormStates								// creates emtpy states

findState 		:: !FormId *FormStates *NWorld 				// find the state value given FormId and a correct type
					-> (Bool, Maybe a,*FormStates,*NWorld)	// true if form has not yet been previously inspected 	
												| gParse{|*|} a & TC a		
replaceState 	:: !FormId a *FormStates *NWorld 			// replace state given FormId
					-> (*FormStates,*NWorld)	| gPrint{|*|} a & TC a

getTriplet  	:: *FormStates -> (!Maybe a, !Maybe b,*FormStates) // triplet, value of changed part
												| gParse{|*|} a & gParse{|*|} b 
getUpdateId 	:: *FormStates -> (String,*FormStates)		// id of previously changed form
getUpdate 		:: *FormStates -> (String,*FormStates)		// value typed in by user as string

// serializing, de-serializing, storage and retrieval of form states

retrieveFormStates 	:: ServerKind (Maybe String) *NWorld -> (*FormStates,*NWorld) 		// retrieves all initial form states wherever they are stored
storeFormStates 	:: !FormStates *NWorld -> (BodyTag,*NWorld) // storage in files (persistent), returning script which stores other states in html for the next round

// fstate handling used for testing only

initTestFormStates 	::  *NWorld -> (*FormStates,*NWorld) 									// creates initial empty form states
setTestFormStates 	::  String String String *FormStates *NWorld -> (*FormStates,*NWorld) 	// stores triplet updateid update in state 

// constants that maybe useful

:: ServerKind
	= External		// An external Server has call to this executable (currently via a PhP script)
	| JustTesting	// No Server attached at all, intended for testing (in collaboration with Gast)
	| Internal		// No external server needed: a Clean Server will be atached to this executable

ThisExe			:: ServerKind -> String		// name of this executable
MyPhP 			:: ServerKind -> String		// name of php script interface between server and this executable
MyDir 			:: ServerKind -> String		// name of directory in which persistent form info is stored

traceHtmlInput	:: ServerKind (Maybe String) -> BodyTag		// for debugging showing the information received from browser

// low level encoding of information

encodeInfo 		:: a -> String | gPrint{|*|} a				// serialization to a Mime format, used to encode input forms 
decodeInfo 		:: String -> Maybe a | gParse{|*|} a		// de-serialization from Mime format to a Clean value
//encodeString	:: String -> String							// ordinary String to String in Mime format
//decodeString 	:: String -> *String						// fString in Mime format to ordinary String
callClean 		:: Script									// script that will take care of sending the required input to this application


urlDecode 		:: [Char] -> [Char]


