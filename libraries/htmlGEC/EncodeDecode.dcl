definition module EncodeDecode

// provides the encoding and decoding of information between browser and the executable
// (c) 2005 - MJP

import StdMaybe
import GenParse, GenPrint
import htmlDataDef, htmlFormData

:: HtmlState  		:== (!Formid,!Lifespan,!StorageFormat,!SerializedState)
:: Formid			:== String		// uniquely named !
:: SerializedState 	:== String 		// not encoded !

:: ServerKind
	= External		// An external Server has call to this executable (currently via a PhP script)
	| JustTesting	// No Server attached at all, intended for testing (in collaboration with Gast)
	| Internal		// No external server needed: a Clean Server will be atached to this executable

// type driven encoding of strings, used to encode triplets

encodeInfo :: a -> String | gPrint{|*|} a
decodeInfo :: String -> Maybe a | gParse{|*|} a

// serializing, de-serializing of iData states to strings stored in the html page

EncodeHtmlStates 			:: ![HtmlState] -> String
DecodeHtmlStatesAndUpdate 	:: ServerKind (Maybe String) -> ([HtmlState],String,String) // + triplet + update

// serializing, de-serializing of iData state stored in files

writeState 			:: !String !String !String !*NWorld -> *NWorld 
readStringState 	:: !String !String !*NWorld -> (!String,!*NWorld) 
readDynamicState 	:: !String !String !*NWorld -> (!String,!*NWorld) 

// constants that maybe useful

ThisExe				:: ServerKind -> String		// name of this executable
MyPhP 				:: ServerKind -> String		// name of php script interface between server and this executable
MyDir 				:: ServerKind -> String		// name of directory in which persistent form info is stored

traceHtmlInput		:: ServerKind (Maybe String) -> BodyTag		// for debugging showing the information received from browser

// low level encoding and decoding 

encodeString :: String -> String
decodeString :: String -> *String
