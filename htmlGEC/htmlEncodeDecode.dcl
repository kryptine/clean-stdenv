definition module htmlEncodeDecode

// encoding and decoding of information
// (c) 2005 - MJP

import StdMaybe
import GenParse, GenPrint, htmlDataDef

//	:: UpdateInfo a b
//			= 	{	updateVal :: Maybe a	// value of submitted input field
//				,	newVal	  :: Maybe b	// new value it should get, if applicable	
//				,	idVal	  :: String		// identification of Clean data structure which has been updated
//				,	stateVal  :: String		// global state stored in html page
//				}

//ParseUpdateInfo :: UpdateInfo a b | gParse{|*|} a & gParse{|*|} b 
//ParseUpdateInfo :: UpdateInfo a b c d | gParse{|*|} a & gParse{|*|} b & gParse{|*|} c & gParse{|*|} d

// decoding of arguments received from client

GetArgs 		:: String 		// returns the arguments passed to this executable as is
ThisExe			:: String		// name of this executable
MyPhP 			:: String		// name of php script interface between server and this executable
UpdateInfo  	:: (String,String,String) // update, new value, id of updated state, global state

CheckUpdateId 	:: String
CheckUpdate 	:: (Maybe a, Maybe b) | gParse{|*|} a & gParse{|*|} b 
CheckGlobalState :: String
ShiftState :: String -> (Maybe (String,a),String) | gParse{|*|} a 


//CheckUpdateInfo :: (Maybe a, Maybe b, Maybe c, Maybe d) | gParse{|*|} a & gParse{|*|} b & gParse{|*|} c & gParse{|*|} d
traceHtmlInput	:: Body			// to show in html page what we have received

// encoding of special input

encodeUpdate :: a -> String | gPrint{|*|} a
encodeHidden :: (String, String) -> String //| gPrint{|*|} a

// basic encoding decoding

encodeInfo 	:: a -> String 			| gPrint{|*|} a		// to store info in Html Page
decodeInfo :: String -> Maybe a 	| gParse{|*|} a 	// to retreive it back

urlEncode 	:: [Char] -> [Char]				// all non alphanum are encoded to %HexHex
urlEncodeS 	:: String -> String				// same for strings

urlDecode 	:: [Char] -> [Char]				// and backwards
urlDecodeS 	:: String -> String				// same for string

		
mkString 	:: [Char] -> String				// just handy utility functions
mkList 		:: String -> [Char]

urlEncodeState :: [(String,String)] -> String
