definition module htmlEncodeDecode

// encoding and decoding of information

import StdMaybe
import GenParse, htmlPrint, htmlDataDef

// decoding of arguments received from client

GetArgs 		:: String 		// returns the arguments passed to this executable as is
ThisExe			:: String		// name of this executable
MyPhP 			:: String		// name of php script interface between server and this executable
UpdateInfo  	:: (String,String,String,String) // update, new value, state, id of state
CheckUpdateInfo :: (Maybe a, Maybe b, Maybe c, Maybe d) | gParse{|*|} a & gParse{|*|} b & gParse{|*|} c & gParse{|*|} d
traceHtmlInput	:: Body			// to show in html page what we have received

// encoding of special input

encodeUpdate :: a -> String | gPrint{|*|} a
encodeHidden :: a -> String | gPrint{|*|} a

// decoding of the special input

//decodeInput :: Maybe (Int,UpdValue)  // parsing input parameters submitted to applications

// basic encoding decoding

encodeInfo 	:: a -> String 			| gPrint{|*|} a		// to store info in Html Page
decodeInfo :: String -> Maybe a 	| gParse{|*|} a 	// to retreive it back

urlEncode 	:: [Char] -> [Char]				// all non alphanum are encoded to %HexHex
urlDecode 	:: [Char] -> [Char]				// and backwards

		
mkString 	:: [Char] -> String				// just handy utility functions
mkList 		:: String -> [Char]

