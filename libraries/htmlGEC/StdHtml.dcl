definition module StdHtml

// main portal for Clean Web applications
// (c) 2005 MJP 

import

// intended for end user:

	 		htmlDataDef			// Clean's ADT representation of Html with generic function to generate Html 
		,	htmlHandler			// generic stuf to convert any Clean type to Html ADT + to deal with form info		
		,	htmlHGEClib			// HGEC library similar to the AGEC lib

// internal stuf
		
		,	htmlEncodeDecode	// low level communication with client & server
		,	htmlTrivial			// some trivial generic bimap derives that we need here
		