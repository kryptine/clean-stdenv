definition module StdHtml

// main portal for Clean Web applications
// (c) 2005 MJP 

import

// intended for end user:

	 		htmlDataDef			// Clean's ADT representation of Html including generic function to generate Html 
		,	htmlHandler			// generic stuf to convert any Clean type to Html		
		,	htmlHGEClib			// HGEC library similar to the AGEC lib
		,	htmlHGECarrow		// HGEC arrow instantiations

		,	GenParse			// Standard Generic Parse module from generic library
		,	GenPrint			// Standard Generic Print module from generic library

// internal stuf
		
		,	htmlEncodeDecode	// low level communication between client browser and the clean executable
		,	htmlTrivial			// some trivial generic bimap derives
		