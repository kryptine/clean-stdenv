definition module StdHtml

// main portal for Clean Web applications
// (c) 2005 MJP 

import

// intended for end user:

	 		htmlDataDef			// Clean's ADT representation of Html
		,	htmlStyleDef		// Clean's ADT representation of Style sheets

		,	htmlFormData		// some common data definitions used for creating forms		
		,	htmlHandler			// kernel module with all kinds of basic functions to generate forms 
		,	htmlFormlib			// a library of more sophisticated form functions 
		,	htmlArrow			// arrow instantiations for html forms

// free to use

		,	htmlStylelib		// style definitions   

// internal stuf
		
		,	htmlEncodeDecode	// low level communication between client browser and the clean executable
		,	htmlTrivial			// some trivial generic bimap derives
		,	GenParse			// Standard Generic Parse module from generic library
		,	GenPrint			// Standard Generic Print module from generic library
		