definition module StdHtml

// main portal for Clean Web applications
// (c) 2005 MJP 

import

// intended for end user:

	 		htmlDataDef			// Clean's ADT representation of Html
		,	htmlStyleDef		// Clean's ADT representation of Style sheets

		,	htmlFormlib			// illustrative collection of form creating functions defined on top of htmlHandler  
		,	htmlFormData		// some common data definitions needed for creating forms		

		,	htmlHandler			// the kernel module for form creation

		,	htmlArrow			// arrow instantiations for html forms

// free to change when the default style is not appealing

		,	htmlStylelib		// style definitions   
		,	htmlTrivial			// some trivial generic bimap derives

// internally used:
		
		,	htmlEncodeDecode	// low level communication between client browser and the clean executable
		,	GenParse			// Standard Generic Parse module from generic library
		,	GenPrint			// Standard Generic Print module from generic library
