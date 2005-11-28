definition module StdHtml

// main portal for Clean Web applications
// (c) 2005 MJP 

import

// intended for end user:

			htmlHandler			// the kernel module for form creation

		,	htmlFormlib			// illustrative collection of form creating functions defined on top of htmlHandler  
		,	htmlFormData		// some common data definitions needed for creating forms		

	 	,	htmlDataDef			// Clean's ADT representation of Html
		,	htmlStyleDef		// Clean's ADT representation of Style sheets

		,	htmlArrow			// arrow instantiations for html forms

// free to change when the default style of the pages is not appealing:

		,	htmlStylelib		// style definitions   

// of general use:

		,	GenParse			// Standard Generic Parse module from generic library
		,	GenPrint			// Standard Generic Print module from generic library
		,	htmlTrivial			// some trivial generic bimap derives
