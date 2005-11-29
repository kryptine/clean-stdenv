definition module StdHtml

// main portal for generating Clean Web applications using the iData / GEC technique
// (c) 2005 MJP 

import

// intended for end user:

			htmlHandler			// the kernel module for iData creation
		,	htmlFormData		// iData type definitions

		,	htmlFormlib			// illustrative collection of form creating functions defined on top of htmlHandler  

	 	,	htmlDataDef			// Clean's ADT representation of Html
		,	htmlStyleDef		// Clean's ADT representation of Style sheets

		,	htmlArrow			// arrow instantiations for iData forms

// free to change when the default style of the generated web pages is not appealing:

		,	htmlStylelib		// style definitions   

// of general use:

		,	htmlTrivial			// some trivial generic bimap derives
