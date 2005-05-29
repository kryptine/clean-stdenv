definition module htmlFormData

// common data type definition used for forms
// (c) 2005 - MJP

import htmlDataDef
import StdMaybe

:: FormId									// properties one has to assign to any form 
	=	{ id 		:: !String				// id *uniquely* identifying the form
		, livetime	:: !Livetime			// livetime of form
		, mode		:: !Mode				// editable or not
		}

:: Livetime									// defines how long a form will be maintained		
	= 	Persistent							// form will live "forever" in a file
	| 	Session								// form will live as long as one browses between the pages offered by the application
	| 	Page								// form will be automatically garbage collected when no reference is made to it			

:: Mode										// one can choose:
	=	Edit								// an editable form
	| 	Display								// a non-editable form

:: HBimap d v 								// swiss army nife allowing to make a distinction between data and view domain
	=	{ toForm   	:: d (Maybe v) -> v		// converts data to view domain, given current view
		, updForm 	:: Bool v -> v			// update function, True when the form is edited 
		, fromForm 	:: Bool v -> d			// converts view back to data domain, True when form is edited
		, resetForm :: Maybe (v -> v)		// can be used to reset view (eg for buttons)
		}

:: Form a 									// result of any form
	=	{ changed 	:: Bool					// the user has edited the form
		, value		:: a					// current value in data domain (feel)
		, body		:: [BodyTag]			// html code to create the form, representing view domain (look)
		}
