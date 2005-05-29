definition module htmlFormData

// common data type definition used for forms
// (c) 2005 - MJP

import htmlDataDef
import StdMaybe

:: FormId = 	{ id 		:: !String				// unique id identifying the form
				, livetime	:: !Livetime				// livetime of form
				}

:: Livetime		= Persistent						// form will be stored in persistent store
				| Session							// form will always be stored during the session (in the page)
				| Page								// form will be garbage collected when no reference is made to it			

:: Mode			= Edit								// indicates an editable form
				| Display							// indicates a non-editable form

:: HBimap d v =	{ toForm   	:: d (Maybe v) -> v		// converts data to view domain, given current view
				, updForm 	:: Bool v -> v			// update function, True when the form is edited 
				, fromForm 	:: Bool v -> d			// converts view back to data domain, True when form is edited
				, resetForm :: Maybe (v -> v)		// can be used to reset view (eg for buttons)
				}

:: Form a =		{ changed 	:: Bool					// the user has edited the form
				, value		:: a					// value in data domain (feel)
				, body		:: [BodyTag]			// html code creating the form, representing view domain (look)
				}
