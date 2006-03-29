definition module loginAdminIData

import loginAdmin, htmlHandler

// login page: 			returns account corresponding to adminstrated user

loginPage  			:: !(Accounts s)	!*HSt -> (Maybe (Account s),[BodyTag],!*HSt)

// changePasswordPage: returns new account if changed password has been approved

changePasswordPage 	:: !(Account s) 	!*HSt -> (Maybe (Account s),[BodyTag],!*HSt)

// Temporal global store in which any editor can report errors:

ExceptionStore 		:: (Judgement -> Judgement) *HSt -> (Judgement,!*HSt)
