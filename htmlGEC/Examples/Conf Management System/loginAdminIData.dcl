definition module loginAdminIData

import loginAdmin, htmlHandler

// login page: 			returns account if user is adminstrated

loginPage  			:: !(Accounts s)	!*HSt -> (Maybe (Account s),[BodyTag],!*HSt)

// changePasswordPage: returns new account if changed password has been approved

changePasswordPage 	:: !(Account s) 	!*HSt -> (Maybe (Account s),[BodyTag],!*HSt)

