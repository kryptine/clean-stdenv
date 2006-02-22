definition module loginAdminIData

import loginAdmin

derive gForm  	Login
derive gUpd 	Login
derive gPrint	Login
derive gParse	Login

// login page: returns LoginState when loged in properly, page info, hst

loginPage  :: !(LoginStates state) !*HSt -> (!Maybe (LoginState state),![BodyTag],!*HSt)
							| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC state

// changePassword page: returns True if change done, LoginState, page info, hst

changePasswordPage :: !Login !*HSt -> (!Maybe Login,![BodyTag],!*HSt)

