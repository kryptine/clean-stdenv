definition module loginAdmin

import StdEnv, StdMaybe, judgement

:: Account s			= 	{ login			:: Login		// login info		
							, state			:: s			// state 
							}
:: Login 				= 	{ loginName 	:: String		// Should be unique
							, password		:: String		// Should remain secret
							}
:: Accounts s			:== [Account s]

// Access functions

instance == (Account s)
instance <  (Account s)

mkLogin 			:: String String 	-> Login
mkAccount			:: Login s 			-> Account s
changePassword 		:: String  (Account s) -> (Account s) 

addAccount 			::	(Account s) (Accounts s) -> (Accounts s) 
removeAccount 		::	(Account s) (Accounts s) -> (Accounts s) 
changeAccount 		::	(Account s) (Accounts s) -> (Accounts s) 
hasAccount 			::	Login		(Accounts s) -> (Maybe (Account s))

//	Invariants

invariantLogins		:: 	[Login] 	 -> Judgement
invariantLogAccounts::	(Accounts s) -> Judgement
