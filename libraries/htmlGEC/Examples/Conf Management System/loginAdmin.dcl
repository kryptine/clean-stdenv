definition module loginAdmin

import StdEnv, StdHtml

:: Account s			= 	{ login			:: Login		// login info		
							, state			:: s			// state 
							}
:: Login 				= 	{ loginName 	:: String		// Should be unique
							, password		:: String		// Should remain secret
							}

:: Accounts s			:== [Account s]

:: Judgement			:==	(Bool,String)					// True if invariant holds, error message otherwise
OK						:== (True,"")

instance == (Account s)
instance <  (Account s)

mkLogin 			:: String String 	-> Login
mkAccount			:: Login s 			-> Account s

addAccount 			:: 			(Account s) (Accounts s) -> (Accounts s) 
changePassword 		:: String 	(Account s) (Accounts s) -> (Accounts s) 
changeAccount 		:: 			(Account s) (Accounts s) -> (Accounts s) 
hasAccount 			:: 			Login		(Accounts s) -> (Maybe (Account s))
removeAccount 		:: 			(Account s) (Accounts s) -> (Accounts s) 

invariantLogins		:: 			[Login] 	 -> Judgement
invariantLogAccounts:: 			(Accounts s) -> Judgement
