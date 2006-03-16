definition module loginAdmin

import StdEnv, StdLib

:: Account s			= 	{ login			:: Login		// login info		
							, state			:: s			// state 
							}
:: Login 				= 	{ loginName 	:: String		// Should be unique
							, password		:: String		// Should remain secret
							}

:: Accounts s			:== [Account s]

instance == (Account s)
instance <  (Account s)

mkLogin 			:: String String 	-> Login
mkAccount			:: Login s 			-> Account s

addAccount 			:: 			(Account s) (Accounts s) -> (Accounts s) 
changePassword 		:: String 	(Account s) (Accounts s) -> (Accounts s) 
changeAccount 		:: 			(Account s) (Accounts s) -> (Accounts s) 
hasAccount 			:: 			Login		(Accounts s) -> (Maybe (Account s))
removeAccount 		:: 			(Account s) (Accounts s) -> (Accounts s) 

// Checking invariants

:: Judgement		:==	(Bool,String)					
OK 					::	Judgement
addJudgement		::	Judgement -> (Judgement -> Judgement)

invariantLogins		:: 	[Login] 	 -> Judgement
invariantLogAccounts::	(Accounts s) -> Judgement
