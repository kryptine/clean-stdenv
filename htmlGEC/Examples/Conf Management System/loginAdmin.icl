implementation module loginAdmin

import StdEnv, StdHtml, StdMaybe, judgement

instance == (Account s)
where
	(==) login1 login2 = login1.login.loginName == login2.login.loginName

instance == Login
where
	(==) login1 login2 = login1.loginName == login2.loginName

instance < (Account s)
where
	(<) login1 login2 = login1.login.loginName < login2.login.loginName

mkAccount :: Login s -> (Account s)
mkAccount login s
	= 	{ login			= login
		, state			= s
		}

mkLogin :: String String -> Login
mkLogin name password = {loginName = name, password = password}

addAccount :: (Account s) (Accounts s) -> (Accounts s) 
addAccount account accounts 
| fst (invariantLogAccounts [account:accounts])	= sort [account:accounts]	
| otherwise 									= accounts

changePassword 	:: String (Account s) -> (Account s) 
changePassword nwpasswrd oldlogin 
= mkAccount (mkLogin oldlogin.login.loginName nwpasswrd) oldlogin.state

changeAccount :: (Account s) (Accounts s) -> (Accounts s) 
changeAccount account accounts
# (before,after) = span ((<>) account) accounts
= updateAt (length before) account accounts

removeAccount :: (Account s) (Accounts s) -> (Accounts s) 
removeAccount login accounts 
# (before,after) = span ((<>) login) accounts
= removeAt (length before) accounts

hasAccount :: Login (Accounts s) -> (Maybe (Account s))
hasAccount login [] = Nothing
hasAccount login [acc:accs]
| login.loginName == acc.login.loginName && login.password == acc.login.password = Just acc
= hasAccount login accs

// Invariants

invariantLogAccounts:: (Accounts s) -> Judgement
invariantLogAccounts accounts = invariantLogins [login \\ {login} <- accounts]

invariantLogins :: [Login] -> Judgement
invariantLogins [] 			= OK
invariantLogins [login=:{loginName,password}:logins]
| loginName == "" 			= (False,"login name is not specified!")
| password  == "" 			= (False,"password required!")
| isMember login logins		= (False,"login name " +++ loginName +++ " is already being used!")
| size password < 6			= (False,"at least 6 characters required for a password!")
= invariantLogins logins

