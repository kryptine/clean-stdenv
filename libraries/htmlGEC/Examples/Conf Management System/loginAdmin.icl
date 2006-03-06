implementation module loginAdmin

import StdEnv, StdHtml, StdMaybe

instance == Login
where
	(==) login1 login2 = login1.loginName == login2.loginName && login1.password == login2.password

instance < Login
where
	(<) login1 login2 = login1.loginName < login2.loginName

mkLogin :: String String -> Login
mkLogin name password
	= 	{ loginName = name
		, password	= password
		}

addLogin :: (LoginState state) (LoginStates state) -> (LoginStates state) 
addLogin (login,state) loginAdmin 
| login.loginName <> "" && login.password  <> "" 
	&& not (isLoggedIn login loginAdmin) = [(login,state):loginAdmin]
| otherwise = loginAdmin

changePassword 	:: (LoginState state) String (LoginStates state) -> (LoginStates state) 
changePassword (oldlogin,state) nwpasswrd  loginAdmin
# newlogin = mkLogin oldlogin.loginName nwpasswrd
= addLogin (newlogin,state) (removeLogin oldlogin loginAdmin)

changeState :: (LoginState state) (LoginStates state) -> (LoginStates state) 
changeState (login,newstate) loginAdmin
# (before,after) = span ((<>) login) (map fst loginAdmin)
= updateAt (length before) (login,newstate) loginAdmin

removeLogin :: Login (LoginStates state) -> (LoginStates state) 
removeLogin login loginAdmin 
# (before,after) = span ((<>) login) (map fst loginAdmin)
= removeAt (length before) loginAdmin

isLoggedIn :: Login (LoginStates state) -> Bool
isLoggedIn login loginAdmin = isMember login (map fst loginAdmin)

getLoginState :: Login (LoginStates state) -> state
getLoginState slogin loginAdmin
= case [state \\ (login,state) <- loginAdmin | login == slogin] of
	list 	= hd list

invariantLogin 	:: (LoginStates state) -> (Bool,String)
invariantLogin loginstates 
= invariant [(login.loginName,login.password) \\ (login,_) <- loginstates]
where
	invariant [] 							= (False,"")
	invariant [(name,password):loginnames]
	| name 		== "" 						= (True,"login name is not specified!")
	| password  == "" 						= (True,"password required!")
	| isMember name (map fst loginnames) 	= (True,"login name " +++ name +++ " is already being used!")
	| size password < 6						= (True,"at least 6 characters required for a password!")
	= invariant loginnames

