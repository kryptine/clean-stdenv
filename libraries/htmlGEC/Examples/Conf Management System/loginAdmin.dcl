definition module loginAdmin

import StdEnv, StdHtml

:: LoginState state		:== (Login,state)
:: LoginStates state	:== [LoginState state]
:: Login 				= 	{ loginName 	:: String
							, password		:: String
							}
//defaultLogin 	:== (Init (mkLogin "root" "secret"))

instance == Login
instance < Login

mkLogin 		:: String String -> Login

addLogin 		:: (LoginState state) (LoginStates state) -> (LoginStates state) 
changePassword 	:: (LoginState state) String (LoginStates state) -> (LoginStates state) 
changeState 	:: (LoginState state) (LoginStates state) -> (LoginStates state) 

isLoggedIn 		:: Login (LoginStates state) -> Bool
getLoginState 	:: Login (LoginStates state) -> state
removeLogin 	:: Login (LoginStates state) -> (LoginStates state) 

invariantLogin 	:: (LoginStates state) -> (Bool,String)
