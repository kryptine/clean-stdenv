implementation module loginAdminIData

import StdEnv, StdHtml, StdMaybe
import loginAdmin

derive gForm  	Maybe, Login
derive gUpd 	Maybe, Login
derive gPrint	Maybe, Login
derive gParse	Maybe, Login

// this global login form should always contain correct login name and password

loginForm :: !(Init,Login) !*HSt -> (Form Login,!*HSt)
loginForm (init,login) hst = mkEditForm (init,sFormId "login" login) hst

// a temperal login form used for changing passwords

passwordForm :: !String !*HSt -> (Form String,!*HSt)
passwordForm fid hst = mkEditForm (Init, nFormId fid "") hst

// a login page

loginPage  :: !(LoginStates state) !*HSt -> (!Maybe (LoginState state),![BodyTag],!*HSt)
				| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC state
loginPage loginDatabase hst
# (login,hst)			= loginForm (Init,mkLogin "" "") hst
= 	( if (isLoggedIn login.value loginDatabase)
			(Just (login.value,getLoginState login.value loginDatabase))
			Nothing
	, [	Txt "Please log in.."
	  ,	Br
	  ,	Br
	  ,	BodyTag login.form
	  ] 
	, hst)
	
changePasswordPage :: !Login !*HSt -> (!Maybe Login,![BodyTag],!*HSt)
changePasswordPage login hst
# (oldpasswrd,hst)		= passwordForm "oldpasswrd" hst
# (newpasswrd1,hst)		= passwordForm "newpasswrd1" hst
# (newpasswrd2,hst)		= passwordForm "newpasswrd2" hst
| oldpasswrd.value <> login.password || newpasswrd1.value <> newpasswrd2.value  || newpasswrd1.value == ""
= (Nothing, changePasswrdBody oldpasswrd newpasswrd1 newpasswrd2, hst)
# (_,hst)				= loginForm (Set,mkLogin login.loginName newpasswrd1.value) hst
= (Just (mkLogin login.loginName newpasswrd1.value), [], hst)
where
	changePasswrdBody oldpasswrd newpasswrd1 newpasswrd2 = 	
		if (oldpasswrd.value <> login.password)
			[	Txt "Retype old password .."
			,	Br, Br
			,	BodyTag oldpasswrd.form, Br
			]
			[	Txt "Type in new password.."
			,	Br, Br
			,	BodyTag newpasswrd1.form
			, 	Br, Br
			:	if (newpasswrd1.value <> newpasswrd2.value && newpasswrd1.value <> "")
					[	Txt "Re_type new name and/or new password.."
					,	Br, Br
					,	BodyTag newpasswrd2.form, Br
					]
					[]
			] 
						
loginStateForm state hst	= mkEditForm 	(Init, nFormId "la_state" 	(mkLogin "" "",state)) hst
addButton fun hst			= simpleButton "Add" fun hst
allStates init states hst	= vertlistForm 	(init, nFormId "la_states" 	states) hst

modifyStatesPage :: !(LoginState state) !(LoginStates state) !*HSt -> (!LoginStates state,![BodyTag],!*HSt)
 					| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC state
modifyStatesPage (login,state) states hst
# (nstate,hst)		= loginStateForm state hst
# (addstate,hst)	= addButton (\states -> addLogin nstate.value states) hst
# (ostates,hst)		= allStates Init states hst
# (nstates,hst)		= allStates Set (addstate.value ostates.value) hst
= 	( nstates.value 
	,	[ BodyTag nstate.form
		, Br
		, BodyTag addstate.form
		, Br
		, Hr []
		, Br
		, BodyTag nstates.form
		]
	, hst)
