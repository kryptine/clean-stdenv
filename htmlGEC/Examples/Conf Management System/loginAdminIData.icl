implementation module loginAdminIData

import StdEnv, StdHtml, StdMaybe
import loginAdmin

derive gForm  	Login
derive gUpd 	Login
derive gPrint	Login
derive gParse	Login

// this session login form can be used at every event to check whether the end user is indeed administrated

loginForm :: !(Init,Login) !*HSt -> (Form Login,!*HSt)
loginForm (init,login) hst = mkEditForm (init,sFormId "adminID_login" login) hst

// a login page

loginPage  :: !(Accounts s) !*HSt -> (Maybe (Account s),[BodyTag],!*HSt)
loginPage accounts hst
# (login,hst) = loginForm (Init,mkLogin "" "") hst
= 	( hasAccount login.value accounts
	, [	Txt "Please log in.."
	  ,	Br
	  ,	Br
	  ,	BodyTag login.form
	  ] 
	, hst)
	
changePasswordPage :: !(Account s) !*HSt -> (Maybe (Account s),[BodyTag],!*HSt)
changePasswordPage account hst
# (oldpasswrd,hst)		= passwordForm "oldpasswrd" hst
# (newpasswrd1,hst)		= passwordForm "newpasswrd1" hst
# (newpasswrd2,hst)		= passwordForm "newpasswrd2" hst
# ok	= oldpasswrd.value == account.login.password &&
		 newpasswrd1.value == newpasswrd2.value  &&
		 newpasswrd1.value <> ""
| not ok				= (Nothing, changePasswrdBody oldpasswrd newpasswrd1 newpasswrd2, hst)

# newaccount			= {account & login.password = newpasswrd1.value}
# ((ok,_),hst)			= ReportStore ((+) (invariantLogAccounts [newaccount])) hst 
| not ok				= (Nothing, changePasswrdBody oldpasswrd newpasswrd1 newpasswrd2, hst)

# (_,hst)				= loginForm (Set,newaccount.login) hst	// password approved
= (Just newaccount, [], hst)
where
	changePasswrdBody oldpasswrd newpasswrd1 newpasswrd2 = 	
		if (oldpasswrd.value <> account.login.password)
			[	Txt "Retype old password .."
			,	Br, Br
			,	BodyTag oldpasswrd.form, Br
			]
			[	Txt "Type in new password.."
			,	Br, Br
			,	BodyTag newpasswrd1.form
			, 	Br, Br
			:	if (newpasswrd1.value <> newpasswrd2.value && newpasswrd1.value <> "")
					[	Txt "Re_type new password.."
					,	Br, Br
					,	BodyTag newpasswrd2.form, Br
					]
					[]
			] 

	passwordForm :: !String !*HSt -> (Form String,!*HSt)
	passwordForm fid hst = mkEditForm (Init, nFormId fid "") hst
						
ReportStore :: (Judgement -> Judgement) *HSt -> (Judgement,!*HSt)
ReportStore judge hst 
# (judgef,hst) = mkStoreForm (Init,ndFormId "cf_alert" OK) judge hst
= (judgef.value,hst)
