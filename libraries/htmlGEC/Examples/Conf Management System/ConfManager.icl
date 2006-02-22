module ConfManager

import StdEnv, StdHtml

import loginAdminIData, stateHandlingIData, confIData

// Here it starts ....

Start world  = doHtmlServer mainEntrance world

test hst
# (but,hst)	= FuncBut (Init,nFormId "CommitBut" (LButton defpixel "commit",id)) hst				// read out exception handler
# (int,hst)	= mkEditForm (Init,nFormId "int" 0) hst					// read out exception handler
= mkHtml "Conference Manager"  // raise alert in case of exception
	[ BodyTag but.form
	, BodyTag int.form
	, toHtml but.changed
	] hst



mainEntrance hst
# (_,hst)		= Exception (\_ -> (False,"")) hst	// reset exception handler
# (body,hst)	= loginhandling hst					// handle login
# (excep,hst)	= Exception id hst					// read out exception handler
= mkHtmlB "Conference Manager" (OnLoadException excep.value) // raise alert in case of exception
	[ BodyTag body
	] hst

// login page

loginhandling :: *HSt -> ([BodyTag],*HSt)
loginhandling  hst
# (loginStates,hst) 			= LoginStatesStore id hst							// read out login database store
# (mbloginState,loginBody,hst)	= loginPage loginStates.value hst					// set up a login page
= case mbloginState of
	Nothing			= (loginBody,hst)												// show login page if not yet logged in
	Just loginState	= doConfPortal loginState loginStates.value hst					// show member page if logged in

// iData defs

doConfPortal :: (LoginState ConfState) (LoginStates ConfState) *HSt -> ([BodyTag],*HSt)
doConfPortal (login,state) states hst
# (navButtons,hst) 		= navigationButtons state.role hst							// setup proper set of navigation buttons
# (currPage,hst)		= currPageStore (homePage state.role) navButtons.value hst	// determine current page to display
# (states,navBody,hst) 	= showCurrPage (login,state) states currPage.value hst		// and show the corresponding page
# (excep,error)			= invariantConvDB states									// check whether invariants still hold
# (_,hst)				= Exception (if excep (\_ -> (excep,error)) id) hst			// raise exception
# (_,hst)				= LoginStatesStore (if excep id (\_.states)) hst			// store the new resulting approved state in global database
= ( [ BodyTag navButtons.form
	, Br
	, Hr []
	, BodyTag navBody
	] , hst)
where
	navigationButtons role hst = ListFuncBut (Init, sFormId "navigation" (navButtons role)) hst
	where
		navButtons ConfManager = 
			[ (LButton defpixel "RootHome", 		\_.RootHomePage)
			, (LButton defpixel "ChangePsswrd", 	\_.ChangePassword)
			, (LButton defpixel "ChangeInfo", 		\_.ChangeInfo)
			, (LButton defpixel "ListPapers", 		\_.ListPapers)
			, (LButton defpixel "AssignPapers", 	\_.AssignPapers)
			, (LButton defpixel "AssignConflict", 	\_.AssignConflict)
			, (LButton defpixel "RefereeForm", 		\_.RefereeForm)
			, (LButton defpixel "ModStates", 		\_.ModifyStates)
			]
		navButtons Referee = 
			[ (LButton defpixel "Home", 			\_.MemberHomePage)
			, (LButton defpixel "ChangePsswrd", 	\_.ChangePassword)
			, (LButton defpixel "ChangeInfo", 		\_.ChangeInfo)
			, (LButton defpixel "ListPapers", 		\_.ListPapers)
			, (LButton defpixel "RefereeForm", 		\_.RefereeForm)
			]

showCurrPage :: (LoginState ConfState) (LoginStates ConfState) CurrPage *HSt -> (LoginStates ConfState,[BodyTag],*HSt)
showCurrPage loginState=:(login,state) states currPage hst 
# (papers,hst)	=	papersStore [initPaper i (toString i) \\ i <- [0..3]] id hst
# (states,body,hst)	
	= case currPage of
		RootHomePage 	-> states <~ rootHomePage hst
		ModifyStates 	-> modifyStatesPage (mkLogin "" "",initialRefereeState 1) states hst
		AssignPapers 	-> assignPapersPage states papers.value hst
		AssignConflict	-> assignConflictsPage states papers.value hst

		ChangePassword 	-> changePasswrdPage loginState states hst
//		ChangeInfo		-> mkSubStateForm (nFormId "info") state.person states
//								(\person states = changeState (login,{state & person = person}) states) hst
		ListPapers 		-> states <~ showPapersPage loginState (Init, nFormId "papers" papers.value) [state \\ (login,state) <- states] hst
	
		RefereeForm 	-> refereeStatusPage loginState states papers.value hst
		MemberHomePage 	-> states <~ memberHomePage hst
		_				-> states <~ ([],hst)
=	(	states
	,	[	Txt ("Welcome " +++ login.loginName +++ " , current page is   : ")
			<.=.> toHtml currPage
			, Br
			, Hr []
			, BodyTag body
		]
	,	hst )

// the different pages the super user can choose from

rootHomePage hst =
	(	[ Txt "Home of root ... "
		]
	, 	hst )

// the different pages a member can choose from

memberHomePage hst =
	(	[ Txt "Home page of a member ... "
		]
	, 	hst )

changePasswrdPage loginState=:(login,state) states hst 
# (mblogin,body,hst)	= changePasswordPage login hst
= case mblogin of
	Nothing 		-> (states,	body, hst)
	Just nlogin 	-> showCurrPage (nlogin,state) 
						(changePassword loginState nlogin.password states) (homePage state.role) hst

// small utility stuf

(<~) infix 
(<~) states (body,hst) = (states,body,hst)

