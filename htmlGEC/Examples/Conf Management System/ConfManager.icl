module ConfManager

import StdEnv, StdHtml

import loginAdminIData, stateHandlingIData, confIData

// Here it starts ....

Start world  = doHtmlServer mainEntrance world

mainEntrance hst
# (body,hst)		= loginhandling hst
= mkHtml "Conference Manager"
	[ BodyTag body
	] hst

// login page

loginhandling :: *HSt -> ([BodyTag],*HSt)
loginhandling  hst
# (loginStates,hst) 			= LoginStatesStore id hst							// read out login database store
# (mbloginState,loginBody,hst)	= loginPage loginStates.value hst					// set up a login page
= case mbloginState of
	Nothing			= (loginBody,hst)												// show login page if not yet logged in
	Just loginState	= doMemberPage loginState loginStates.value hst					// show member page if logged in

// iData defs

doMemberPage :: (LoginState State) (LoginStates State) *HSt -> ([BodyTag],*HSt)
doMemberPage (login,state) states hst
# (navButtons,hst) 		= if (isRoot state) rootNavigation memberNavigation hst		// setup proper set of navigation buttons
# (currPage,hst)		= currPageStore state.initialPage navButtons.value hst		// determine current page to display
# (states,navBody,hst) 	= doNavigation (login,state) states currPage.value hst		// and show the corresponding page
# (_,hst)				= LoginStatesStore (\_.states) hst							// store new states in global database
= ( [ BodyTag navButtons.form
	, Br
	, Hr []
	, BodyTag navBody
	] , hst)
where
	rootNavigation hst = ListFuncBut (Init, sFormId "rootNavigation" mbuttons) hst
	where
		mbuttons  = 
			[ (LButton defpixel "RootHome", 		\_.RootHomePage)
			, (LButton defpixel "ChangePsswrd", 	\_.ChangePassword)
			, (LButton defpixel "ChangeInfo", 		\_.ChangeInfo)
			, (LButton defpixel "ListPapers", 		\_.ListPapers)
			, (LButton defpixel "AssignPapers", 	\_.AssignPapers)
			, (LButton defpixel "AssignConflict", 	\_.AssignConflict)
			, (LButton defpixel "ModStates", 		\_.ModifyStates)
			]

	memberNavigation hst = ListFuncBut (Init, sFormId "memberNavigation" mbuttons) hst
	where
		mbuttons  = 
			[ (LButton defpixel "Home", 			\_.MemberHomePage)
			, (LButton defpixel "ChangePsswrd", 	\_.ChangePassword)
			, (LButton defpixel "ChangeInfo", 		\_.ChangeInfo)
			, (LButton defpixel "ListPapers", 		\_.ListPapers)
			]

doNavigation :: (LoginState State) (LoginStates State) CurrPage *HSt -> (LoginStates State,[BodyTag],*HSt)
doNavigation loginState=:(login,state) states currPage hst 
# (papers,hst)	=	papersStore [initPaper (toString i) \\ i <- [0..3]] id hst
# (states,body,hst)	
	= case currPage of
		RootHomePage 	-> states <~ rootHomePage hst
		ModifyStates 	-> modifyStatesPage loginState states hst
		AssignPapers 	-> assignPapersPage states papers.value hst
		AssignConflict	-> assignConflictsPage states papers.value hst

		ChangePassword 	-> changePasswrdPage loginState states hst
//		ChangeInfo		-> mkSubStateForm (nFormId "info") state.person states
//								(\person states = changeState (login,{state & person = person}) states) hst
		ListPapers 		-> states <~ showPapersPage loginState (Init, nFormId "papers" papers.value) [state \\ (login,state) <- states] hst
	
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
	Just nlogin 	-> doNavigation (nlogin,state) 
						(changePassword loginState nlogin.password states) state.initialPage hst

// small utility stuf

(<~) infix 
(<~) states (body,hst) = (states,body,hst)

