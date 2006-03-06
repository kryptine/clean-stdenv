module ConfManager

import StdEnv, StdHtml

import loginAdminIData, stateHandlingIData, confIData

// Here it starts ....

Start world  = doHtmlServer mainEntrance world

mainEntrance hst
# (body,hst) = loginhandling hst													// each time the login will be checked
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
	Just loginState	= doConfPortal loginState loginStates.value hst					// show member page if logged in

// iData defs

doConfPortal :: (LoginState ConfState) (LoginStates ConfState) *HSt -> ([BodyTag],*HSt)
doConfPortal (login,state) states hst
# (navButtons,hst) 		= navigationButtons state.role hst							// setup proper set of navigation buttons
# (currPage,hst)		= currPageStore (homePage state.role) navButtons.value hst	// determine current page to display
# (states,navBody,hst) 	= showCurrPage (login,state) states currPage.value hst		// and show the corresponding page
# (excep,errormessage)	= invariantConvDB states									// check whether invariants still hold
# (_,hst)				= LoginStatesStore (if excep id (\_.states)) hst			// if so, store the new resulting approved state in global database
= ( [ mkSTable2 [ [EmptyBody,B [] "Conference" <.||.> B [] "Manager ",Oeps excep errormessage currPage.value]
				, [mkColForm navButtons.form, EmptyBody, BodyTag navBody]
				]
	] , hst)
where
	navigationButtons role hst = ListFuncBut (Init, sFormId "navigation" (navButtons role)) hst
	where
		navButtons ConfManager = 
			[ (LButton defpixel "RootHome", 		\_.RootHomePage)
			, (LButton defpixel "ChangePsswrd", 	\_.ChangePassword)
			, (LButton defpixel "ChangeInfo", 		\_.ChangeInfo)
			, (LButton defpixel "AssignConflict", 	\_.AssignConflict)
			, (LButton defpixel "AssignPapers", 	\_.AssignPapers)
			, (LButton defpixel "ListPapers", 		\_.ListPapers)
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

	mkSTable2 :: [[BodyTag]] -> BodyTag
	mkSTable2 table
	= Table []	(mktable table)
	where
		mktable table 	= [Tr [] (mkrow rows) \\ rows <- table]	
		mkrow rows 		= [Td [Td_VAlign Alo_Top] [row] \\ row <- rows] 
	
	Oeps exception errormessage currpage
	| exception = Font [Fnt_Color (`Colorname Yellow)]	[B [] (printToString currpage +++ " - ERROR! - " +++ errormessage)]
	= Font [Fnt_Color (`Colorname Silver)]	[B [] (printToString currpage)] 

showCurrPage :: (LoginState ConfState) (LoginStates ConfState) CurrPage *HSt -> (LoginStates ConfState,[BodyTag],*HSt)
showCurrPage loginState=:(login,state) states currPage hst 
# (papersform,hst)	= papersStore [initPaper i (toString i) \\ i <- [0..3]] id hst
# papers			= papersform.value
# (states,body,hst)	
	= case currPage of
		RootHomePage 	-> states <~ rootHomePage hst
		ModifyStates 	-> modifyStatesPage 		  states hst
		AssignPapers 	-> assignPapersPage    papers states hst
		AssignConflict	-> assignConflictsPage papers states hst

		ChangePassword 	-> changePasswrdPage loginState states hst
		ChangeInfo		-> changeInfo	 	 loginState states hst  
		ListPapers 		-> states <~ showPapersPage papers loginState states hst
	
		RefereeForm 	-> refereeStatusPage papers loginState states hst
		MemberHomePage 	-> states <~ memberHomePage hst
		_				-> states <~ ([],hst)
=	(	states
	,	body
	,	hst )
where
	(<~) infix 
	(<~) states (body,hst) = (states,body,hst)

// the different pages the super user can choose from

rootHomePage hst =
	(	[ Txt "Welcome Conference Manager ... "
		]
	, 	hst )

// the different pages a member can choose from

memberHomePage hst =
	(	[ Txt "Welcome Referee ... "
		]
	, 	hst )

changePasswrdPage loginState=:(login,state) states hst 
# (mblogin,body,hst) = changePasswordPage login hst
= case mblogin of
	Nothing 		-> (states,	body, hst)
	Just nlogin 	-> showCurrPage (nlogin,state) 
						(changePassword loginState nlogin.password states) (homePage state.role) hst


