module ConfManager

import StdEnv, StdHtml

import loginAdminIData, confIData, stateHandlingIData 

// Here it starts ....

Start world  = doHtmlServer mainEntrance world

mainEntrance hst
# (body,hst) 	= loginhandling hst				// a login will be checked on correctness each time a page is requested !
= mkHtml "Conference Manager" 
	[ BodyTag body
	] hst

// login page handling

loginhandling :: *HSt -> ([BodyTag],*HSt)
loginhandling  hst											
# (accounts,hst) 			= AccountsDB Init [initManagerAccount initManagerLogin] hst	// read out all accounts read only
# (mblogin,loginBody,hst)	= loginPage accounts hst			// set up a login page
= case mblogin of												// check result of login procedure
	Nothing		= (loginBody,hst)								// show login page when (still) not logged in
	Just login	= doConfPortal login accounts hst				// show member page otherwise

// The different pages that exists:

:: CurrPage 	= 	RootHomePage			// root pages
				| 	AssignPapers
				| 	ModifyStates

				| 	AuthorsHomePage			// authors
				| 	SubmitPaper

				| 	ChangePassword			// common			
				| 	ChangeInfo

				| 	ListPapers				// referees + root
				| 	ListReports
				|	RefereeForm				
				| 	RefereeHomePage			// referees

derive gForm 	CurrPage
derive gUpd 	CurrPage
derive gPrint 	CurrPage
derive gParse 	CurrPage

homePage (ConfManager info) = RootHomePage
homePage (Referee info) 	= RefereeHomePage
homePage (Authors info) 	= AuthorsHomePage

// you are in, determine what to do

doConfPortal :: ConfAccount ConfAccounts *HSt -> ([BodyTag],*HSt)
doConfPortal account accounts hst
# (navButtons,hst) 	= navigationButtons account.state hst							// setup proper set of navigation buttons
# (currPage,hst)	= currPageStore (homePage account.state) navButtons.value hst	// determine which current page to display
# (navBody,hst)		= handleCurrPage currPage.value account accounts  hst			// and handle the corresponding page
# (exception,hst)	= ExceptionStore id hst											// see if an error has occured somewhere
= ( [ mkSTable2 [ [EmptyBody,B [] "Conference" <.||.> B [] "Manager ",Oeps exception currPage.value]
				, [mkColForm navButtons.form, EmptyBody, BodyTag navBody]
				]
	] // for debugging ++ [Txt (printToString accounts)]
	, hst)
where
	navigationButtons state hst = ListFuncBut (Init, sFormId "navigation" (navButtons state)) hst
	where
		navButtons (ConfManager info) = 
			[ (LButton defpixel "RootHome", 		\_.RootHomePage)
			, (LButton defpixel "ChangePsswrd", 	\_.ChangePassword)
			, (LButton defpixel "ChangeInfo", 		\_.ChangeInfo)
			, (LButton defpixel "ListPapers", 		\_.ListPapers)
			, (LButton defpixel "ListReports", 		\_.ListReports)
			, (LButton defpixel "RefereeForm", 		\_.RefereeForm)
			, (LButton defpixel "AssignPapers", 	\_.AssignPapers)
			, (LButton defpixel "ModStates", 		\_.ModifyStates)
			]
		navButtons (Referee info) = 
			[ (LButton defpixel "Home", 			\_.RefereeHomePage)
			, (LButton defpixel "ChangePsswrd", 	\_.ChangePassword)
			, (LButton defpixel "ChangeInfo", 		\_.ChangeInfo)
			, (LButton defpixel "ListPapers", 		\_.ListPapers)
			, (LButton defpixel "ListReports", 		\_.ListReports)
			, (LButton defpixel "RefereeForm", 		\_.RefereeForm)
			]
		navButtons (Authors info) = 
			[ (LButton defpixel "Home", 			\_.AuthorsHomePage)
			, (LButton defpixel "SubmitPaper", 		\_.SubmitPaper)
			, (LButton defpixel "ChangePsswrd", 	\_.ChangePassword)
			, (LButton defpixel "ChangeInfo", 		\_.ChangeInfo)
			]

	mkSTable2 :: [[BodyTag]] -> BodyTag
	mkSTable2 table
	= Table []	(mktable table)
	where
		mktable table 	= [Tr [] (mkrow rows) \\ rows <- table]	
		mkrow rows 		= [Td [Td_VAlign Alo_Top] [row] \\ row <- rows] 
	
	Oeps (Just (id,message)) currpage
	= Font [Fnt_Color (`Colorname Yellow)] [B [] (print currpage), Br, B [] (id +++ " : " +++ message)]
	Oeps Nothing currpage
	= Font [Fnt_Color (`Colorname Silver)] [B [] (print currpage)] 

	print currpage = printToString (currpage) +++ " of " +++ account.login.loginName

	currPageStore :: !CurrPage  !(CurrPage -> CurrPage) *HSt -> (!Form CurrPage,!*HSt)	// current page to display
	currPageStore currpage cbf hst = mkStoreForm (Init, sFormId "cf_currPage" currpage) cbf hst 

handleCurrPage :: CurrPage ConfAccount ConfAccounts *HSt -> ([BodyTag],*HSt)
handleCurrPage currPage account accounts  hst 
= case currPage of
		RootHomePage 	-> rootHomePage hst
		RefereeHomePage -> refereeHomePage hst
		AuthorsHomePage -> authorsHomePage hst

		ChangePassword 	-> changePasswrdPage account accounts hst
		ModifyStates 	-> modifyStatesPage	accounts hst
		AssignPapers 	-> assignPapersConflictsPage accounts hst

		ListPapers 		-> showPapersPage accounts hst
		ListReports		-> showReportsPage account accounts hst

		ChangeInfo		-> changeInfo account hst  
		SubmitPaper		-> submitPaperPage account hst
	
		RefereeForm 	-> submitReportPage account accounts hst
		_				-> ([],hst)

where
	changePasswrdPage account accounts hst 
	# (mbaccount,body,hst) = changePasswordPage account hst
	= case mbaccount of
		Nothing 		-> (body, hst)
		Just naccount 	
						# accounts			= changeAccount naccount accounts	// replace changed account in accounts
						# (accounts,hst)	= AccountsDB Set accounts hst		// store accounts in database administration
						-> handleCurrPage (homePage account.state) naccount accounts  hst

// the different pages the super user can choose from

rootHomePage hst =
	(	[ Txt "Welcome Conference Manager ... "
		]
	, 	hst )


refereeHomePage hst =
	(	[ Txt "Welcome Referee ... "
		]
	, 	hst )

authorsHomePage hst =
	(	[ Txt "Welcome Author ... "
		]
	, 	hst )


