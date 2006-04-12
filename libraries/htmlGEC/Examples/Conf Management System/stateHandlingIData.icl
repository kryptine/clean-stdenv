implementation module stateHandlingIData

import StdHtml
import stateHandling
import loginAdminIData, confIData
import StdListExtensions

// Utility code for DeReferences of pointers

getAllPersons :: !ConfAccounts !*HSt -> ([RefPerson],[Person],!*HSt)
getAllPersons accounts hst
# allrefperson 			= [ refperson \\ acc <- accounts , (Just refperson) <- [getRefPerson acc.state]]	
# (allpersonsf,hst)		= maplSt editorRefPerson [(Init,xtFormId ("shd_coll_pers" <+++ i) pers) \\ i <- [0..] & pers <- allrefperson] hst
# allpersons 			= map (\v -> v.value) allpersonsf
= (allrefperson,allpersons,hst)

getAllMyReports :: !ConfAccount !ConfAccounts !*HSt -> ([(Int,[(Person, Maybe Report)])],!*HSt)
getAllMyReports account accounts hst
# (allrefpersons,allpersons,hst) = getAllPersons accounts hst
# allirefreports 		= getMyRefReports account accounts
# allrefreports			= [refreport 	\\	(_,refperson_refreports) <- allirefreports
										, 	refreport <- map snd refperson_refreports]
# (allreportsf,hst)		= maplSt editorRefReport 
							[(Init,xtFormId ("shwrep_coll_rep" <+++ i) refreport) 
							\\ i <- [0..] & refreport <- allrefreports] hst
# allreports 			= map (\v -> v.value) allreportsf
# allireports 			= [(nr,	[(findperson refperson allrefpersons allpersons,report)	\\ (refperson,refreport) <- refperson_refreports
																						&	report <- allreports
								])
						  \\ (nr,refperson_refreports) <- allirefreports
						  ]

= (allireports,hst)
where
	findperson refperson refpersons persons = hd [p \\ ref <- refpersons & p <- persons | ref == refperson]

getMyOwnReports :: !ConfAccount !*HSt -> ([(Int,Maybe Report)],!*HSt)
getMyOwnReports account hst
# allirefreports 		= getMyReports account 
# (allreportsf,hst)		= maplSt editorRefReport 
							[(Init,ndFormId ("shwrep_rep" <+++ i) refreport) 
							\\ i <- [0..] & refreport <- map snd allirefreports
							] hst
# allreports 			= map (\v -> v.value) allreportsf
# allireports			= [(nr,reports) \\ nr <- map fst allirefreports & reports <- allreports]
= (allireports,hst)

// Entrance

guestAccountStore :: ((Bool,ConfAccount) -> (Bool,ConfAccount)) !*HSt -> (Form (Bool,ConfAccount),!*HSt)
guestAccountStore fun hst = mkStoreForm (Init,nFormId "shd_temp_guest" (False,guest)) fun hst
where
	guest = mkAccount (mkLogin "guest" "temppassword") (Guest createDefault)

loginHandlingPage  :: !ConfAccounts !*HSt -> (Maybe ConfAccount,[BodyTag],!*HSt)
loginHandlingPage accounts hst
# (mbaccount,login,hst) = loginPage accounts hst	// has account ?
| isJust mbaccount		= (mbaccount,[],hst)		// ok, goto memeber area
# (forgotf,hst)			= passwordForgotten accounts hst
# (yes,addauthorf,hst)	= addAuthorPage accounts hst
# (guest,hst)			= guestAccountStore (if yes (\(_,guest) -> (True,guest)) id) hst
# mbaccount				= if (fst guest.value) (Just (snd guest.value)) mbaccount
= 	( mbaccount
	, [	B [] "Members Area: ", Br, Br 
	  ,	BodyTag login
	  , split
	  , BodyTag forgotf
	  , split
	  , BodyTag addauthorf
	  , split
	  ] 
	, hst)
where
	split = BodyTag [Br, Br, Hr [], Br]

passwordForgotten ::  !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
passwordForgotten accounts hst
# (emailadres,hst)	= mkEditForm (Init,nFormId "email_addr" "") hst
# (mailmebut,hst)	= simpleButton "MailMe" id hst
# (_,persons,hst) 	= getAllPersons accounts hst
# found 			= search emailadres.value persons accounts
=	(	[ B [] "Password / login forgotten ?", Br, Br	
		, Txt "Type in your e-mail address: "
		, BodyTag emailadres.form, Br, Br
		, BodyTag mailmebut.form, Br, Br
		, if (	mailmebut.changed && 
				emailadres.value <> "") 
					(if (isJust found) 
						(Txt "e-mail has been send")  			// **** But I don't know yet how to do that
						(Txt "you are not administrated")) 
					EmptyBody
		]
	, hst)
where
	search emailaddress persons account 
		= case [acc.login \\ pers <- persons & acc <- account | pers.emailAddress == emailaddress] of
			[] -> Nothing
			[x:_] -> Just x 
			
addAuthorPage :: !ConfAccounts !*HSt -> (Bool,[BodyTag],!*HSt)
addAuthorPage accounts hst 
# (yessubmitf,hst)	= simpleButton "Yes" id hst
=	(	yessubmitf.changed
	,	[ B [] "Paper Submission Area:", Br, Br	
		, Txt "Deadline is due on xx-yy-2006", Br, Br
		, Txt "Do you want to submit a paper ?", Br, Br
		, BodyTag yessubmitf.form
		]
	, hst)

// Conference manager editors for changing account information, may conflict with other members

tempAccountsId accounts = nFormId "cfm_temp_states" accounts 	// temp editor for accounts

modifyStatesPage :: !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
modifyStatesPage accounts hst
# (naccounts,hst)	= vertlistFormButs 15 True (Init,tempAccountsId accounts) hst	// make a list editor to mofify all accounts
# (accounts,hst)	= if naccounts.changed (AccountsDB Set naccounts.value hst) (naccounts.value,hst) 							// store in global database
# (naccounts,hst)	= vertlistFormButs 15 True (Set,tempAccountsId accounts) hst 	// store in temp editor
= (naccounts.form, hst)

assignPapersConflictsPage :: !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
assignPapersConflictsPage accounts hst
# (accountsf,hst)	= vertlistFormButs 15 True (Init,tempAccountsId accounts) hst	// make a list editor to mofify all accounts
# accounts			= accountsf.value												// current value in temp editor
# (assignf,hst) 	= ListFuncCheckBox (Init, nFormId "cfm_assigments" (showAssignments accounts)) hst
# (conflictsf,hst) 	= ListFuncCheckBox (Init, nFormId "cfm_conflicts"  (showConflicts   accounts)) hst
# accounts			= (fst assignf.value)    accounts
# accounts			= (fst conflictsf.value) accounts
# (accounts,hst)	= AccountsDB Set accounts hst 									// if correct store in global database
# (_,hst)			= vertlistFormButs 15 True (Set,tempAccountsId accounts) hst 	// store in temp editor
= (	[B [] "Assign papers to referees:", Br,Br] ++
	table (allRefereeNames accounts) assignf.form accounts ++ 
	[Br,B [] "Specify the conflicting papers:", Br,Br] ++
	table (allRefereeNames accounts) conflictsf.form accounts 
	,hst)
where
	allPaperNumbers acc	= map fst (getRefPapers acc)
	allRefereeNames acc	= [Txt person \\ (RefPerson (Refto person),_,_) <- getConflictsAssign acc]
	allPaperNames   acc	= [Txt (toString nr +++ " ") \\ nr <- allPaperNumbers acc]

	table referees assignm acc
		 = [	[B [] "paper nr: ":referees] <=|> 
				group (length (allPaperNumbers acc)) (allPaperNames acc ++ assignm)]

	group n list = [BodyTag (take n list): group n (drop n list)] 

	showAssignments  accounts 
		= [(check "cfm_ck_assign" (isMember papernr assigment) papernr person
			, adjustAssignments papernr (RefPerson (Refto person))
			) 
			\\ (RefPerson (Refto person),_,assigment) <- getConflictsAssign accounts 
			,  papernr <- allPaperNumbers accounts
			]

	showConflicts accounts 
		= [(check "cfm_ck_confl" (isMember papernr conflicts) papernr person
			, adjustConflicts papernr (RefPerson (Refto person))
			) 
			\\ (RefPerson (Refto person),conflicts,_) <- getConflictsAssign accounts
			,  papernr <- allPaperNumbers accounts
			]

	check prefix bool i name 
	| bool	= CBChecked (prefix +++ toString i +++ name)
	= CBNotChecked (prefix +++ toString i +++ name)

	adjustAssignments:: !Int !RefPerson !Bool ![Bool] !ConfAccounts -> ConfAccounts
	adjustAssignments nr person True  bs accounts 	= addAssignment 	nr person accounts
	adjustAssignments nr person False bs accounts 	= removeAssignment  nr person accounts

	adjustConflicts:: !Int !RefPerson !Bool ![Bool] !ConfAccounts -> ConfAccounts
	adjustConflicts nr person True  bs accounts 	= addConflict 	nr person accounts
	adjustConflicts nr person False bs accounts 	= removeConflict  nr person accounts

// general editors

changeInfo :: !ConfAccount !*HSt -> ([BodyTag],!*HSt)
changeInfo account hst
# (personf,hst) = mkEditForm (Init,nFormId "cfm_ch_person" (fromJust (getRefPerson account.state))) hst
= ([Br, Txt "Change your personel information:", Br, Br] ++ personf.form,hst)

submitPaperPage ::  !ConfAccount !*HSt -> ([BodyTag],!*HSt)
submitPaperPage account hst
# [(nr,refpaper):_]	= getRefPapers [account]
# (paperf,hst)	= mkEditForm (Init,nFormId "cfm_sbm_paper" refpaper) hst
= (paperf.form,hst)

showPapersPage :: !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
showPapersPage  accounts hst
# (papersf,hst) = vertlistFormButs 10 False (Init,sdFormId "cfm_shw_papers" (getRefPapers accounts)) hst
= (papersf.form,hst)

submitReportPage :: !ConfAccount !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
submitReportPage account accounts hst
# rreports			= getMyReports account
# mypapers			= map fst rreports
| mypapers == []	= ([ Txt "There are no papers for you to referee (yet)" ],hst)
# myreports			= [DisplayMode ("Paper Nr: " +++ toString i) <|> rreport \\ (i,rreport) <- rreports] 			
# (reportsf,hst)	= vertlistFormButs 10 False (Init,sFormId "cfm_mk_reports" myreports) hst
= (show1 mypapers /*++ show2 mypapers rreports ++ show3 mypapers rreports*/ ++ reportsf.form,hst)

where
	show1 mypapers 			= [Txt ("The following papers have been assigned to you: "), B [] (print mypapers),Br]
	show2 mypapers reports	= [Txt ("You have done: "), B [] (print [i \\ i <- mypapers & (_ <|> Just report) <- reports]), Br]
	show3 mypapers reports	= [Txt ("You still have to do: "), B [] (print [i \\ i <- mypapers & (_ <|> Nothing) <- reports]), Br, Br ]

	print [] = "Nothing"
	print ps = printToString ps

showReportsPage :: !ConfAccount !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
showReportsPage account accounts hst
# allreports = [("paper " +++ toString nr,map (\(RefPerson (Refto name),report) -> (name,report)) reports) 
				\\ (nr,reports) <- getMyRefReports account accounts]
# (reportsf,hst) 	= vertlistFormButs 5 False (Set,ndFormId "cfm_shw_reports" allreports) hst
= (reportsf.form,hst)

discussPapersPage :: !ConfAccount !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
discussPapersPage account accounts hst
# (allreports,hst)	= getAllMyReports account accounts hst
# allpapernrs		= map fst allreports
# pdmenu			= (0, [("Show " +++ toString nr, \_ -> i) \\ i <- [0 .. ] & nr <- allpapernrs]) 
# (pdfun,hst)		= FuncMenu (Init, nFormId "cfm_dpp_pdm" pdmenu) hst
# selected			= snd pdfun.value
# selectedpaper		= allpapernrs!!selected
# mbpaperrefinfo	= getPaperInfo selectedpaper accounts
# (RefDiscussion (Refto name)) = (fromJust mbpaperrefinfo).discussion
# (disclist,hst)	= universalDB Init (\_ _ -> Ok) name (Discussion []) hst
# (time,date,hst)	= getTimeAndDate hst
# (newsubmit,newdiscf,hst)	
					= mkSubStateForm (if pdfun.changed Set Init, nFormId "cfm_dpp_adddisc" (TS 80 "")) disclist
						(\s -> addItemTextInput (account.login.loginName) time date (toS s)) hst
# (_,hst)			= if newsubmit (universalDB Set (\_ _ -> Ok) name newdiscf.value hst) (undef,hst)
# (disclistf,hst) 	= mkEditForm (Set,tdFormId "cfm_show_disc" newdiscf.value) hst
= (	pdfun.form ++ [Br,Hr []] <|.|>  
	hd (mkdisplay allreports selectedpaper) ++ [Br,Hr [], Br] <|.|>
	newdiscf.form <|.|> [Br,Hr []] 
	++ disclistf.form,hst)
where
	addItemTextInput name time date message (Discussion list) 
		=  Discussion [{messageFrom = name, date = date, time = time, message = message}:list]

	toS (TS _ s) = s

	mkdisplay allrep snr =	[ 	[mkTable 	[	[B [] "Paper nr:"	, B [] (toString nr)]
									 		,	[B [] "Status"		, toHtml (paperInfo nr).status] 
									 		]
								] ++
								[mkTable	[ 	[ B [] "Referee: ", Txt (ref.firstName +++ " " +++ ref.lastName)] ++ summarize report	
											\\ ref <- map fst refs_reports & report <- map snd refs_reports 
											]
								]
								\\ (nr,refs_reports) <- allrep | nr == snr
							]
	where
		paperInfo nr = fromJust (getPaperInfo nr accounts)

		summarize Nothing 		= [EmptyBody]
		summarize (Just report)	= [ toHtml report.recommendation , toHtml report.familiarity]	

	