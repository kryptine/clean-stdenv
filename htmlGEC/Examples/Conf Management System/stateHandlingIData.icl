implementation module stateHandlingIData

import StdHtml
import stateHandling
import loginAdminIData, confIData

// Conference manager editors chabging accounts

tempAccountsId accounts = sFormId "cfm_temp_states" accounts 	// temp editor for accounts

modifyStatesPage :: !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
modifyStatesPage accounts hst
# (naccounts,hst)	= vertlistFormButs 5 (Init,tempAccountsId accounts) hst			// make a list editor to mofify all accounts
# (naccounts,hst)	= AccountsDB Edit Set naccounts.value hst 						// store in global database
# (naccounts,hst)	= vertlistFormButs 5 (Set,tempAccountsId naccounts.value) hst 	// store in temp editor
= (naccounts.form ++ [Br,Txt (printToString naccounts.value)], hst)

assignPapersConflictsPage :: !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
assignPapersConflictsPage accounts hst
# (accountsf,hst)	= vertlistFormButs 5 (Init,tempAccountsId accounts) hst						// make a list editor to mofify all accounts
# accounts			= accountsf.value													// current value in temp editor
# (assignf,hst) 	= ListFuncCheckBox (Init, nFormId "cfm_assigments" (showAssignments accounts)) hst
# (conflictsf,hst) 	= ListFuncCheckBox (Init, nFormId "cfm_conflicts"  (showConflicts   accounts)) hst
# accounts			= (fst assignf.value)    accounts
# accounts			= (fst conflictsf.value) accounts
# (_,hst)			= AccountsDB Edit Set accounts hst 									// if correct store in global database
# (_,hst)			= vertlistFormButs 5 (Set,tempAccountsId accounts) hst 	// store in temp editor
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
# (personf,hst) = mkEditForm (Init,sFormId "cfm_ch_person" (getRefPerson account.state)) hst
= (personf.form,hst)

showPapersPage :: !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
showPapersPage  accounts hst
# (papersf,hst) 	= vertlistFormButs 5 (Set,sdFormId "cfm_shw_papers" (getRefPapers accounts)) hst
= (papersf.form,hst)

showReportsPage :: !ConfAccount !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
showReportsPage account accounts hst
# allreports = [(nr,map (\(RefPerson (Refto name),report) -> (name,report)) reports) 
				\\ (nr,reports) <- getMyRefReports account accounts]
# (reportsf,hst) 	= vertlistFormButs 5 (Set,sdFormId "cfm_shw_reports" allreports) hst
= (reportsf.form,hst)

submitPaperPage ::  !ConfAccount !*HSt -> ([BodyTag],!*HSt)
submitPaperPage account hst
# [(nr,refpaper):_]	= getRefPapers [account]
| nr > 0
	# (paperf,hst)	= mkEditForm (Init,nFormId "cfm_sbm_paper" refpaper) hst
	= (paperf.form,hst)
= ([],hst)

submitReportPage :: !ConfAccount !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
submitReportPage account accounts hst
# todo				= getMyReports account
# mypapers			= map fst todo
# paperlist 		= [DisplayMode ("paper " <+++ i) \\ i <- mypapers]
# myreports			= [(nr,edit) \\ nr <- paperlist & edit <- map snd todo]
| todo == []		= ([ Txt "There are no papers for you to referee (yet)" ],hst)
# (reportsf,hst)	= vertlistFormButs 5 (Set,sFormId "cfm_mk_reports" myreports) hst
= (reportsf.form,hst)



// forms
/*
showPapersPage ::  !Papers !ConfState !ConfStates !*HSt -> (![BodyTag],!*HSt)
showPapersPage papers cstate cstates hst 
# formid			= nFormId "cms_papers" papers
# (list,hst) 		= listForm 		(Init,{subFormId formid "list"  shortlist & mode = Display}) hst
# (ibuts,hst) 		= ListFuncBut  	(Init,subFormId formid "ibuts" paperInfoButs) hst
# (rbuts,hst) 		= ListFuncBut2 	(Init,subFormId formid "rbuts" refereeInfoButs) hst
# (okbut,hst) 		= FuncBut  		(Init,subFormId formid "okbut" okBut) hst
= (showbody list cstates ibuts rbuts okbut,hst)
where
	shortlist	= [(nr,paper.title,summary nr) \\ paper <- papers & nr <- [0..]]
	summary i	= [ short report 
					\\ state <- cstates
					, (papernr,report) <- getReports state.reports 
				  	| papernr == i && not (isConflict papernr cstate)]

	short Nothing = Nothing
	short (Just report) = Just (report.recommendation,report.familiarity)
	paperInfoButs = [(LButton defpixel "PaperInfo",\_ -> i) \\ _ <- papers & i <- [0..]]
	
	refereeInfoButs = [(mode i,LButton defpixel "RefereeInfo",\_ -> i) \\ _ <- papers & i <- [0..]]
	where
		mode i
		| isConflict i cstate = Display
		= Edit
	
	okBut = (LButton defpixel "OK",id)

	showbody list states ibuts rbuts okbut
	| ibuts.changed = [toHtml (papers!!ibuts.value 0):okbut.form]
	| rbuts.changed = [toHtml (findReports (rbuts.value 0) states):okbut.form]
	= [[ib <.=.> rb \\ ib <- ibuts.form & rb <- rbuts.form] <=|> list.form]
	


refereeStatusPage :: !Papers !(Login ConfState) !(Logins ConfState)  !*HSt -> (!Logins ConfState,![BodyTag],!*HSt)
refereeStatusPage papers login loginstates  hst
# todo				= papersToReferee login.state														// determine which papers to referee
| todo == []		= (loginstates,noPapersMessage,hst)											// nothing to referee, done
# defaultpaper		= hd todo																	// something to referee
# (papernr,hst) 	= FuncMenu (Init, nFormId ("pdme" <+++ sum todo)(0,[("paper " <+++ i,(\_ -> i)) \\ i <- todo])) hst	// create pull down menu
# chosennr			= if papernr.changed ((fst papernr.value) 0) (todo!!snd papernr.value)											// determine which report to show
# myreport			= case findReport chosennr login.state of															
						Nothing 	-> emptyReport												// show an empty report to fill in
						Just report	-> report													// show previous report
# (new,nstate,hst)	= mkSubStateForm (Init,sFormId  ("myreps" <+++ chosennr) myreport) login.state (addRep chosennr) hst // show report or new empty report
# newreport			= findReport chosennr nstate.value													// determine new report
= ( if new (changeState {login & state = nstate.value} loginstates) loginstates							// add submitted report to state
  ,	[paperstate nstate.value] ++
 	[Br, Txt "Show me the report of: ":papernr.form] ++ 
  	[Br, Br, Hr [], Br, Txt "You currently looking at your referee report of paper: ",Br] ++
	paperinfo chosennr ++
	[Br,Br,BodyTag (paperPrompt newreport),Br,Br] ++ 
	nstate.form
  ,hst)
where
	noPapersMessage 	= 	[ Txt "There are no papers for you to referee (yet)" ]
	paperstate state 
		= STable []	[ 	[ B [] "The following paper(s) have been assigned to you: "
						, B [] (foldr (\i j -> toString i +++ ", " <+++ j) "" (papersToReferee state))
						]
					, case papersRefereed state of
						[]		-> 	[]
						list 	-> 	[ B [] "You already have refereed the following paper(s):"
									, B [] (foldr (\i j -> toString i +++ ", " <+++ j) "" list)
									]
					, case papersNotRefereed state of
						[]		->	[]
						list 	->	[ B [] "You still have to referee the following paper(s):"
									, B [] (foldr (\i j -> toString i +++ ", " <+++ j) "" list)
									]
					]
	paperinfo chosennr = case findPaper chosennr papers of
							Just paper = [Br,Txt ("paper title: " <+++ paper.title),Br]
							Nothing = [Br, Txt ("Error. Cannot find paper with indicated number " <+++ chosennr),Br]
	paperPrompt report	= case report of															// determine feedback
							Nothing -> 		[B [] "You did not commit a report about this paper yet.",
											 Br,
											 Txt "Please fill in your report and commit the form!"]
							Just report	-> 	[B [] "You have commited this report previously. Thank you!",
											 Br,
											 Txt "You can change your current report by recomitting the form!"]
	addRep chosennr report state = addReport chosennr (Just report) state

*/
		