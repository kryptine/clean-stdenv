implementation module stateHandlingIData

import StdHtml
import stateHandling
import loginAdminIData, confIData
import StdListExtensions

// Conference manager editors for changing account information, may conflict with other members

tempAccountsId accounts = sFormId "cfm_temp_states" accounts 	// temp editor for accounts

modifyStatesPage :: !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
modifyStatesPage accounts hst
# (naccounts,hst)	= vertlistFormButs 5 True (Init,tempAccountsId accounts) hst	// make a list editor to mofify all accounts
# (accounts,hst)	= AccountsDB Set naccounts.value hst 							// store in global database
# (naccounts,hst)	= vertlistFormButs 5 True (Set,tempAccountsId accounts) hst 	// store in temp editor
= (naccounts.form, hst)

assignPapersConflictsPage :: !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
assignPapersConflictsPage accounts hst
# (accountsf,hst)	= vertlistFormButs 5 True (Init,tempAccountsId accounts) hst	// make a list editor to mofify all accounts
# accounts			= accountsf.value												// current value in temp editor
# (assignf,hst) 	= ListFuncCheckBox (Init, nFormId "cfm_assigments" (showAssignments accounts)) hst
# (conflictsf,hst) 	= ListFuncCheckBox (Init, nFormId "cfm_conflicts"  (showConflicts   accounts)) hst
# accounts			= (fst assignf.value)    accounts
# accounts			= (fst conflictsf.value) accounts
# (accounts,hst)	= AccountsDB Set accounts hst 									// if correct store in global database
# (_,hst)			= vertlistFormButs 5 True (Set,tempAccountsId accounts) hst 	// store in temp editor
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
# (personf,hst) = mkEditForm (Init,nFormId "cfm_ch_person" (getRefPerson account.state)) hst
= (personf.form,hst)

showPapersPage :: !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
showPapersPage  accounts hst
# (papersf,hst) = vertlistFormButs 5 False (Set,ndFormId "cfm_shw_papers" (getRefPapers accounts)) hst
= (papersf.form,hst)

showReportsPage :: !ConfAccount !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
showReportsPage account accounts hst
# allreports = [(nr,map (\(RefPerson (Refto name),report) -> (name,report)) reports) 
				\\ (nr,reports) <- getMyRefReports account accounts]
# (reportsf,hst) 	= vertlistFormButs 5 False (Set,ndFormId "cfm_shw_reports" allreports) hst
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
# myrefreport		= map snd todo			
# paperlist 		= [DisplayMode ("paper " <+++ i) \\ i <- mypapers]
# myreports			= [nr <|> edit \\ nr <- paperlist & edit <- myrefreport]
| todo == []		= ([ Txt "There are no papers for you to referee (yet)" ],hst)
# (reportsf,hst)	= vertlistFormButs 5 False (Set,sFormId "cfm_mk_reports" myreports) hst
# (results,hst)		= maplSt editorRefReport [(Init,xFormId ("tmp" <+++ i) ref) \\ i <- mypapers & ref <- myrefreport] hst
# resultvalue		= [res.value \\ res <- results]
= (show1 mypapers ++ show2 mypapers resultvalue ++ show3 mypapers resultvalue ++ reportsf.form,hst)
where
	show1 mypapers = [Txt ("The following papers have been assigned to you: "), B [] (print mypapers),Br]
	show2 mypapers reports =  [Txt ("You have done: "), B [] (print [i \\ i <- mypapers & (Just report) <- reports]), Br]
	show3 mypapers reports =  [Txt ("You still have to do: "), B [] (print [i \\ i <- mypapers & rep <- reports | isNothing rep]), Br, Br ]

	print [] = "Nothing"
	print ps = printToString ps



	