implementation module stateHandlingIData

import StdHtml
import stateHandling
import loginAdminIData, confIData

derive gForm 	CurrPage, State, Person, Paper, Report, Recommendation, Familiarity, Maybe
derive gUpd 	CurrPage, State, Person, Paper, Report, Recommendation, Familiarity, Maybe
derive gPrint 	CurrPage, State, Person, Paper, Report, Recommendation, Familiarity, Maybe 
derive gParse 	CurrPage, State, Person, Paper, Report, Recommendation, Familiarity, Maybe

showPapersPage :: !(LoginState State) !(InIDataId Papers) [State] !*HSt -> (![BodyTag],!*HSt)
showPapersPage (login,state) (init,formid) states hst 
# (list,hst) 		= listForm 		(init,subFormId formid "list"  initpapers ) hst
# (ibuts,hst) 		= ListFuncBut  	(init,subFormId formid "ibuts" paperInfoButs) hst
# (rbuts,hst) 		= ListFuncBut2 	(init,subFormId formid "rbuts" refereeInfoButs) hst
# (okbut,hst) 		= FuncBut  		(init,subFormId formid "okbut" okBut) hst
= (showbody list ibuts rbuts okbut,hst)
where
	initpapers			= formid.ival
	shortlist 			= [(nr,paper.title) \\ paper <- initpapers & nr <- [0..]]

	paperInfoButs = [(LButton defpixel "PaperInfo",\_ -> i) \\ _ <- initpapers & i <- [0..]]
	
	refereeInfoButs = [(mode i,LButton defpixel "RefereeInfo",\_ -> i) \\ _ <- initpapers & i <- [0..]]
	where
		mode i
		| isConflict i state = Display
		= Edit
	
	okBut = (LButton defpixel "OK",id)

	showbody list ibuts rbuts okbut
	| ibuts.changed = [toHtml (initpapers!!ibuts.value 0):okbut.form]
	| rbuts.changed = [toHtml (findReports (rbuts.value 0) states):okbut.form]
	= [list.form <=|> [ib <.=.> rb \\ ib <- ibuts.form & rb <- rbuts.form]]
	
assignPapersPage :: !(LoginStates State) !Papers *HSt -> (!LoginStates State,![BodyTag],!*HSt)
assignPapersPage loginstates papers hst
# (referees,hst) 	= mkEditForm 	   (Set, ndFormId "referees" [logins.loginName \\ (logins,_) <- loginstates]) hst
# (papers,hst) 		= ListFuncCheckBox (Init, nFormId "papers" (showpapers loginstates)) hst
# loginstates 		= (fst papers.value) loginstates
# (papers,hst) 		= ListFuncCheckBox (Set, nFormId "papers" (showpapers loginstates)) hst
= (loginstates,table referees.form papers.form,hst)
where
	table referees npapers = [	referees <=|> group (length papers) npapers ]

	group n list = [BodyTag (take n list): group n (drop n list)] 

	showpapers	loginstates = [(check i login.loginName state,adjustPapers login i) 
						\\ (login,state) <-  loginstates, i <- [0..] & j <- papers]
	where
		check i name state
		| isRefereeOf i state && not (isConflict i state) = CBChecked ("check" +++ toString i +++ name)
		= CBNotChecked ("check" +++ toString i +++ name)

		adjustPapers:: !Login !Int Bool [Bool] (LoginStates State) -> (LoginStates State)
		adjustPapers login papernr dopaper _ states
		# state = getLoginState login states
		| isConflict papernr state = changeState (login,deletePaper papernr state) states
		# newstate = if dopaper (assignPaper papernr state) (deletePaper papernr state)
		=  changeState (login,newstate) states
		
assignConflictsPage :: !(LoginStates State) !Papers *HSt -> (!LoginStates State,![BodyTag],!*HSt)
assignConflictsPage loginstates papers hst
# (referees,hst) 	= mkEditForm 	   (Set, ndFormId "refereesC" [logins.loginName \\ (logins,_) <- loginstates]) hst
# (papers,hst) 		= ListFuncCheckBox (Init, nFormId "papersC" (showpapers loginstates)) hst
# loginstates 		= (fst papers.value) loginstates
= (loginstates,table referees.form papers.form,hst)
where
	table referees npapers = [	referees <=|> group (length papers) npapers ]

	group n list = [BodyTag (take n list): group n (drop n list)] 

	showpapers	loginstates = [(check i login.loginName state,adjustPapers login i) 
						\\ (login,state) <-  loginstates, i <- [0..] & j <- papers]
	where
		check i name state
		| isConflict i state = CBChecked ("check" +++ toString i +++ name)
		= CBNotChecked ("check" +++ toString i +++ name)

		adjustPapers:: !Login !Int Bool [Bool] (LoginStates State) -> (LoginStates State)
		adjustPapers login papernr dopaper _ states
		# state = getLoginState login states
		# newstate = if dopaper (deletePaper papernr (assignConflict papernr state)) (deleteConflict papernr state)
		=  changeState (login,newstate) states

refereeStatusPage :: !Int (LoginState State) !(LoginStates State) !Papers *HSt -> (!LoginStates State,![BodyTag],!*HSt)
refereeStatusPage papernr (login,state) loginstates papers hst
# (referees,hst) 	= mkEditForm (Init, ndFormId "refereepap" reports) hst
= (loginstates,referees.form,hst)
where
	reports = [editable report \\ report <- findReports papernr [state \\ (_,state) <- loginstates]]
	where
		editable (person,report)
		| person.lastName == state.person.lastName = EditMode (person,report)
		= DisplayMode (person,report)


		
		
		