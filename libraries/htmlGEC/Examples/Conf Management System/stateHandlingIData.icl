implementation module stateHandlingIData

import StdHtml
import stateHandling
import loginAdminIData, confIData

changeInfo :: !ConfAccount !*HSt -> ([BodyTag],!*HSt)
changeInfo account hst
# (_,personf,hst) = editPerson (Edit,Init,(getRefPerson account.state,Display,Init)) hst
= (personf.form,hst)

modifyStatesPage :: !ConfAccount !ConfAccounts !*HSt -> (Judgement,(ConfAccount,ConfAccounts),[BodyTag],!*HSt)
modifyStatesPage account accounts hst
# myid				= sFormId "la_states" accounts 					// local id
# (naccounts,hst)	= vertlistFormButs 5 (Init,myid) hst			// make editor
# (ok,judge)		= invariantLogAccounts naccounts.value			// test invariants
# (naccounts,hst)	= if ok 
						(vertlistFormButs 5 (setID  myid (setInvariantAccounts naccounts.value)) hst) // adjust references
						(naccounts,hst)								// leave it as it is
= ((ok,judge),(account,naccounts.value), naccounts.form, hst)


/*
gForm {|Reports|} formid hst 
	= specialize myeditor (Init,formid) hst
where
	myeditor (init,formid) hst
	# (papernrform,hst)	= vertlistForm (Init,{reuseFormId formid reflist & mode = Display, lifespan = Page} ) hst
	= (	{ changed		= papernrform.changed
		, value 		= Reports reflist
		, form 			= papernrform.form
		}
		,hst )
	(Reports reflist) = formid.ival

*/

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
	
assignPapersPage :: !Papers !(Logins ConfStates,ConfStates) !*HSt -> (!ConfStates,![BodyTag],!*HSt)
assignPapersPage papers (logins,cstates) hst
# (referees,hst) 	= mkEditForm 	   (Set, ndFormId "referees" [login.loginName \\ login <- logins]) hst
# (papers,hst) 		= ListFuncCheckBox (Init, nFormId "papers" (showpapers logins cstates)) hst
# cstates 			= (fst papers.value) cstates
# (papers,hst) 		= ListFuncCheckBox (Set, nFormId "papers"  (showpapers logins cstates)) hst
= (cstates,table referees.form papers.form,hst)
where
	table referees npapers = [	referees <=|> group (length papers) npapers ]

	group n list = [BodyTag (take n list): group n (drop n list)] 

	showpapers logins cstates = [(check i login.loginName cstate,adjustPapers j i) 
								\\ j <- [1..] & cstate <- cstates & login <- logins, i <- [0..] & _ <- papers]
	where
		check i name state
		| isRefereeOf i state = CBChecked ("check" +++ toString i +++ name)
		= CBNotChecked ("check" +++ toString i +++ name)

		adjustPapers:: !Int !Int Bool [Bool] ConfStates -> ConfStates
		adjustPapers statenr papernr dopaper _ cstates
		# state = cstates!!statenr
		# newstate = if dopaper (assignPaper papernr state) (deletePaper papernr state)
		=  updateAt statenr  newstate cstates
		
assignConflictsPage :: !Papers !(Logins ConfState) !*HSt -> (!Logins ConfState,![BodyTag],!*HSt)
assignConflictsPage papers loginstates  hst
# (referees,hst) 	= mkEditForm 	   (Set, ndFormId "refereesC" [loginName \\ loginName <- loginstates]) hst
# (papers,hst) 		= ListFuncCheckBox (Init, nFormId "papersC" (showpapers loginstates)) hst
# loginstates 		= (fst papers.value) loginstates
= (loginstates,table referees.form papers.form,hst)
where
	table referees npapers = [referees <=|> group (length papers) npapers]

	group n list = [BodyTag (take n list): group n (drop n list)] 

	showpapers	loginstates = [(check i loginName state,adjustPapers login i) 
						\\ login=:{loginName,state} <-  loginstates, i <- [0..] & j <- papers]
	where
		check i name state
		| isConflict i state = CBChecked ("check" +++ toString i +++ name)
		= CBNotChecked ("check" +++ toString i +++ name)

		adjustPapers:: !(Login ConfState) !Int Bool [Bool] (Logins ConfState) -> (Logins ConfState)
		adjustPapers login papernr dopaper _ states
		# state = getLoginState login states
		# newstate = if dopaper (assignConflict papernr state) (deleteConflict papernr state)
		=  changeState {login & state = newstate} states

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

//mkSubStateForm :: !(InIDataId !subState) !state !(subState state -> state) !*HSt -> (Bool,Form state,!*HSt)

changeInfo :: !(Login ConfState) !(Logins ConfState) !*HSt -> (Logins ConfState,[BodyTag],*HSt)
changeInfo login states hst
# (ok,statesform,hst) = mkSubStateForm (Init,nFormId "cms_info" login.state.person) states
						(\person states = changeState {login & state.person = person} states) hst
= (statesform.value,statesform.form,hst)

modifyStatesPage :: !(Logins ConfState) !*HSt -> (!Logins ConfState,![BodyTag],!*HSt)
modifyStatesPage states hst
# (nstates,hst)		= vertlistFormButs 5 (Init, nFormId "la_states" states) hst
= 	(nstates.value,	nstates.form, hst)

*/
		