implementation module stateHandlingIData

import StdHtml
import stateHandling
import loginAdminIData, confIData

derive gForm 	CurrPage, Role, ConfState, Person, Paper, /*Reports, */Report, Recommendation, Familiarity
derive gUpd 	CurrPage, Role, ConfState, Person, Paper, Reports, Report, Recommendation, Familiarity
derive gPrint 	CurrPage, Role, ConfState, Person, Paper, Reports, Report, Recommendation, Familiarity
derive gParse 	CurrPage, Role, ConfState, Person, Paper, Reports, Report, Recommendation, Familiarity

gForm {|Reports|} formid hst 
	= specialize myeditor (Init,formid) hst
where
	myeditor (init,formid) hst
	# (papernrform,hst)	= vertlistForm (Init,{reuseFormId formid reflist & mode = Display} ) hst
	= (	{ changed		= papernrform.changed
		, value 		= Reports reflist
		, form 			= papernrform.form
		}
		,hst )
	(Reports reflist) = formid.ival


// forms

showPapersPage ::  !Papers !(LoginState ConfState)!(LoginStates ConfState) !*HSt -> (![BodyTag],!*HSt)
showPapersPage papers (login,state) loginstates hst 
# formid			= nFormId "cms_papers" papers
# states 			= [state \\ (_,state) <- loginstates]
# (list,hst) 		= listForm 		(Init,{subFormId formid "list"  shortlist & mode = Display}) hst
# (ibuts,hst) 		= ListFuncBut  	(Init,subFormId formid "ibuts" paperInfoButs) hst
# (rbuts,hst) 		= ListFuncBut2 	(Init,subFormId formid "rbuts" refereeInfoButs) hst
# (okbut,hst) 		= FuncBut  		(Init,subFormId formid "okbut" okBut) hst
= (showbody list states ibuts rbuts okbut,hst)
where
	shortlist	= [(nr,paper.title,summary nr) \\ paper <- papers & nr <- [0..]]
	summary i	= [ short report 
					\\ (_,thisstate) <- loginstates
					, (papernr,report) <- getReports thisstate.reports 
				  	| papernr == i && not (isConflict papernr state)]

	short Nothing = Nothing
	short (Just report) = Just (report.recommendation,report.familiarity)
	paperInfoButs = [(LButton defpixel "PaperInfo",\_ -> i) \\ _ <- papers & i <- [0..]]
	
	refereeInfoButs = [(mode i,LButton defpixel "RefereeInfo",\_ -> i) \\ _ <- papers & i <- [0..]]
	where
		mode i
		| isConflict i state = Display
		= Edit
	
	okBut = (LButton defpixel "OK",id)

	showbody list states ibuts rbuts okbut
	| ibuts.changed = [toHtml (papers!!ibuts.value 0):okbut.form]
	| rbuts.changed = [toHtml (findReports (rbuts.value 0) states):okbut.form]
	= [[ib <.=.> rb \\ ib <- ibuts.form & rb <- rbuts.form] <=|> list.form]
	
assignPapersPage :: !Papers !(LoginStates ConfState) !*HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)
assignPapersPage papers loginstates hst
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
		| isRefereeOf i state = CBChecked ("check" +++ toString i +++ name)
		= CBNotChecked ("check" +++ toString i +++ name)

		adjustPapers:: !Login !Int Bool [Bool] (LoginStates ConfState) -> (LoginStates ConfState)
		adjustPapers login papernr dopaper _ states
		# state = getLoginState login states
		# newstate = if dopaper (assignPaper papernr state) (deletePaper papernr state)
		=  changeState (login,newstate) states
		
assignConflictsPage :: !Papers !(LoginStates ConfState) !*HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)
assignConflictsPage papers loginstates  hst
# (referees,hst) 	= mkEditForm 	   (Set, ndFormId "refereesC" [logins.loginName \\ (logins,_) <- loginstates]) hst
# (papers,hst) 		= ListFuncCheckBox (Init, nFormId "papersC" (showpapers loginstates)) hst
# loginstates 		= (fst papers.value) loginstates
= (loginstates,table referees.form papers.form,hst)
where
	table referees npapers = [referees <=|> group (length papers) npapers]

	group n list = [BodyTag (take n list): group n (drop n list)] 

	showpapers	loginstates = [(check i login.loginName state,adjustPapers login i) 
						\\ (login,state) <-  loginstates, i <- [0..] & j <- papers]
	where
		check i name state
		| isConflict i state = CBChecked ("check" +++ toString i +++ name)
		= CBNotChecked ("check" +++ toString i +++ name)

		adjustPapers:: !Login !Int Bool [Bool] (LoginStates ConfState) -> (LoginStates ConfState)
		adjustPapers login papernr dopaper _ states
		# state = getLoginState login states
		# newstate = if dopaper (assignConflict papernr state) (deleteConflict papernr state)
		=  changeState (login,newstate) states

refereeStatusPage :: !Papers !(LoginState ConfState) !(LoginStates ConfState)  !*HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)
refereeStatusPage papers (login,state) loginstates  hst
# todo				= papersToReferee state														// determine which papers to referee
| todo == []		= (loginstates,noPapersMessage,hst)											// nothing to referee, done
# defaultpaper		= hd todo																	// something to referee
# (papernr,hst) 	= FuncMenu (Init, nFormId ("pdme" <+++ sum todo)(0,[("paper " <+++ i,(\_ -> i)) \\ i <- todo])) hst	// create pull down menu
# chosennr			= if papernr.changed ((fst papernr.value) 0) (todo!!snd papernr.value)											// determine which report to show
# myreport			= case findReport chosennr state of															
						Nothing 	-> emptyReport												// show an empty report to fill in
						Just report	-> report													// show previous report
# (new,nstate,hst)	= mkSubStateForm (Init,sFormId  ("myreps" <+++ chosennr) myreport) state (addRep chosennr) hst // show report or new empty report
# newreport			= findReport chosennr nstate.value													// determine new report
= ( if new (changeState (login,nstate.value) loginstates) loginstates							// add submitted report to state
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


changeInfo :: !(LoginState ConfState) !(LoginStates ConfState) !*HSt -> (LoginStates ConfState,[BodyTag],*HSt)
changeInfo (login,state) states hst
# (ok,statesform,hst) = mkSubStateForm (Init,nFormId "cms_info" state.person) states
						(\person states = changeState (login,{state & person = person}) states) hst
= (statesform.value,statesform.form,hst)

modifyStatesPage :: !(LoginStates ConfState) !*HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)
modifyStatesPage states hst
# (nstates,hst)		= vertlistFormButs 5 (Init, nFormId "la_states" states) hst
= 	(nstates.value,	nstates.form, hst)


		