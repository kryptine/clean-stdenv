implementation module stateHandlingIData

import StdHtml
import stateHandling
import loginAdminIData, confIData

derive gForm 	CurrPage, Role, ConfState, Person, Paper, /*Reports, */Report, Recommendation, Familiarity/*, Maybe*/
derive gUpd 	CurrPage, Role, ConfState, Person, Paper, Reports, Report, Recommendation, Familiarity, Maybe
derive gPrint 	CurrPage, Role, ConfState, Person, Paper, Reports, Report, Recommendation, Familiarity, Maybe 
derive gParse 	CurrPage, Role, ConfState, Person, Paper, Reports, Report, Recommendation, Familiarity, Maybe

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

//derive gForm Maybe

gForm {|Maybe|} ga formid hst 
# elem = formid.ival
= case elem of
	Nothing = ({value=Nothing,changed =False,form=[toHtml "Not yet done",Br]},hst)
	Just val
		# (valform,hst)	= ga (reuseFormId formid val) hst
		= ({value=Just valform.value,changed =valform.changed,form=valform.form},hst)

// forms

showPapersPage :: !(LoginState ConfState) !(InIDataId Papers) [ConfState] !*HSt -> (![BodyTag],!*HSt)
showPapersPage (login,state) (init,formid) states hst 
# (list,hst) 		= listForm 		(init,{subFormId formid "list"  shortlist & mode = Display}) hst
# (ibuts,hst) 		= ListFuncBut  	(init,subFormId formid "ibuts" paperInfoButs) hst
# (rbuts,hst) 		= ListFuncBut2 	(init,subFormId formid "rbuts" refereeInfoButs) hst
# (okbut,hst) 		= FuncBut  		(init,subFormId formid "okbut" okBut) hst
= (showbody list ibuts rbuts okbut,hst)
where
	allpapers	= formid.ival
	shortlist	= [(nr,paper.title,summary nr) \\ paper <- allpapers & nr <- [0..]]
	summary i	= [short report \\ thisstate <- states, (papernr,report) <- getReports thisstate.reports 
				  | papernr == i && not (isConflict papernr state)]

	short Nothing = Nothing
	short (Just report) = Just (report.recommendation,report.familiarity)
	paperInfoButs = [(LButton defpixel "PaperInfo",\_ -> i) \\ _ <- allpapers & i <- [0..]]
	
	refereeInfoButs = [(mode i,LButton defpixel "RefereeInfo",\_ -> i) \\ _ <- allpapers & i <- [0..]]
	where
		mode i
		| isConflict i state = Display
		= Edit
	
	okBut = (LButton defpixel "OK",id)

	showbody list ibuts rbuts okbut
	| ibuts.changed = [toHtml (allpapers!!ibuts.value 0):okbut.form]
	| rbuts.changed = [toHtml (findReports (rbuts.value 0) states):okbut.form]
	= [[ib <.=.> rb \\ ib <- ibuts.form & rb <- rbuts.form] <=|> list.form]
	
assignPapersPage :: !(LoginStates ConfState) !Papers *HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)
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

		adjustPapers:: !Login !Int Bool [Bool] (LoginStates ConfState) -> (LoginStates ConfState)
		adjustPapers login papernr dopaper _ states
		# state = getLoginState login states
		| isConflict papernr state = changeState (login,deletePaper papernr state) states
		# newstate = if dopaper (assignPaper papernr state) (deletePaper papernr state)
		=  changeState (login,newstate) states
		
assignConflictsPage :: !(LoginStates ConfState) !Papers *HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)
assignConflictsPage loginstates papers hst
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
		# newstate = if dopaper (deletePaper papernr (assignConflict papernr state)) (deleteConflict papernr state)
		=  changeState (login,newstate) states

refereeStatusPage :: (LoginState ConfState) !(LoginStates ConfState) !Papers *HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)
refereeStatusPage (login,state) loginstates papers hst
# todo				= papersToReferee state														// determine which papers to referee
| todo == []		= (loginstates,noPapersMessage,hst)											// nothing to referee, done
# defaultpaper		= hd todo																	// something to referee
# (papernr,hst) 	= FuncMenu (Init, sFormId "pdme" (0,[("paper " <+++ i,(\_ -> i)) \\ i <- todo])) hst	// create pull down menu
# chosennr			= if papernr.changed ((fst papernr.value) 0) (todo!!snd papernr.value)											// determine which report to show
# oldreport			= findReport chosennr state													// find this report
# myreport			= case oldreport of															
						Nothing 	-> emptyReport												// show an empty report to fill in
						Just report	-> report													// show previous report
# (new,nstate,hst)	= mkSubStateForm (Init,sFormId  ("myreps" <+++ chosennr) myreport) state (addRep chosennr) hst // show report or new empty report
# newreport			= findReport chosennr nstate.value													// determine new report
= ( if new (changeState (login,nstate.value) loginstates) loginstates							// add submitted report to state
  ,	[paperstate nstate.value] ++
 	[Br, Txt "Show me the report of: ":papernr.form] ++ 
  	[Br, Br, Txt "You currently looking at your referee report of paper: ",Br] ++
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



modifyStatesPage :: !(LoginState state) !(LoginStates state) !*HSt -> (!LoginStates state,![BodyTag],!*HSt)
 					| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC state
modifyStatesPage (login,state) states hst
# (nstates,hst)		= vertlistFormButs 5 (Init, nFormId "la_states" states) hst

= 	( nstates.value 
	,	[ BodyTag nstates.form
		]
	, hst)


		