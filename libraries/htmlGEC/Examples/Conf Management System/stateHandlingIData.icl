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
	= (	{ changed			= papernrform.changed
		, value 			= Reports reflist
		, form 				= papernrform.form
		}
		,hst )
	(Reports reflist) = formid.ival

gForm {|Maybe|} ga formid hst 
# elem = formid.ival
= case elem of
	Nothing = ({value=Nothing,changed =False,form=[toHtml "Not yet done"]},hst)
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
	summary i	= [report \\ thisstate <- states, (papernr,report) <- getReports thisstate.reports 
				  | papernr == i && not (isConflict papernr state)]

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
# todo				= papersToReferee state							// determine which papers to referee
| todo == []		= (loginstates,[Txt "There are no papers for you to referee (yet)"],hst)							// nothing to referee
# defaultpaper		= hd todo
# (papernr,hst) 	= FuncMenu (Init, nFormId "pdme" (0,[("paper " <+++ i,(\_ -> i)) \\ i <- todo])) hst
# chosennr			= (fst papernr.value) defaultpaper
# oldreport			= findReport chosennr state
# (prompt,myreport)	= case oldreport of
						Nothing -> (Txt "Please fill in your report and commit the form!",emptyReport)
						Just report	-> (Txt "You can change your form by recomitting!",report)
# (nstate,hst)		= mkSubStateForm (Init,nFormId ("myrep" <+++ chosennr) myreport) state (addRep chosennr) hst
= ( changeState (login,nstate.value) loginstates,
	papernr.form ++ 
	paperinfo chosennr ++
	[Br,Br,prompt,Br,Br] ++ 
	nstate.form,hst)
where
	addRep chosennr report state = addReport chosennr (Just report) state
	paperinfo chosennr = case findPaper chosennr papers of
							Just paper = [Br,Br, Txt ("paper title: " <+++ paper.title)]
							Nothing = [Br,Br, Txt ("Error. Cannot find paper with indicated number " <+++ chosennr)]



modifyStatesPage :: !(LoginState state) !(LoginStates state) !*HSt -> (!LoginStates state,![BodyTag],!*HSt)
 					| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC state
modifyStatesPage (login,state) states hst
# (nstates,hst)		= vertlistFormButs 5 (Init, nFormId "la_states" states) hst

= 	( nstates.value 
	,	[ BodyTag nstates.form
		]
	, hst)


		