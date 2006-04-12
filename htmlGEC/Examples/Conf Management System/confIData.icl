implementation module confIData

import StdHtml, StdList

import stateHandling
import loginAdmin, loginAdminIData

// global account database editor

AccountsDB :: !Init  !ConfAccounts *HSt -> (ConfAccounts,!*HSt) // conf management database
AccountsDB init accounts hst 
# accounts = setInvariantAccounts accounts										// ensure that all links are correct
= universalDB init (\s a -> invariantLogAccounts s a + invariantConfAccounts s a) 
	uniqueDBname accounts hst 

PaperNrStore :: !(Int -> Int) *HSt -> (Int,!*HSt) // paper counter
PaperNrStore fun hst 
# (intf,hst) = mkStoreForm (Init,{pFormId "LastPaperNr" 1 & mode = NoForm}) fun hst
= (intf.value,hst)

editorRefPerson :: !(InIDataId RefPerson) !*HSt -> (Form Person,!*HSt)
editorRefPerson (init,formid) hst
# (RefPerson refperson) = formid.ival
# (Refto name) 			= refperson
= universalRefEditor (init,reuseFormId formid refperson) (invariantPerson name) hst

editorRefPaper :: !(InIDataId RefPaper) !*HSt -> (Form Paper,!*HSt)
editorRefPaper (init,formid) hst
# (RefPaper refpaper) 	= formid.ival
# (Refto name) 			= refpaper
= universalRefEditor (init,reuseFormId formid refpaper) (invariantPaper name) hst

editorRefReport :: !(InIDataId RefReport) !*HSt -> (Form (Maybe Report),!*HSt)
editorRefReport (init,formid) hst
# (RefReport refreport) = formid.ival
# (Refto name) 			= refreport
= universalRefEditor (init,reuseFormId formid refreport) (invariant name)  hst
where
	invariant name Nothing 			= Ok
	invariant name (Just report)	= invariantReport name report
	
editorRefDiscussion :: !(InIDataId RefDiscussion) !*HSt -> (Form Discussion,!*HSt)
editorRefDiscussion (init,formid) hst
# (RefDiscussion refdiscus) = formid.ival
= universalRefEditor (init,reuseFormId formid refdiscus) (\_ -> Ok) hst

// specialized forms

gForm {|RefPerson|} iniformid hst = specialize (invokeRefEditor editorRefPerson) 	iniformid hst
	
gForm {|RefPaper|}  iniformid hst = specialize (invokeRefEditor editorRefPaper)  	iniformid hst

gForm {|RefReport|} iniformid hst = specialize (invokeRefEditor editorRefReport)	iniformid hst

gForm {|RefDiscussion|} iniformid hst = specialize (invokeRefEditor editorRefDiscussion)	iniformid hst

gForm {|Reports|} informid hst = specialize myeditor informid hst
where
	myeditor (init,formid) hst
	# (Reports reports) 		= formid.ival
	# (reportsf,hst)			= vertlistFormButs 10 True (init,subsFormId formid "report" reports) hst
	= ({reportsf & value = Reports reportsf.value},hst)

gForm {|Conflicts|} informid hst = specialize myeditor informid hst
where
	myeditor (init,formid) hst
	# (Conflicts papernrs) 		= formid.ival
	# (papersf,hst)				= vertlistFormButs 10 True (init,subsFormId formid "conflict" papernrs) hst
	= ({papersf & value = Conflicts papersf.value},hst)

gForm {|Co_authors|} informid hst = specialize myeditor informid hst
where
	myeditor (init,formid) hst
	# (Co_authors authors) 	= formid.ival
	# (authorsf,hst)		= vertlistFormButs 10 True (init,subsFormId formid "authors" authors) hst
	= ({authorsf & value = Co_authors authorsf.value},hst)

gForm {|Discussion|} informid hst = specialize myeditor informid hst
where
	myeditor (init,formid) hst
	# (Discussion messages)		= formid.ival
	= ({changed = False, form 	= showDiscussion messages, value = formid.ival},hst)
	where
		showDiscussion [] 	= []
		showDiscussion [{messageFrom,date,time,message}:more] 
							= 	[ mkTable [	[ Txt "date: ", toHtml date, Txt "time: ", toHtml time]
										  ,	[ Txt "from: ", B [] messageFrom ]
								]] ++ 
								[ Txt "message:", Txt message] ++
								[Hr []] ++ showDiscussion more

gForm {|[]|} gHa (init,formid) hst 
= case formid.ival of
	[x:xs]
	# (x,hst) 	= gHa (init,subFormId formid (toString (length xs)) x) hst
	# (xs,hst) 	= gForm {|*->*|} gHa (init,reuseFormId formid xs) hst
	= ({changed = x.changed||xs.changed,form = x.form ++ xs.form,value = [x.value:xs.value]},hst)
	[] 
	= ({changed = False,form = [],value = []},hst)

// derived forms ....

derive gForm /*[], */Maybe
derive gUpd [], Maybe
derive gPrint Maybe
derive gParse Maybe

derive gForm 	
				Login, Account, Member, ManagerInfo, RefereeInfo, /*Conflicts, */
				/*RefPerson, */Person,
				/*Reports, *//*RefReport, */ Report, Recommendation, Familiarity, 
				/*RefPaper, */Paper, PaperInfo,/* RefDiscussion,*/ 
				PaperStatus/*, Discussion */ , DiscussionStatus, Message 
derive gUpd 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts, 
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors, RefDiscussion,
				PaperStatus, Discussion, DiscussionStatus, Message
derive gPrint 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors, RefDiscussion,
				PaperStatus, Discussion, DiscussionStatus, Message
derive gParse 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts, 
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors, RefDiscussion,
				PaperStatus, Discussion, DiscussionStatus, Message
				
