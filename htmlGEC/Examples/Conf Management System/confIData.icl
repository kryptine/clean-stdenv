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

editorRefPerson :: !(InIDataId RefPerson) !*HSt -> (Form Person,!*HSt)
editorRefPerson (init,formid) hst
# (RefPerson refperson) = formid.ival
# (Refto name) 			= refperson
= universalRefEditor formid.mode (invariantPerson name) refperson hst

editorRefPaper :: !(InIDataId RefPaper) !*HSt -> (Form Paper,!*HSt)
editorRefPaper (init,formid) hst
# (RefPaper refpaper) 	= formid.ival
# (Refto name) 			= refpaper
= universalRefEditor formid.mode (invariantPaper name) refpaper hst

editorRefReport :: !(InIDataId RefReport) !*HSt -> (Form (Maybe Report),!*HSt)
editorRefReport (init,formid) hst
# (RefReport refreport) = formid.ival
# (Refto name) 			= refreport
= universalRefEditor formid.mode (invariant name) refreport hst
where
	invariant name Nothing 			= OK
	invariant name (Just report)	= invariantReport name report
	
// specialized forms

gForm {|RefPerson|} iniformid hst = specialize (invokeRefEditor editorRefPerson) 	iniformid hst
	
gForm {|RefPaper|}  iniformid hst = specialize (invokeRefEditor editorRefPaper)  	iniformid hst

gForm {|RefReport|} iniformid hst = specialize (invokeRefEditor editorRefReport)	iniformid hst

gForm {|Reports|} informid hst = specialize myeditor informid hst
where
	myeditor (init,formid) hst
	# (Reports reports) 		= formid.ival
	# (reportsf,hst)			= vertlistFormButs 10 (init,subsFormId formid "report" reports) hst
	= ({reportsf & value = Reports reportsf.value},hst)

gForm {|Conflicts|} informid hst = specialize myeditor informid hst
where
	myeditor (init,formid) hst
	# (Conflicts papernrs) 		= formid.ival
	# (papersf,hst)				= vertlistFormButs 10 (init,subsFormId formid "conflict" papernrs) hst
	= ({papersf & value = Conflicts papersf.value},hst)

gForm {|Co_authors|} informid hst = specialize myeditor informid hst
where
	myeditor (init,formid) hst
	# (Co_authors authors) 	= formid.ival
	# (authorsf,hst)		= vertlistFormButs 10 (init,subsFormId formid "authors" authors) hst
	= ({authorsf & value = Co_authors authorsf.value},hst)

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
				/*RefPaper, */Paper, PaperInfo 
derive gUpd 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts, 
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors 
derive gPrint 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors  
derive gParse 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts, 
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors 
				
