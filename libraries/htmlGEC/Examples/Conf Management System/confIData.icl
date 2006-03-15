implementation module confIData

import htmlHandler, StdList
import loginAdmin, stateHandling
import loginAdminIData

// general forms display settings

derive gUpd [], Maybe
derive gPrint Maybe
derive gParse Maybe

derive gForm 	
				Login, Account, Member, ManagerInfo, RefereeInfo, /* Conflicts, */
				/*RefPerson, */Person,
				Reports, /*RefReport, */Report, Recommendation, Familiarity, 
				/*RefPaper, */Paper, PaperInfo 
derive gUpd 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo 
derive gPrint 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo 
derive gParse 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo 


gForm {|[]|} gHa formid hst 
= case formid.ival of
	[x:xs]
	# (x,hst) 	= gHa (subFormId formid (toString (length xs)) x) hst
	# (xs,hst) 	= gForm {|*->*|} gHa (reuseFormId formid xs) hst
	= ({changed = x.changed||xs.changed,form = x.form ++ xs.form,value = [x.value:xs.value]},hst)
	[] 
	= ({changed = False,form = [],value = []},hst)

gForm {|Maybe|} ga formid hst 
# elem = formid.ival
= case elem of
	Nothing = ({value=Nothing,changed =False,form=[toHtml "Not yet done",Br]},hst)
	Just val
		# (valform,hst)	= ga (reuseFormId formid val) hst
		= ({value=Just valform.value,changed =valform.changed,form=valform.form},hst)

gForm {|RefPerson|} formid hst 
# (RefPerson refperson) 	= formid.ival
# (refpersonf,personf,hst)	= editPerson (Edit,Init,(refperson,Edit,Init)) hst
= ({value = RefPerson refpersonf.value, form = personf.form,changed = personf.changed},hst)

gForm {|RefPaper|} formid hst 
# (RefPaper refpaper) 		= formid.ival
# (refpaperf,paperf,hst)	= editPaper (Edit,Init,(refpaper,Edit,Init)) hst
= ({value = RefPaper refpaperf.value, form = paperf.form,changed = paperf.changed},hst)

gForm {|RefReport|} formid hst 
# (RefReport refreport) 	= formid.ival
# (refreportf,reportf,hst)	= editReport (Edit,Init,(refreport,Edit,Init)) hst
= ({value = RefReport refreportf.value, form = reportf.form,changed = reportf.changed},hst)

gForm {|Conflicts|} formid hst = specialize myeditor (Init,formid) hst
where
	myeditor (Init,formid) hst
	# (Conflicts papernrs) 	= formid.ival
	# (papersf,hst)			= vertlistFormButs 5 (Init,subsFormId formid "conflicts" papernrs) hst
	= ({value = Conflicts papersf.value, form = papersf.form,changed = papersf.changed},hst)

// editor for shared structures...

editRefto :: (!Mode,!Init,(!(!Refto a,!a),!!Mode,!Init)) *HSt -> 
		(Form (Refto a),Form a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
editRefto (rmode,rinit,((Refto s,a),pmode,pinit)) hst
	= reftoEditForm rmode rinit (pinit,
			{nFormId ("Refto_" +++ s) (Refto s, a) & mode = pmode}) hst

editPerson :: (!Mode,!Init,(!Refto Person,!Mode,!Init)) *HSt -> (Form (Refto Person),Form Person,!*HSt)
editPerson (rmode,rinit,(Refto person,pmode,pinit)) hst
	= editRefto (rmode,rinit,((Refto person,initPerson person),pmode,pinit)) hst

getPerson :: !RefPerson *HSt -> (!Person,!*HSt)
getPerson (RefPerson refperson) hst
# (refpersonf,personf,hst) = editPerson (Display,Init,(refperson,Display,Init)) hst
= (personf.value,hst) 

editReport :: (!Mode,!Init,(!Refto Report,!Mode,!Init)) *HSt -> (Form (Refto Report),Form Report,!*HSt)
editReport (rmode,rinit,(Refto report,pmode,pinit)) hst
	= editRefto (rmode,rinit,((Refto report,initReport),pmode,pinit)) hst

getReport :: !RefReport *HSt -> (!Report,!*HSt)
getReport (RefReport refreport) hst
# (refreportf,reportf,hst) = editReport (Display,Init,(refreport,Display,Init)) hst
= (reportf.value,hst) 

editPaper :: (!Mode,!Init,(!Refto Paper,!Mode,!Init)) *HSt -> (Form (Refto Paper),Form Paper,!*HSt)
editPaper (rmode,rinit,(Refto paper,pmode,pinit)) hst
	= editRefto (rmode,rinit,((Refto paper,initPaper paper),pmode,pinit)) hst

getPaper :: !RefPaper *HSt -> (!Paper,!*HSt)
getPaper (RefPaper refpaper) hst
# (refpaperf,paperf,hst) = editPaper (Display,Init,(refpaper,Display,Init)) hst
= (paperf.value,hst) 

editAccounts :: !Mode !Init !ConfAccounts *HSt -> (!Form ConfAccounts,!*HSt) // login administration database
editAccounts mode Init accounts hst 
	= mkEditForm (Init,
		{pFormId uniqueDBname accounts
			 & mode = mode, lifespan = ifEdit mode Persistent PersistentRO}) hst
editAccounts mode Set accounts hst 
	= mkEditForm (Set,
		{pFormId uniqueDBname accounts
			 & mode = mode, lifespan = ifEdit mode Persistent PersistentRO}) hst

ReportStore :: (Judgement -> Judgement) *HSt -> (Judgement,!*HSt)
ReportStore judge hst 
# (judgef,hst) = mkStoreForm (Init,sdFormId "cf_alert" OK) judge hst
= (judgef.value,hst)

