implementation module confIData

import htmlHandler, htmlTrivial, StdList
import stateHandling
import loginAdmin, loginAdminIData

// global editors and stores

editAccounts :: !Mode !Init !ConfAccounts *HSt -> (Form ConfAccounts,!*HSt) // login administration database
editAccounts mode Init accounts hst 
	= mkEditForm (Init,
		{pFormId uniqueDBname accounts
			 & mode = mode, lifespan = ifEdit mode Persistent PersistentRO}) hst
editAccounts mode Set accounts hst 
	= mkEditForm (Set,
		{pFormId uniqueDBname accounts
			 & mode = mode, lifespan = ifEdit mode Persistent PersistentRO}) hst

// editor for shared structures...

editRefto :: (!Mode,!Init,(!(!Refto a,!a),!!Mode,!Init)) *HSt -> 
		(Form (Refto a),Form a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
editRefto (rmode,rinit,((Refto s,a),pmode,pinit)) hst
	= reftoEditForm rmode rinit (pinit,
			{nFormId ("Refto_" +++ s) (Refto s, a) & mode = pmode}) hst

// reference to Persons

getPerson :: !(Refto Person) *HSt -> (!Person,!*HSt)
getPerson refperson hst
# (_,personf,hst) = editRefto (Display,Init,((refperson,createDefault),Display,Init)) hst
= (personf.value,hst) 

setPerson :: !(Refto Person) !Person *HSt -> (!Person,!*HSt)
setPerson refperson person hst
# (_,personf,hst) = editRefto (Edit,Set,((refperson,person),Display,Init)) hst
= (personf.value,hst) 

editPerson :: !Mode !Init !(Refto Person) *HSt -> (Form Person,!*HSt)
editPerson mode init (Refto person) hst
# (personv,hst)	= getPerson (Refto person) hst									// read out file
# (personf,hst)	= mkEditForm (init,{sFormId person personv & mode = mode}) hst	// make an editor
# (ok,msg)		= invariantPerson personf.value									// check invariants
# (_,hst)		= ReportStore (addJudgement (ok,msg)) hst						// report them
# (_,_,hst)		= if (ok && mode == Edit) 										// store 
						(editRefto (Edit,Set,((Refto person,personf.value),Display,Init)) hst)
						(undef,undef,hst)
= (personf,hst)


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


// special idata

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
# (personf,hst)				= editPerson Edit Init refperson hst
= ({value = RefPerson refperson, form = personf.form,changed = personf.changed},hst)

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
				
				
				
/*
editPerson :: (!Mode,!Init,(!Refto Person,!Mode,!Init)) *HSt -> (Form (Refto Person),Form Person,!*HSt)
editPerson (rmode,rinit,(Refto person,pmode,pinit)) hst
# (personv,hst)				= getPerson (Refto person)
//# (refpersonf,personf,hst) 	= editRefto (rmode,rinit,((Refto person,initPerson person),pmode,pinit)) hst
# (personf,hst)				= mkEdit (rinit,sFormId person peronv)
# (ok,msg)					= invariantPerson personf.value
# (_,hst) 					= ReportStore (addJudgement (ok,msg)) hst
# (_,hst)					= if ok
									(editRefto (rmode,rinit,((Refto person,initPerson person),pmode,pinit)) hst)
= (refpersonf,personf,hst)
*/ 