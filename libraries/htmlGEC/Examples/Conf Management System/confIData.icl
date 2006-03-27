implementation module confIData

import htmlHandler, htmlTrivial, StdList
import stateHandling
import loginAdmin, loginAdminIData

// global editors and stores

AccountsDB :: !Mode !Init !ConfAccounts *HSt -> (Form ConfAccounts,!*HSt) // conf management database
AccountsDB Edit Set accounts hst 
# ((ok,judge),hst)	= ReportStore (\_ -> (invariantLogAccounts  accounts +
										  invariantConfAccounts accounts )	) hst	// test invariants
# accounts			= setInvariantAccounts accounts										// set invariants
= mkEditForm (Set,
		{sFormId uniqueDBname accounts
			 & lifespan = if ok Persistent PersistentRO}													// store conf accounts
			 ) hst
AccountsDB mode init accounts hst 
	= mkEditForm (init,
		{sFormId uniqueDBname accounts
			 & lifespan = PersistentRO}													// nothing stored
			 ) hst

// general deRefto approach

editRefto :: (!Mode,!Init,(!(!Refto a,!a),!!Mode,!Init)) *HSt -> 
		(Form (Refto a),Form a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
editRefto (rmode,rinit,((Refto s,a),pmode,pinit)) hst
	= reftoEditForm rmode rinit (pinit,
			{nFormId ("Refto_" +++ s) (Refto s, a) & mode = pmode}) hst

universalRefEditor :: !Mode  !(a -> Judgement) !(Refto a) *HSt -> (Form a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
universalRefEditor mode invariant (Refto filename) hst
# (_,filef,hst)		= editRefto (Display,Init,((Refto filename,createDefault),Display,Init)) hst	// read out file
# (copyf,hst)		= mkEditForm (Init,{sFormId filename filef.value & mode = mode}) hst			// make an editor
# (ok,msg)			= invariant copyf.value															// check invariants
# (_,hst)			= if (filename <> "") (ReportStore ((+) (ok,msg)) hst) (undef,hst)				// report them
# (_,_,hst)			= if (ok && mode == Edit) 														// write to file if ok
						(editRefto (Edit,Set,((Refto filename,copyf.value),Display,Init)) hst)
						(undef,undef,hst)
= (copyf,hst)

// specialized forms

gForm {|RefPerson|} (init,formid) hst = specialize myeditor (init,formid) hst
where
	myeditor (init,formid) hst
	# (RefPerson refperson) 	= formid.ival
	# (personf,hst)				= universalRefEditor formid.mode invariantPerson refperson hst
	= ({personf & value = RefPerson refperson},hst)

gForm {|RefReport|} (init,formid) hst = specialize myeditor (init,formid) hst
where
	myeditor (init,formid) hst
	# (RefReport refreport) 	= formid.ival
	# (reportf,hst)				= universalRefEditor formid.mode (\_ -> OK) refreport hst
	= ({reportf & value = RefReport refreport},hst)

gForm {|RefPaper|} (init,formid) hst = specialize myeditor (init,formid) hst
where
	myeditor (init,formid) hst
	# (RefPaper refpaper) 		= formid.ival
	# (reportf,hst)				= universalRefEditor formid.mode invariantPaper refpaper hst
	= ({reportf & value = RefPaper refpaper},hst)


gForm {|Reports|} informid hst = specialize myeditor informid hst
where
	myeditor (init,formid) hst
	# (Reports reports) 		= formid.ival
	# (reportsf,hst)			= vertlistFormButs 5 (init,subsFormId formid "report" reports) hst
	= ({reportsf & value = Reports reportsf.value},hst)

gForm {|Conflicts|} informid hst = specialize myeditor informid hst
where
	myeditor (init,formid) hst
	# (Conflicts papernrs) 		= formid.ival
	# (papersf,hst)				= vertlistFormButs 5 (init,subsFormId formid "conflict" papernrs) hst
	= ({papersf & value = Conflicts papersf.value},hst)

gForm {|Co_authors|} informid hst = specialize myeditor informid hst
where
	myeditor (init,formid) hst
	# (Co_authors authors) 	= formid.ival
	# (authorsf,hst)		= vertlistFormButs 5 (init,subsFormId formid "authors" authors) hst
	= ({authorsf & value = Co_authors authorsf.value},hst)

gForm {|[]|} gHa (init,formid) hst 
= case formid.ival of
	[x:xs]
	# (x,hst) 	= gHa (init,subFormId formid (toString (length xs)) x) hst
	# (xs,hst) 	= gForm {|*->*|} gHa (init,reuseFormId formid xs) hst
	= ({changed = x.changed||xs.changed,form = x.form ++ xs.form,value = [x.value:xs.value]},hst)
	[] 
	= ({changed = False,form = [],value = []},hst)

/*
gForm {|Maybe|} ga (init,formid) hst 
# elem = formid.ival
= case elem of
	Nothing = ({value=Nothing,changed =False,form=[toHtml "Not yet done",Br]},hst)
	Just val
		# (valform,hst)	= ga (init,reuseFormId formid val) hst
		= ({value=Just valform.value,changed =valform.changed,form=valform.form},hst)
*/
// general forms display settings

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
				
/*
PersonDB :: !Mode !Init !(Refto Person) *HSt -> (Form Person,!*HSt)
PersonDB mode init refperson hst = universalRefEditor Edit invariantPerson refperson hst


uniqueKey :: *HSt -> (Form Int,!*HSt)
uniqueKey hst = mkStoreForm (Init,pFormId "key" 1) inc hst

mkUniqueKey  prefix "" hst 
# (intf,hst)	=	uniqueKey hst
= (prefix +++ toString intf.value,hst)
mkUniqueKey  prefix key hst 
= (key,hst)

*/

				