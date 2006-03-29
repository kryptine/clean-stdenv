implementation module confIData

import htmlHandler, htmlTrivial, StdList
import stateHandling
import loginAdmin, loginAdminIData

// global editors and stores

AccountsDB :: !Init  !ConfAccounts *HSt -> (ConfAccounts,!*HSt) // conf management database
AccountsDB init accounts hst 
# accounts = setInvariantAccounts accounts										// ensure that all links are correct
= universalDB init (\a -> invariantLogAccounts a + invariantConfAccounts a) 
	uniqueDBname accounts hst 

// editor for persistent information

universalDB :: !Init !(a -> Judgement) !String !a *HSt -> (a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
universalDB init invariant filename value hst
# (dbf,hst)			= myDatabase Display 0 value hst			// create / read out database file
# dbversion			= fst dbf.value								// version number stored in database
# dbvalue			= snd dbf.value								// value stored in database
# (versionf,hst)	= myVersion Init 0 hst 						// create / read out version number expected by this application
# version			= versionf.value							// current version number assumed in this application
| init == Init													// we only want to read, no version conflict
	# (_,hst)		= myVersion Set dbversion hst 				// synchronize version number and
	= (dbvalue,hst)												// return current value stored in database
| dbversion <> version											// we want to write and have a version conflict
	# (_,hst)		= myVersion Set dbversion hst				// synchronize with new version
	# (_,hst)		= ExceptionStore ((+) (False,"DB Version conflict with database named " +++ filename)) hst	// Raise exception
	= (dbvalue,hst)												// return current version stored in database 
# (ok,msg)			= invariant value							// no version conflict, check invariants															// check invariants
| not ok														// we want to write, but invariants don't hold
	# (_,hst)		= ExceptionStore ((+) (ok,msg)) hst 		// report them 
	= (value,hst)												// return disapproved value such that it can be improved
# (versionf,hst)	= myVersion Set (dbversion + 1) hst 		// increment version number
# (_,hst)			= myDatabase Edit versionf.value value hst 	// update database file
= (value,hst)
where
	myDatabase Display cnt value hst = mkEditForm (Init,{rFormId filename (cnt,value) & mode = NoForm}) hst 	// read the database
	myDatabase Edit    cnt value hst = mkEditForm (Set, {pFormId filename (cnt,value) & mode = NoForm}) hst	// write the database

	myVersion init cnt hst	= mkEditForm (init,sdFormId ("vrs_" +++ filename) cnt) hst		// to remember version number

// editor for reference to a file

editRefto :: (!Mode,!Init,(!(!Refto a,!a),!!Mode,!Init)) *HSt -> 
		(Form (Refto a),Form a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
editRefto (rmode,rinit,((Refto s,a),pmode,pinit)) hst
	= reftoEditForm rmode rinit (pinit,
			{nFormId ("Refto_" +++ s) (Refto s, a) & mode = pmode, lifespan = onMode pmode Persistent PersistentRO PersistentRO}) hst


universalRefEditor :: !Mode  !(a -> Judgement) !(Refto a) *HSt -> (Form a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
universalRefEditor mode invariant (Refto filename) hst
| filename == ""	// just temperal
	= myEditor Init mode createDefault hst
# (_,dbf,hst)		= myDatabase Init 0 createDefault hst		// create / read out database file
# dbversion			= fst dbf.value								// version number stored in database
# dbvalue			= snd dbf.value								// value stored in database
# (versionf,hst)	= myVersion Init 0 hst 						// create / read out version number expected by this application
# version			= versionf.value							// current version number assumed in this application
# (valuef,hst)		= myEditor Init mode dbvalue hst			// create / read out current value 
| mode == Display												// we only want to read, no version conflict
	# (_,hst)		= myVersion Set version hst 				// synchronize version number and
	= (valuef,hst)												// return current value in editor
| dbversion <> version											// we have a version conflict and want to write
	# (_,hst)		= myVersion Set dbversion hst				// synchronize with new version
	# (_,hst)		= ExceptionStore ((+) (False,"Ref Editor Version conflict with information in " +++ filename)) hst	// Raise exception
	= myEditor Set mode dbvalue hst								// return current version stored in database 
# (ok,msg)			= invariant valuef.value					// no version conflict, check invariants															// check invariants
| not ok														// we want to write, but invariants don't hold
	# (_,hst)		= ExceptionStore ((+) (ok,msg)) hst 		// report them 
	= (valuef,hst)												// return wrong value such that it can be improved
//| not valuef.changed											// nothing has changed,
//	= (valuef,hst)												// so we don't write anything
# (versionf,hst)	= myVersion Set (dbversion + 1) hst 		// increment version number
# (_,_,hst)			= myDatabase Set versionf.value valuef.value hst // update database file
= (valuef,hst)
where
	myDatabase Init cnt value hst = editRefto (Display,Init,((Refto filename,(cnt,value)),Display,Init)) hst 	// read the database
	myDatabase Set  cnt value hst = editRefto (Edit   ,Set, ((Refto filename,(cnt,value)),Display,Init)) hst	// write the database

	myVersion init cnt hst	= mkEditForm (init,sdFormId ("vrs_" +++ filename) cnt) hst						// to remember version number

	myEditor :: !Init !Mode !a *HSt -> (Form a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
	myEditor init mode value hst	= mkEditForm (init,{sFormId ("copy_" +++ filename) value & mode = mode}) hst	// to remember version number


// specialized forms

gForm {|RefPerson|} (init,formid) hst = specialize myeditor (init,formid) hst
where
	myeditor (init,formid) hst
	# (RefPerson refperson) 	= formid.ival
	# (Refto name) 				= refperson
	# (personf,hst)				= universalRefEditor formid.mode (invariantPerson name) refperson hst
	= ({personf & value = RefPerson refperson},hst)
	

gForm {|RefReport|} (init,formid) hst = specialize myeditor (init,formid) hst
where
	myeditor (init,formid) hst
	# (RefReport refreport) 	= formid.ival
	# (Refto name) 				= refreport
	# (reportf,hst)				= universalRefEditor formid.mode (invariant name) refreport hst
	= ({reportf & value = RefReport refreport},hst)

	invariant name Nothing 			= OK
	invariant name (Just report)	= invariantReport name report

gForm {|RefPaper|} (init,formid) hst = specialize myeditor (init,formid) hst
where
	myeditor (init,formid) hst
	# (RefPaper refpaper) 		= formid.ival
	# (Refto name) 				= refpaper
	# (reportf,hst)				= universalRefEditor formid.mode (invariantPaper name) refpaper hst
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
AccountsDB Edit Set accounts hst 
# ((ok,judge),hst)	= ReportStore (\old -> (old + 
											invariantLogAccounts  accounts +
										  	invariantConfAccounts accounts)	) hst	// test invariants
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
*/
/*
universalRefEditor :: !Mode  !(a -> Judgement) !(Refto a) *HSt -> (Form a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
universalRefEditor mode invariant (Refto filename) hst
# (_,filef,hst)		= editRefto (Display,Init,((Refto filename,createDefault),Display,Init)) hst	// read out file
# (copyf,hst)		= mkEditForm (Init,{sFormId ("cp_" +++ filename) filef.value & mode = mode}) hst			// make an editor
# (ok,msg)			= invariant copyf.value															// check invariants
# (_,hst)			= if (filename <> "") (ReportStore ((+) (ok,msg)) hst) (undef,hst)				// report them
# (_,_,hst)			= if (ok && mode == Edit) 														// write to file if ok
						(editRefto (Edit,Set,((Refto filename,copyf.value),Display,Init)) hst)
						(undef,undef,hst)
= (copyf,hst)
*/
				