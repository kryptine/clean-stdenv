implementation module htmlRefFormlib

// Handy collection of Form's
// (c) MJP 2005

import StdEnv, StdHtml, StdLib

derive gForm []; derive gUpd []
derive gForm 	Refto, Maybe
derive gUpd 	Refto, Maybe
derive gPrint 	Refto, Maybe
derive gParse 	Refto, Maybe

:: Refto a = Refto String


reftoEditForm :: !Mode !Init !(InIDataId (Refto a,a)) !*HSt -> (Form (Refto a),Form a,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
reftoEditForm  modev initv (init,formid) hst 
# (ref,hst)		= mkEditForm (init, reuseFormId formid (Refto filename)) hst
# (Refto nname)	= ref.value
# (file,hst) 	= mkEditForm (initv,{ formid 	& id 		= nname
												, ival		= a
												, lifespan 	= onMode modev Persistent PersistentRO PersistentRO // ???
												, mode		= modev 
									}) hst
= 	( {ref  & form = [[toHtml ("File Name: " )] <=> ref.form]}
	, {file & form = [[toHtml (nname +++ ": ")] <=> file.form]}
	, hst
	)
where
	(Refto filename,a) = formid.ival

reftoVertListForm :: !Mode !Init !(InIDataId [(Refto a,a)]) !*HSt -> (Form [Refto a],Form [a],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
reftoVertListForm modev initv (init,formid) hst
# storeid		= reuseFormId formid [Refto name\\ (Refto name,_) <- formid.ival]
# (store,hst)	= mkStoreForm (init,storeid) id hst												// store for refto list	
# (twof,hst) 	= maplSt (reftoEditForm` modev initv) [(init,subnFormId formid (name <+++ i) (Refto name,a)) 
											\\ (Refto name) <- store.value 
											& (_,a) <- formid.ival ++ repeat (Refto "",createDefault)
											& i <- [1..] ] hst
# (fref,ffile)	= unzip twof
# frefvalue		= [elem.value \\ elem <- fref]
# (store,hst)	= mkStoreForm (init,storeid) (\list -> frefvalue) hst
= (	{ changed 	= or [elem.changed \\ elem <- fref]
	, value 	= [elem.value \\ elem <- fref]
	, form		= [BodyTag elem.form \\ elem <- fref]
	}
  , { changed 	= or [elem.changed \\ elem <- ffile]
	, value 	= [elem.value \\ elem <- ffile]
	, form		= [BodyTag elem.form \\ elem <- ffile]
	},hst)
where
	reftoEditForm` modev initv all hst
	# (fref,ffile,hst) = reftoEditForm modev initv all hst
	= ((fref,ffile),hst)

reftoListFormButs :: !Int !Mode !Init !(InIDataId [(Refto a,a)]) !*HSt -> (Form [Refto a],Form [a],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
reftoListFormButs nbuts mode initv (init,formid) hst
# indexId		= {subsFormId formid "idx" 0 & mode = Edit}
# (index,hst)	= mkEditForm (init,indexId) hst
# (rlist,vlist,hst)= reftoVertListForm mode initv (init,formid) hst
# lengthlist	= length rlist.value

# pdmenu		= PullDown (1,defpixel) (0, [toString lengthlist +++ " More... " :["Show " +++ toString i \\ i <- [1 .. max 1 lengthlist]]]) 
# pdmenuId		= {subsFormId formid "pdm" pdmenu & mode = Edit}
# (pdbuts,hst)	= mkEditForm (Init, pdmenuId) hst
# (PullDown _ (step,_))	= pdbuts.value

| step == 0		= (rlist,{form=pdbuts.form,value=vlist.value,changed=rlist.changed || vlist.changed || pdbuts.changed},hst)		

# bbutsId		= {subsFormId formid "bb" index.value & mode = Edit}
# (obbuts, hst)	= browseButtons (Init, bbutsId) step lengthlist nbuts hst

# addId			= subnFormId formid "add" addbutton
# (add	,hst) 	= ListFuncBut (Init, addId) hst	

# dellId		= subnFormId formid "dell" (delbutton obbuts.value step)
# (del	,hst) 	= ListFuncBut (Init, dellId) hst	
# insrtId		= subnFormId formid "ins"  (insertbutton createDefault obbuts.value step)
# (ins	,hst) 	= ListFuncBut (Init, insrtId) hst	
# appId			= subnFormId formid "app"  (appendbutton createDefault obbuts.value step)
# (app	,hst) 	= ListFuncBut (Init, appId) hst	

# elemId		= subsFormId formid "copyelem" createDefault
# copyId		= subnFormId formid "copy"  (copybutton obbuts.value step)
# (copy	,hst) 	= ListFuncBut (Init, copyId) hst	
# (elemstore,hst)= mkStoreForm (Init,elemId) (if copy.changed (\_ -> vlist.value!!copy.value 0) id) hst

# pasteId		= subnFormId formid "paste"  (pastebutton obbuts.value step)
# (paste,hst) 	= ListFuncBut (Init, pasteId) hst	

# newlist		= rlist.value
# newlist		= ins.value newlist 
# newlist		= add.value newlist
# newlist		= app.value newlist 
# newlist		= del.value newlist 

# (_,_,hst)		= if paste.changed (reftoEditForm mode Set 
						(Init,nFormId "ttt" (rlist.value!!(paste.value 0),elemstore.value )) hst)
						(undef,undef,hst)
						
# (rlist,vlist,hst)= reftoVertListForm mode initv (setID formid [(reftoa,createDefault) \\ reftoa <- newlist]) hst
# lengthlist	= length rlist.value

# (index,hst)	= mkEditForm (setID indexId obbuts.value) hst
# (bbuts, hst)	= browseButtons (Init, bbutsId) step lengthlist nbuts hst

# betweenindex	= (bbuts.value,bbuts.value + step - 1)

# pdmenu		= PullDown (1,defpixel) (step, [toString lengthlist +++ " More... ":["Show " +++ toString i \\ i <- [1 .. max 1 lengthlist]]]) 
# (pdbuts,hst)	= mkEditForm (setID pdmenuId pdmenu) hst
 
= 	(	rlist	
	,	{ form 		= pdbuts.form ++ bbuts.form ++ 
						[[(toHtml ("nr " <+++ (i+1)  <+++ " / " <+++ length rlist.value) <.||.> 
							(onMode formid.mode (del <.=.> ins <.=.> app  <.=.> copy  <.=.> paste) EmptyBody EmptyBody)) 
						\\ del <- del.form & ins <- ins.form & app <- app.form & copy <- copy.form & paste <- paste.form 
						& i <- [bbuts.value..]] 
							<=|> [re <.=.> ve \\ re <- rlist.form%betweenindex & ve <- vlist.form%betweenindex]] ++ 
						(if (lengthlist <= 0) add.form [])
		, value 	= vlist.value
		, changed 	= rlist.changed || vlist.changed || obbuts.changed || del.changed || pdbuts.changed ||ins.changed ||
						add.changed || copy.changed || paste.changed
		}
	,	hst )
where
	addbutton = 
		[ (but 1 "Append", \_ -> [createDefault])]
	but i s	= LButton (defpixel/i) s
	delbutton index step = 
		[ (but 5 "D", \list -> removeAt i list) \\ i <- [index .. index + step]]
	insertbutton e index step = 
		[ (but 5 "I", \list -> insertAt i e list) \\ i <- [index .. index + step]]
	appendbutton e index step = 
		[ (but 5 "A", \list -> insertAt (i+1) e list) \\ i <- [index .. index + step]]
	copybutton index step = 
		[ (but 5 "C", \_ -> i) \\ i <- [index .. index + step]]
	pastebutton index step = 
		[ (but 5 "P", \_ -> i) \\ i <- [index .. index + step]]


// editor for persistent information

universalDB :: !Init !(String a -> Judgement) !String !a *HSt -> (a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
universalDB init invariant filename value hst
# (dbf,hst)			= myDatabase Display 0 value hst			// create / read out database file
# dbversion			= fst dbf.value								// version number stored in database
# dbvalue			= snd dbf.value								// value stored in database
# (versionf,hst)	= myVersion Init dbversion hst 				// create / read out version number expected by this application
# version			= versionf.value							// current version number assumed in this application
| init == Init													// we only want to read, no version conflict
	# (_,hst)		= myVersion Set dbversion hst 				// synchronize version number and
	= (dbvalue,hst)												// return current value stored in database
| dbversion <> version											// we want to write and have a version conflict
	# (_,hst)		= myVersion Set dbversion hst				// synchronize with new version
	# (_,hst)		= ExceptionStore ((+) (Just (filename,"version conflict; latest value restored"))) hst	// Raise exception
	= (dbvalue,hst)												// return current version stored in database 
# exception			= invariant filename value					// no version conflict, check invariants															// check invariants
| isJust exception												// we want to write, but invariants don't hold
	# (_,hst)		= ExceptionStore ((+) exception) hst 		// report them 
	= (value,hst)												// return disapproved value such that it can be improved
# (versionf,hst)	= myVersion Set (dbversion + 1) hst 		// increment version number
# (_,hst)			= myDatabase Edit versionf.value value hst 	// update database file
= (value,hst)
where
	myDatabase Display cnt value hst = mkEditForm (Init,{rFormId filename (cnt,value) & mode = NoForm}) hst 	// read the database
	myDatabase Edit    cnt value hst = mkEditForm (Set, {pFormId filename (cnt,value) & mode = NoForm}) hst		// write the database

	myVersion init cnt hst	= mkEditForm (init,sdFormId ("vrs_" +++ filename) cnt) hst		// to remember version number

// editor for reference to a file

editRefto :: (!Mode,!Init,(!(!Refto a,!a),!!Mode,!Init)) *HSt -> 
		(Form (Refto a),Form a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
editRefto (rmode,rinit,((Refto s,a),pmode,pinit)) hst
	= reftoEditForm rmode rinit (pinit,
			{nFormId ("Refto_" +++ s) (Refto s, a) & mode = pmode}) hst


universalRefEditor :: !Mode  !(a -> Judgement) !(Refto a) *HSt -> (Form a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
universalRefEditor mode invariant (Refto filename) hst
| filename == ""	// just temporal
	= myEditor Init mode createDefault hst
# (_,dbf,hst)		= myDatabase Init 0 createDefault hst		// create / read out database file
# dbversion			= fst dbf.value								// version number stored in database
# dbvalue			= snd dbf.value								// value stored in database
# (versionf,hst)	= myVersion Init dbversion hst 				// create / read out version number expected by this application
# version			= versionf.value							// current version number assumed in this application
| mode == Display												// we only want to read, no version conflict
	# (valuef,hst)	= myEditor  Set mode dbvalue hst			// synchronize with latest value 
	# (_,hst)		= myVersion Set version hst 				// synchronize version number and
	= (valuef,hst)												// return current value in editor
# (valuef,hst)		= myEditor Init mode dbvalue hst			// create / read out current value 
| dbversion <> version											// we have a version conflict and want to write
	# (_,hst)		= myVersion Set dbversion hst				// synchronize with new version
	# (_,hst)		= ExceptionStore ((+) (Just (filename,"ref version conflict; latest value restored"))) hst	// Raise exception
	= myEditor Set mode dbvalue hst								// return current version stored in database 
# exception			= invariant valuef.value					// no version conflict, check invariants															// check invariants
| isJust exception												// we want to write, but invariants don't hold
	# (_,hst)		= ExceptionStore ((+) exception) hst 		// report them 
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


invokeRefEditor :: (!(InIDataId b) !*HSt -> (Form d,!*HSt)) (InIDataId b) !*HSt -> (Form b,!*HSt)
invokeRefEditor editor (init,formid) hst
# (formf,hst)		= editor (init,formid) hst
= ({formf & value = formid.ival},hst)

// Exception handling 

Ok :: Judgement
Ok = Nothing

instance + Judgement
where
	(+) j1 j2 = addJudgement j1 j2
	where
		addJudgement _  (Just j2) 	= (Just j2)
		addJudgement (Just j1) _ 	= (Just j1)
		addJudgement _ _ 			= Nothing

ExceptionStore :: (Judgement -> Judgement) !*HSt -> (Judgement,!*HSt)
ExceptionStore judge hst 
# (judgef,hst) = mkStoreForm (Init,{nFormId "handle_exception" Ok & mode = NoForm, lifespan = Temp}) judge hst
= (judgef.value,hst)

