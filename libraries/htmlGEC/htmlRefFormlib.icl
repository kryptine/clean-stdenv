implementation module htmlRefFormlib

// Handy collection of Form's
// (c) MJP 2005

import StdEnv, StdHtml, StdLib
import GenEq

derive gForm []; derive gUpd []
derive gForm 	Maybe
derive gUpd 	Maybe
derive gPrint 	Maybe
derive gParse 	Maybe

:: Ref2 a = Ref2 String

instance == (Ref2 a)
where
	(==)(Ref2 file1) (Ref2 file2) = file1 == file2

ref2EditForm :: !(InIDataId a) !(InIDataId (Ref2 a))  !*HSt -> (Form a,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
ref2EditForm  (inita,formida) (_,{ival=Ref2 refname}) hst 
| refname == "" = mkEditForm (Init,reuseFormId formida createDefault) hst
| otherwise     = mkEditForm (inita,{formida & id = refname}) hst

invokeRefEditor :: (!(InIDataId b) !*HSt -> (Form d,!*HSt)) (InIDataId b) !*HSt -> (Form b,!*HSt)
invokeRefEditor editor (init,formid) hst
# (idata,hst)		= editor (init,formid) hst
= ({idata & value = formid.ival},hst)

universalRefEditor 	:: !(InIDataId (Ref2 a)) !(a -> Judgement)  !*HSt -> (Form a,!*HSt)   	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
universalRefEditor (init,formid) invariant  hst
# (Ref2 filename)	= formid.ival
| filename == ""	= mkEditForm (Init,xtFormId "ure_TEMP" createDefault) hst
# (dbf,hst)			= myDatabase Init filename (0,createDefault) hst		
																// create / read out current value in file file
# dbvalue			= dbf.value	
# dbversion			= fst dbvalue								// version number stored in database
# dbvalue			= snd dbvalue								// value stored in database
# (versionf,hst)	= myVersion Init filename dbversion hst 	// create / read out version number expected by this application
# version			= versionf.value							// current version number assumed in this application
| init == Init && 
			(formid.mode == Display || formid.mode == NoForm || filename == "") 	// we only want to read, no version conflict
	= myEditor Init filename dbvalue hst							// synchronize with latest value 
| dbversion > version											// we have a version conflict and want to write
	# (_,hst)		= ExceptionStore ((+) (Just (filename, "Ref Your screen data is out of date; I have retrieved the latest data."))) hst	// Raise exception
	# (_,hst)		= myVersion Set filename dbversion hst		// synchronize with new version
	= myEditor Set filename dbvalue hst							// return current version stored in database 
# (valuef,hst)		= myEditor Init filename dbvalue hst		// editor is in sync; create / read out current value 
# exception			= invariant valuef.value					// check invariants															// check invariants
| isJust exception												// we want to write, but invariants don't hold
	# (_,hst)		= ExceptionStore ((+) exception) hst 		// report them 
	= (valuef,hst)												// return wrong value such that it can be improved
# (versionf,hst)	= myVersion  Set filename (dbversion + 1) hst// increment version number
# (_,hst)			= myDatabase Set filename (dbversion + 1,valuef.value) hst // update database file
= ({valuef & changed = True},hst)
where
	myDatabase dbinit filename cntvalue hst 
	# databaseId	= {reuseFormId formid (Ref2 filename) & id = formid.id +++ "refto" /*+++ filename*/}
	= case dbinit of
		Init = ref2EditForm (Init,xrFormId "" cntvalue) (init,databaseId) hst 	// read the database
		Set	 = ref2EditForm (Set, xpFormId "" cntvalue) (init,databaseId) hst	// write the database

	myVersion init filename cnt hst	= mkEditForm (init,{reuseFormId formid cnt & id = ("vrs_r_" +++ filename)
																				, mode = NoForm}) hst						// to remember version number

	myEditor :: !Init  !String !a *HSt -> (Form a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
	myEditor init filename value hst	
	# formId = {reuseFormId formid value & id = "copy_r_" +++ filename}
	= mkShowHideForm (init,formId) hst	// copy of database information 


// editor for persistent information

universalDB :: !Init !(String a -> Judgement) !String !a !*HSt -> (a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
universalDB init invariant filename value hst
# (dbf,hst)			= myDatabase Init 0 value hst				// create / read out database file
# dbversion			= fst dbf.value								// version number stored in database
# dbvalue			= snd dbf.value								// value stored in database
# (versionf,hst)	= myVersion Init dbversion hst 				// create / read out version number expected by this application
# version			= versionf.value							// current version number assumed in this application
| init == Init													// we only want to read, no version conflict
	# (_,hst)		= myVersion Set dbversion hst 				// synchronize version number and
	= (dbvalue,hst)												// return current value stored in database
| dbversion <> version											// we want to write and have a version conflict
	# (_,hst)		= myVersion Set dbversion hst				// synchronize with new version
	# (_,hst)		= ExceptionStore ((+) (Just (filename,"Your screen data is out of date; I have retrieved the latest data."))) hst	// Raise exception
	= (dbvalue,hst)												// return current version stored in database 
# exception			= invariant filename value					// no version conflict, check invariants															// check invariants
| isJust exception												// we want to write, but invariants don't hold
	# (_,hst)		= ExceptionStore ((+) exception) hst 		// report them 
	= (value,hst)												// return disapproved value such that it can be improved
# (versionf,hst)	= myVersion Set (dbversion + 1) hst 		// increment version number
# (_,hst)			= myDatabase Set versionf.value value hst 	// update database file
= (value,hst)
where
	myDatabase Init cnt value hst = mkEditForm (Init,{rFormId filename (cnt,value) & mode = NoForm}) hst 	// read the database
	myDatabase Set  cnt value hst = mkEditForm (Set, {pFormId filename (cnt,value) & mode = NoForm}) hst	// write the database

	myVersion init cnt hst	= mkEditForm (init,xtFormId ("vrs_db_" +++ filename) cnt) hst		// to remember version number

// Exception handling 

Ok :: Judgement
Ok = Nothing

noException :: Judgement -> Bool
noException judgement = isNothing judgement

yesException :: Judgement -> Bool
yesException judgement = not (noException judgement)

instance + Judgement
where
	(+) j1 j2 = addJudgement j1 j2
	where
//		addJudgement (Just (r1,j1)) (Just (r2,j2)) 	= (Just ((r1 +++ " " +++ r2),(j1 +++ " " +++ j2))) //for debugging
		addJudgement (Just j1) _ 	= Just j1
		addJudgement _  (Just j2) 	= Just j2
		addJudgement _ _ 			= Nothing

ExceptionStore :: (Judgement -> Judgement) !*HSt -> (Judgement,!*HSt)
ExceptionStore judge hst 
# (judgef,hst) = mkStoreForm (Init,{nFormId "handle_exception" Ok & mode = NoForm, lifespan = Temp}) judge hst
= (judgef.value,hst)

/*

:: Refto a = Refto String


reftoEditForm :: !(!Init,!Mode!,!Lifespan) !(InIDataId (Refto a,a)) !*HSt -> (Form (Refto a),Form a,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
reftoEditForm  (initv,modev,lifespan) (init,formid) hst 
# (ref,hst)		= mkEditForm (init, reuseFormId formid (Refto filename)) hst
# (Refto nname)	= ref.value
# (file,hst) 	= mkEditForm (initv,{ formid 	& id 		= nname
												, ival		= a
												, lifespan 	= lifespan
												, mode		= modev 
									}) hst
= 	( {ref  & form = [[toHtml ("File Name: " )] <=> ref.form]}
	, {file & form = [[toHtml (nname +++ ": ")] <=> file.form]}
	, hst
	)
where
	(Refto filename,a) = formid.ival


// special editors on top of this ...

reftoVertListForm :: !(!Init,!Mode!,!Lifespan) !(InIDataId [(Refto a,a)]) !*HSt -> (Form [Refto a],Form [a],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
reftoVertListForm options  (init,formid) hst
# storeid		= reuseFormId formid [Refto name\\ (Refto name,_) <- formid.ival]
# (store,hst)	= mkStoreForm (init,storeid) id hst												// store for refto list	
# (twof,hst) 	= maplSt (reftoEditForm` options) [(init,subnFormId formid (name <+++ i) (Refto name,a)) 
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
	reftoEditForm` options all hst
	# (fref,ffile,hst) = reftoEditForm options all hst
	= ((fref,ffile),hst)

reftoListFormButs :: !Int !(!Init,!Mode!,!Lifespan) !(InIDataId [(Refto a,a)]) !*HSt -> (Form [Refto a],Form [a],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
reftoListFormButs nbuts (initv,mode,lifespan) (init,formid) hst
# indexId		= {subsFormId formid "idx" 0 & mode = Edit}
# (index,hst)	= mkEditForm (init,indexId) hst
# (rlist,vlist,hst)= reftoVertListForm (initv,mode,lifespan) (init,formid) hst
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

# (_,_,hst)		= if paste.changed (reftoEditForm (Set,mode,Persistent)
						(Init,nFormId "ttt" (rlist.value!!(paste.value 0),elemstore.value )) hst)
						(undef,undef,hst)
						
# (rlist,vlist,hst)= reftoVertListForm (initv,mode,lifespan) (setID formid [(reftoa,createDefault) \\ reftoa <- newlist]) hst
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



*/