implementation module htmlRefFormlib

// Database storages and reference types that allow destructive sharing of model data types.
// Both can be guarded by consistency checking functions.
// Both handle version management of the stored and shared data respectively.
// (c) MJP 2006

import StdHtml

derive gForm []; derive gUpd []
derive gForm 	Maybe
derive gUpd 	Maybe
derive gPrint 	Maybe
derive gParse 	Maybe

:: Ref2 a		= Ref2 String

instance == (Ref2 a) where (==) (Ref2 file1) (Ref2 file2) = file1 == file2


invokeRefEditor :: !((InIDataId b) *HSt -> (Form d,*HSt)) (InIDataId b) !*HSt -> (Form b,!*HSt)
invokeRefEditor editor (init,formid) hst
# (idata,hst)				= editor (init,formid) hst
= ({idata & value = formid.ival},hst)

//	iData for destructively shared model data:

universalRefEditor :: !Lifespan !(InIDataId (Ref2 a)) !(a -> Judgement) !*HSt -> (Form a,!*HSt) | iData a
universalRefEditor lifespan (init,formid=:{ival=Ref2 filename}) invariant hst
| filename == ""			= mkEditForm (Init,xtFormId "ure_TEMP" createDefault) hst
# (dbf,hst)					= myDatabase Init filename (0,createDefault) hst				// create / read out current value in file file
# (dbversion,dbvalue)		= dbf.value														// version number and value stored in database
# (versionf,hst)			= myVersion Init filename dbversion hst 						// create / read out version number expected by this application
# version					= versionf.value												// current version number assumed in this application
| init == Init && isMember formid.mode [Display,NoForm]										// we only want to read, no version conflict
	= myEditor Init filename dbvalue hst													// synchronize with latest value 
| dbversion > version																		// we have a version conflict and want to write
	# (_,hst)				= ExceptionStore ((+) (Just (filename, "Ref Your screen data is out of date; I have retrieved the latest data."))) hst
																							// Raise exception
	# (_,hst)				= myVersion Set filename dbversion hst							// synchronize with new version
	= myEditor Set filename dbvalue hst														// return current version stored in database 
# (valuef,hst)				= myEditor Init filename dbvalue hst							// editor is in sync; create / read out current value 
# exception					= invariant valuef.value										// check invariants															// check invariants
| isJust exception																			// we want to write, but invariants don't hold
	# (_,hst)				= ExceptionStore ((+) exception) hst 							// report them 
	= (valuef,hst)																			// return wrong value such that it can be improved
# (versionf,hst)			= myVersion  Set filename (dbversion + 1) hst					// increment version number
# (_,hst)					= myDatabase Set filename (dbversion + 1,valuef.value) hst		// update database file
= ({valuef & changed = True},hst)
where
	myDatabase init filename cntvalue hst													// write the database
							= mkEditForm (init, if (lifespan == Persistent) xpFormId xdbFormId "" cntvalue <@ filename) hst
	myVersion  init filename cnt hst														// track version number
							= mkEditForm (init,reuseFormId formid cnt <@ ("vrs_r_" +++ filename) <@ NoForm) hst
	myEditor   init filename value hst														// copy of database information
							= mkShowHideForm (init,reuseFormId formid value <@ "copy_r_" +++ filename) hst


// editor for persistent information:

universalDB :: !(!Init,!Lifespan,!a,!String) !(String a -> Judgement) !*HSt -> (a,!*HSt) | iData a
universalDB (init,lifespan,value,filename) invariant hst
# (dbf,hst)					= myDatabase Init (0,value) hst									// create / read out database file
# (dbversion,dbvalue)		= dbf.value														// version number and value stored in database
# (versionf,hst)			= myVersion Init dbversion hst 									// create / read out version number expected by this application
# version					= versionf.value												// current version number assumed in this application
| init == Init																				// we only want to read, no version conflict
	# (_,hst)				= myVersion Set dbversion hst 									// synchronize version number and
	= (dbvalue,hst)																			// return current value stored in database
| dbversion <> version																		// we want to write and have a version conflict
	# (_,hst)				= myVersion Set dbversion hst									// synchronize with new version
	# (_,hst)				= ExceptionStore ((+) (Just (filename,"Your screen data is out of date; I have retrieved the latest data."))) hst
																							// Raise exception
	= (dbvalue,hst)																			// return current version stored in database 
# exception					= invariant filename value										// no version conflict, check invariants															// check invariants
| isJust exception																			// we want to write, but invariants don't hold
	# (_,hst)				= ExceptionStore ((+) exception) hst 							// report them 
	= (value,hst)																			// return disapproved value such that it can be improved
# (versionf,hst)			= myVersion  Set (dbversion + 1) hst 							// increment version number
# (_,hst)					= myDatabase Set (versionf.value,value) hst						// update database file
= (value,hst)
where
	myDatabase init cntvalue hst 															// read the database
							= mkEditForm (init,if (lifespan == Persistent) pFormId dbFormId filename cntvalue <@ NoForm) hst
	myVersion  init cnt hst	= mkEditForm (init,xtFormId ("vrs_db_" +++ filename) cnt) hst	// to remember version number

// Exception handling 

Ok :: Judgement
Ok							= Nothing

noException :: Judgement -> Bool
noException judgement		= isNothing judgement

yesException :: Judgement -> Bool
yesException judgement		= not (noException judgement)

instance + Judgement where
//	(+) (Just (r1,j1)) (Just (r2,j2)) 	= (Just ((r1 +++ " " +++ r2),(j1 +++ " " +++ j2))) //for debugging
	(+) (Just j1) _ 		= Just j1
	(+) _  (Just j2) 		= Just j2
	(+) _ _ 				= Nothing

ExceptionStore :: (Judgement -> Judgement) !*HSt -> (Judgement,!*HSt)
ExceptionStore judge hst 
# (judgef,hst)				= mkStoreForm (Init,nFormId "handle_exception" Ok <@ NoForm <@ Temp) judge hst
= (judgef.value,hst)
