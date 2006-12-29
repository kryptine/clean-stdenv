module sum

import StdEnv, StdHtml

// choose one of the following variants

//Start world = doHtmlServer (singleUserTask count) world
Start world = doHtmlServer (singleUserTask count2) world
//Start world = doHtmlServer (multiUserTask 3 [] countMU) world
//Start world = doHtmlServer (multiUserTask 3 [setTaskAttribute Persistent] countMU) world
//Start world = doHtmlServer countIData world

// Change the type to any type one can apply addition to

initVal :: Int
initVal = createDefault

// single user: give first value, then give second, then show sum

count tst
# (v1,tst) 	= STask "Set" initVal tst
# (v2,tst) 	= STask "Set" initVal tst
# tst		= returnF [Txt "+",Hr []] tst
= returnTask (v1 + v2) tst

// single user, monadic style

count2 tst
= 	(			STask "Set" initVal 
	=>> \v1 -> 	STask "Set" initVal
	=>> \v2 ->	[Txt "+",Hr []] 
				!>>	returnTask (v1 + v2)
	) tst

// multi user variant

countMU tst
# (v1,tst) 	= ((1,"number") @: STask "Set" initVal) tst	// user 1
# (v2,tst) 	= ((2,"number") @: STask "Set" initVal) tst	// user 2
# tst		= returnF [Txt "+",Hr []] tst				// user 0
= returnTask (v1 + v2) tst								// user 0

// multi user variant, monadic atyle

count2MU tst
= 	(			(1,"number") @: STask "Set" initVal
	=>> \v1 -> 	(2,"number") @: STask "Set" initVal 
	=>> \v2 ->	[Txt "+",Hr []] 
				!>> returnTask (v1 + v2) 
	) tst


// iData variant to show what iTasks do for you

countIData hst
# ((d1,v1),t1,hst) = myEdit "v1" 0 hst
# ((d2,v2),t2,hst) = myEdit "v2" 0 hst
=	mkHtml "Solution using iData without iTasks"
	[ t1
	, if d1 t2 EmptyBody
	, if d2 (BodyTag [Txt "+",Hr [],toHtml (v1 + v2)]) EmptyBody
	]  hst
where
	myEdit name val hst
	# (idata,hst)	= mkEditForm (Init, nFormId name (HideMode False,val) <@ Submit) hst
	# nval			= snd idata.value
	# done			= idata.changed || fst idata.value == HideMode True
	| done
		# (idata,hst) = mkEditForm (Set,  nFormId name (HideMode done,nval) <@ Display) hst	
		= ((True,nval),BodyTag idata.form,hst)	
	= ((False,nval),BodyTag idata.form,hst)	
	
	