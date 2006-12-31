module sum

import StdEnv, StdHtml

// choose one of the following variants

Start world = doHtmlServer (singleUserTask count) world
//Start world = doHtmlServer (singleUserTask count2) world
//Start world = doHtmlServer (multiUserTask 3 [] countMU) world
//Start world = doHtmlServer (multiUserTask 3 [setTaskAttribute Persistent] countMU) world
//Start world = doHtmlServer countIData world

// single user, give first value, then give second, then show sum
// monadic style

count :: Task Int
count
= 				STask "Set" 0 
	=>> \v1 -> 	STask "Set" 0
	=>> \v2 ->	[Txt "+",Hr []] 
				!>>	returnTask (v1 + v2)
// multi user variant, monadic atyle

countMU :: Task Int
countMU 
= 				(1,"number") @: STask "Set" 0
	=>> \v1 -> 	(2,"number") @: STask "Set" 0 
	=>> \v2 ->	[Txt "+",Hr []] 
				!>> returnTask (v1 + v2) 

// single user, normal Clean style 

count2 :: TSt -> (Int,TSt)
count2 tst
# (v1,tst) 	= STask "Set" 0 tst
# (v2,tst) 	= STask "Set" 0 tst
# tst		= returnF [Txt "+",Hr []] tst
= returnTask (v1 + v2) tst

// multi user variant, normal Clean style

count2MU :: TSt -> (Int,TSt)
count2MU tst
# (v1,tst) 	= ((1,"number") @: STask "Set" 0) tst
# (v2,tst) 	= ((2,"number") @: STask "Set" 0) tst
# tst		= returnF [Txt "+",Hr []] tst
= returnTask (v1 + v2) tst

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
	
	