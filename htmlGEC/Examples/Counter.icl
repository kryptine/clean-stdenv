module Counter

import StdEnv
import StdHtml

//from buttonGEC import :: Button (..), :: UpDown (..)


derive gUpd   (,,)
derive gHGEC  (,), (,,)
derive gPrint (,), (,,)
derive gParse (,), (,,)
derive gHpr   (,), (,,)


initCounter = (0,upButton,downButton)
upButton 	= CHButton defsize "Up"
downButton 	= CHButton defsize "Down"

Start world  = doHtml MyPage initCounter world

MyPage counter=:(cnt,up,down)
	= Head 
		[ Hd_Title "Counter Example"
		] 
		[ H1 "Counter Example"
		, Br  
		, showClean (updCounter counter)
		, showClean (updCounter counter)
		, showClean up, showClean cnt
		]
where
	updCounter (n,CHPressed,_)  = (n+1,upButton,downButton)
	updCounter (n,_,CHPressed) 	= (n-1,upButton,downButton)
	updCounter else 			= else


