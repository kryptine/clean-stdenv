module Counter

import StdEnv
import StdHtml

derive gUpd   (,,)
derive gPrint (,,)
derive gParse (,,)
derive gHpr   (,,)


initCounter = (0,down,up)
down 	= CHButton defsize "-"
up 		= CHButton defsize "+"

Start world  = doHtml MyPage world

MyPage hst
# (_,(counterGEC,hst)) = counterHGEC "counter" 0 hst
= (Head 
	[ Hd_Title "Counter Example"
	] 
	[ H1 "Counter Example"
	, Br  
	, counterGEC
	],hst)

counterHGEC :: String Int HSt -> (Int,(Body,HSt))
counterHGEC name i hst 
# (nc, result) = mkHGEC name updCounter (toCounter i) hst
= (fromCounter nc, result)
where
	toCounter n = (n,down,up)

	fromCounter (n,_,_) = n

	updCounter (n,CHPressed,_)  = (n-1,down,up)
	updCounter (n,_,CHPressed) 	= (n+1,down,up)
	updCounter else 			= else

	up 		= CHButton defsize "+"
	down	= CHButton defsize "-"


