module test

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

Start world  = doHtml MyPage (0,0,0) world

MyPage (i1,i2,i3)
	= Head 
		[ Hd_Title "Testing"
		] 
		[ H1 "Counter Example"
		, counter1
		, counter2
		, showClean (res1,res2,res1 + res2)
		, traceHtmlInput
		]
where
	(counter1,res1) = HCounter "first"  i1
	(counter2,res2) = HCounter "second" i2


doHtml2 :: (a -> Body, a -> Body) a -> Body | gHpr{|*|} a & gUpd{|*|}  a  & gParse{|*|} a 
doHtml2 (before,after) nv = undef


HCounter :: String Int -> (Body,Int)
HCounter name init
= (Body [	T "Lets count this counter named ", T name, T " :", Br
		,   showClean result
  		], fst3 result)
where
	result = updCounter (toCounter  init)

	updCounter (n,CHPressed,_)  = (n+1,upButton,downButton)
	updCounter (n,_,CHPressed) 	= (n-1,upButton,downButton)
	updCounter else 			= else

	toCounter i = (i,upButton,downButton)

