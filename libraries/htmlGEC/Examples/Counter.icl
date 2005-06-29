module Counter

import StdEnv
import StdHtml

//Start world  = doHtml MyPage world
Start world  = doHtmlServer MyPage world

MyPage hst
# (counter,hst) = counterForm (nFormId "counter") 0 hst
= mkHtml "Counter Example"
	[ H1 [] "Counter Example"
	, toBody counter
	] hst



