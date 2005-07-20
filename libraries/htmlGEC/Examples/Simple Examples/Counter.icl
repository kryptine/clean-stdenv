module Counter

import StdEnv
import StdHtml

//Start world  = doHtml MyPage world
Start world  = doHtmlServer MyPage world

MyPage hst
# (counter0,hst) = counterForm (nFormId "counter0") 0 hst
# (counter1,hst) = counterForm (nFormId "counter1") 0 hst
= mkHtml "Counter Example"
	[ H1 [] "Counter Example"
	, BodyTag counter0.form
	, toBody counter0
	, toBody counter1
	, toHtml (counter0.value + counter1.value)
	] hst



