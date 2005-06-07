module Counter

import StdEnv
import StdHtml

Start world  = doHtml MyPage world

MyPage hst
# (counter,hst) = counterForm (nFormId "counter") 0 hst
= mkHtml "Counter Example"
	[ H1 [] "Counter Example"
	, Br  
	, toBody counter
	] hst



