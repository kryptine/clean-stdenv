module Counter

import StdEnv
import StdHtml

Start world  = doHtml MyPage world

MyPage hst
# ((_,counterGEC),hst) = counterHGEC "counter" Edit 0 hst
= mkHtml "Counter Example"
	[ H1 [] "Counter Example"
	, Br  
	, counterGEC
	] hst
where
	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags



