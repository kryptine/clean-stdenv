module choice

import StdEnv
import StdHtml


Start world  = doHtml choice world


choice hst
# ((nval,ibody),hst) = mkEditHGEC "rinus" HEdit 0 hst
# (((cbf,j),body),hst) = ListFuncRadio -5 "aap" HEdit mycbfs hst
= mkHtml "Choosing is difficult"
	[ H1 [] "testing radio buttons"
	, Br
	, ibody
	, Br
	, BodyTag body
	, Br
	, Txt ("Current value is :" +++ toString j)
	, Br
	, Txt ("Function returns :" +++ (cbf (toString j)))
	, Br,Br
	]  hst
where

	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags

	mycbf i s = toString i +++ " " +++ s
	
	mycbfs = [mycbf,mycbf,mycbf,mycbf,mycbf,mycbf,mycbf,mycbf] 