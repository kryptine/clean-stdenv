module choice

import StdEnv
import StdHtml


Start world  = doHtml choice world


choice hst
# (((_,sel),menubody),hst) = FuncMenu -2 "rinus" Edit functions hst
//# ((nmenu,menubody),hst) = mkEditForm "test" HEdit (PullDown (2,defpixel) (1,menu)) hst
# (((cbf,j),body),hst) = ListFuncRadio sel "aap" Edit mycbfs hst
= mkHtml "Choosing is difficult"
	[ H1 [] "testing radio buttons"
	, Br
	, menubody
	, Br
	, BodyTag body
	, Br
	, Txt ("Current value is :" +++ toString sel)
	, Br
	, Txt ("Function returns :" +++ (cbf (toString j)))
	, Br
//	, traceHtmlInput
	,Br
	]  hst
where

	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags

	mycbf i s = toString i +++ " " +++ s

	functions = [("aap",cbf),("noot",cbf),("mies",cbf)]	
	where
		cbf n = n
	mycbfs = [mycbf,mycbf,mycbf,mycbf,mycbf,mycbf,mycbf,mycbf]
	
	menu = ["jan","piet","klaas"]

	index (PullDown _(i,_)) = i
