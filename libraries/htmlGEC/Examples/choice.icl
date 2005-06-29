module choice

import StdEnv
import StdHtml

//Start world  = doHtml choice world
Start world  = doHtmlServer choice world

choice hst
# (selection,hst) 	= FuncMenu -2 (nFormId "rinus") functions hst
# (fun,selected) 	= selection.value
# (radio,hst) 		= ListFuncRadio selected (nFormId "aap") mycbfs hst
# (rfun,j) 			= radio.value
= mkHtml "Choosing is difficult"
	[ H1 [] "testing radio buttons"
	, Br
	, BodyTag selection.form
	, Br
	, BodyTag radio.form
	, Br
	, Txt ("Current value is :" +++ toString selected)
	, Br
	, Txt ("Function returns :" +++ (rfun (toString j)))
	, Br
	, toHtml (selection.changed,radio.changed)
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
