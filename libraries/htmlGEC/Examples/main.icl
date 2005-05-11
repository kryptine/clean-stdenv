module main

import StdEnv
import StdHtml

derive gForm  	Tree, []
derive gUpd 	Tree, []
derive gPrint 	Tree
derive gParse 	Tree
derive gHpr 	Tree

:: Tree a = Node (Tree a) a (Tree a) | Leaf

Start world  = doHtml MyPage world

MyPage  hst
# (treef,hst) 	= mkEditForm "tree" Edit (Node Leaf 1 Leaf) hst
# (buttonf,hst) = mkEditForm "button" Display (LButton defpixel "oeps") hst
# (inp,hst)		= mkEditForm "xxx" Edit (TI 10 23, TR 10 4.5, TS 30 "hallo, hoe is het") hst
= mkHtml "Main Test Program"
	[ H1 [] "My Test"
	, Br, Txt "Here we show an editor for a tree data structure :", Br, Br
	, toBody treef
	, Br
	, toBody buttonf
	, Br, traceHtmlInput
	, Br
	, toBody inp
	, toHtml inp.value
	] hst
where
	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags
