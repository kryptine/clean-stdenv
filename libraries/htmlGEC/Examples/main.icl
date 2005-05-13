module main

import StdEnv
import StdHtml

derive gForm  	Tree, []
derive gUpd 	Tree, []
derive gPrint 	Tree
derive gParse 	Tree
derive gHpr 	Tree

:: Tree a = Node (Tree a) a (Tree a) | Leaf


Start world  = doHtml MyPage2 world

MyPage2  hst
# (next,hst)	= mkEditForm "next" Edit 0 hst
# (add,hst) 	= ListFuncBut False "add" Edit [(LButton 80 "add", \xs -> next.value + xs)] hst
# (basket,hst) 	= mkStoreForm "basket" add.value 0 hst
# (basket1,hst) = mkStoreForm "basket" add.value 0 hst
# (basket2,hst) = mkStoreForm "basket" add.value 0 hst
# (basket3,hst) = mkStoreForm "basket" add.value 0 hst
= mkHtml "Main Test Program"
	[ H1 [] "My Test"
	, toBody basket, toHtml basket.changed
	, Br
	, toBody basket1
	, Br
	, toBody basket2
	, Br
	, toBody basket3
	, Br
	, toBody next, toHtml next.changed, toHtml add.changed
	, Br
	, toBody add
//	, traceHtmlInput
	, Br

	] hst
where
	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags


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
