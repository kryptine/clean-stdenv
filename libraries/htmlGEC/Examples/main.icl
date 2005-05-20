module main

import StdEnv
import StdHtml

derive gForm  	Tree, []
derive gUpd 	Tree, []
derive gPrint 	Tree
derive gParse 	Tree
derive gHpr 	Tree

:: Tree a = Node (Tree a) a (Tree a) | Leaf


Start world  = doHtml MyPage4 world

MyPage4  hst
# (next,hst)	= mkEditForm "next" Edit 0 hst
# (next1,hst)	= browseButtons False next.value 3 66 10 "aap" Edit hst
= mkHtml "Main Test Program"
	[ H1 [] "My Test"
	, toBody next, toHtml next.changed
	, Br
	, toBody next1, toHtml next1.changed
	
	, toHtml next1.value
//	, traceHtmlInput
	, Br

	] hst
where
	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags


 



MyPage3  hst
# (next,hst)	= mkEditForm "next" Edit 0 hst
# (next1,hst)	= mkEditForm "next" Edit 5 hst
= mkHtml "Main Test Program"
	[ H1 [] "My Test"
	, toBody next, toHtml next.changed
	, Br
	, toBody next1, toHtml next1.changed
//	, traceHtmlInput
	, Br

	] hst
where
	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags

stroref f hst = mkStoreForm "basket" f 0 hst

MyPage2  hst
# (next,hst)	= mkEditForm "next" Edit 0 hst
# (add,hst) 	= ListFuncBut False "add" Edit [(LButton 80 "add", \xs -> next.value + xs)] hst
# (basket,hst) 	= stroref add.value hst
# (dec,hst) 	= ListFuncBut False "less" Edit [(LButton 80 "decr", \xs -> xs - next.value)] hst
# (basket1,hst) = stroref dec.value hst
= mkHtml "Main Test Program"
	[ H1 [] "My Test"
	, toBody basket, toHtml basket.changed
	, Br
	, toBody basket1
	, Br
	, toBody next, toHtml next.changed, toHtml add.changed
	, Br
	, toBody add, toBody dec
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
