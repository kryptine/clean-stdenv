module main

import StdEnv
import StdHtml

derive gForm  	Tree, []
derive gUpd 	Tree, []
derive gPrint 	Tree
derive gParse 	Tree
derive gHpr 	Tree

derive gForm NFormState//, NState

//:: Tree a = Node (Tree a) a (Tree a) | Leaf



Start world  = doHtml MyPage2 world

MyPage2  hst
# (j,hst) 		= mkEditForm "i1" Edit 0 hst
# (i,hst) = mkStoreForm "store" (\s -> s + j.value) 0 hst
# (i1,hst) = mkStoreForm "store" (\s -> i.value + 1) 0 hst
# (i2,hst) = mkStoreForm "store" (\s -> i1.value + 1) 0 hst
# (i3,hst) = mkStoreForm "store" (\s -> i2.value + 1) 0 hst
# (i4,hst) = mkStoreForm "store" (\s -> i3.value + 1) 0 hst
= mkHtml "Main Test Program"
	[ H1 [] "My Test"

	, traceHtmlInput
	, Br

	, toBody j,toBody i
	, Br
	, toHtml (i.value,i.changed)
	, toHtml (j.value,j.changed)

	, toBody i1
	, toHtml (i1.value,i1.changed)
	, Br
	, toBody i2
	, toHtml (i2.value,i2.changed)
	, Br
	, toBody i3
	, toHtml (i3.value,i3.changed)
	, Br
	, toBody i4
	, toHtml (i4.value,i4.changed)
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
