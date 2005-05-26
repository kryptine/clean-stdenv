module main

import StdEnv
import StdHtml

derive gForm  	Tree, []
derive gUpd 	Tree, []
derive gPrint 	Tree
derive gParse 	Tree
derive gHpr 	Tree

:: Tree a = Node (Tree a) a (Tree a) | Leaf


Start world  = myTest
Start world  = doHtml MyPage4 world

MyPage4  hst
# (next,hst)	= mkEditForm "next" 0 Edit hst
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
# (next,hst)	= mkEditForm "next" 0 Edit hst
# (next1,hst)	= mkEditForm "next" 0 Edit hst
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

stroref f hst = mkStoreForm "basket" 0 f hst

MyPage2  hst
# (next,hst)	= mkEditForm "next" 0 Edit hst
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
# (treef,hst) 	= mkEditForm "tree" (Node Leaf 1 Leaf) Edit  hst
# (buttonf,hst) = mkEditForm "button" (LButton defpixel "oeps") Display  hst
# (inp,hst)		= mkEditForm "xxx" (TI 10 23, TR 10 4.5, TS 30 "hallo, hoe is het") Edit hst
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

:: Test = E.a: {dyn::Dynamic,v::a,p::a->String}

printTest :: Test -> String
printTest {v,p} = p v

initTest :: a  -> Test | TC a &  gPrint{|*|} a 
initTest nv = {dyn = dynamic nv,v = nv,p = printToString}

storeTest :: a Test -> Test | TC a &  gPrint{|*|} a 
storeTest nv {dyn = (v::a^)} = {dyn = dynamic nv,v = nv,p = printToString}
storeTest nv old = old

fetchTest :: Test -> Maybe a | TC a
fetchTest {dyn = (v::a^)} = Just v
fetchTest _ = Nothing

myTest
# test = initTest [1]
# test = storeTest [1..10] test
# mylist = fetchTest test
# test = storeTest (map (\x -> x * x * 1) (fromJust mylist)) test
= printTest test
