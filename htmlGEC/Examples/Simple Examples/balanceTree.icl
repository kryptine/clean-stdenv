module balanceTree

import StdEnv
import StdHtml

import tree

derive gForm []
derive gUpd []

Start world  = doHtmlServer MyPage  world
//Start world  = doHtml MyPage  world

MyPage hst
# (balancedtree,hst) = mkSelfForm (nFormId "BalancedTree") 
							(fromListToBalTree [0]) balanceTree hst
=	mkHtml "Balanced Tree"
	[ H1 [] "Balanced Tree"
	, BodyTag balancedtree.form
	]  hst

MyPage2 hst
# (sortedlist,hst) = mkSelfForm (nFormId "SortedList") 
							[0] sort hst
=	mkHtml "Sorted List"
	[ H1 [] "Sorted List"
	, BodyTag sortedlist.form
	, toHtml (reverse sortedlist.value)
	]  hst

MyPage3 hst
# (treef,hst) = startCircuit mycircuit (Node Leaf 1 Leaf) hst
= mkHtml "Self Balancing Tree"
	[ H1 [] "Self Balancing Tree"
	, toBody treef
	] hst
where
	mycircuit = feedback (edit (nFormId "tree")) (arr  balanceTree)
	
MyPage4 hst
# (sortedlist,hst) = startCircuit mycircuit [1] hst
# (test,hst) = mkApplyEditForm (nFormId "test") 1 1 hst
# (test2,hst) = mkApplyEditForm (nFormId "test") 1 1 hst
= mkHtml "Self Balancing Tree"
	[ H1 [] "Self Balancing Tree"
	, toBody sortedlist
	, toHtml sortedlist.changed
	, toHtml sortedlist.value
	, toBody test
	, toBody test
	, toHtml test.changed
	, toHtml test2.changed
	] hst
where
	mycircuit = feedback (edit (nFormId "list")) (arr  sort)
	
