module balanceTree

import StdEnv
import StdHtml

import tree

derive gForm []
derive gUpd []

Start world  = doHtmlServer MyPage3  world
//Start world  = doHtml MyPage  world

MyPage hst
# (balancedtree,hst) = mkSelfForm (nFormId "BalancedTree") 
							(Init (fromListToBalTree [0])) balanceTree hst
=	mkHtml "Balanced Tree"
	[ H1 [] "Balanced Tree"
	, BodyTag balancedtree.form
	]  hst

MyPage2 hst
# (sortedlist,hst) = mkSelfForm (nFormId "SortedList") 
							(Init [0]) sort hst
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
	
	