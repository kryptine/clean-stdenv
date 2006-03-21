module balanceTree

import StdEnv
import StdHtml

import tree

derive gForm []
derive gUpd []

derive gForm 	Record
derive gUpd 	Record
derive gParse 	Record
derive gPrint 	Record


Start world  = doHtmlServer MyPage4  world
//Start world  = doHtml MyPage  world


:: Record = {name :: String, address :: String, zipcode :: Int}

myrecord :: [Record]
myrecord = []

MyPage4 hst
# (myrecord,hst) = mkEditForm (Init,nFormId "bla" myrecord)  hst
=	mkHtml "Myrecord"
	[ H1 [] "myrecord"
	, BodyTag myrecord.form
	]  hst



myBalancedTree 	= nFormId "BalancedTree" 	(fromListToBalTree [0])
mySortedList	= nFormId "SortedList"  	[0]

MyPage hst
# (balancedtree,hst) = mkSelfForm (initID myBalancedTree) balanceTree hst
=	mkHtml "Balanced Tree"
	[ H1 [] "Balanced Tree"
	, BodyTag balancedtree.form
	]  hst

MyPage2 hst
# (sortedlist,hst) = mkSelfForm (initID mySortedList) sort hst
=	mkHtml "Sorted List"
	[ H1 [] "Sorted List"
	, BodyTag sortedlist.form
	, toHtml (reverse sortedlist.value)
	]  hst

MyPage3 hst
# (treef,hst) = startCircuit mycircuit (Node Leaf 112 Leaf) hst
= mkHtml "Self Balancing Tree"
	[ H1 [] "Self Balancing Tree"
	, toBody treef
	] hst
where
	mycircuit = feedback (edit myBalancedTree) (arr  balanceTree)
	
	