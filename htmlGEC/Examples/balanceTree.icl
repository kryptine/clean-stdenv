module balanceTree

import StdEnv
import StdHtml

derive gHGEC  	Tree
derive gPrint 	Tree
derive gParse 	Tree
derive gUpd 	Tree
derive gHpr		Tree

import tree

Start world  = doHtml MyPage  world

MyPage hst
# (_,(treeGEC,hst)) = mkSelfHGEC "tree" balanceTree (Node Leaf 1 Leaf) hst
= (Head 
	[Hd_Title "Self Balancing Tree"
	] 
	[ H1 "Self Balancing Tree"
	, Br, Br
	, treeGEC
	], hst)
