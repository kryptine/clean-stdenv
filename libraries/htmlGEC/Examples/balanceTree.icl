module balanceTree

import StdEnv
import StdHtml

derive gHGEC  	Tree
derive gPrint 	Tree
derive gParse 	Tree
derive gUpd 	Tree
derive gHpr		Tree

import tree

Start world  = doHtml MyPage (Node Leaf 1 Leaf) world

MyPage tree
	 = Head 
		[Hd_Title "Balance Tree"
		] 
		[ H1 "Balance Tree"
		, Br
		, showClean (balanceTree tree)
		]
