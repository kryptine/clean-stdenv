module main

import StdEnv
import StdHtml

derive gHGEC  	Tree, []
derive gUpd 	Tree, []
derive gPrint 	Tree
derive gParse 	Tree
derive gHpr 	Tree

:: Tree a = Node (Tree a) a (Tree a) | Leaf

Start world  = doHtml MyPage world

MyPage  hst
# (_,(treeGEC,hst)) = mkEditHGEC "tree" HEdit [Node Leaf 1 Leaf] hst
= (Head 
		[Hd_Title "Main Test Program"
		] 
		[ H1 "My Test"
		, Br, T "Here we show an editor for a tree data structure :", Br, Br
		, treeGEC
		],hst)
