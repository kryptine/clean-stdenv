module main

import StdEnv
import StdHtml

derive gHGEC  	Tree, []
derive gUpd 	Tree, []
derive gPrint 	Tree
derive gParse 	Tree
derive gHpr 	Tree

:: Tree a = Node (Tree a) a (Tree a) | Leaf

Start world  = doHtml MyPage [Node Leaf 1 Leaf] world

MyPage  mydata
	= Head 
		[Hd_Title "Main Test Program"
		] 
		[ H1 "My Test"
		, Br
		, showClean mydata
		]

