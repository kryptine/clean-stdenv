module listTobalanceTree

import StdEnv
import StdHtml

derive gUpd   Tree, []
derive gHGEC  Tree, []
derive gPrint Tree
derive gParse Tree

import tree

Start world  = doHtml MyPage [1] world

MyPage list
	= Head 
		[Hd_Title "List to Balance Tree"
		] 
		[ H1 "List to Balance Tree"
		, Br , T "This is the list: ", Br
		, showClean list
		, Br , T "This is the resulting balanced Tree: ", Br
		, showClean (fromListToBalTree list)
		]


