module listTobalanceTree

import StdEnv
import StdHtml

derive gUpd   Tree, []
derive gHGEC  Tree, []
derive gPrint Tree
derive gParse Tree

import tree

Start world  = doHtml MyPage world

MyPage hst
# (list,(listGEC,hst)) = mkHGEC "list" id [1] hst
# (_,   (treeGEC,hst)) = mkHGEC "tree" id (fromListToBalTree list) hst
= (Head 
	[Hd_Title "List to Balance Tree"
	] 
	[ H1 "List to Balance Tree"
	, Br , T "This is the list: ", Br, Br
	, listGEC
	, Br , T "This is the resulting balanced Tree: ", Br, Br
	, treeGEC
	],hst)

