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
# ((_,[list,tree:_]),hst) = startCircuit mycircuit [1] hst
= (Head 
	[Hd_Title "List to Balance Tree"
	] 
	[ H1 "List to Balance Tree"
	, T "This is the list :", Br
	, list
	, T "This is the resulting balanced tree :", Br
	, tree
	],hst)
where
	mycircuit :: GecCircuit [Int] (Tree Int)
	mycircuit = edit "list" >>> arr fromListToBalTree >>> display "tree"
