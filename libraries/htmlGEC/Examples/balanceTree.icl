module balanceTree

import StdEnv
import StdHtml

import tree

Start world  = doHtml MyPage  world

MyPage hst
# ((_,treeform),hst) = startCircuit mycircuit (Node Leaf 1 Leaf) hst
= (Head 
	[Hd_Title "Self Balancing Tree"
	] 
	[ H1 "Self Balancing Tree"
	, Br, Br
	, Body treeform
	, Br
	, traceHtmlInput
	], hst)
where
	mycircuit = feedback (edit "tree") (arr  balanceTree)
