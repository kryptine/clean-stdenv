module balanceTree

import StdEnv
import StdHtml

import tree

Start world  = doHtml MyPage  world

MyPage hst
# (treef,hst) = startCircuit mycircuit (Node Leaf 1 Leaf) hst
= mkHtml "Self Balancing Tree"
	[ H1 [] "Self Balancing Tree"
	, toBody treef
	] hst
where
	mycircuit = feedback (edit (nFormId "tree")) (arr  balanceTree)


