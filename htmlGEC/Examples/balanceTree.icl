module balanceTree

import StdEnv
import StdHtml

import tree

Start world  = doHtml MyPage  world

MyPage hst
# (treef,hst) = startCircuit mycircuit (Node Leaf 1 Leaf) hst
= mkHtml "Self Balancing Tree"
	[ H1 [] "Self Balancing Tree"
	, Br, Br
	, toBody treef
	, Br
	] hst
where
	mycircuit = feedback (edit (nFormId "tree")) (arr  balanceTree)

	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags

