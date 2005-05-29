module listTobalanceTree

import StdEnv
import StdHtml

derive gUpd   []
derive gForm  []

import tree

Start world  = doHtml MyPage world


MyPage hst
# (mycircuitf,hst) = startCircuit mycircuit [1,5,2] hst
# [list,tree:_] = mycircuitf.body
= mkHtml "Balancing Tree From List"
	[ Txt "List :", Br
	, list
	, Txt "Balanced Tree :", Br
	, tree
	] hst
where
	mycircuit :: GecCircuit [Int] (Tree Int)
	mycircuit = edit (nFormId "list") >>> arr fromListToBalTree >>> display (nFormId "tree")

	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags
