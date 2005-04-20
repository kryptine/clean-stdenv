module listTobalanceTree

import StdEnv
import StdHtml

derive gUpd   []
derive gForm  []

import tree

Start world  = doHtml MyPage world


MyPage hst
# ((_,[list,tree:_]),hst) = startCircuit mycircuit [1,5,2] hst
= mkHtml "Balancing Tree From List"
	[ Txt "List :", Br
	, list
	, Txt "Balanced Tree :", Br
	, tree
	] hst
where
	mycircuit :: GecCircuit [Int] (Tree Int)
	mycircuit = edit "list" >>> arr fromListToBalTree >>> display "tree"

	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags
