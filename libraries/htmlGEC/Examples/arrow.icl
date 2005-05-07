module arrow

import StdEnv
import StdHtml

derive gUpd   []
derive gForm  []

import tree

Start world  = doHtml MyPage world

MyPage hst
# (mycircuitf,hst) = startCircuit mycircuit [1] hst
# [list,tree:_] = mycircuitf.body
= mkHtml "List to Balance Tree"
	[ H1 [] "List to Balance Tree"
	, Txt "This is the list :", Br
	, list
	, Txt "This is the resulting balanced tree :", Br
	, tree
	] hst
where
	mycircuit :: GecCircuit [Int] (Tree Int)
	mycircuit = edit "list" >>> arr fromListToBalTree >>> display "tree"

	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags
