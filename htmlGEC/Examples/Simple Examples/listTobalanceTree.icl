module listTobalanceTree

import StdEnv
import StdHtml

derive gUpd   []
derive gForm  []

import tree

//Start world  = doHtml MyPage world
Start world  = doHtmlServer MyPage world

myListId = nFormId "list" []
myTreeId = nFormId "tree" Leaf

MyPage hst
# (mycircuitf,hst) = startCircuit mycircuit [1,5,2] hst
= mkHtml "Balancing Tree From List"
	[ Txt "List to Balanced Tree", Br
	, BodyTag mycircuitf.form
	] hst
where
	mycircuit :: GecCircuit [Int] (Tree Int)
	mycircuit = edit myListId >>> arr fromListToBalTree >>> display myTreeId
