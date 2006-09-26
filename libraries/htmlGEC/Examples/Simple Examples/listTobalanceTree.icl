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
# (iList,hst) = mkEditForm (Init, nFormId "mylist" initVal) hst
# (iTree,hst) = mkEditForm (Set, ndFormId "mytree" (fromListToBalTree iList.value)) hst
= mkHtml "Balancing Tree From List"
		[ Txt "Converting a list:", Br, Br
          , BodyTag iList.form
       	  , Txt "to a balanced tree:", Br, Br
          , BodyTag iTree.form
          ] hst

initVal :: [Int]
initVal = createDefault

MyPageArr hst
# (mycircuitf,hst) = startCircuit mycircuit [1,5,2] hst
= mkHtml "Balancing Tree From List"
	[ Txt "List to Balanced Tree", Br
	, BodyTag mycircuitf.form
	] hst
where
	mycircuit :: GecCircuit [Int] (Tree Int)
	mycircuit = edit myListId >>> arr fromListToBalTree >>> display myTreeId

