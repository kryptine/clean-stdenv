module listTobalanceTree

import StdEnv, StdStrictLists
import StdHtml

derive gUpd   []
derive gForm  []

import tree


//Start world  = doHtml MyPage world
//Start world  = doHtmlServer MyPage world
Start world = Start3 world
myListId = nFormId "list" []
myTreeId = nFormId "tree" Leaf

import dynamic_string

//Start3 :: *World -> Dynamic
Start3 world 
# inout					= [|]
# (gerda,world)			= openGerda "bla" world	
# nworld 				= { worldC = world, inout = inout, gerda = gerda}	
# nworld				= writeState (MyDir Internal) "mylist"  mydynamic nworld
# (string,nworld)		= readState (MyDir Internal) "mylist" nworld
=  string_to_dynamic` string
where
	string_to_dynamic` :: {#Char} -> Dynamic	// just to make a unique copy as requested by string_to_dynamic
	string_to_dynamic` s = string_to_dynamic {s` \\ s` <-: s}

	mydynamic = dynamic_to_string (dynamic [1..10])

Start2 :: *World -> Dynamic
Start2 world 
# (ok,file,world)	= fopen "bla.txt" FWriteData world
| not ok 			= dynamic 0
# file				= fwrites (dynamic_to_string (dynamic [1..10])) file
# (ok,world)		= fclose file world
# (ok,file,world)	= fopen "bla.txt" FReadData world
# (string,file)		= freads file 10000
=  string_to_dynamic string

MyPage hst
# (iList,hst) = mkEditForm (Init, pDFormId "mylist" initVal) hst
= mkHtml "Balancing Tree From List"
		[ Txt "Converting a list:", Br, Br
          , BodyTag iList.form
//          , BodyTag iTree.form
          ] hst

initVal :: [Int]
initVal = [1..10]

MyPageArr hst
# (mycircuitf,hst) = startCircuit mycircuit [1,5,2] hst
= mkHtml "Balancing Tree From List"
	[ Txt "List to Balanced Tree", Br
	, BodyTag mycircuitf.form
	] hst
where
	mycircuit :: GecCircuit [Int] (Tree Int)
	mycircuit = edit myListId >>> arr fromListToBalTree >>> display myTreeId

