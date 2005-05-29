module main

import StdEnv
import StdHtml
import Directory 

derive gForm  	Tree, []
derive gUpd 	Tree, []
derive gPrint 	Tree
derive gParse 	Tree
derive gHpr 	Tree

:: Tree a = Node (Tree a) a (Tree a) | Leaf

Start world  = doHtml MyPage3 world

MyPage3  hst
# (nval,hst)	= mkEditForm (pFormId "edit") "marie" Edit hst
# (nval2,hst)	= mkEditForm (pFormId "edit2") 8 Edit hst
# (nval3,str,hst) = readState "edit" hst
= mkHtml "Main Test Program"
	[ H1 [] "My Test"
	, toBody nval,toBody nval2
	, Br
	, toHtml (case nval3 of
				Just n -> n
				else -> "not parsed")
	, Br
	, Txt str
	, Br
	, toHtml (case parseString str of
				Just n -> n
				else -> "not parsed")
	, Br
	, traceHtmlInput
	, Br

	] hst
where
	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags
	

	readState filename env
	#(_,env) = case getFileInfo mydir env of
				((DoesntExist,fileinfo),env) -> createDirectory mydir env
				(_,env) -> (NoDirError,env)
	# (ok,file,env)	= fopen (MyDir +++ "/" +++ filename) FReadData env
	| not ok 		= (Nothing,"file cannot be opened",env)
	# (string,file)	= freads file big
	| not ok 		= (Nothing,"file cannot be read",env)
	# (ok,env)		= fclose file env
	# string 		= { s \\ s <-: string%(1,size string - 2) | s <> '\\' }
//	# string 		= string%(1,size string - 2)
	= (parseString  string,string,env) 
	where
		big = 100000
		mydir = RelativePath [PathDown MyDir]
		


stroref f hst = mkStoreForm (nFormId "basket") 0 f hst

MyPage2  hst
# (next,hst)	= mkEditForm (nFormId "next") 0 Edit hst
# (add,hst) 	= ListFuncBut False (nFormId "add") Edit [(LButton 80 "add", \xs -> next.value + xs)] hst
# (basket,hst) 	= stroref add.value hst
# (dec,hst) 	= ListFuncBut False (nFormId "less") Edit [(LButton 80 "decr", \xs -> xs - next.value)] hst
# (basket1,hst) = stroref dec.value hst
= mkHtml "Main Test Program"
	[ H1 [] "My Test"
	, toBody basket, toHtml basket.changed
	, Br
	, toBody basket1
	, Br
	, toBody next, toHtml next.changed, toHtml add.changed
	, Br
	, toBody add, toBody dec
//	, traceHtmlInput
	, Br

	] hst
where
	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags


MyPage  hst
# (treef,hst) 	= mkEditForm (nFormId "tree") (Node Leaf 1 Leaf) Edit  hst
# (buttonf,hst) = mkEditForm (nFormId "button") (LButton defpixel "oeps") Display  hst
# (inp,hst)		= mkEditForm (nFormId "xxx") (TI 10 23, TR 10 4.5, TS 30 "hallo, hoe is het") Edit hst
= mkHtml "Main Test Program"
	[ H1 [] "My Test"
	, Br, Txt "Here we show an editor for a tree data structure :", Br, Br
	, toBody treef
	, Br
	, toBody buttonf
	, Br, traceHtmlInput
	, Br
	, toBody inp
	, toHtml inp.value
	] hst
where
	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags

:: Test = E.a: {dyn::Dynamic,v::a,p::a->String}

printTest :: Test -> String
printTest {v,p} = p v

initTest :: a  -> Test | TC a &  gPrint{|*|} a 
initTest nv = {dyn = dynamic nv,v = nv,p = printToString}

storeTest :: a Test -> Test | TC a &  gPrint{|*|} a 
storeTest nv {dyn = (v::a^)} = {dyn = dynamic nv,v = nv,p = printToString}
storeTest nv old = old

fetchTest :: Test -> Maybe a | TC a
fetchTest {dyn = (v::a^)} = Just v
fetchTest _ = Nothing

myTest
# test = initTest [1]
# test = storeTest [1..10] test
# mylist = fetchTest test
# test = storeTest (map (\x -> x * x * 1) (fromJust mylist)) test
= printTest test
