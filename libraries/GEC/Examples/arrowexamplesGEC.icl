module arrowexamplesGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import basicAGEC, calcAGEC, dynamicAGEC
import GecArrow
//from StdGecComb import selfGEC, :: CGEC

// TO TEST JUST REPLACE THE EXAMPLE NAME IN THE START RULE WITH ANY OF THE EXAMPLES BELOW
// ALL EXAMPLES HAVE TO BE OF FORM pst -> pst

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	example1
 	world  

:: BalancedTree a  	
				= BNode .(BalancedNode a)
				| BEmpty 
:: BalancedNode a =
				{ bigger :: .BalancedTree a
				, bvalue  :: a 
				, smaller:: .BalancedTree a
				} 

derive gGEC   	BalancedTree, BalancedNode//, OperatorTest

BalanceTree :: (BalancedTree a) -> (BalancedTree a) | Ord a
BalanceTree tree = toBalTree (BalTreetoList tree)

BalTreetoList :: (BalancedTree a) -> [a]
BalTreetoList BEmpty = []
BalTreetoList (BNode record) = (BalTreetoList record.bigger) ++ [record.bvalue] ++ (BalTreetoList record.smaller)	

toBalTree :: [a] -> BalancedTree a | Ord a
toBalTree list = Balance (sort list)
where
	Balance [] = BEmpty
	Balance [x] = BNode {bigger=BEmpty,bvalue=x,smaller=BEmpty}
	Balance xs
		= case splitAt (length xs/2) xs of
			(a,[b:bs]) = BNode {bigger=Balance bs,bvalue=b,smaller=Balance a}
			(as,[])    = BNode {bigger=BEmpty,bvalue=hd (reverse as),smaller=Balance (reverse (tl (reverse as)))} 

example1 = startCircuit mycircuit2 [1..5]  // connecting two editors 
where
//	mycircuit  = edit "list" <<@ toBalTree >>> edit "balanced tree"
	mycircuit2 = edit "list" >>> arr toBalTree >>> edit "balanced tree" // alternative definition

derive generate BalancedTree, BalancedNode

/*
example2 = startCircuit mycircuit3 (toBalTree [1..5]) // self balancing tree
where
	mycircuit  = feedback (edit "self balancing tree" >>> arr BalanceTree)
	mycircuit2 = feedback (edit "self balancing tree" <<@ BalanceTree) // alternative
	mycircuit3 = feedback (arr BalanceTree >>> edit "self balancing tree") // alternative
	mycircuit4 = feedback (BalanceTree @>> edit "self balancing tree") // alternative

example3 = startCircuit mycircuit [1..5] // merge
where
	mycircuit     = evenCircuit &&& oddCircuit >>> balancedtree
	evenCircuit   = takeEven  @>> edit "part1"
	oddCircuit    = takeOdd   @>> edit "part2"
	balancedtree  = convert   @>> edit "balanced tree"

	takeEven list = [e \\ e <- list | isEven e]
	takeOdd list  = [e \\ e <- list | isOdd e]
	convert (f,s) = toBalTree (s ++ f) 


:: OperatorTest a b c d = { arg1 :: a
			 	      ,  arg2 :: b
			 	      ,  operator   :: c
			 	      ,  result  :: d
			 	      }
example5 = startCircuit mycircuit (toGEC initOperatorTest)  // buttons + function
where
	mycircuit   = feedback  ((toGEC o applyFun o fromGEC) @>> edit "test operator")

//	mycircuit`  = selfGEC (toGEC o applyFun o fromGEC) initOperatorTest			// version without combinators 


	initOperatorTest :: OperatorTest Int Int (Int -> Int -> Int) Int 
	initOperatorTest = { arg1 = 0, arg2 = 0, operator = (+), result = 0 }
	 
	applyFun oper = {oper & result = oper.operator oper.arg1 oper.arg2}
	
	toGEC oper = { arg1 	= counterAGEC oper.arg1
				 , arg2 	= counterAGEC oper.arg2
				 , operator = dynamicAGEC2 oper.operator
				 , result	= modeAGEC (Display oper.result)
				 }
	
	fromGEC oper = { arg1 	= ^^ oper.arg1
				 , arg2 	= ^^ oper.arg2
				 , operator = ^^ oper.operator
				 , result	= ^^ oper.result
				 }

// old stuf --

/*
example_rec1	=	CGEC (mutualGEC  "Exchange Euros to Pounds"  toEuro toPounds)
{euros=0.0}   
where
	toPounds {euros} 	= {pounds = euros / exchangerate}
	toEuro {pounds} 	= {euros = pounds * exchangerate}
	exchangerate 		= 1.4
example_cnt1 	= CGEC (mkGEC "Counter") (counterAGEC 0)
example_cnt2 	= CGEC (selfGEC "Counter" updateDoubleCounters) {cntr1=counterAGEC
0,cntr2=intcalcAGEC 0,sum=0}
where
	updateDoubleCounters cntrs = {cntrs & sum = ^^ cntrs.cntr1 + ^^ cntrs.cntr2}
example_cnt3 	= CGEC (selfGEC "Counter" updateTwoIntCounters) (intcalcAGEC 0 <|>
counterAGEC 0 <|> 0)
where
	updateTwoIntCounters (i1 <|> i2 <|> sum) = (i1 <|> i2 <|> ^^ i1 + ^^ i2)
example_cnt4 	= CGEC (selfGEC "Counter" updateTwoIntCounters) (idAGEC 0 <|>
idAGEC 0 <|> counterAGEC 0)
where
	updateTwoIntCounters (i1 <|> i2 <|> sum) = (i1 <|> i2 <|> sum ^= (^^ i1 + ^^
i2))
example_cnt5 	= CGEC mycounter 0
example_cnt6 	= CGEC mydoublecounter 0 
example_cnt7	= CGEC (edit "kwadrateer") kwadrateer
where
	kwadrateer = applyAGEC (\x -> x + 1) (applyAGEC (\x -> x * x) (idAGEC 0))
example_cnt8 	= CGEC kwadrateer 0
where
	kwadrateer = gecloop (f  @| edit "res")
	
	f (0,y) = (100,0)
	f (x,y) = (x - 1,y + 1)
example_cnt9	= CGEC (edit "counter") initcounter 
where
	initcounter = {gec = mydoublecounter, inout = (0,0)}
example_cnt10 	= CGEC (selfGEC "Counter" updateCounter) (Tuple2 0 Neutraal)
example_const 	= CGEC (%| ( (gecConst 4 |>| edit "constant") |@ (\(x,y) -> x
+ y) )) 23 

:: Tree a  	= Node (Tree a) a (Tree a) 
				| Leaf
:: Rose a 		= Rose a  .[Rose a]
:: Pounds 		= {pounds :: Real}
:: Euros  		= {euros :: Real}
:: MyDoubleCounter = {cntrs1::GecComb Int Int, cntrs2::GecComb Int Int
,csum::Int}
:: DoubleCounter = {cntr1::AGEC Int, cntr2::AGEC Int,sum::Int}
:: MyCounter 	= Tuple2 Int Up_Down
:: Up_Down 		= GoUp | GoDown | Neutraal

derive gGEC   	Tree, BalancedTree, BalancedNode, Rose, 
				Pounds, Euros, 
				MyDoubleCounter, DoubleCounter, MyCounter, Up_Down 

instance + [a] | + a
where
	(+) [a:as] [b:bs] = [a+b:as+bs]
	(+) _ _ = []

toTree ::[a] -> Tree a | Ord a
toTree list = inserts list Leaf
where
	inserts [] tree = tree
	inserts [a:as] tree = inserts as (insert a tree)

	insert a (Node b e o)
		| a <= e	=	Node b e (insert a o)
		| a > e		=	Node (insert a b) e o
	insert a Leaf = Node Leaf a Leaf

toList :: (Tree a) -> [a]
toList Leaf = []
toList (Node b a o) = (toList o) ++ [a] ++ (toList b)	

toBalancedTree :: [a] -> Tree a | Ord a
toBalancedTree list = Balance (sort list)
where
	Balance [] = Leaf
	Balance [x] = Node Leaf x Leaf
	Balance xs
		= case splitAt (length xs/2) xs of
			(a,[b:bs]) = Node (Balance bs) b (Balance a)
			(as,[]) = Node Leaf (hd (reverse as)) (Balance (reverse (tl (reverse as))))


mycounter = Mybimap toCounter fromCounter updateCounter (edit "scounter")
where
	toCounter i = (i, Neutral)
	fromCounter (i, _) = i
	updateCounter (n,UpPressed) 	= (n+one,Neutral)
	updateCounter (n,DownPressed) 	= (n-one,Neutral)
	updateCounter any 		 	 	= any

	Mybimap fab fba fbb gecbb = fab @| %| (gecbb |@ fbb) |@ fba

mydoublecounter = ((mycounter |>| mycounter) |@ (\(x, y) -> x + y) |&|
gecDisplay "scounter" )

updateCounter (Tuple2 n GoUp) 	= Tuple2 (n+1) Neutraal
updateCounter (Tuple2 n GoDown) = Tuple2 (n-1) Neutraal
updateCounter any 		 = any
*/
*/