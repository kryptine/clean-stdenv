module smallexamplesGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import basicAGEC, calcAGEC
import GecArrow

// TO TEST JUST REPLACE THE EXAMPLE NAME IN THE START RULE WITH ANY OF THE EXAMPLES BELOW
// ALL EXAMPLES HAVE TO BE OF FORM pst -> pst

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	example_rec1
 	world  

:: T = C1 (P Int (AGEC Int))
	 | C2 (P Int (AGEC Real))
	 
derive gGEC T

testX` pst = let (cGEC,pst`) = createNGEC "self" Interactive (C1 (P -10 (counterAGEC 0))) (\_ t -> cGEC.gecSetValue NoUpdate (test t)) pst
             in  pst`
where
	 test (C1 (P x igec))
	 	| isEven i	= C1 (P x (counterAGEC i))
	 	| otherwise	= C2 (P x (counterAGEC (toReal (^^ igec) + 5.0)))
	 where
	 	i	= ^^igec
	 test (C2 (P x rgec))
	 	| isEven i	= C2 (P x (counterAGEC ((^^ rgec) + 5.0)))
	 	| otherwise	= C1 (P x (counterAGEC i))
	 where
	 	i	= toInt (^^rgec)

:: T` = C1` (P Int  Bool)
	  | C2` (P Real Bool)
:: P a b = P a b
derive gGEC T`,P

testXX` pst = let (cGEC,pst`) = createNGEC "self" Interactive (C1` (P 0 False)) (\_ t -> cGEC.gecSetValue NoUpdate (test t)) pst
             in  pst`
//testX` = CGEC (selfGEC "self" test) (C1` 0)
where
	 test (C1` (P i b)) = C2` (P (toReal i) b)
	 test (C2` (P r b)) = C1` (P (toInt  r) b)

example_l1		= 	startCircuit (edit		"Simple List Editor")					[1] 
example_l2  	=	startCircuit (edit	"list" >>@ sum >>> edit "sum") 				[1..5]  									
example_l4  	=	startCircuit (edit	"list" >>@ (\(a,b) ->  a + b) >>> edit "Sum List Elements") ([1..5],[5,4..1])			
example_l6  	=	startCircuit (feedback (edit "Sorted List" >>@ sort))			[5,4..1] 									
example_t1		=	startCircuit (edit		"Tree")									(Node Leaf 1 Leaf)									
example_t2		=	startCircuit (edit 	"Tree")									(toBalancedTree	 [1,5,2,8,3,9])
example_t3		=	startCircuit (edit 	"Tree")			         				(toTree [8,34,2,-4,0,31]) 					
example_t4		=	startCircuit (edit "Tree" >>@ toBalancedTree >>> edit "balanced tree") 		[1,5,2,8,3,9]
example_t7		=	startCircuit (feedback (edit "self Balancing Tree" >>@ toBalancedTree o toList))			(toBalancedTree [1,5,2,8,3,9])
example_tr1		=	startCircuit (feedback (edit "Balanced Tree with Records" >>@ toBalTree o BalTreetoList))	(toBalTree [1,5,2,8,3,9])
example_rose	=	startCircuit (edit "Rose") (Rose 1 []) 
example_rec1	=	startCircuit (feedback (	edit "Exchange Pounds to Euros"  
											>>@ toEuro 
											>>> edit  "Exchange Euros to Pounds" 
											>>@ toPounds
											)
								 ) {pounds=0.0}   
where
	toPounds {euros} 	= {pounds = euros / exchangerate}
	toEuro {pounds} 	= {euros = pounds * exchangerate}
	exchangerate 		= 1.4
example_cnt1 	= startCircuit (edit "Counter") (counterAGEC 0)
example_cnt2 	= startCircuit (feedback (edit "Counter" >>@ updateDoubleCounters)) {cntr1=counterAGEC 0,cntr2=intcalcAGEC 0,sum=0}
where
	updateDoubleCounters cntrs = {cntrs & sum = ^^ cntrs.cntr1 + ^^ cntrs.cntr2}
example_cnt3 	= startCircuit (feedback (edit "Counter" >>@ updateTwoIntCounters)) (intcalcAGEC 0 <|> counterAGEC 0 <|> 0)
where
	updateTwoIntCounters (i1 <|> i2 <|> sum) = (i1 <|> i2 <|> ^^ i1 + ^^ i2)
example_cnt4 	= startCircuit (feedback (edit "Counter" >>@ updateTwoIntCounters)) (idAGEC 0 <|> idAGEC 0 <|> counterAGEC 0)
where
	updateTwoIntCounters (i1 <|> i2 <|> sum) = (i1 <|> i2 <|> sum ^= (^^ i1 + ^^ i2))
example_cnt5 	= startCircuit mycounter 0
example_cnt6 	= startCircuit mydoublecounter 0 
example_cnt7	= startCircuit (edit "kwadrateer") kwadrateer
where
	kwadrateer = applyAGEC (\x -> x + 1) (applyAGEC (\x -> x * x) (idAGEC 0))
example_cnt9	= startCircuit (edit "counter") initcounter 
where
	initcounter = {gec = mydoublecounter, inout = (0,0)}
example_cnt10 	= startCircuit (feedback (edit "Counter" >>@ updateCounter)) (Tuple2 0 Neutraal)
//example_const 	= startCircuit (feedback ( (\x -> 4 &&& edit "constant") >>@ (\(x,y) -> x + y) )) 23 

:: Tree a  	= Node (Tree a) a (Tree a) 
				| Leaf
:: BalancedTree a  	
				= BNode .(BalancedNode a)
				| BEmpty 
:: BalancedNode a =
				{ bigger :: .BalancedTree a
				, bvalue  :: a 
				, smaller:: .BalancedTree a
				} 
:: Rose a 		= Rose a  .[Rose a]
:: Pounds 		= {pounds :: Real}
:: Euros  		= {euros :: Real}
:: MyDoubleCounter = {cntrs1::GecComb Int Int, cntrs2::GecComb Int Int ,csum::Int}
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

mycounter = Mybimap toCounter fromCounter updateCounter (edit "scounter")
where
	toCounter i = (i, Neutral)
	fromCounter (i, _) = i
	updateCounter (n,UpPressed) 	= (n+one,Neutral)
	updateCounter (n,DownPressed) 	= (n-one,Neutral)
	updateCounter any 		 	 	= any

	Mybimap fab fba fbb gecbb = fab @>> feedback (gecbb >>@ fbb) >>@ fba

mydoublecounter = ((mycounter &&& mycounter) >>@ (\(x, y) -> x + y) >>> display "tcounter" )

updateCounter (Tuple2 n GoUp) 	= Tuple2 (n+1) Neutraal
updateCounter (Tuple2 n GoDown) = Tuple2 (n-1) Neutraal
updateCounter any 		 = any
