module smallexamplesGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import StdGecComb

// TO TEST JUST REPLACE THE EXAMPLE NAME IN THE START RULE WITH ANY OF THE EXAMPLES BELOW
// ALL EXAMPLES HAVE TO BE OF FORM pst -> pst

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	example_l1
 	world  

example_l1		= 	CGEC (mkGEC		"Simple List Editor")					[1] 
example_l2  	=	CGEC (applyGEC	"Sum of List" sum) 						[1..5]  									
example_l4  	=	CGEC (applyGEC	"Sum List Elements" (\(a,b) ->  a + b)) ([1..5],[5,4..1])			
example_l5  	=	CGEC (applyGEC2	"Sum of List A and List B" (+)) 		([1..5],[5,4..1]) 							
example_l6  	=	CGEC (selfGEC 	"Sorted List" 			sort)			[5,4..1] 									
example_t1		=	CGEC (mkGEC		"Tree")									(Node Leaf 1 Leaf)									
example_t2		=	CGEC (mkGEC 	"Tree")									(toBalancedTree	 [1,5,2,8,3,9])
example_t3		=	CGEC (mkGEC 	"Tree")			         				(toTree [8,34,2,-4,0,31]) 					
example_t4		=	CGEC (applyGEC 	"Balanced Tree"	 toBalancedTree) 		[1,5,2,8,3,9]
example_t5		=	CGEC (apply2GEC "List to Balanced Tree"(toBalancedTree o toList) toTree) 	[1,5,2,8,3,9]	
example_t6		=	CGEC (mutualGEC "BalancedTree to List" toList toBalancedTree) 				[1..5]   
example_t7		=	CGEC (selfGEC 	"self Balancing Tree" (toBalancedTree o toList))			(toBalancedTree [1,5,2,8,3,9])
example_tr1		=	CGEC (selfGEC 	"Balanced Tree with Records" (toBalTree o BalTreetoList))	(toBalTree [1,5,2,8,3,9])
example_rose	=	CGEC (mkGEC "Rose") (Rose 1 []) 
example_rec1	=	CGEC (mutualGEC  "Exchange Euros to Pounds"  toEuro toPounds) {euros=0.0}   
where
	toPounds {euros} 	= {pounds = euros / exchangerate}
	toEuro {pounds} 	= {euros = pounds * exchangerate}
	exchangerate 		= 1.4
example_cnt1 	= CGEC (mkGEC "Counter") (counterGEC 0)
example_cnt2 	= CGEC (selfGEC "Counter" updateDoubleCounters) {cntr1=counterGEC 0,cntr2=intcalcGEC 0,sum=0}
where
	updateDoubleCounters cntrs = {cntrs & sum = ^^ cntrs.cntr1 + ^^ cntrs.cntr2}
example_cnt3 	= CGEC (selfGEC "Counter" updateTwoIntCounters) (intcalcGEC 0 <|> counterGEC 0 <|> 0)
where
	updateTwoIntCounters (i1 <|> i2 <|> sum) = (i1 <|> i2 <|> ^^ i1 + ^^ i2)
example_cnt4 	= CGEC (selfGEC "Counter" updateTwoIntCounters) (idGEC 0 <|> idGEC 0 <|> counterGEC 0)
where
	updateTwoIntCounters (i1 <|> i2 <|> sum) = (i1 <|> i2 <|> sum ^= (^^ i1 + ^^ i2))
example_cnt5 	= CGEC mycounter 0
example_cnt6 	= CGEC mydoublecounter 0 
example_cnt7	= CGEC (gecEdit "kwadrateer") kwadrateer
where
	kwadrateer = applyAGEC (\x -> x + 1) (applyAGEC (\x -> x * x) (idGEC 0))
example_cnt8 	= CGEC kwadrateer 0
where
	kwadrateer = gecloop (f  @| gecEdit "res")
	
	f (0,y) = (100,0)
	f (x,y) = (x - 1,y + 1)
example_cnt9	= CGEC (gecEdit "counter") initcounter 
where
	initcounter = {gec = mydoublecounter, inout = (0,0)}
example_cnt10 	= CGEC (selfGEC "Counter" updateCounter) (Tuple2 0 Neutraal)
example_const 	= CGEC (%| ( (gecConst 4 |>| gecEdit "constant") |@ (\(x,y) -> x + y) )) 23 

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

mycounter = Mybimap toCounter fromCounter updateCounter (gecEdit "scounter")
where
	toCounter i = (i, Neutral)
	fromCounter (i, _) = i
	updateCounter (n,UpPressed) 	= (n+one,Neutral)
	updateCounter (n,DownPressed) 	= (n-one,Neutral)
	updateCounter any 		 	 	= any

	Mybimap fab fba fbb gecbb = fab @| %| (gecbb |@ fbb) |@ fba

mydoublecounter = ((mycounter |>| mycounter) |@ (\(x, y) -> x + y) |&| gecDisplay "scounter" )

updateCounter (Tuple2 n GoUp) 	= Tuple2 (n+1) Neutraal
updateCounter (Tuple2 n GoDown) = Tuple2 (n-1) Neutraal
updateCounter any 		 = any
