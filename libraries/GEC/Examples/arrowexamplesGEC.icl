module arrowexamplesGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import basicAGEC, calcAGEC, dynamicAGEC
import GecArrow

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	exampleb`
 	world  

:: BalancedTree a  	
				= BNode .(BalancedNode a)
				| BEmpty 
:: BalancedNode a =
				{ bigger :: .BalancedTree a
				, bvalue  :: a 
				, smaller:: .BalancedTree a
				} 

derive gGEC   	BalancedTree, BalancedNode


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

examplea = StartCircuit mycircuit [1..5]  // connecting two editors 
where
	mycircuit = gecEdit "list" >>> arr toBalTree >>> gecDisplay "balancedtree"

exampleb = StartCircuit mycircuit2 (toBalTree [1..5]) // self balancing tree
where
	mycircuit  = gecFix (gecEdit "self balancing tree" >>> arr BalanceTree)
	mycircuit2 = gecFix (arr BalanceTree >>> gecEdit "self balancing tree")

exampleb` = StartCircuit mycircuit ([1..5])
where
	mycircuit = gecEdit "list" >>> arr toBalTree >>> gecFixViaLoop`ViaGecLoopViaLoop` (gecEdit "self balancing tree" >>> arr BalanceTree)

examplec = StartCircuit mycircuit [1..5]
where
	mycircuit     = evenCircuit &&& oddCircuit >>> balancedtree
	evenCircuit   = takeEven  @>> gecEdit "part1"
	oddCircuit    = takeOdd   @>> gecEdit "part2"
	balancedtree  = convert   @>> gecEdit "balanced tree"

	takeEven list = [e \\ e <- list | isEven e]
	takeOdd list  = [e \\ e <- list | isOdd e]
	convert (f,s) = (s ++ f) 