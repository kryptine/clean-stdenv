module demoGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import GenMap

:: VT t
	=	VTEither (VT t) (VT t)
	|	VTPair (VT t) (VT t)
	|	VTLeaf t

generic valueT t :: VT t
genMap :== gMap {|*->*|}
derive gMap VT
derive bimap VT

valueT{|UNIT|}
	=	VTLeaf UNIT
valueT{|Int|}
	=	VTLeaf 0
valueT{|EITHER|} l r
	=	VTEither (genMap LEFT l) (genMap RIGHT r)
valueT{|CONS|} v
	=	genMap CONS v
valueT{|OBJECT|} v
	=	genMap OBJECT v
valueT{|FIELD|} v
	=	genMap FIELD v
valueT{|PAIR|} l r
	=	undef // mergeT l r



// valueT{|PAIR|} v1 v2
//	=	VTList [PAIR a b \\ a <- v1, b <- v2]



generic values t :: [t]

values{|UNIT|}
	=	[UNIT]
values{|PAIR|} v1 v2
	=	[PAIR a b \\ a <- v1, b <- v2]
values{|EITHER|} l r
	=	genMap LEFT l ++ genMap RIGHT r
values{|CONS|} v
	=	genMap CONS v
values{|OBJECT|} v
	=	genMap OBJECT v
values{|FIELD|} v
	=	genMap FIELD v
values{|Int|}
	=	[1, 2]

:: TV = CV1 TV2 // | CV2 | CV3 | CV4 TV2
:: TV2 = CV21 | CV22

derive valueT TV
derive valueT TV2
/*
Start :: VT TV
Start
	=	valueT{|*|}
*/
gMap{| (->) |} _ _ _
	=	undef

x1 = gMap{|*|}
x2 = gMap{|*->*|}
x3 = gMap{|*->*->*|}
// x4 = gMap{|(*->*)->*|}

:: GenStructure
	=	GenStructureUNIT
	|	GenStructureEITHER (EITHER GenStructure GenStructure)
	|	GenStructurePAIR (PAIR GenStructure GenStructure)
	|	GenStructureOBJECT {#Char} (OBJECT GenStructure)
	|	GenStructureCONS {#Char} (CONS GenStructure)
	|	GenStructureFIELD {#Char} (FIELD GenStructure)
	|	GenStructureINT Int

generic genStructure t :: t -> GenStructure

genStructure{|UNIT|} UNIT
	=	GenStructureUNIT
genStructure{|EITHER|} sa sb (LEFT a)
	=	GenStructureEITHER (LEFT (sa a))
genStructure{|EITHER|} sa sb (RIGHT b)
	=	GenStructureEITHER (RIGHT (sb b))
genStructure{|PAIR|} sa sb (PAIR a b)
	=	GenStructurePAIR (PAIR (sa a) (sb b))
genStructure{|OBJECT of {gtd_name}|} sa (OBJECT a)
	=	GenStructureOBJECT gtd_name (OBJECT (sa a))
genStructure{|CONS of {gcd_name}|} sa (CONS a)
	=	GenStructureCONS gcd_name (CONS (sa a))
genStructure{|FIELD of {gfd_name}|} sa (FIELD a)
	=	GenStructureFIELD gfd_name (FIELD (sa a))
genStructure{|Int|} int
	=	GenStructureINT int

generic testG t :: t -> (t, z) | zero z

testG{|UNIT|} UNIT
	=	(UNIT, zero)
testG{|EITHER|} sa _ (LEFT a)
	=	(LEFT (fst (sa a)), zero)
testG{|EITHER|} _ sb (RIGHT b)
	=	(RIGHT (fst (sb b)), zero)
testG{|PAIR|} sa sb (PAIR a b)
	=	(PAIR (fst (sa a)) (fst (sb b)), zero)
testG{|OBJECT|} sa (OBJECT a)
	=	(OBJECT (fst (sa a)), zero)
testG{|CONS|} sa (CONS a)
	=	(CONS (fst (sa a)), zero)
testG{|FIELD|} sa (FIELD a)
	=	(FIELD (fst (sa a)), zero)
testG{|Int|} int
	=	(int, zero)

/*
// recursion (not yet used)
:: REC a = REC a 			// recursion mark

// for constructor information
:: OBJECT a = OBJECT a		// object marking
:: CONS a = CONS a 			// constructor marking
*/

derive genStructure L
derive testG L

structure :: t -> GenStructure | genStructure{|*|} t
structure t
	=	genStructure{|*|} t

strange1 = genStructure{|*->*|}
strange2 = genStructure{|*->*->*|}
// strange3 = genShow{|(*->*)->*|}
/*
t :: t -> (t, z) |  testG{|*|} t & zero z
t a
	=	testG{|*|} a
*/

Start0 :: (L Int, Int)
Start0
	=	testG{|*|} (C 1 N)

:: L a = N | C a (L a)
:: R = {f::Int}

// TO TEST JUST REPLACE THE EXAMPLE NAME IN THE START RULE WITH ANY OF THE EXAMPLES BELOW

Start world = Start2 world

Start2 :: *World -> *World
Start2 world = 
	startIO MDI Void initGUI [ProcessClose closeProcess] world
	where
	    initGUI :: (PSt .ps) -> PSt .ps | bimap{|*|} ps
	    initGUI pSt	= example_l5 pSt

testRWS
	=	("RWS", C 1 N ) // [1]) // ("RWS", Product (left {field1="f1",field2='c'} 0.0, right [True] 17) "s")

left :: a b -> Coproduct a b
left a b
	=	InLeft a

right :: a b -> Coproduct a b
right a b
	=	InRight b


derive gGEC Product, Coproduct, Record, L
:: Product a b = Product a b
:: Coproduct a b = InLeft a | InRight b
:: Record a b = {field1 :: a, field2 :: b}

// Examples on standard lists

derive gGEC []

example_l1	=	mk_GEC 		("Simple List Editor",[1]) 
example_l2  =	apply_GEC 	("Sum of List",sum)  					("List",[1..5]) 									
example_l3  =	apply_GEC 	("Sum of List",sum)  					("Sum of List",[1..5]) 									
example_l4  =	apply_GEC 	("Sum List Elements",\(a,b) ->  a + b) 	("Sum List Elements",([1..5],[5,4..1])) 			
example_l5  =	apply_GEC2 	("Sum of List A and List B",(+))		("List A",[1..5]) 	("List B",[5,4..1]) 							
example_l6  =	self_GEC 	sort 									("Sorted List",[5,4..1]) 									
example_l7	=	self_GEC 	updsheet								("spreadsheet",mksheet inittable) 
where
		updsheet (table <-> _ <|>
		          _ <-> _ )			= mksheet (^^ table)
		mksheet table				= tableGEC table <-> vertlistGEC rowsum <|>
									  horlistGEC colsum <-> sum rowsum
		where
			rowsum					= map sum table
			colsum 					= map sum transpose
			transpose				= [[table!!i!!j \\ i <- [0..(length table)    - 1]]
												    \\ j <- [0..length (table!!0) - 1]
									  ]
		inittable	  				= [map ((+) i) [1..5] \\ i <- [0,5..25]]	


instance + [a] | + a
where
	(+) [a:as] [b:bs] = [a+b:as+bs]
	(+) _ _ = []

// Examples on Trees

derive gGEC   Tree 

::	Tree a  	= Node (Tree a) a (Tree a) 
				| Leaf

example_t1	=	mk_GEC		("Tree",Node Leaf 1 Leaf)									
example_t2	=	mk_GEC 		("Tree",toBalancedTree	 [1,5,2,8,3,9])
example_t3	=	mk_GEC 		("Tree",toTree [8,34,2,-4,0,31]) 					
example_t4	=	apply_GEC 	("Balanced Tree",toBalancedTree)		 	("List",[1,5,2,8,3,9]) 
example_t5	=	apply2_GEC 	("List to Balanced Tree",toBalancedTree o toList)	("List to Tree",toTree) ("List",[1,5,2,8,3,9])	
example_t6	=	mutual_GEC  [1..5] ("BalancedTree to List",toList) ("List to Balanced Tree",toBalancedTree)   

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

// List to balanced tree, with balanced tree defined as record

derive gGEC   BalancedTree, BalancedNode

::	BalancedTree a  	
				= BNode .(BalancedNode a)
				| BEmpty 
				
::  BalancedNode a =
				{ bigger :: .BalancedTree a
				, bvalue  :: a 
				, smaller:: .BalancedTree a
				} 
example_tr1	=	self_GEC 	(toBalTree o BalTreetoList)	("Balanced Tree with Records",(toBalTree [1,5,2,8,3,9])) 

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

// Example of a simple record

derive gGEC   MyAdminstration, ZipCode 


example_db = mk_GEC ("demo",listGEC True [MyRecord])

::	MyAdminstration 
				= 	{ name		::String
					, street	::String
					, zipcode	::String
					, number	::Int
					, age		::Int
					}
::	ZipCode 	= Number Char Char

MyRecord = 	{ name = "rinus plasmeijer"
			, street="knollenberg"
			, number=17
			, zipcode="6585WJ"
			, age = 50
			}

example_r1	=	mk_GEC ("My Database",	[MyRecord]) 					
example_r2	=	pred_GEC checkrecord ("My Database",[MyRecord])					
where
	checkrecord rs = and (map check rs)
	where
		check r = r.age >= 0 && r.age <= 110 && legal r.zipcode
		legal zipcode = size zipcode >= 6 	&& isDigit zipcode.[0] // 8, string includes CR + LF
											&& isDigit zipcode.[1]
											&& isDigit zipcode.[2]
											&& isDigit zipcode.[3]
											&& isAlpha zipcode.[4]
											&& isAlpha zipcode.[5] //|| zipcode==""

// An more complicated recursive datastructure

derive gGEC   Rose

::  Rose a 		= Rose a  .[Rose a]

example_rose	=	mk_GEC 		("Rose",Rose 1 []) 

// Convert Pounds to Euro's Example

derive gGEC   Pounds, Euros

:: Pounds = {pounds :: Real}
:: Euros  = {euros :: Real}

example_rec1	=	mutual_GEC  {euros=0.0} ("Exchange Euros to Pounds",toEuro) ("Exchange Euros to Pounds",toPounds)   
where
	toPounds {euros} 	= {pounds = euros / exchangerate}
	toEuro {pounds} 	= {euros = pounds * exchangerate}
	exchangerate 		= 1.4

// Examples on buttons

example_but5	= self_GEC ButtonWithState ("Button",(Button "0",Hide 0))
where
	ButtonWithState (Pressed,Hide i)
	| i < 3 	= (Button (toString (i+1)),Hide (i+1))
	| otherwise = (Button "0",Hide 0)
	ButtonWithState else = else

// display lists

example_lists1	= mk_GEC ("InfiniteListDisplay",listGEC False allprimes) 
where
	allprimes = sieve [2..]

	sieve [x:xs] = [x : sieve  (filter x xs)]
	where
		filter x [y:ys] | y rem x == 0 = filter x ys
		| otherwise = [y: filter x ys]
	
example_lists2	= mk_GEC ("ListDisplay",listGEC True initrecords) 
where
	initrecords 	= [MyRecord]

// drawing stuf

derive gGEC  Rectangle,Point2,Colour,RGBColour,ShapeAttributes,Shape,Oval,Box 

:: Shape =
	  Box Box
	| Oval Oval

:: ShapeAttributes =
	{ pen_colour 	:: Colour
	, pen_size	 	:: AGEC Int
	, fill_colour	:: Colour
	, x_offset		:: AGEC Int 
	, y_offset		:: AGEC Int
	}

example_draw pst
#	(wid,pst) 	= openId pst
#    pst 		= snd (openWindow Void (Window "Drawings" NilLS [WindowId wid]) pst)
=	selfStateGECps (mydrawfun wid) ("Rectangle Attributes",listGEC True initstates) initstates pst
	
	where
		mydrawfun wid abs_nrects orects pst 
		# nrects = ^^ abs_nrects
		# pst = appPIO (setWindowLook wid True (True,drawfun nrects orects)) pst 
		= (abs_nrects,nrects,pst)
		
		drawfun [Box nrect<|>nattr:nrects] [Box orect<|>oattr:orects]  nx nxx
											=	drawfun nrects orects nx nxx o
												drawshape nrect nattr orect oattr
		drawfun [Oval nrect<|>nattr:nrects] [Oval orect<|>oattr:orects]  nx nxx
											=	drawfun nrects orects nx nxx o
												drawshape nrect nattr orect oattr
		drawfun _ _  _ _					=	setPenColour Black 

		drawshape nshape nattr oshape oattr =	drawAt n_offset nshape o
												setPenSize (^^ nattr.pen_size) o 
												setPenColour nattr.pen_colour o 
												fillAt n_offset nshape o 
												setPenColour nattr.fill_colour o 
												unfillAt o_offset oshape o
												undrawAt o_offset oshape 
		where
			n_offset = {x= ^^ nattr.x_offset,y= ^^ nattr.y_offset}
			o_offset = {x= ^^ oattr.x_offset,y= ^^ oattr.y_offset}
	
		initstates= [initstate]
		initstate= initbox <|> initattr
		initbox = Box {box_w=30,box_h=30}
		initattr = {pen_colour=Black,pen_size=counterGEC 1,fill_colour=White,x_offset=counterGEC 100,y_offset=counterGEC 100}

// Examples on counters

derive gGEC DoubleCounter

example_cnt4 = mk_GEC ("Counter",counterGEC 'a')

:: DoubleCounter = {counter1::AGEC Int, counter2::AGEC Int,sum::Int}
example_cnt5 = self_GEC updateDoubleCounters ("Counter",{counter1=idGEC 0,counter2=counterGEC 0,sum=0})
where
	updateDoubleCounters cntrs = {cntrs & sum = ^^ cntrs.counter1 + ^^ cntrs.counter2}

example_cnt6 = self_GEC updateTwoIntCounters ("Counter",intcalcGEC 0 <|> counterGEC 0 <|> 0)
where
	updateTwoIntCounters (i1 <|> i2 <|> sum) = (i1 <|> i2 <|> ^^ i1 + ^^ i2)

//example_calc 

example_calc	= self_GEC update_calc ("Calculator",calculator)
where
	calculator	= 	zero  			  <|> 
					calc zero  <|> 
					horlistGEC buttons

	update_calc (mem <|> i <|> pressed) = (nmem <|> calc ni <|> horlistGEC buttons)
	where
		(nmem,ni)	= case whichopper (^^ pressed) operators of
							[] 		= (mem,^^ i)
							[f:_]	= (f mem (^^ i),zero)

	calc		= realcalcGEC
//	calc		= intcalcGEC 
	buttons		= [Button "+", Button "-", Button "*"]
	operators 	= [(+),(-),(*)]
	whichopper buttons operators = [x \\ (Pressed,x) <- (zip2 buttons operators)]

	
example_buts2 = mk_GEC ("demo",realcalcGEC 0.0)
	
example_timer1 = timer_GEC clock ("TickTack",(100,0<->0<->0))
where
	clock (tics,min<->secs<->9) = clock (tics,min<->secs+1<->0)
	clock (tics,min<->59<->msecs) = clock (tics,min+1<->0<->msecs)
	clock (tics,min<->secs<->msecs) = (tics,min<->secs<->msecs+1)


example_timer2 = timer_GEC (till 10) ("TickTack",(1000,0))
where
	till n (i,j)
	| j < n 	= (i,j+1)
	| otherwise = (0,j)	
