module properties
/*
Pieter Koopman 2002
Nijmegen University, The Netherlands

Some properties to be tested by the generic Test System

Run this program with project option "Show basic values only"
*/

import stdTest4
//import StdTest//Small
//import StdTestSmall
import GenBimap

:: Tree a = Tip | Bin a (Tree a) (Tree a)
:: Color  = Red | Yellow | Blue // | Black
:: List a = Nil | Cons a (List a)
:: Rose a = Rose a [Rose a]
:: Fork a = Fork a a
:: Sequ a = SequEmpty | SequZero .(Sequ .(Fork a)) | SequOne a .(Sequ .(Fork a))
:: Rec    = { x::Int, y::Int, c::Color}
:: T = C

(===>) infix 1 :: Bool Bool -> Bool
(===>) p q = (not p) || q 

derive generate Tree, Color, List, Rose, Fork, Sequ, Rec, T , Bit, Day, Result //, Trace
derive genShow Tree, Color, List, Rose, Fork, Sequ, Rec, T, Bit, Day, Result //, Trace
derive gEq T, Tree, Color, List, Rose, Fork, Sequ, Rec, Bit, Day, Result //, Trace

propSquare :: Property
propSquare = Exists \x . x^2 == x

propSquare2 :: Property
propSquare2 = Exists \x . let b = x^2 == x in classify b x b 

propZero :: Int -> Property
propZero x = prop (x+0 == 0+x)

propPlus :: Int Int -> Bool
propPlus x y = x+y==y+x

propSqrt1 :: Real -> Bool
propSqrt1 x = (\s = s*s) (sqrt x) == x

propSqrt :: Real -> Property
propSqrt x = name "(sqrt x)^2=x" (x>=0.0 ==> let s = sqrt x in s*s == x)

propSqrt2 :: Real -> Bool
propSqrt2 x = x>=0.0 ===> let s = sqrt x in s*s == x

propSqrt3 :: Real -> Property
propSqrt3 x = x>=0.0 ==> let s = sqrt x in name ("sqrt",x) @ s*s == x

propAssoc :: Real Real Real -> Bool
propAssoc x y z = (x+(y+z) == (x+y)+z)

propMax :: Int Int -> Property
propMax x y = label x @ x<=y ==> max x y == y

propRevRev :: [Bool] -> Bool
propRevRev xs = reverse (reverse xs) == xs

propRevRevl :: [T] -> Property
propRevRevl xs = classify (isEmpty xs) xs (reverse (reverse xs) === xs)

propRevReval :: [Bit] -> Property
propRevReval xs = label (length xs) (reverse (reverse xs) === xs)

propDrop :: Int Int [Int] -> Bool
propDrop m n xs = drop n (drop m xs) == drop (n+m) xs

propDrop2 :: Int Int [Int] -> Property
propDrop2 m n xs = n>=0 && m>=0 ==> drop n (drop m xs) == drop (n+m) xs

propDeMorganl :: Bool Bool -> Property
propDeMorganl x y = label ('x',x) (label ('y',y) ((x&&y) == not (not x || not y)))

propDeMorgan :: Bool Bool -> Bool
propDeMorgan x y = x&&y == not (not x || not y)

propOr :: Bool Bool -> Bool
propOr x y = x||y == or x y
where
	or x y = nand (not x) (not y)
	not x  = nand x x

nand :: Bool Bool -> Bool
nand x y = not (x&&y)

propNot :: Bool -> Property
propNot b = label b case b of
				True  = not b == False
				False = not b == True

propFix :: (Int->Int) -> Property
propFix f = Exists \x = f x == x

propFixL :: (Int->Int) -> Property
propFixL f = Exists \x = let b = f x == x in classify b x b // show the fixpoints found


propTupLists :: ([Bit],[Bit]) -> Property
propTupLists (xs,ys) = classify (isEmpty xs) ("xs",xs) (classify (isEmpty ys) ("ys",ys) (length xs + length ys >= 0))

// --- Alternating bit-protocol
:: Bit	= O | I

instance ~ Bit
where
	~ O = I
	~ I = O

:: Message c		=	M c Bit | MError
:: Ack				=	A Bit | AError
:: SenderState c	=	Send Bit | Exp Bit c
:: ReceiverState	=	Rec Bit

sender :: (SenderState c) [c] [Ack] -> [Message c]
sender (Send b) []     as = []
sender (Send b) [c:cs] as = [M c b: sender (Exp b c) cs as]
sender state=:(Exp b c) cs [a:as]
 = case a of
	A b` | b===b`	= sender (Send (~b)) cs as
	_				= [M c b: sender state cs as]
sender (Exp b c) cs [] = abort "Sender: Acknowledgement expected"

receiver :: ReceiverState [Message c] -> ([Ack],[c])
receiver rState [] = ([],[])
receiver rState=:(Rec b) [m:ms]
 = case m of
	M c b` | b===b`	= ([A   b :as],[c:cs]) where (as,cs) = receiver (Rec (~b)) ms
	_				= ([A (~b):as],   cs ) where (as,cs) = receiver rState ms

sChannel :: (Int->Bool) [Message c] -> [Message c]
sChannel error ms = [ if (error n) MError m \\ m <- ms & n <- [1..]]

rChannel :: (Int->Bool) [Ack] -> [Ack]
rChannel error as = [ if (error n) AError a \\ a <- as & n <- [1..]]

abpSystem :: (Int->Bool) (Int->Bool) [c] -> [c]
abpSystem sError rError toSend
	= received
where
	msSend = sender (Send O) toSend (rChannel rError acks)
	(acks,received) = receiver (Rec O) (sChannel sError msSend)

propAltBit :: (Int->Bool) (Int->Bool) Int -> Bool
propAltBit sError rError n = toSend == received
where
	toSend = [1..n]
	received = abpSystem sError rError toSend

// --- Stack --- //

:: Stack a :== [a]

pop :: (Stack a) -> Stack a
pop [_:r] = r
pop _ = abort "pop from empty stack"

top :: (Stack a) -> a
top [a:_] = a
top _ = abort "pop from empty stack"

push :: a (Stack a) -> Stack a
push a s = [a:s]

propStack :: a (Stack a) -> Bool | gEq{|*|} a
propStack e s = top (push e s) === e && pop (push e s) === s

propStackN :: a (Stack a) -> Property | gEq{|*|} a
propStackN e s = label (length s) @ p1 /\ p2 /\ p3
where	p1 = name "top (push e s)=e" @ top (push e s) === e
		p2 = name "pop (push e s)=s" @ pop (push e s) === s
		p3 = name "push t (pop s)=s" @ (~(isEmpty s) ==> (let t = top s in push t (pop s) === s))

propStackNInt :: (Int (Stack Int) -> Property)
propStackNInt = propStackN

propStackInt :: (Int (Stack Int) -> Bool)
propStackInt = propStack

propStackL :: Int (Stack Int) -> Property
propStackL e s = label (e,s) @ top (push e s) === e && pop (push e s) === s

propStackC :: Int (Stack Int) -> Property
propStackC e s = classify (isEmpty s) s (propStack e s) //@ top (push e s) === e && pop (push e s) === s

propStackFA :: Property
propStackFA = ForAll \e = ForEach [[],[1],[1,2]] \s = label (e,s) @ top (push e s) === e && pop (push e s) === s

propStack2 :: (Stack Int)[Int] -> Property
propStack2 s es = label (length es,s) @ pushAndPop es s === s
where
	pushAndPop :: [e] (Stack e) -> Stack e
	pushAndPop l s = popn (length l) (pushl l s)
	
	pushl [] s = s
	pushl [e:r] s = pushl r (push e s)
	
	popn 0 s = s
	popn n s = popn (n-1) (pop s)

// ---

propMapInt :: ((Int->Int) (Int->Int) [Int] -> Bool)
propMapInt = propMap

propMap :: (a->b) (b->c) [a] -> Bool | gEq{|*|} c
propMap f g xs = map g (map f xs) === map (g o f) xs

propMapC :: (Int->Int) (Int->Int) -> [Int] -> Bool
propMapC f g = \xs = map f (map g xs) == map (f o g) xs

propMapL :: (Int->Int) (Int->Int) [Int] -> Property
propMapL f g xs = label (length xs) @ map f (map g xs) == map (f o g) xs

propRec :: [Rec] -> Property
propRec list = label (length list) @ length list == length (map (\r = r.c) list)

Fib 0 = 1
Fib 1 = 1
Fib n = Fib (n-1) + Fib (n-2)

FibLin n = f n 1 1
where
	f 0 a b = a
	f n a b = f (n-1) b (a+b)

propFibR :: Property
propFibR = propFib For [0..15]

propFibR2 :: Property
propFibR2 = ForEach [0..15] @ \n = n>=0 ==> Fib n == FibLin n

propFibR3 :: Property
propFibR3 = ForEach [0..15] @ \n = Fib n == FibLin n

propFib :: Int -> Property
propFib n = n>=0 ==> Fib n == FibLin n

propCount :: Property
//propCount =  (\x = label x True) For [1..]
//propCount = (\y = (\x = label (x,y) True) For [1..]) For [1..]
//propCount = (\y = label y ((\x = True) For [1..])) For [1..]
propCount = (\y = ((\x = label (x,y) True) For [1..])) For [1..]
//propCount = (\y = label y (ForAll (\x = x =/= x+1))) For [1..]

propList :: (Bit,Bit) -> Property
//propList :: Bit -> Property
//propList :: [Bit] -> Property
propList l = label l True

propInts :: Int Int -> Property
propInts n m = label (n,m) True

//propInts2 :: !Int -> Bool
propInts2 :: ![Int] -> Bool
propInts2 n = n==n

propInts2l :: ![Int] -> Property
propInts2l n = label (length n) (n==n)
/*
propRoots :: Real Real Real Real -> Property
propRoots a b c x = label (a,b,c,x) @ and (map (\r = abs (f r) < 0.01) (roots a b c))
where f x = a*x*x + b*x + c
*/
propRoots :: Real Real Real -> Property
//propRoots a b c = label (a,b,c) @ and (map (\r = abs (f r) < 0.01) (roots a b c))
propRoots a b c = /*label (a,b,c,length rs) @*/ classify (~ok) ("roots",(a,b,c),rs,map f rs) ok
where
	f x = a*x*x + b*x + c
	rs = (roots a b c)
	ok = and (map (\r = abs (f r) < 0.01) rs)

roots :: Real Real Real -> [Real]
roots a b c
	| a == 0.0
		| b == 0.0
			= []
			= [~c/b]
	# dsq = b*b - 4.0*a*c
	| dsq < 0.0
		 = []
	| dsq == 0.0
		= [~b/(2.0*a)]
	# d = sqrt dsq
	= [(~b-d)/(2.0*a),(~b+d)/(2.0*a)]

instance toString Bit
where
	toString I = "I"
	toString O = "O"

instance toString (a,b) | toString a & toString b
where
	toString (a,b) = "("+toString a+","+toString b+")"

propExists :: !a -> Property | TestArg a & gEq{|*|} a
propExists x = label x @ Exists \y = x===y

propExistsColors :: ([Color] -> Property)
propExistsColors = \x = (length x>0) ==> propExists x

:: Day = Mo | Tu | We | Th | Fr | Sa | Su

tomorrow :: Day -> Day
tomorrow d
 = case d of
	Mo = Tu
	Tu = We 
	We = Th 
	Th = Fr
	Fr = Sa
	Sa = Su
	Su = Mo

propTomorrow :: Day -> Property
propTomorrow day = Exists \d = tomorrow day === d

propTomorrow2 :: Day -> Property
propTomorrow2 day = Exists \d = tomorrow d === day

propSurjection = propTomorrow2

//propInjection :: Day -> Property
//propInjection day = Exists \d = (tomorrow day === tomorrow d) ==> (day === d)
propInjectionD :: Day Day -> Property
propInjectionD d1 d2 = (tomorrow d1 === tomorrow d2) ==> (d1 === d2)

propInjection :: (a->b) a a -> Property | gEq{|*|} a & gEq{|*|} b
propInjection f x y = (f x === f y) ==> (x === y)

propInverse :: Color -> Property
propInverse c = Exists p where p c` = c === inverse c`

inverse :: Color -> Color
inverse c 
 = case c of
	Red		= Blue
	Yellow	= Red
	Blue	= Yellow

propImplication :: Int -> Property
propImplication n = (Exists \m = label m (m==n)) ==> True

//---

propForFor :: Property
propForFor = (\y = ((\x = label (x,y) True) For [1..])) For [1..]

propAnd :: Bool Bool -> Property
propAnd x y = name "(x/\y) == (prop x /\ prop y)" (label ("And",x,y) @ (x/\y) <==> (prop x /\ prop y))

propOr2 :: Bool Bool -> Property
propOr2 x y = name "(x\/y) == (prop x \/ prop y)" (label ("Or",x,y) @ (x||y) <==> (prop x \/ prop y))

propProperty :: Bool Bool -> Property
propProperty x y = propAnd x y /\ propOr2 x y

propOrEvery2 :: Bool Bool -> Property
propOrEvery2 b c = prop (c==b) \/ prop (c == ~b)

propOrEvery :: Bool -> Property
propOrEvery b = name "c==b" (\c = (c==b)) \/ name "c == ~b" (\c = (c == ~b))

propAndEvery2 :: Bool Bool -> Property
propAndEvery2 b c = prop (c==b) /\ prop (c == ~b)

propAndEvery :: Bool -> Property
propAndEvery b = ForAll \c= (c==b) /\ ForAll \d= prop (d == ~b)

propNot2 :: Bool -> Property
propNot2 b = ~b <==> ~(prop b)

propEveryAll :: (Int->Bool) -> Property
//propEveryAll p = ForAll (\i = p i) <==> ~(Exists (\i = ~(p i)))
propEveryAll p = ForAll p <==> ~(Exists (~p))

trace :: (a->b) (a b->Bool) a -> Property | genShow{|*|} a & genShow{|*|} b 
trace f p x = label ("f(",x,")=",y) (p x y) where y = f x

Start :: *World -> [String]
Start world
    # (rs, world) = randomStream world
	= verbosen 100 rs
//	= quietn 2000 rs
//	= concisen 1000 rs
//	= test rs
				// propMapL
				// (trace Fib (\x y = True) For [0..10])
				// (trace not (\x y = True))
				// (propFib For [0..10])
				// propMax
				// propImplication
				// propAnd
				// propProperty
				// propOrEvery
				// propNot2
				// (name "Property" propProperty /\ name "EveryAll" propEveryAll /\ name "not" propNot2)
				// propEveryAll
				// propAndEvery
				// propOr2
				// propDrop2
				//(ForAll \n m xs = n>=0 && m>=0 ==> propDrop n m xs)
				//( \n = ForAll \m xs = n>=0 && m>=0 ==> propDrop n m xs)
				(ForAll \n = ForAll \m = ForAll \xs = n>=0 && m>=0 ==> propDrop n m xs)
				// propRec
				// propRevReval
				// propDeMorgan
				// propOr
				// propNot
				// propList
				// propInts
				// propInts2l
				// propFibR3
				// propExistsColors
				// propSquare
				// propSquare2
				// propInverse
				// propTomorrow
				// propTomorrow2
				// (propInjection tomorrow)
				// (propInjection (\n = n rem 3))
				// propSurjection
				// propRoots
				// propTupLists
				// propAltBit
				// propFix
				// propFixL
				// propStackInt
				// propStackFA
				// propStackC
				// propStackL
				// ((propStackN ForAndGen [-1]) )//ForAndGen [[10..25],[1..100]])
				// propForFor
				// propCount
				// propStackNInt
				// propMapC
				// propFibR
				// propSqrt
				// propSqrt2
				// propSqrt3
				// propExists
				// propMapInt
				// propTrace
				// propAssoc


//Start = (Undef -<-Rej,Rej-<-OK,OK-<-CE,Undef-<-CE)