implementation module commondef


//	Version 1.2

//	Common types for the I/O system and their access-rules.


import	StdArray, StdBool, StdChar, StdClass, StdEnum, StdFunc, StdInt, StdList, StdMisc, StdReal, StdString
from	ostypes		import Rect	// PA: Rect type is defined in the ostypes module.
import	osrgn
import	StdIOCommon



/*	Extensions of StdFunc:
*/
K` :: .x !.y -> .y
K` _ y = y


/*	Calculation rules on Integers:
*/
Dist :: !Int !Int -> Int
Dist x y
	| d>=0		= d
	| otherwise	= y-x
where
	d			= x-y

SetBetween :: !Int !Int !Int -> Int
SetBetween x low up
	| x<=low	= low
	| x>=up		= up
	| otherwise	= x

IsBetween :: !Int !Int !Int -> Bool
IsBetween x low up
	| x<low		= False
	| otherwise	= x<=up

minmax :: !Int !Int -> (!Int,!Int)
minmax a b
	| a<=b		= (a,b)
	| otherwise	= (b,a)


/*	Calculation rules on Points, Sizes, and Vectors:
*/
addPointSize :: !Size !Point2 -> Point2
addPointSize {w,h} {x,y} = {x=x+w,y=y+h}


instance zero Rect where
	zero :: Rect
	zero = {rleft=0,rtop=0,rright=0,rbottom=0}
instance == Rect where
	(==) :: !Rect !Rect -> Bool
	(==) r1 r2 = r1.rleft==r2.rleft && r1.rtop==r2.rtop && r1.rright==r2.rright && r1.rbottom==r2.rbottom
class addVector a :: !Vector2 !a -> a	// add the vector argument to the second argument
instance addVector Point2 where
	addVector :: !Vector2 !Point2 -> Point2
	addVector {vx,vy} {x,y} = {x=x+vx,y=y+vy}
instance addVector Rect where
	addVector :: !Vector2 !Rect -> Rect
	addVector {vx,vy} {rleft,rtop,rright,rbottom} = {rleft=rleft+vx,rtop=rtop+vy,rright=rright+vx,rbottom=rbottom+vy}
instance addVector Rectangle where
	addVector :: !Vector2 !Rectangle -> Rectangle
	addVector v {corner1,corner2} = {corner1=addVector v corner1,corner2=addVector v corner2}
class subVector a :: !Vector2 !a -> a	// subtract the vector argument from the second argument
instance subVector Point2 where
	subVector :: !Vector2 !Point2 -> Point2
	subVector {vx,vy} {x,y} = {x=x-vx,y=y-vy}
instance subVector Rect where
	subVector :: !Vector2 !Rect -> Rect
	subVector {vx,vy} {rleft,rtop,rright,rbottom} = {rleft=rleft-vx,rtop=rtop-vy,rright=rright-vx,rbottom=rbottom-vy}
instance subVector Rectangle where
	subVector :: !Vector2 !Rectangle -> Rectangle
	subVector v {corner1,corner2} = {corner1=subVector v corner1,corner2=subVector v corner2}

RectangleToRect :: !Rectangle -> Rect
RectangleToRect {corner1={x=a,y=b},corner2={x=a`,y=b`}}
	| x_less_x` && y_less_y`= {rleft=a, rtop=b, rright=a`,rbottom=b`}
	| x_less_x`				= {rleft=a, rtop=b`,rright=a`,rbottom=b }
	| y_less_y`				= {rleft=a`,rtop=b, rright=a, rbottom=b`}
	| otherwise				= {rleft=a`,rtop=b`,rright=a, rbottom=b }

where
	x_less_x` = a<=a`
	y_less_y` = b<=b`

RectToRectangle :: !Rect -> Rectangle
RectToRectangle {rleft,rtop,rright,rbottom}
	= {corner1={x=rleft,y=rtop},corner2={x=rright,y=rbottom}}

IsEmptyRect :: !Rect -> Bool
IsEmptyRect {rleft,rtop,rright,rbottom}
	= rleft==rright || rtop==rbottom

IsEmptyRectangle :: !Rectangle -> Bool
IsEmptyRectangle {corner1,corner2}
	= corner1.x==corner2.x || corner1.y==corner2.y

PointInRect :: !Point2 !Rect -> Bool
PointInRect {x,y} {rleft,rtop,rright,rbottom}
	= IsBetween x rleft rright && IsBetween y rtop rbottom

PointInRectangle :: !Point2 !Rectangle -> Bool
PointInRectangle point rectangle
	= PointInRect point (RectangleToRect rectangle)

PosSizeToRect :: !Point2 !Size -> Rect
PosSizeToRect {x,y} {w,h}
	= {rleft=left,rtop=top, rright=right,rbottom=bottom}
where
	(left,right)	= minmax x (x+w)
	(top,bottom)	= minmax y (y+h)

PosSizeToRectangle :: !Point2 !Size -> Rectangle
PosSizeToRectangle pos=:{x,y} {w,h}
	= {corner1=pos,corner2={x=x+w,y=y+h}}

SizeToRect :: !Size -> Rect
SizeToRect size
	= PosSizeToRect zero size

SizeToRectangle :: !Size -> Rectangle
SizeToRectangle {w,h}
	= {zero & corner2={x=w,y=h}}

DisjointRects :: !Rect !Rect -> Bool
DisjointRects rect1 rect2
	= IsEmptyRect rect1 || IsEmptyRect rect2 || rect1.rleft>=rect2.rright || rect1.rbottom<=rect2.rtop || rect1.rright<=rect2.rleft || rect1.rtop>=rect2.rbottom

IntersectRects :: !Rect !Rect -> Rect
IntersectRects rect1 rect2
	| DisjointRects rect1 rect2	= zero
	| otherwise					= {	rleft	= max rect1.rleft   rect2.rleft
								  ,	rtop	= max rect1.rtop    rect2.rtop
								  ,	rright	= min rect1.rright  rect2.rright
								  ,	rbottom	= min rect1.rbottom rect2.rbottom
								  }

SubtractRects :: !Rect !Rect -> [Rect]
SubtractRects rect1 rect2
	= SubtractFittingRect rect1 (IntersectRects rect1 rect2)
where
//	SubtractFittingRect r1 r2 subtracts r2 from r1 assuming that r2 fits inside r1
	SubtractFittingRect :: !Rect !Rect -> [Rect]
	SubtractFittingRect {rleft=l1,rtop=t1,rright=r1,rbottom=b1} {rleft=l2,rtop=t2,rright=r2,rbottom=b2}
		= filter (not o IsEmptyRect) (map fromTuple4 [(l1,t1,r1,t2),(l1,t2,l2,b2),(r2,t2,r1,b2),(l1,b2,r1,b1)])


RectSize :: !Rect -> Size
RectSize {rleft,rtop,rright,rbottom}
	= {w=abs (rright-rleft),h=abs (rbottom-rtop)}


/*	Rules on RgnHandles and Rects:
*/
IntersectRgnRect :: !OSRgnHandle !Rect !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
IntersectRgnRect rgnH rect tb
	# (aidRgn,tb)	= osnewrectrgn rect tb
	# (secRgn,tb)	= ossectrgn rgnH aidRgn tb
	# tb			= osdisposergn   aidRgn tb
	= (secRgn,tb)

/*	PA: DisjointRgnRect made invisible
DisjointRgnRect :: !RgnHandle !Rect !Toolbox -> (!Bool,!Toolbox)
DisjointRgnRect rgnH rect tb
#	(aidRgn, tb)	= IntersectRgnRect rgnH rect tb
	(isEmpty,tb)	= QEmptyRgn aidRgn tb
	tb				= QDisposeRgn aidRgn tb
=	(isEmpty,tb)
*/

/*	PA: Conversion of Size, Point2, and Vector2 to tuples (toTuple) and from tuples (fromTuple):
*/
class toTuple   a :: !a -> (!Int,!Int)
class fromTuple a :: !(!Int,!Int) -> a

instance toTuple Size where
	toTuple :: !Size  -> (!Int,!Int)
	toTuple {w,h} = (w,h)
instance toTuple Point2 where
	toTuple :: !Point2 -> (!Int,!Int)
	toTuple {x,y} = (x,y)
instance toTuple Vector2 where
	toTuple :: !Vector2 -> (!Int,!Int)
	toTuple {vx,vy} = (vx,vy)

instance fromTuple Size where
	fromTuple :: !(!Int,!Int) -> Size
	fromTuple (w,h) = {w=w,h=h}
instance fromTuple Point2 where
	fromTuple :: !(!Int,!Int) -> Point2
	fromTuple (x,y) = {x=x,y=y}
instance fromTuple Vector2 where
	fromTuple :: !(!Int,!Int) -> Vector2
	fromTuple (vx,vy) = {vx=vx,vy=vy}

/*	PA: Conversion of Rect, and Rectangle to 4-tuples (toTuple4) and from 4-tuples (fromTuple4):
*/
class toTuple4   a :: !a -> (!Int,!Int,!Int,!Int)
class fromTuple4 a :: !(!Int,!Int,!Int,!Int) -> a

instance toTuple4 Rect where
	toTuple4 :: !Rect -> (!Int,!Int,!Int,!Int)
	toTuple4 {rleft,rtop,rright,rbottom} = (rleft,rtop,rright,rbottom)
instance toTuple4 Rectangle where
	toTuple4 :: !Rectangle -> (!Int,!Int,!Int,!Int)
	toTuple4 r = toTuple4 (RectangleToRect r)
instance fromTuple4 Rect where
	fromTuple4 :: !(!Int,!Int,!Int,!Int) -> Rect
	fromTuple4 r = RectangleToRect (fromTuple4 r)
instance fromTuple4 Rectangle where
	fromTuple4 :: !(!Int,!Int,!Int,!Int) -> Rectangle
	fromTuple4 (l,t,r,b) = {corner1={x=l,y=t},corner2={x=r,y=b}}


/*	Error generation rule:
*/
Error :: !String !String !String -> .x
Error rule moduleName error
	= abort ("Error in rule "+++rule+++" ["+++moduleName+++"]: "+++error+++".\n")

//	PA: new version of Error to dump fatal errors.
FatalError :: !String !String !String -> .x
FatalError rule moduleName error
	= abort ("Fatal error in rule "+++rule+++" ["+++moduleName+++"]: "+++error+++".\n")


/*	Universal dummy value (!!evaluation causes termination with the message: "Fatal error: dummy evaluated!"!!)
*/
dummy :: String -> .x
dummy error = abort ("Fatal error: dummy evaluated! "+++error+++".\n")


/*	Max Integer constants:
*/
MaxSigned2ByteInt	:== 32767		// 2^15-1
MaxSigned4ByteInt	:== 2147483647	// 2^31-1


/*	Bound data type:
*/
::	Bound
	=	Finite Int												// Fix a finite positive bound of N
	|	Infinite												// No bound

instance == Bound where
	(==) :: !Bound !Bound -> Bool
	(==) (Finite i)	(Finite j)	= i==j || i<=0 && j<=0
	(==) Infinite	Infinite	= True
	(==) _			_			= False

zeroBound:: !Bound -> Bool
zeroBound (Finite i)	= i<=0
zeroBound _				= False

decBound :: !Bound -> Bound
decBound (Finite i)
	| i<=0		= Finite 0
	| otherwise	= Finite (i-1)
decBound bound	= bound

incBound :: !Bound -> Bound
incBound (Finite i)
	| i<=0		= Finite 1
	| otherwise	= Finite (i+1)
incBound bound	= bound


/*	PA: code changed and moved to oswindow.
/*	Standard Scroll Bar settings:
	Internally, scrollbars always have the following internal range:
	*	if the SliderState is not empty (sliderMin<>sliderMax): (StdSliderMin,StdSliderMax)
	*	if the SliderState is empty     (sliderMin==sliderMax): (StdSliderMin,StdSliderMin).
	The thumb is always set proportionally (see toSliderRange).
	Its value can be recalculated by fromSliderRange given the actual SliderState range.
*/
StdSliderMin		:== 0			// 0
StdSliderMax		:== 32767		// MaxSigned2ByteInt
StdSliderRange		:== 32767		// StdSliderMax-StdSliderMin

fromSliderRange :: !Int !Int !Int -> Int
fromSliderRange min max x
	| min==max	= min
	| otherwise	= toInt ((toReal (x-StdSliderMin))*k)+min
where
	k = toReal (max-min) / toReal StdSliderRange

toSliderRange :: !Int !Int !Int -> Int
toSliderRange min max x
	| min==max	= StdSliderMin
	| otherwise	= toInt ((toReal (x-min))/k)+StdSliderMin
where
	k = toReal (max-min) / toReal StdSliderRange
*/


/*	List operations:
*/
::	Cond  x :== x -> Bool
::	UCond x :== x -> *(Bool,x)

IsSingleton :: ![.x] -> Bool
IsSingleton [x]	= True
IsSingleton _	= False

HdTl :: !u:[.x] -> (!.x, !u:[.x])
HdTl [x:xs]		= (x,xs)

InitLast :: ![.x] -> (![.x],!.x)
InitLast [x]
	= ([],x)
InitLast [x:xs]
	# (init,last)	= InitLast xs
	= ([x:init],last)

Split :: !Int !u:[.x] -> (![.x],!u:[.x])
Split _ []
	= ([],[])
Split n xs
	| n<=0
		= ([],xs)
	# (x, xs)	= HdTl xs
	# (ys,zs)	= Split (n-1) xs
	= ([x:ys],zs)

CondMap :: (Cond x) !(IdFun x) ![x] -> (!Bool,![x])
CondMap c f [x:xs]
	# (b,xs)	= CondMap c f xs
	| c x		= (True,[f x:xs])
	| otherwise	= (b,   [x:xs])
CondMap _ _ _
	= (False, [])

Uspan :: !(UCond .a) !u:[.a] -> (![.a],!u:[.a])		// Same as span (StdList), but preserving uniqueness
Uspan c [x:xs]
	# (keep,x) = c x
	| keep
		= ([x:ys],zs)
	with
		(ys,zs) = Uspan c xs
	| otherwise
		= ([],[x:xs])
Uspan _ _
	= ([],[])

FilterMap :: !(.x -> *(Bool,.y)) ![.x] -> [.y]
FilterMap f [x:xs]
	#! (keep,y)	= f x
	#! ys		= FilterMap f xs
	| keep		= [y:ys]
	| otherwise	= ys
FilterMap _ _
	= []

StateMap :: !(.x -> .s -> *(.y,.s)) ![.x] !.s -> (![.y],!.s)
StateMap f [x:xs] s
	#! (y, s)	= f x s
	#! (ys,s)	= StateMap f xs s
	= ([y:ys],s)
StateMap _ _ s
	= ([],s)

StateMap2 :: !(.x -> .s -> .s) ![.x] !.s -> .s
StateMap2 f [x:xs] s
	= StateMap2 f xs (f x s)
StateMap2 _ _ s
	= s

StrictSeq :: ![.(.s -> .s)] !.s	-> .s		// Same as seq (StdFunc), but with strict state argument
StrictSeq [f:fs] s
	= StrictSeq fs (f s)
StrictSeq _ s
	= s

StrictSeqList :: !.[.St .s .x] !.s -> (![.x],!.s)	// Same as seqList (StdFunc), but with strict state argument
StrictSeqList [f:fs] s
	# (x, s) = f s
	# (xs,s) = StrictSeqList fs s
	= ([x:xs],s)
StrictSeqList _ s
	= ([],s)

Contains :: !(Cond x) ![x] -> Bool
Contains c [x:xs]	= c x || Contains c xs
Contains _ _		= False

UContains :: !(UCond .x) !u:[.x] -> (!Bool,!u:[.x])
UContains c [x:xs]
	# (cond,x) = c x
	| cond
		= (True,[x:xs])
	| otherwise
		# (b,xs) = UContains c xs
		= (b,[x:xs])
UContains _ nil
	= (False,nil)

Select :: !(Cond x) x ![x] -> (!Bool, x)
Select c n [x:xs]
	| c x		= (True,x)
	| otherwise	= Select c n xs
Select _ n _	= (False,n)

Access :: !(St .x *(Bool,.y)) .y !u:[.x] -> (!Bool,.y,!u:[.x])
Access acc n [x:xs]
	# ((cond,y),x) = acc x
	| cond
		= (True,y,[x:xs])
	| otherwise
		# (b,y,xs) = Access acc n xs
		= (b,y,[x:xs])
Access _ n nil
	= (False,n,nil)

AccessList :: !(St .x .y) ![.x] -> (![.y],![.x])
AccessList acc [x:xs]
	# (y, x)	= acc x
	# (ys,xs)	= AccessList acc xs
	= ([y:ys],[x:xs])
AccessList _ _
	= ([],[])

Remove :: !(Cond x) x ![x] -> (!Bool,x,![x])
Remove c n [x:xs]
	| c x
		= (True,x,xs)
	| otherwise
		# (b,y,xs) = Remove c n xs
		= (b,y,[x:xs])
Remove _ n _
	= (False,n,[])

URemove :: !(UCond .x) .x !u:[.x] -> (!Bool,.x,!u:[.x])
URemove c n [x:xs]
	# (cond,x)	= c x
	| cond
		= (True,x,xs)
	| otherwise
		# (b,y,xs)	= URemove c n xs
		= (b,y,[x:xs])
URemove _ n nil
	= (False,n,nil)

Replace :: !(Cond x) x ![x] -> (!Bool,![x])
Replace c y [x:xs]
	| c x
		= (True,[y:xs])
	| otherwise
		# (b,xs)	= Replace c y xs
		= (b,[x:xs])
Replace _ _ _
	= (False,[])

UReplace :: !(UCond .x) .x !u:[.x] -> (!Bool,!u:[.x])
UReplace c y [x:xs]
	# (cond,x)= c x
	| cond
		= (True,[y:xs])
	| otherwise
		# (b,xs)	= UReplace c y xs
		= (b,[x:xs])
UReplace _ _ nil
	= (False,nil)

ReplaceOrAppend :: !(Cond x) x ![x] -> [x]
ReplaceOrAppend c y [x:xs]
	| c x
		= [y:xs]
	| otherwise
		= [x:ReplaceOrAppend c y xs]
ReplaceOrAppend _ y _
	= [y]

UReplaceOrAppend :: !(UCond .x) .x !u:[.x] -> u:[.x]
UReplaceOrAppend c y [x:xs]
	# (cond,x)= c x
	| cond
		= [y:xs]
	| otherwise
		= [x:UReplaceOrAppend c y xs]
UReplaceOrAppend _ y _
	= [y]

Ulength :: ![.x] -> (!Int,![.x])
Ulength [x:xs]
	# (length,xs)= Ulength xs
	= (length+1,[x:xs])
Ulength _
	= (0,[])

RemoveCheck :: x !u:[x] -> (!Bool, !u:[x])	| Eq x
RemoveCheck y [x:xs]
	| y==x
		= (True,xs)
	| otherwise
		# (b,xs)	= RemoveCheck y xs
		= (b,[x:xs])
RemoveCheck _ _
	= (False,[])

RemoveSpecialChars :: ![Char] !{#Char} -> {#Char}
RemoveSpecialChars sc string
	= {c\\c<-RemoveSpecialChars` sc [c\\c<-:string]}
where
	RemoveSpecialChars` :: ![Char] ![Char] -> [Char]
	RemoveSpecialChars` sc [c1:cs1=:[c2:cs2]]
		| isMember c1 sc	= [c2:RemoveSpecialChars` sc cs2]
		| otherwise			= [c1:RemoveSpecialChars` sc cs1]
	RemoveSpecialChars` sc [c]
		| isMember c sc		= []
		| otherwise			= [c]
	RemoveSpecialChars` _ _
		= []

disjointLists :: ![x] ![x] -> Bool	| Eq x
disjointLists xs ys
	| isEmpty xs || isEmpty ys	= True
	| shorter xs ys				= disjointLists` xs ys
	| otherwise					= disjointLists` ys xs
where
	shorter :: ![x] ![x] -> Bool
	shorter [] _				= True
	shorter [x:xs] [y:ys]		= shorter xs ys
	shorter _ _					= False
	
	disjointLists` :: ![x] ![x] -> Bool	| Eq x
	disjointLists` [x:xs] ys	= not (isMember x ys) && disjointLists` xs ys
	disjointLists` _ _			= True

noDuplicates :: ![x] -> Bool	| Eq x
noDuplicates [x:xs]	= not (isMember x xs) && noDuplicates xs
noDuplicates _		= True

unzip3 :: ![(.a,.b,.c)] -> (![.a],![.b],![.c])
unzip3 [(a,b,c):abcs]
	#! (as,bs,cs) = unzip3 abcs
	= ([a:as],[b:bs],[c:cs])
unzip3 []
	= ([],[],[])

unzip4 :: ![(.a,.b,.c,.d)]	-> (![.a],![.b],![.c],![.d])
unzip4 [(a,b,c,d):abcds]
	#! (as,bs,cs,ds) = unzip4 abcds
	= ([a:as],[b:bs],[c:cs],[d:ds])
unzip4 []
	= ([],[],[],[])
