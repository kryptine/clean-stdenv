definition module commondef


//	Clean Object I/O library, version 1.2

//	Common types for the Clean Object I/O system and their access rules:


import	StdClass
import	StdIOCommon
from	StdFunc		import St
from	osrgn		import OSRgnHandle
from	ostoolbox	import OSToolbox
from	ostypes		import Rect


K`						:: .x !.y -> .y

/*	Calculation rules on Integers:
*/
Dist					::		!Int !Int -> Int
SetBetween				:: !Int !Int !Int -> Int
IsBetween				:: !Int !Int !Int -> Bool
minmax					::      !Int !Int -> (!Int,!Int)			// minmax a b = (min a b,max a b)


/*	Calculation rules on Points, Sizes, and Vectors:
*/
addPointSize			:: !Size !Point2 -> Point2					// {w, h }+{x,y}={x=x+w, y=y+h }

instance zero Rect
instance ==   Rect
class addVector a :: !Vector2 !a -> a	// add the vector argument to the second argument
instance addVector Point2
instance addVector Rect
instance addVector Rectangle
class subVector a :: !Vector2 !a -> a	// subtract the vector argument from the second argument
instance subVector Point2
instance subVector Rect
instance subVector Rectangle

RectangleToRect			:: !Rectangle			-> Rect				// (l,t, r,b) such that l<=r && t<=b
RectToRectangle			:: !Rect				-> Rectangle		// (l,t, r,b) -> {{x=l,y=t},{x=r,y=b}}
IsEmptyRect				:: !Rect				-> Bool				// (l,t, r,b) -> l==r || t==b
IsEmptyRectangle		:: !Rectangle			-> Bool				// {corner1,corner2} -> corner1.x==corner2.x || corner1.y==corner2.y
PointInRect				:: !Point2 !Rect		-> Bool				// {x,y} (l,t, r,b) -> l<=x<=r && t<=y<=b
PointInRectangle		:: !Point2 !Rectangle	-> Bool				// PointInRect point (RectangleToRect rectangle)
PosSizeToRect			:: !Point2 !Size		-> Rect				// {x,y} {w,h} -> ( x,y,  x+w,y+h )	// no check on negative size
PosSizeToRectangle		:: !Point2 !Size		-> Rectangle		// {x,y} {w,h} -> {{x,y},{x+w,y+h}}	// no check on negative size
SizeToRect				::		   !Size		-> Rect				//       {w,h} -> ( 0,0,    w,  h )	// no check on negative size
SizeToRectangle			::		   !Size		-> Rectangle		//       {w,h} -> {zero, {  w,  h}}	// no check on negative size
DisjointRects			:: !Rect   !Rect		-> Bool
IntersectRects			:: !Rect   !Rect		-> Rect				// if disjoint: EmptyRect; otherwise the intersection
SubtractRects			:: !Rect   !Rect		-> [Rect]			// subtract @2 from @1
RectSize				:: !Rect				-> Size				// (l,t, r,b)          -> {abs (r-l), abs (b-t)}

/*	Rules on RgnHandles and Rects:
*/
IntersectRgnRect		:: !OSRgnHandle !Rect !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)	// the intersection of the two arguments

/*	PA: Conversion of Size, Point2, and Vector2 to tuples (toTuple) and from tuples (fromTuple):
*/
class toTuple   a :: !a -> (!Int,!Int)
class fromTuple a :: !(!Int,!Int) -> a

instance toTuple Size;		instance fromTuple Size
instance toTuple Point2;	instance fromTuple Point2
instance toTuple Vector2;	instance fromTuple Vector2

/*	PA: Conversion of Rect, and Rectangle to 4-tuples (toTuple4) and from 4-tuples (fromTuple4):
*/
class toTuple4   a :: !a -> (!Int,!Int,!Int,!Int)
class fromTuple4 a :: !(!Int,!Int,!Int,!Int) -> a

instance toTuple4 Rect;		instance fromTuple4 Rect
instance toTuple4 Rectangle;instance fromTuple4 Rectangle


/*	Common Error generation rule:
*/
Error					:: !String !String !String -> .x
FatalError				:: !String !String !String -> .x


/*	Universal dummy value (!!evaluation causes termination with the message: "Fatal error: dummy evaluated!"!!)
*/
dummy					:: String -> .x


/*	Max Integer constants:
*/
MaxSigned2ByteInt		:== 32767		// 2^15-1
MaxSigned4ByteInt		:== 2147483647	// 2^31-1


/*	Bound data type:
*/
::	Bound
	=	Finite Int												// Fix a finite positive bound of N
	|	Infinite												// No bound

instance == Bound												// Finite i == Finite j && max 0 i == max 0 j; Infinite == Infinite
zeroBound:: !Bound -> Bool										// Finite i && i<=0
decBound :: !Bound -> Bound										// Finite i -> Finite (max 0 (i-1)); Infinite -> Infinite
incBound :: !Bound -> Bound										// Finite i -> Finite (max 1 (i+1)); Infinite -> Infinite


/*	List operations:
*/
::	Cond  x :== x -> Bool
::	UCond x :== x -> *(Bool,x)

IsSingleton				:: ![.x] -> Bool
HdTl					:: !u:[.x] -> (!.x, !u:[.x])
InitLast				:: ![.x] -> (![.x],!.x)
Split					:: !Int !u:[.x] -> (![.x],!u:[.x])

CondMap					:: (Cond x) !(IdFun x)		![x]		-> (!Bool, ![x])
Uspan					:: !(UCond .x)				!u:[.x]		-> (![.x],!u:[.x])	// Same as span (StdList), but preserving uniqueness
FilterMap				:: !(.x -> *(Bool,.y))		![.x]		-> [.y]
StateMap				:: !(.x -> .s -> *(.y,.s))	![.x] !.s	-> (![.y], !.s)
StateMap2				:: !(.x -> .s -> .s)		![.x] !.s	-> .s
StrictSeq				:: ![.(.s -> .s)]				  !.s	-> .s				// Same as seq (StdFunc), but with strict state argument
StrictSeqList			:: !.[.St .s .x]				  !.s	-> (![.x],!.s)		// Same as seqList (StdFunc), but with strict state argument

Contains				:: !(Cond    x)				![ x] -> Bool
UContains				:: !(UCond  .x)				!u:[.x] -> (!Bool,	!u:[.x])
Select					:: !(Cond    x)		 x		![ x] -> (!Bool, x)
Access					:: !(St .x *(Bool,.y)) .y	!u:[.x] -> (!Bool,.y,!u:[.x])
AccessList				:: !(St .x .y)				![.x] -> (![.y],	![.x])
Remove					:: !(Cond    x)		 x		![ x] -> (!Bool, x,	![ x])
URemove					:: !(UCond  .x)		.x		!u:[.x] -> (!Bool,.x,	!u:[.x])
Replace					:: !(Cond    x)		 x		![ x] -> (!Bool,	![ x])
UReplace				:: !(UCond  .x)		.x		!u:[.x] -> (!Bool,	!u:[.x])
ReplaceOrAppend			:: !(Cond    x)      x		![ x] -> [ x]
UReplaceOrAppend		:: !(UCond  .x)     .x		!u:[.x] -> u:[.x]
RemoveCheck				::					 x	  !u:[x] -> (!Bool,  !u:[ x])	| Eq x
RemoveSpecialChars		:: ![Char] !{#Char}	-> {#Char}
Ulength					:: ![.x]			-> (!Int, ![.x])
disjointLists			:: ![x] ![x]		-> Bool		| Eq x
noDuplicates			:: ![x]				-> Bool		| Eq x
unzip3					:: ![(.a,.b,.c)]	-> (![.a],![.b],![.c])
unzip4					:: ![(.a,.b,.c,.d)]	-> (![.a],![.b],![.c],![.d])
