implementation module osrgn

//	Clean Object I/O library, version 1.2

import	StdInt, StdList
from	ostypes	import Rect
import	pictCCall_12, rgnCCall_12

::	OSRgnHandle
	:==	Int
::	OSPointH
	:==	Int

//	Region creation and disposal operations.
osnewrgn :: !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osnewrgn tb
	= WinCreateRectRgn 0 0 1 1 tb

osnewrectrgn :: !Rect !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osnewrectrgn {rleft,rtop,rright,rbottom} tb
	= WinCreateRectRgn rleft rtop rright rbottom tb

osdisposergn :: !OSRgnHandle !*OSToolbox -> *OSToolbox
osdisposergn osrgn tb
	= WinDeleteObject osrgn tb


//	Setting the shape of a Region.
osrectrgn :: !Rect !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osrectrgn {rleft,rtop,rright,rbottom} osrgn tb
	= WinSetRgnToRect rleft rtop rright rbottom osrgn tb

ospolyrgn :: !(!Int,!Int) ![(Int,Int)] !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
ospolyrgn base shape osrgn tb
	# (osrgn,tb)	= WinCombineRgn osrgn osrgn osrgn RGN_DIFF tb
	| len==0
	= (osrgn,tb)
	# (shapeH,tb)	= WinAllocPolyShape len tb
	# tb			= setpolyshape shapeH 0 base shape tb
	# (prgn,tb)		= WinCreatePolygonRgn shapeH len WINDING tb
	# (osrgn,tb)	= WinCombineRgn osrgn prgn prgn RGN_COPY tb
	# tb			= WinDeleteObject prgn tb
	# tb			= WinFreePolyShape shapeH tb
	= (osrgn,tb)
where
	len	= length shape
	
	setpolyshape :: !OSPointH !Int !(!Int,!Int) ![(Int,Int)] !*OSToolbox -> *OSToolbox
	setpolyshape shapeH i (x,y) [(vx,vy):vs] tb
		# tb	= WinSetPolyPoint i x y shapeH tb
		# tb	= setpolyshape shapeH (i+1) (x+vx,y+vy) vs tb
		= tb
	setpolyshape _ _ _ _ tb
		= tb


//	Combining the shapes of two Regions into a new Region.
ossectrgn :: !OSRgnHandle !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
ossectrgn rgn1 rgn2 tb
	# (rrgn,tb)	= WinCreateRectRgn 0 0 1 1 tb
	# (rrgn,tb)	= WinCombineRgn rrgn rgn1 rgn2 RGN_AND tb
	= (rrgn,tb)

osunionrgn :: !OSRgnHandle !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osunionrgn rgn1 rgn2 tb
	# (rrgn,tb)	= WinCreateRectRgn 0 0 1 1 tb
	# (rrgn,tb)	= WinCombineRgn rrgn rgn1 rgn2 RGN_OR tb
	= (rrgn,tb)

osdiffrgn :: !OSRgnHandle !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osdiffrgn rgn1 rgn2 tb
	# (rrgn,tb)	= WinCreateRectRgn 0 0 1 1 tb
	# (rrgn,tb)	= WinCombineRgn rrgn rgn1 rgn2 RGN_DIFF tb
	= (rrgn,tb)


//	Region property access functions.
osgetrgnbox	:: !OSRgnHandle !*OSToolbox -> (!Bool,!Rect,!*OSToolbox)
osgetrgnbox rgn tb
	# (l,t, r,b, isRect,_,tb)	= WinGetRgnBox rgn tb
	= (isRect,{rleft=l,rtop=t,rright=r,rbottom=b}, tb)

osisemptyrgn:: !OSRgnHandle !*OSToolbox -> (!Bool,!*OSToolbox)
osisemptyrgn rgn tb
	# (_,_,_,_,_,isempty,tb)	= WinGetRgnBox rgn tb
	= (isempty,tb)
