implementation module controldraw


//	Clean Object I/O library, version 1.2

//	Drawing in customised controls

import	StdBool, StdFunc, StdList
from	ospicture		import packPicture, unpackPicture, pictsetcliprgn, copyPen
from	osrgn			import osnewrectrgn, osdisposergn, ossectrgn
from	oswindow		import OSWindowMetrics, OSgrabWindowPictContext, OSreleaseWindowPictContext, OSscrollbarsAreVisible
import	commondef, wstate
from	windowaccess	import getWItemCompoundInfo,  getWItemCustomButtonInfo,  getWItemCustomInfo, getCompoundContentRect
from	wstateaccess	import getWItemCompoundInfo`, getWItemCustomButtonInfo`, getWItemCustomInfo`


/*	The following functions apply the current Look function of the given control.
*/

/*	drawCompoundLook(`) able parentWindow contextClip itemH
		applies the Look function of the compound control given the current selectstate (True iff Able).
		Drawing is clipped inside contextClip (in window coordinates) and the content rectangle of the compound control. 
		The function assumes that itemH refers to a CompoundControl(`) which ClipState is valid.
	Note that drawCompoundLook(`) draws in the graphics context of the parent window (OS(grab/release)WindowPictContext).
		This is done because the ClipState of the CompoundControl is given relative to the left-top of the window.
*/
drawCompoundLook :: !OSWindowMetrics !Bool !OSWindowPtr !Rect !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
drawCompoundLook wMetrics able wPtr contextClip itemH=:{wItemInfo} tb
	| isNothing info.compoundLookInfo
		= (itemH,tb)
	| otherwise
		#! (contextRgn,tb)			= osnewrectrgn contextClip tb
		#! (clipRgn,tb)				= ossectrgn contextRgn itemClip.clipRgn tb
		#! (osPict,tb)				= OSgrabWindowPictContext wPtr tb
		#! picture					= packPicture (origin-itemPos) (copyPen itemLook.lookPen) True osPict tb
		#! picture					= pictsetcliprgn clipRgn picture
		#! picture					= itemLook.lookFun selectState updState picture
		#! (_,pen,_,osPict,tb)		= unpackPicture picture
		#! tb						= OSreleaseWindowPictContext wPtr osPict tb
		#! tb						= StateMap2 osdisposergn [contextRgn,clipRgn] tb
		   info						= {info & compoundLookInfo=Just {compoundLookInfo & compoundLook={itemLook & lookPen=pen}}}
		= ({itemH & wItemInfo=CompoundInfo info},tb)
where
	itemPos							= itemH.wItemPos
	itemSize						= itemH.wItemSize
	info							= getWItemCompoundInfo wItemInfo
	(origin,domainRect,hasScrolls)	= (info.compoundOrigin,info.compoundDomain,(isJust info.compoundHScroll,isJust info.compoundVScroll))
	visScrolls						= OSscrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
	contentRect						= getCompoundContentRect wMetrics visScrolls (PosSizeToRect origin itemSize)
	clipRectangle					= RectToRectangle (addVector (toVector (origin-itemPos)) (IntersectRects (addVector (toVector (itemPos-origin)) contentRect) contextClip))
	viewFrame						= RectToRectangle contentRect
	updState						= {oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}
	compoundLookInfo				= fromJust info.compoundLookInfo
	itemLook						= compoundLookInfo.compoundLook
	itemClip						= compoundLookInfo.compoundClip
	selectState						= if able Able Unable

drawCompoundLook` :: !OSWindowMetrics !Bool !OSWindowPtr !Rect !WItemHandle` !*OSToolbox -> (!WItemHandle`,!*OSToolbox)
drawCompoundLook` wMetrics able wPtr contextClip itemH=:{wItemInfo`} tb
	| isNothing info.compoundLookInfo
		= (itemH,tb)
	| otherwise
		#! (contextRgn,tb)			= osnewrectrgn contextClip tb
		#! (clipRgn,tb)				= ossectrgn contextRgn itemClip.clipRgn tb
		#! (osPict,tb)				= OSgrabWindowPictContext wPtr tb
		#! picture					= packPicture (origin-itemPos) (copyPen itemLook.lookPen) True osPict tb
		#! picture					= pictsetcliprgn clipRgn picture
		#! picture					= itemLook.lookFun selectState updState picture
		#! (_,pen,_,osPict,tb)		= unpackPicture picture
		#! tb						= OSreleaseWindowPictContext wPtr osPict tb
		#! tb						= StateMap2 osdisposergn [contextRgn,clipRgn] tb
		   info						= {info & compoundLookInfo=Just {compoundLookInfo & compoundLook={itemLook & lookPen=pen}}}
		= ({itemH & wItemInfo`=CompoundInfo` info},tb)
where
	itemPos							= itemH.wItemPos`
	itemSize						= itemH.wItemSize`
	info							= getWItemCompoundInfo` wItemInfo`
	(origin,domainRect,hasScrolls)	= (info.compoundOrigin,info.compoundDomain,(isJust info.compoundHScroll,isJust info.compoundVScroll))
	visScrolls						= OSscrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
	contentRect						= getCompoundContentRect wMetrics visScrolls (PosSizeToRect origin itemSize)
	clipRectangle					= RectToRectangle (addVector (toVector (origin-itemPos)) (IntersectRects (addVector (toVector (itemPos-origin)) contentRect) contextClip))
	viewFrame						= RectToRectangle contentRect
	updState						= {oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}
	compoundLookInfo				= fromJust info.compoundLookInfo
	itemLook						= compoundLookInfo.compoundLook
	itemClip						= compoundLookInfo.compoundClip
	selectState						= if able Able Unable

/*	drawCustomButtonLook(`) able parentWindow contextClip itemH
		applies the Look function of the custom button control given the current selectstate (True iff Able).
		Drawing is clipped inside contextClip and the content rectangle of the custom button control. 
		The function assumes that itemH refers to a custom button control.
*/
drawCustomButtonLook :: !Bool !OSWindowPtr !Rect !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
drawCustomButtonLook able wPtr contextClip itemH=:{wItemPtr,wItemInfo,wItemPos,wItemSize} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb
	#! (osPict,tb)			= OSgrabWindowPictContext wPtr tb		// PA: use window HDC instead of control HDC because of clipstate
	#! picture				= packPicture (zero-wItemPos) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! picture				= appClipPicture (toRegion clipRectangle) (itemLook.lookFun selectState updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= OSreleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb
	   info					= CustomButtonInfo {info & cButtonInfoLook={itemLook & lookPen=pen}}
	= ({itemH & wItemInfo=info},tb)
where
	info					= getWItemCustomButtonInfo wItemInfo
	itemLook				= info.cButtonInfoLook
	viewFrame				= SizeToRectangle wItemSize
	selectState				= if able Able Unable
	clipRectangle			= RectToRectangle (IntersectRects (subVector (toVector wItemPos) contextClip) (SizeToRect wItemSize))
	updState				= {oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}

drawCustomButtonLook` :: !Bool !OSWindowPtr !Rect !WItemHandle` !*OSToolbox -> (!WItemHandle`,!*OSToolbox)
drawCustomButtonLook` able wPtr contextClip itemH=:{wItemPtr`,wItemInfo`,wItemPos`,wItemSize`} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb
	#! (osPict,tb)			= OSgrabWindowPictContext wPtr tb		// PA: use window HDC instead of control HDC because of clipstate
	#! picture				= packPicture (zero-wItemPos`) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! picture				= appClipPicture (toRegion clipRectangle) (itemLook.lookFun selectState updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= OSreleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb
	   info					= CustomButtonInfo` {info & cButtonInfoLook={itemLook & lookPen=pen}}
	= ({itemH & wItemInfo`=info},tb)
where
	info					= getWItemCustomButtonInfo` wItemInfo`
	itemLook				= info.cButtonInfoLook
	viewFrame				= SizeToRectangle wItemSize`
	selectState				= if able Able Unable
	clipRectangle			= RectToRectangle (IntersectRects (subVector (toVector wItemPos`) contextClip) (SizeToRect wItemSize`))
	updState				= {oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}

/*	drawCustomLook(`) able parentWindow itemH
		applies the Look function of the custom control given the current selectstate (True iff Able).
		Drawing is clipped inside contextClip and the content rectangle of the custom button control. 
		The function assumes that itemH refers to a custom control.
*/
drawCustomLook :: !Bool !OSWindowPtr !Rect !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
drawCustomLook able wPtr contextClip itemH=:{wItemPtr,wItemInfo,wItemPos,wItemSize} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb
	#! (osPict,tb)			= OSgrabWindowPictContext wPtr tb		// PA: use window HDC instead of control HDC because of clipstate
	#! picture				= packPicture (zero-wItemPos) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! picture				= appClipPicture (toRegion clipRectangle) (itemLook.lookFun selectState updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= OSreleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb
	   info					= CustomInfo {info & customInfoLook={itemLook & lookPen=pen}}
	= ({itemH & wItemInfo=info},tb)
where
	info					= getWItemCustomInfo wItemInfo
	itemLook				= info.customInfoLook
	viewFrame				= SizeToRectangle wItemSize
	selectState				= if able Able Unable
	clipRectangle			= RectToRectangle (IntersectRects (subVector (toVector wItemPos) contextClip) (SizeToRect wItemSize))
	updState				= {oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}

drawCustomLook` :: !Bool !OSWindowPtr !Rect !WItemHandle` !*OSToolbox -> (!WItemHandle`,!*OSToolbox)
drawCustomLook` able wPtr contextClip itemH=:{wItemPtr`,wItemInfo`,wItemPos`,wItemSize`} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb
	#! (osPict,tb)			= OSgrabWindowPictContext wPtr tb		// PA: use window HDC instead of control HDC because of clipstate
	#! picture				= packPicture (zero-wItemPos`) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! picture				= appClipPicture (toRegion clipRectangle) (itemLook.lookFun selectState updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= OSreleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb
	   info					= CustomInfo` {info & customInfoLook={itemLook & lookPen=pen}}
	= ({itemH & wItemInfo`=info},tb)
where
	info					= getWItemCustomInfo` wItemInfo`
	itemLook				= info.customInfoLook
	viewFrame				= SizeToRectangle wItemSize`
	selectState				= if able Able Unable
	clipRectangle			= RectToRectangle (IntersectRects (subVector (toVector wItemPos`) contextClip) (SizeToRect wItemSize`))
	updState				= {oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}


/*	The following functions apply a picture access function to the given control picture.
*/

/*	drawInCompound(`) assumes that the WItemHandle(`) argument refers to a non transparent compound control 
	with a valid ClipState.
*/
drawInCompound :: !OSWindowPtr !.(St *Picture .x) !Rect !(WItemHandle .ls .pst) !*OSToolbox -> (.x,!WItemHandle .ls .pst,!*OSToolbox)
drawInCompound wPtr drawfun contextClip itemH=:{wItemPtr,wItemInfo,wItemPos,wItemSize} tb
	#! (contextRgn,tb)			= osnewrectrgn contextClip tb
	#! (clipRgn,tb)				= ossectrgn contextRgn compoundClip.clipRgn tb
	#! (osPict,tb)				= OSgrabWindowPictContext wPtr tb		// PA: use window HDC instead of control HDC because of clipstate
	#! picture					= packPicture (origin-wItemPos) (copyPen compoundLook.lookPen) True osPict tb
	#! picture					= pictsetcliprgn clipRgn picture
	#! (x,picture)				= drawfun picture
	#! (_,pen,_,osPict,tb)		= unpackPicture picture
	#! tb						= OSreleaseWindowPictContext wPtr osPict tb
	#! tb						= StateMap2 osdisposergn [contextRgn,clipRgn] tb
	   info						= {info & compoundLookInfo=Just {compoundLookInfo & compoundLook={compoundLook & lookPen=pen}}}
	   itemH					= {itemH & wItemInfo=CompoundInfo info}
	= (x,itemH,tb)
where
	info						= getWItemCompoundInfo wItemInfo
	origin						= info.compoundOrigin
	compoundLookInfo			= fromJust info.compoundLookInfo
	{compoundLook,compoundClip}	= compoundLookInfo

drawInCompound` :: !OSWindowPtr !.(St *Picture .x) !Rect !WItemHandle` !*OSToolbox -> (.x,!WItemHandle`,!*OSToolbox)
drawInCompound` wPtr drawfun contextClip itemH=:{wItemPtr`,wItemInfo`,wItemPos`,wItemSize`} tb
	#! (contextRgn,tb)			= osnewrectrgn contextClip tb
	#! (clipRgn,tb)				= ossectrgn contextRgn compoundClip.clipRgn tb
	#! (osPict,tb)				= OSgrabWindowPictContext wPtr tb		// PA: use window HDC instead of control HDC because of clipstate
	#! picture					= packPicture (origin-wItemPos`) (copyPen compoundLook.lookPen) True osPict tb
	#! picture					= pictsetcliprgn clipRgn picture
	#! (x,picture)				= drawfun picture
	#! (_,pen,_,osPict,tb)		= unpackPicture picture
	#! tb						= OSreleaseWindowPictContext wPtr osPict tb
	#! tb						= StateMap2 osdisposergn [contextRgn,clipRgn] tb
	   info						= {info & compoundLookInfo=Just {compoundLookInfo & compoundLook={compoundLook & lookPen=pen}}}
	   itemH					= {itemH & wItemInfo`=CompoundInfo` info}
	= (x,itemH,tb)
where
	info						= getWItemCompoundInfo` wItemInfo`
	origin						= info.compoundOrigin
	compoundLookInfo			= fromJust info.compoundLookInfo
	{compoundLook,compoundClip}	= compoundLookInfo

drawInCustomButton :: !OSWindowPtr !.(St *Picture .x) !Rect !(WItemHandle .ls .ps) !*OSToolbox -> (.x,!WItemHandle .ls .ps,!*OSToolbox)
drawInCustomButton wPtr drawfun contextClip itemH=:{wItemPtr,wItemInfo,wItemPos,wItemSize} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb						// PA+++: clip also inside contextClip
	#! (osPict,tb)			= OSgrabWindowPictContext wPtr tb
	#! picture				= packPicture (zero-wItemPos) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture					// PA+++: set new clipping region
	#! (x,picture)			= accClipPicture (toRegion (SizeToRectangle wItemSize)) drawfun picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= OSreleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb							// PA+++: dispose clipping region
	   info					= {info & cButtonInfoLook={itemLook & lookPen=pen}}
	   itemH				= {itemH & wItemInfo=CustomButtonInfo info}
	= (x,itemH,tb)
where
	info					= getWItemCustomButtonInfo wItemInfo
	itemLook				= info.cButtonInfoLook

drawInCustomButton` :: !OSWindowPtr !.(St *Picture .x) !Rect !WItemHandle` !*OSToolbox -> (.x,!WItemHandle`,!*OSToolbox)
drawInCustomButton` wPtr drawfun contextClip itemH=:{wItemPtr`,wItemInfo`,wItemPos`,wItemSize`} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb						// PA+++: clip also inside contextClip
	#! (osPict,tb)			= OSgrabWindowPictContext wPtr tb
	#! picture				= packPicture (zero-wItemPos`) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture					// PA+++: set new clipping region
	#! (x,picture)			= accClipPicture (toRegion (SizeToRectangle wItemSize`)) drawfun picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= OSreleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb							// PA+++: dispose clipping region
	   info					= {info & cButtonInfoLook={itemLook & lookPen=pen}}
	   itemH				= {itemH & wItemInfo`=CustomButtonInfo` info}
	= (x,itemH,tb)
where
	info					= getWItemCustomButtonInfo` wItemInfo`
	itemLook				= info.cButtonInfoLook

drawInCustom :: !OSWindowPtr !.(St *Picture .x) !Rect !(WItemHandle .ls .ps) !*OSToolbox -> (.x,!WItemHandle .ls .ps,!*OSToolbox)
drawInCustom wPtr drawfun contextClip itemH=:{wItemPtr,wItemInfo,wItemPos,wItemSize} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb						// PA+++: clip also inside contextClip
	#! (osPict,tb)			= OSgrabWindowPictContext wPtr tb
	#! picture				= packPicture (zero-wItemPos) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture					// PA+++: set new clipping region
	#! (x,picture)			= accClipPicture (toRegion (SizeToRectangle wItemSize)) drawfun picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= OSreleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb							// PA+++: dispose clipping region
	   info					= {info & customInfoLook={itemLook & lookPen=pen}}
	   itemH				= {itemH & wItemInfo=CustomInfo info}
	= (x,itemH,tb)
where
	info					= getWItemCustomInfo wItemInfo
	itemLook				= info.customInfoLook

drawInCustom` :: !OSWindowPtr !.(St *Picture .x) !Rect !WItemHandle` !*OSToolbox -> (.x,!WItemHandle`,!*OSToolbox)
drawInCustom` wPtr drawfun contextClip itemH=:{wItemPtr`,wItemInfo`,wItemPos`,wItemSize`} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb						// PA+++: clip also inside contextClip
	#! (osPict,tb)			= OSgrabWindowPictContext wPtr tb
	#! picture				= packPicture (zero-wItemPos`) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture					// PA+++: set new clipping region
	#! (x,picture)			= accClipPicture (toRegion (SizeToRectangle wItemSize`)) drawfun picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= OSreleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb							// PA+++: dispose clipping region
	   info					= {info & customInfoLook={itemLook & lookPen=pen}}
	   itemH				= {itemH & wItemInfo`=CustomInfo` info}
	= (x,itemH,tb)
where
	info					= getWItemCustomInfo` wItemInfo`
	itemLook				= info.customInfoLook
