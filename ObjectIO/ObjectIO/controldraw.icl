implementation module controldraw


import	commondef, wstate
import	ospicture, osrgn, ossystem, oswindow
from	windowaccess	import getWItemCompoundInfo,  getWItemCustomButtonInfo,  getWItemCustomInfo
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
drawCompoundLook :: !OSWindowMetrics !Bool !OSWindowPtr !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
drawCompoundLook wMetrics able wPtr contextClip itemH=:{wItemInfo} tb
	#! (contextRgn,tb)				= osnewrectrgn contextClip tb
	#! (clipRgn,tb)					= ossectrgn contextRgn itemClip.clipRgn tb
	#! (osPict,tb)					= osGrabWindowPictContext wPtr tb
	#! picture						= packPicture (origin-itemPos) (copyPen itemLook.lookPen) True osPict tb
	#! picture						= pictsetcliprgn clipRgn picture
	#! picture						= itemLook.lookFun selectState updState picture
	#! (_,pen,_,osPict,tb)			= unpackPicture picture
	#! tb							= osReleaseWindowPictContext wPtr osPict tb
	#! tb							= stateMap2 osdisposergn [contextRgn,clipRgn] tb
	   info							= {info & compoundLookInfo={compoundLookInfo & compoundLook={itemLook & lookPen=pen}}}
	= ({itemH & wItemInfo=CompoundInfo info},tb)
where
	itemPos							= itemH.wItemPos
	itemSize						= itemH.wItemSize
	info							= getWItemCompoundInfo wItemInfo
	(origin,domainRect,hasScrolls)	= (info.compoundOrigin,info.compoundDomain,(isJust info.compoundHScroll,isJust info.compoundVScroll))
	visScrolls						= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
	contentRect						= osGetCompoundContentRect wMetrics visScrolls (posSizeToRect origin itemSize)
	clipRectangle					= rectToRectangle (addVector (toVector (origin-itemPos)) (intersectRects (addVector (toVector (itemPos-origin)) contentRect) contextClip))
	viewFrame						= rectToRectangle contentRect
	updState						= {oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}
	compoundLookInfo				= info.compoundLookInfo
	itemLook						= compoundLookInfo.compoundLook
	itemClip						= compoundLookInfo.compoundClip
	selectState						= if able Able Unable

drawCompoundLook` :: !OSWindowMetrics !Bool !OSWindowPtr !OSRect !WItemHandle` !*OSToolbox -> (!WItemHandle`,!*OSToolbox)
drawCompoundLook` wMetrics able wPtr contextClip itemH=:{wItemInfo`} tb
	#! (contextRgn,tb)				= osnewrectrgn contextClip tb
	#! (clipRgn,tb)					= ossectrgn contextRgn itemClip.clipRgn tb
	#! (osPict,tb)					= osGrabWindowPictContext wPtr tb
	#! picture						= packPicture (origin-itemPos) (copyPen itemLook.lookPen) True osPict tb
	#! picture						= pictsetcliprgn clipRgn picture
	#! picture						= itemLook.lookFun selectState updState picture
	#! (_,pen,_,osPict,tb)			= unpackPicture picture
	#! tb							= osReleaseWindowPictContext wPtr osPict tb
	#! tb							= stateMap2 osdisposergn [contextRgn,clipRgn] tb
	   info							= {info & compoundLookInfo={compoundLookInfo & compoundLook={itemLook & lookPen=pen}}}
	= ({itemH & wItemInfo`=CompoundInfo` info},tb)
where
	itemPos							= itemH.wItemPos`
	itemSize						= itemH.wItemSize`
	info							= getWItemCompoundInfo` wItemInfo`
	(origin,domainRect,hasScrolls)	= (info.compoundOrigin,info.compoundDomain,(isJust info.compoundHScroll,isJust info.compoundVScroll))
	visScrolls						= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
	contentRect						= osGetCompoundContentRect wMetrics visScrolls (posSizeToRect origin itemSize)
	clipRectangle					= rectToRectangle (addVector (toVector (origin-itemPos)) (intersectRects (addVector (toVector (itemPos-origin)) contentRect) contextClip))
	viewFrame						= rectToRectangle contentRect
	updState						= {oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}
	compoundLookInfo				= info.compoundLookInfo
	itemLook						= compoundLookInfo.compoundLook
	itemClip						= compoundLookInfo.compoundClip
	selectState						= if able Able Unable

/*	drawCustomButtonLook(`) able parentWindow contextClip itemH
		applies the Look function of the custom button control given the current selectstate (True iff Able).
		Drawing is clipped inside contextClip and the content rectangle of the custom button control. 
		The function assumes that itemH refers to a custom button control.
*/
drawCustomButtonLook :: !Bool !OSWindowPtr !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
drawCustomButtonLook able wPtr contextClip itemH=:{wItemPtr,wItemInfo,wItemPos,wItemSize} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb
	#! (osPict,tb)			= osGrabWindowPictContext wPtr tb		// PA: use window HDC instead of control HDC because of clipstate
	#! picture				= packPicture (zero-wItemPos) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! picture				= appClipPicture (toRegion clipRectangle) (itemLook.lookFun selectState updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= osReleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb
	   info					= CustomButtonInfo {info & cButtonInfoLook={itemLook & lookPen=pen}}
	= ({itemH & wItemInfo=info},tb)
where
	info					= getWItemCustomButtonInfo wItemInfo
	itemLook				= info.cButtonInfoLook
	viewFrame				= sizeToRectangle wItemSize
	selectState				= if able Able Unable
	clipRectangle			= rectToRectangle (intersectRects (subVector (toVector wItemPos) contextClip) (sizeToRect wItemSize))
	updState				= {oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}

drawCustomButtonLook` :: !Bool !OSWindowPtr !OSRect !WItemHandle` !*OSToolbox -> (!WItemHandle`,!*OSToolbox)
drawCustomButtonLook` able wPtr contextClip itemH=:{wItemPtr`,wItemInfo`,wItemPos`,wItemSize`} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb
	#! (osPict,tb)			= osGrabWindowPictContext wPtr tb		// PA: use window HDC instead of control HDC because of clipstate
	#! picture				= packPicture (zero-wItemPos`) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! picture				= appClipPicture (toRegion clipRectangle) (itemLook.lookFun selectState updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= osReleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb
	   info					= CustomButtonInfo` {info & cButtonInfoLook={itemLook & lookPen=pen}}
	= ({itemH & wItemInfo`=info},tb)
where
	info					= getWItemCustomButtonInfo` wItemInfo`
	itemLook				= info.cButtonInfoLook
	viewFrame				= sizeToRectangle wItemSize`
	selectState				= if able Able Unable
	clipRectangle			= rectToRectangle (intersectRects (subVector (toVector wItemPos`) contextClip) (sizeToRect wItemSize`))
	updState				= {oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}

/*	drawCustomLook(`) able parentWindow itemH
		applies the Look function of the custom control given the current selectstate (True iff Able).
		Drawing is clipped inside contextClip and the content rectangle of the custom button control. 
		The function assumes that itemH refers to a custom control.
*/
drawCustomLook :: !Bool !OSWindowPtr !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
drawCustomLook able wPtr contextClip itemH=:{wItemPtr,wItemInfo,wItemPos,wItemSize} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb
	#! (osPict,tb)			= osGrabWindowPictContext wPtr tb		// PA: use window HDC instead of control HDC because of clipstate
	#! picture				= packPicture (zero-wItemPos) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! picture				= appClipPicture (toRegion clipRectangle) (itemLook.lookFun selectState updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= osReleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb
	   info					= CustomInfo {info & customInfoLook={itemLook & lookPen=pen}}
	= ({itemH & wItemInfo=info},tb)
where
	info					= getWItemCustomInfo wItemInfo
	itemLook				= info.customInfoLook
	viewFrame				= sizeToRectangle wItemSize
	selectState				= if able Able Unable
	clipRectangle			= rectToRectangle (intersectRects (subVector (toVector wItemPos) contextClip) (sizeToRect wItemSize))
	updState				= {oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}

drawCustomLook` :: !Bool !OSWindowPtr !OSRect !WItemHandle` !*OSToolbox -> (!WItemHandle`,!*OSToolbox)
drawCustomLook` able wPtr contextClip itemH=:{wItemPtr`,wItemInfo`,wItemPos`,wItemSize`} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb
	#! (osPict,tb)			= osGrabWindowPictContext wPtr tb		// PA: use window HDC instead of control HDC because of clipstate
	#! picture				= packPicture (zero-wItemPos`) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! picture				= appClipPicture (toRegion clipRectangle) (itemLook.lookFun selectState updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= osReleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb
	   info					= CustomInfo` {info & customInfoLook={itemLook & lookPen=pen}}
	= ({itemH & wItemInfo`=info},tb)
where
	info					= getWItemCustomInfo` wItemInfo`
	itemLook				= info.customInfoLook
	viewFrame				= sizeToRectangle wItemSize`
	selectState				= if able Able Unable
	clipRectangle			= rectToRectangle (intersectRects (subVector (toVector wItemPos`) contextClip) (sizeToRect wItemSize`))
	updState				= {oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}


/*	The following functions apply a picture access function to the given control picture.
*/

/*	drawInCompound(`) assumes that the WItemHandle(`) argument refers to a non transparent compound control 
	with a valid ClipState.
*/
drawInCompound :: !OSWindowPtr !.(St *Picture .x) !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (.x,!WItemHandle .ls .pst,!*OSToolbox)
drawInCompound wPtr drawfun contextClip itemH=:{wItemPtr,wItemInfo,wItemPos,wItemSize} tb
	#! (contextRgn,tb)			= osnewrectrgn contextClip tb
	#! (clipRgn,tb)				= ossectrgn contextRgn compoundClip.clipRgn tb
	#! (osPict,tb)				= osGrabWindowPictContext wPtr tb		// PA: use window HDC instead of control HDC because of clipstate
	#! picture					= packPicture (origin-wItemPos) (copyPen compoundLook.lookPen) True osPict tb
	#! picture					= pictsetcliprgn clipRgn picture
	#! (x,picture)				= drawfun picture
	#! (_,pen,_,osPict,tb)		= unpackPicture picture
	#! tb						= osReleaseWindowPictContext wPtr osPict tb
	#! tb						= stateMap2 osdisposergn [contextRgn,clipRgn] tb
	   info						= {info & compoundLookInfo={compoundLookInfo & compoundLook={compoundLook & lookPen=pen}}}
	   itemH					= {itemH & wItemInfo=CompoundInfo info}
	= (x,itemH,tb)
where
	info						= getWItemCompoundInfo wItemInfo
	origin						= info.compoundOrigin
	compoundLookInfo			= info.compoundLookInfo
	{compoundLook,compoundClip}	= compoundLookInfo

drawInCompound` :: !OSWindowPtr !.(St *Picture .x) !OSRect !WItemHandle` !*OSToolbox -> (.x,!WItemHandle`,!*OSToolbox)
drawInCompound` wPtr drawfun contextClip itemH=:{wItemPtr`,wItemInfo`,wItemPos`,wItemSize`} tb
	#! (contextRgn,tb)			= osnewrectrgn contextClip tb
	#! (clipRgn,tb)				= ossectrgn contextRgn compoundClip.clipRgn tb
	#! (osPict,tb)				= osGrabWindowPictContext wPtr tb		// PA: use window HDC instead of control HDC because of clipstate
	#! picture					= packPicture (origin-wItemPos`) (copyPen compoundLook.lookPen) True osPict tb
	#! picture					= pictsetcliprgn clipRgn picture
	#! (x,picture)				= drawfun picture
	#! (_,pen,_,osPict,tb)		= unpackPicture picture
	#! tb						= osReleaseWindowPictContext wPtr osPict tb
	#! tb						= stateMap2 osdisposergn [contextRgn,clipRgn] tb
	   info						= {info & compoundLookInfo={compoundLookInfo & compoundLook={compoundLook & lookPen=pen}}}
	   itemH					= {itemH & wItemInfo`=CompoundInfo` info}
	= (x,itemH,tb)
where
	info						= getWItemCompoundInfo` wItemInfo`
	origin						= info.compoundOrigin
	compoundLookInfo			= info.compoundLookInfo
	{compoundLook,compoundClip}	= compoundLookInfo

drawInCustomButton :: !OSWindowPtr !.(St *Picture .x) !OSRect !(WItemHandle .ls .ps) !*OSToolbox -> (.x,!WItemHandle .ls .ps,!*OSToolbox)
drawInCustomButton wPtr drawfun contextClip itemH=:{wItemPtr,wItemInfo,wItemPos,wItemSize} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb						// PA+++: clip also inside contextClip
	#! (osPict,tb)			= osGrabWindowPictContext wPtr tb
	#! picture				= packPicture (zero-wItemPos) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture					// PA+++: set new clipping region
	#! (x,picture)			= accClipPicture (toRegion (sizeToRectangle wItemSize)) drawfun picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= osReleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb							// PA+++: dispose clipping region
	   info					= {info & cButtonInfoLook={itemLook & lookPen=pen}}
	   itemH				= {itemH & wItemInfo=CustomButtonInfo info}
	= (x,itemH,tb)
where
	info					= getWItemCustomButtonInfo wItemInfo
	itemLook				= info.cButtonInfoLook

drawInCustomButton` :: !OSWindowPtr !.(St *Picture .x) !OSRect !WItemHandle` !*OSToolbox -> (.x,!WItemHandle`,!*OSToolbox)
drawInCustomButton` wPtr drawfun contextClip itemH=:{wItemPtr`,wItemInfo`,wItemPos`,wItemSize`} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb						// PA+++: clip also inside contextClip
	#! (osPict,tb)			= osGrabWindowPictContext wPtr tb
	#! picture				= packPicture (zero-wItemPos`) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture					// PA+++: set new clipping region
	#! (x,picture)			= accClipPicture (toRegion (sizeToRectangle wItemSize`)) drawfun picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= osReleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb							// PA+++: dispose clipping region
	   info					= {info & cButtonInfoLook={itemLook & lookPen=pen}}
	   itemH				= {itemH & wItemInfo`=CustomButtonInfo` info}
	= (x,itemH,tb)
where
	info					= getWItemCustomButtonInfo` wItemInfo`
	itemLook				= info.cButtonInfoLook

drawInCustom :: !OSWindowPtr !.(St *Picture .x) !OSRect !(WItemHandle .ls .ps) !*OSToolbox -> (.x,!WItemHandle .ls .ps,!*OSToolbox)
drawInCustom wPtr drawfun contextClip itemH=:{wItemPtr,wItemInfo,wItemPos,wItemSize} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb						// PA+++: clip also inside contextClip
	#! (osPict,tb)			= osGrabWindowPictContext wPtr tb
	#! picture				= packPicture (zero-wItemPos) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture					// PA+++: set new clipping region
	#! (x,picture)			= accClipPicture (toRegion (sizeToRectangle wItemSize)) drawfun picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= osReleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb							// PA+++: dispose clipping region
	   info					= {info & customInfoLook={itemLook & lookPen=pen}}
	   itemH				= {itemH & wItemInfo=CustomInfo info}
	= (x,itemH,tb)
where
	info					= getWItemCustomInfo wItemInfo
	itemLook				= info.customInfoLook

drawInCustom` :: !OSWindowPtr !.(St *Picture .x) !OSRect !WItemHandle` !*OSToolbox -> (.x,!WItemHandle`,!*OSToolbox)
drawInCustom` wPtr drawfun contextClip itemH=:{wItemPtr`,wItemInfo`,wItemPos`,wItemSize`} tb
	#! (clipRgn,tb)			= osnewrectrgn contextClip tb						// PA+++: clip also inside contextClip
	#! (osPict,tb)			= osGrabWindowPictContext wPtr tb
	#! picture				= packPicture (zero-wItemPos`) (copyPen itemLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture					// PA+++: set new clipping region
	#! (x,picture)			= accClipPicture (toRegion (sizeToRectangle wItemSize`)) drawfun picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= osReleaseWindowPictContext wPtr osPict tb
	#! tb					= osdisposergn clipRgn tb							// PA+++: dispose clipping region
	   info					= {info & customInfoLook={itemLook & lookPen=pen}}
	   itemH				= {itemH & wItemInfo`=CustomInfo` info}
	= (x,itemH,tb)
where
	info					= getWItemCustomInfo` wItemInfo`
	itemLook				= info.customInfoLook
