implementation module windowdraw


//	Clean Object I/O library, version 1.2

//	Drawing operations on windows.

import	StdBool, StdFunc, StdInt, StdList
import	ospicture, osrgn, oswindow
import	commondef, controllayout, StdPicture, windowaccess


/*	drawwindowlook wPtr includeBackground window
		applies the Look function of window.
	The wPtr argument must be the OSWindowPtr of window.
	It is assumed that window refers to a Window with a valid ClipState.
	If includeBackground is True then also the background outside the WindowViewDomain is drawn.
*/
drawwindowlook :: !OSWindowMetrics !OSWindowPtr !(IdFun *Picture) !UpdateState !(WindowHandle .ls .pst) !*OSToolbox
																			-> (!WindowHandle .ls .pst, !*OSToolbox)
drawwindowlook wMetrics wPtr drawFirst updState wH=:{whSelect,whSize,whWindowInfo} tb
	#! (osPict,tb)			= OSgrabWindowPictContext wPtr tb
	#! picture				= packPicture origin (copyPen look.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! picture				= drawFirst picture
	#! picture				= appClipPicture (toRegion wFrame) (look.lookFun select updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= OSreleaseWindowPictContext wPtr osPict tb
	#! tb					= OSvalidateWindowRgn wPtr clipRgn tb		// PA: added to eliminate update of window (in drawing part)
	#! look					= {look & lookPen=pen}
	#! info					= {info & windowLook=look}
	= ({wH & whWindowInfo=WindowInfo info},tb)
where
	select					= if whSelect Able Unable
	info					= getWindowInfoWindowData whWindowInfo
	domainRect				= info.windowDomain
	origin					= info.windowOrigin
	look					= info.windowLook
	clip					= info.windowClip
	clipRgn					= clip.clipRgn
	hasScrolls				= (isJust info.windowHScroll,isJust info.windowVScroll)
	visScrolls				= OSscrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	{w,h}					= RectSize (getWindowContentRect wMetrics visScrolls (SizeToRect whSize))
	wFrame					= {corner1=origin,corner2={x=origin.x+w,y=origin.y+h}}

drawwindowlook` :: !OSWindowMetrics !OSWindowPtr !(St *Picture [Rect]) !UpdateState !(WindowHandle .ls .pst) !*OSToolbox
																				 -> (!WindowHandle .ls .pst, !*OSToolbox)
drawwindowlook` wMetrics wPtr drawFirst updState wH=:{whSelect,whSize,whWindowInfo} tb
	#! (osPict,tb)			= OSgrabWindowPictContext wPtr tb
	#! picture				= packPicture origin (copyPen look.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clipRgn picture
	#! (additionalUpdateArea,picture)
							= drawFirst picture
	   updState				= {updState & updArea = [RectToRectangle r \\ r<-additionalUpdateArea | not (IsEmptyRect r)] ++ updState.updArea}
	#! picture				= appClipPicture (toRegion wFrame) (look.lookFun select updState) picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= OSreleaseWindowPictContext wPtr osPict tb
	#! tb					= OSvalidateWindowRgn wPtr clipRgn tb		// PA: added to eliminate update of window (in drawing part)
	#! look					= {look & lookPen=pen}
	#! info					= {info & windowLook=look}
	= ({wH & whWindowInfo=WindowInfo info},tb)
where
	select					= if whSelect Able Unable
	info					= getWindowInfoWindowData whWindowInfo
	domainRect				= info.windowDomain
	origin					= info.windowOrigin
	look					= info.windowLook
	clip					= info.windowClip
	clipRgn					= clip.clipRgn
	hasScrolls				= (isJust info.windowHScroll,isJust info.windowVScroll)
	visScrolls				= OSscrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	{w,h}					= RectSize (getWindowContentRect wMetrics visScrolls (SizeToRect whSize))
	wFrame					= {corner1=origin,corner2={x=origin.x+w,y=origin.y+h}}


/*	drawinwindow wPtr drawfun window
		applies the drawing function to the picture of the window.
	The wPtr argument must be the OSWindowPtr of the window.
	It is assumed that window refers to a Window with a valid ClipState.
*/
drawinwindow :: !OSWindowMetrics !OSWindowPtr !.(St *Picture .x) !(WindowHandle .ls .pst) !*OSToolbox
														 -> (.x, ! WindowHandle .ls .pst, !*OSToolbox)
drawinwindow wMetrics wPtr drawfun wH=:{whSize,whWindowInfo} tb
	#! (domainRgn,tb)		= osnewrectrgn contentRect tb
	#! (clip,tb)			= ossectrgn domainRgn clipRgn tb
	#! (osPict,tb)			= OSgrabWindowPictContext wPtr tb
	#! picture				= packPicture origin (copyPen windowLook.lookPen) True osPict tb
	#! picture				= pictsetcliprgn clip picture
	#! (x,picture)			= drawfun picture
	#! (_,pen,_,osPict,tb)	= unpackPicture picture
	#! tb					= OSreleaseWindowPictContext wPtr osPict tb
	#! tb					= StateMap2 osdisposergn [domainRgn,clip] tb
	#! info					= {info & windowLook={windowLook & lookPen=pen}}
	= (x,{wH & whWindowInfo=WindowInfo info},tb)
where
	info					= getWindowInfoWindowData whWindowInfo
	domainRect				= info.windowDomain
	origin					= info.windowOrigin
	windowLook				= info.windowLook
	windowClip				= info.windowClip
	clipRgn					= windowClip.clipRgn
	hasScrolls				= (isJust info.windowHScroll,isJust info.windowVScroll)
	visScrolls				= OSscrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	contentRect				= getWindowContentRect wMetrics visScrolls (SizeToRect whSize)
