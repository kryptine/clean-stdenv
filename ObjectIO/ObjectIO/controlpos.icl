implementation module controlpos


//	Clean Object I/O library version 1.2

import	StdBool, StdFunc, StdInt, StdList, StdMisc, StdTuple
import	commondef, windowaccess
from	controllayout	import layoutControls
from	controlrelayout	import relayoutControls
from	windowclipstate	import forceValidWindowClipState
from	windowdefaccess	import isWindowItemSpace,    getWindowItemSpaceAtt,
								isWindowHMargin,     getWindowHMarginAtt,
								isWindowVMargin,     getWindowVMarginAtt
from	windowdraw		import drawwindowlook`
from	windowupdate	import updatewindowbackgrounds
from	ospicture		import pictscroll
from	osrgn			import osgetrgnbox
from	ostypes			import Rect
from	ostoolbox		import OSToolbox
from	oswindow		import OSWindowMetrics, OSscrollbarsAreVisible, OSsetWindowSliderThumb, toOSscrollbarRange, OSMinWindowSize


controlposFatalError :: String String -> .x
controlposFatalError function error
	= FatalError function "controlpos" error

/*	movewindowviewframe moves the current view frame of the WindowHandle by the given Vector2. 
	movewindowviewframe assumes that the argument WindowHandle is a Window.
*/
movewindowviewframe :: !OSWindowMetrics !Vector2 !WIDS !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,!*OSToolbox)
movewindowviewframe wMetrics v wids=:{wPtr} wH=:{whWindowInfo,whItems=oldItems,whSize,whAtts,whSelect} tb
	| newOrigin==oldOrigin
		= (wH,tb)
	| otherwise
		# tb					= setsliderthumb hasHScroll wMetrics wPtr True  (minx,newOrigin.x,maxx) vieww (toTuple whSize) tb
		# tb					= setsliderthumb hasVScroll wMetrics wPtr False (miny,newOrigin.y,maxy) viewh (toTuple whSize) tb
		  reqSize				= {w=contentSize.w-fst hMargins-snd hMargins,h=contentSize.h-fst vMargins-snd vMargins}
		# (_,newItems,tb)		= layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(domain,newOrigin)] oldItems tb
		  windowInfo			= {windowInfo & windowOrigin=newOrigin}
		  wH					= {wH & whItems=newItems,whWindowInfo=WindowInfo windowInfo}
		# (wH,tb)				= forceValidWindowClipState wMetrics True wPtr wH tb
		# (isRect,areaRect,tb)	= case wH.whWindowInfo of
		  							WindowInfo {windowClip={clipRgn}} -> osgetrgnbox clipRgn tb
		  							_                                 -> controlposFatalError "movewindowviewframe" "unexpected whWindowInfo field"
		# (updRgn,tb)			= relayoutControls wMetrics whSelect contentRect contentRect zero zero wPtr wH.whDefaultId oldItems wH.whItems tb
		# (wH,tb)				= updatewindowbackgrounds wMetrics updRgn wids wH tb
		  newFrame				= PosSizeToRectangle newOrigin contentSize
		  toMuch				= (abs (newOrigin.x-oldOrigin.x)>=w`) || (abs (newOrigin.y-oldOrigin.y)>=h`)
		  (updArea,updAction)	= if (not lookInfo.lookSysUpdate || toMuch || not isRect)
		  							([newFrame],return []) (calcScrollUpdateArea oldOrigin newOrigin areaRect)
		  updState				= {oldFrame=PosSizeToRectangle oldOrigin contentSize,newFrame=newFrame,updArea=updArea}
		# (wH,tb)				= drawwindowlook` wMetrics wPtr updAction updState wH tb
		= (wH,tb)
where
	windowInfo					= getWindowInfoWindowData whWindowInfo
	(oldOrigin,domainRect,hasHScroll,hasVScroll,lookInfo)
								= (windowInfo.windowOrigin,windowInfo.windowDomain,isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll,windowInfo.windowLook)
	domain						= RectToRectangle domainRect
	visScrolls					= OSscrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
	contentRect					= getWindowContentRect wMetrics visScrolls (SizeToRect whSize)
	contentSize					= RectSize contentRect
	{w=w`,h=h`}					= contentSize
	(minx,maxx,vieww)			= (domainRect.rleft,domainRect.rright, contentSize.w)
	(miny,maxy,viewh)			= (domainRect.rtop, domainRect.rbottom,contentSize.h)
	newOrigin					= {	x = SetBetween (oldOrigin.x+v.vx) minx (max minx (maxx-vieww))
								  ,	y = SetBetween (oldOrigin.y+v.vy) miny (max miny (maxy-viewh))
								  }
	(defMinW,defMinH)			= OSMinWindowSize
	minSize						= {w=defMinW,h=defMinH}
	(defHSpace,defVSpace)		= (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)
	hMargins					= getWindowHMarginAtt   (snd (Select isWindowHMargin   (WindowHMargin 0 0) whAtts))
	vMargins					= getWindowVMarginAtt   (snd (Select isWindowVMargin   (WindowVMargin 0 0) whAtts))
	spaces						= getWindowItemSpaceAtt (snd (Select isWindowItemSpace (WindowItemSpace defHSpace defVSpace) whAtts))
	
	setsliderthumb :: !Bool !OSWindowMetrics !OSWindowPtr !Bool !(!Int,!Int,!Int) !Int !(!Int,!Int) !*OSToolbox -> *OSToolbox
	setsliderthumb hasScroll wMetrics wPtr isHScroll scrollValues viewSize maxcoords tb
		| hasScroll				= OSsetWindowSliderThumb wMetrics wPtr isHScroll osThumb maxcoords True tb
		| otherwise				= tb
	where
		(_,osThumb,_,_)			= toOSscrollbarRange scrollValues viewSize
	
/*	calcScrollUpdateArea p1 p2 area calculates the new update area that has to be updated. 
	Assumptions: p1 is the origin before scrolling,
	             p2 is the origin after  scrolling,
	             area is the visible area of the window view frame.
*/
	calcScrollUpdateArea :: !Point2 !Point2 !Rect -> (![Rectangle],!St *Picture [Rect])
	calcScrollUpdateArea oldOrigin newOrigin areaRect
		= (map RectToRectangle updArea,scroll {newOriginAreaRect & rright=rright+1,rbottom=rbottom+1} restArea v)
	where
		newOriginAreaRect			= addVector (toVector newOrigin) areaRect
		{rleft,rtop,rright,rbottom}	= newOriginAreaRect
		v							= toVector (oldOrigin-newOrigin)
		{vx,vy}						= v
		(updArea,restArea)			= if (vx<=0 && vy<=0)
										(	[{newOriginAreaRect & rleft=rright+vx,rbottom=rbottom+vy},{newOriginAreaRect & rtop=rbottom+vy}]
										,	 {newOriginAreaRect & rright=rright+vx,rbottom=rbottom+vy}
										)
									 (if (vx<=0 && vy>=0)
									 	(	[{newOriginAreaRect & rbottom=rtop+vy},{newOriginAreaRect & rleft=rright+vx,rtop=rtop+vy}]
									 	,	 {newOriginAreaRect & rtop=rtop+vy,rright=rright+vx}
									 	)
									 (if (vx>=0 && vy<=0)
									 	(	[{newOriginAreaRect & rright=rleft+vx,rbottom=rbottom+vy},{newOriginAreaRect & rtop=rbottom+vy}]
									 	,	 {newOriginAreaRect & rleft=rleft+vx,rbottom=rbottom+vy}
									 	)
								//	  if (vx>=0 && vy>=0)
									 	(	[{newOriginAreaRect & rbottom=rtop+vy},{newOriginAreaRect & rtop=rtop+vy,rright=rleft+vx}]
									 	,	 {newOriginAreaRect & rleft=rleft+vx,rtop=rtop+vy}
									 	)))
		
		scroll :: !Rect !Rect !Vector2 !*Picture -> (![Rect],!*Picture)
		scroll scrollRect restRect v picture
			# (updRect,picture)	= pictscroll scrollRect v picture
			| updRect==zero
				= ([],picture)
			| otherwise
				= ([restRect],picture)
