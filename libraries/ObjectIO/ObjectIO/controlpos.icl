implementation module controlpos


//	Clean Object I/O library version 1.2

import	StdBool, StdFunc, StdMisc, StdTuple
import	commondef, windowaccess
from	controllayout	import layoutControls, getWindowContentRect
from	controlrelayout	import relayoutControls
from	windowclipstate	import forceValidWindowClipState
from	windowdefaccess	import isWindowItemSpace,    getWindowItemSpaceAtt,
								isWindowHMargin,     getWindowHMarginAtt,
								isWindowVMargin,     getWindowVMarginAtt
from	windowdraw		import drawwindowlook
from	windowupdate	import updatewindowbackgrounds
from	ostypes			import Rect
from	ostoolbox		import OSToolbox
from	oswindow		import OSWindowMetrics, OSscrollbarsAreVisible, OSsetWindowSliderThumb, toOSscrollbarRange, OSMinWindowSize


/*	movewindowviewframe moves the current view frame of the WindowHandle by the given Vector2. 
	movewindowviewframe assumes that the argument WindowHandle is a Window.
*/
movewindowviewframe :: !OSWindowMetrics !Vector2 !WIDS !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,!*OSToolbox)
movewindowviewframe wMetrics v wids=:{wPtr} wH tb
	# visScrolls				= OSscrollbarsAreVisible wMetrics (RectangleToRect domain) (toTuple wSize) (hasHScroll,hasVScroll)
	  contentRect				= getWindowContentRect wMetrics visScrolls (SizeToRect wSize)
	  contentSize				= RectSize contentRect
	  (minx,maxx,viewx)			= (domainRect.rleft,domainRect.rright, contentSize.w)
	  (miny,maxy,viewy)			= (domainRect.rtop, domainRect.rbottom,contentSize.h)
	  newOrigin					= movePoint v oldOrigin
	  newOrigin					= {x=SetBetween newOrigin.x minx (maxx-viewx),y=SetBetween newOrigin.y miny (maxy-viewy)}
	| newOrigin==oldOrigin
		= (wH,tb)
	| otherwise
		# tb					= setsliderthumb hasHScroll wMetrics wPtr True  (minx,newOrigin.x,maxx) viewx (toTuple wSize) tb
		# tb					= setsliderthumb hasVScroll wMetrics wPtr False (miny,newOrigin.y,maxy) viewy (toTuple wSize) tb
		  (defHSpace,defVSpace)	= (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)
		  hMargins				= getWindowHMarginAtt   (snd (Select isWindowHMargin   (WindowHMargin 0 0) atts))
		  vMargins				= getWindowVMarginAtt   (snd (Select isWindowVMargin   (WindowVMargin 0 0) atts))
		  spaces				= getWindowItemSpaceAtt (snd (Select isWindowItemSpace (WindowItemSpace defHSpace defVSpace) atts))
		  reqSize				= {w=contentSize.w-fst hMargins-snd hMargins,h=contentSize.h-fst vMargins-snd vMargins}
		# (_,newItems,tb)		= layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(domain,newOrigin)] oldItems tb
		  windowInfo			= {windowInfo & windowOrigin=newOrigin}
	//	  wH					= {wH & whItems=newItems,whWindowInfo=Just windowInfo}	Mike: changed Just into WindowInfo
		  wH					= {wH & whItems=newItems,whWindowInfo=WindowInfo windowInfo}
		# (wH,tb)				= forceValidWindowClipState wMetrics True wPtr wH tb
		# (updRgn,tb)			= relayoutControls wMetrics whSelect contentRect contentRect zero zero wPtr whDefaultId oldItems wH.whItems tb
		# (wH,tb)				= updatewindowbackgrounds wMetrics updRgn wids wH tb
		  newFrame				= PosSizeToRectangle newOrigin contentSize			// PA: drawinwindow might be redundant because of updatewindowbackgrounds
		  updState				= {oldFrame=PosSizeToRectangle oldOrigin contentSize,newFrame=newFrame,updArea=[newFrame]}
		# (wH,tb)				= drawwindowlook wMetrics wPtr id updState wH tb
		= (wH,tb)
where
	wSize						= wH.whSize
//	windowInfo					= fromJust wH.whWindowInfo	Mike: changed fromJust into getWindowInfoWindowData
	windowInfo					= getWindowInfoWindowData wH.whWindowInfo
	domainRect					= windowInfo.windowDomain
	domain						= RectToRectangle domainRect
	oldOrigin					= windowInfo.windowOrigin
	hasHScroll					= isJust windowInfo.windowHScroll
	hasVScroll					= isJust windowInfo.windowVScroll
	atts						= wH.whAtts
	oldItems					= wH.whItems
	whSelect					= wH.whSelect
	whDefaultId					= wH.whDefaultId
	(defMinW,defMinH)			= OSMinWindowSize
	minSize						= {w=defMinW,h=defMinH}
	
	setsliderthumb :: !Bool OSWindowMetrics OSWindowPtr Bool (Int,Int,Int) Int (Int,Int) !*OSToolbox -> *OSToolbox
	setsliderthumb hasScroll wMetrics wPtr isHScroll scrollValues viewSize maxcoords tb
		| hasScroll				= OSsetWindowSliderThumb wMetrics wPtr isHScroll osThumb maxcoords True tb
		| otherwise				= tb
	where
		(_,osThumb,_,_)			= toOSscrollbarRange scrollValues viewSize
