implementation module windowvalidate


//	Clean Object I/O library, version 1.2

//	Window validation.


import	StdBool, StdList, StdFunc, StdTuple, StdMisc
from	StdSystem		import maxScrollWindowSize
import	osdocumentinterface, ostypes, oswindow
from	ospicture		import defaultPen, setPenAttribute
from	ossystem		import OSscreenrect, OSstripOuterSize
import	commondef, controllayout, keyfocus, StdControlAttribute, StdId, StdWindowAttribute, windowaccess
from	iostate			import IOSt, IOStGetIdTable


windowvalidateError :: String String -> .x
windowvalidateError function error
	= Error function "windowvalidate" error

windowvalidateFatalError :: String String -> .x
windowvalidateFatalError function error
	= FatalError function "windowvalidate" error


/*	validateWindowId checks whether the Id of the window/dialogue has already been bound.
	If so, Nothing is returned; otherwise a proper Id value for the window/dialogue is returned.
	The Id is not bound.
*/
validateWindowId :: !(Maybe Id) !(IOSt .l) -> (!Maybe Id,!IOSt .l)
validateWindowId Nothing ioState
	# (wId,ioState)				= openId ioState
	= (Just wId,ioState)
validateWindowId (Just id) ioState
	# (idtable,ioState)			= IOStGetIdTable ioState
	| memberIdTable id idtable	= (Nothing,ioState)
	| otherwise					= (Just id,ioState)


/*	Validate the given window.
*/
validateWindow :: !OSWindowMetrics !OSDInfo !(WindowHandle .ls .pst) !(WindowHandles .pst) !*OSToolbox
		   -> (!Index,!Point2,!Size,!Vector2,!WindowHandle .ls .pst,  !WindowHandles .pst, !*OSToolbox)

validateWindow wMetrics _ wH=:{whMode=mode,whKind=IsDialog,whItemNrs,whItems,whAtts} windows tb
	# atts						= filter isValidDialogAttribute whAtts
	  (index,atts,windows)		= validateWindowIndex mode				atts windows
	  (pos,  atts,windows)		= validateWindowPos   mode				atts windows
	  sizeAtt					= attrSize								atts			// Retrieve Window(View/Outer)Size (identical for Dialogs)
	  (hMargins,vMargins)		= attrMargins         IsDialog wMetrics	atts
	  spaces					= getWindowItemSpaces IsDialog wMetrics	atts
	  (defid,whItems)			= getOkId								atts whItems
	  (canid,whItems)			= getCancelId							atts whItems
	  (atts, whItems)			= validateWindowInitActive				atts whItems
	  reqSize					= determineRequestedSize zero sizeAtt
	  (minWidth,minHeight)		= OSMinWindowSize
	  minSize					= {w=minWidth,h=minHeight}
	  domain					= SizeToRectangle reqSize
	# (derSize,items,tb)		= layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(domain,zero)] whItems tb
	  (itemNrs,items)			= genWElementItemNrs whItemNrs items
	  (focusItems,items)		= getWElementKeyFocusIds True items
	  derSize					= determineRequestedSize derSize sizeAtt
	  domain					= SizeToRectangle derSize
	# okSize					= exactWindowSize wMetrics domain derSize False False IsDialog
	# (okPos,windows,tb)		= exactWindowPos wMetrics okSize pos IsDialog mode windows tb
	= (	index
	  ,	okPos
	  ,	okSize
	  , zero
	  ,	{	wH	&	whItemNrs	= itemNrs
				,	whKeyFocus	= newFocusItems focusItems
				,	whItems		= items
				,	whSelect	= True
				,	whAtts		= atts
				,	whDefaultId	= defid
				,	whCancelId	= canid
				,	whSize		= okSize
		}
	  ,	windows
	  ,	tb
	  )

validateWindow wMetrics osdInfo wH=:{whKind=IsWindow,whItemNrs,whItems,whAtts} windows tb
	# atts						= filter isValidWindowAttribute whAtts
	  mode						= Modeless
	  (domain,atts)				= validateWindowDomain					atts
	  (maybe_hScroll,atts)		= validateWindowHScroll					atts
	  (maybe_vScroll,atts)		= validateWindowVScroll					atts
	  (sysLook,look,atts)		= validateWindowLook					atts
	  (reqSize,atts,tb)			= validateWindowSize wMetrics domain isMDI True (isJust maybe_hScroll,isJust maybe_vScroll) atts tb
	  (index,atts,windows)		= validateWindowIndex mode				atts windows
	  (pos,  atts,windows)		= validateWindowPos   mode				atts windows
	  (penAtts,atts)			= attrPen								atts
	  (hMargins,vMargins)		= attrMargins         IsWindow wMetrics	atts
	  spaces					= getWindowItemSpaces IsWindow wMetrics	atts
	  isAble					= attrSelectState						atts
	  (defid,whItems)			= getOkId								atts whItems
	  (canid,whItems)			= getCancelId							atts whItems
	  (atts, whItems)			= validateWindowInitActive				atts whItems
	  pen						= StateMap2 setPenAttribute (reverse penAtts) defaultPen
	# (derSize,items,tb)		= layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(domain,domain.corner1)] whItems tb
	  (itemNrs,items)			= genWElementItemNrs whItemNrs items
	  (focusItems,items)		= getWElementKeyFocusIds True items
	  (origin,atts)				= validateOrigin derSize domain atts
	# okSize					= exactWindowSize wMetrics domain derSize (isJust maybe_hScroll) (isJust maybe_vScroll) IsWindow
	# (okPos,windows,tb)		= exactWindowPos wMetrics okSize pos IsWindow mode windows tb
	  (hScroll,vScroll)			= validScrollInfos wMetrics okSize maybe_hScroll maybe_vScroll
	= (	index
	  ,	okPos
	  ,	okSize
	  , toVector (origin-domain.corner1)
	  ,	{	wH	&	whItemNrs	= itemNrs
				,	whKeyFocus	= newFocusItems focusItems
				,	whWindowInfo= WindowInfo
									{	windowDomain	= RectangleToRect domain
									,	windowOrigin	= domain.corner1
									,	windowHScroll	= hScroll
									,	windowVScroll	= vScroll
									,	windowLook		= {lookFun=look,lookPen=pen,lookSysUpdate=sysLook}
									,	windowClip		= {clipRgn=0,clipOk=False}
									}
				,	whItems		= items
				,	whSelect	= isAble
				,	whAtts		= atts
				,	whDefaultId	= defid
				,	whCancelId	= canid
				,	whSize		= okSize
		}
	  ,	windows
	  ,	tb
	  )
where
	minSize			= fromTuple OSMinWindowSize
	isMDI			= getOSDInfoDocumentInterface osdInfo == MDI
	
	validScrollInfos :: OSWindowMetrics !Size !(Maybe ScrollFunction) !(Maybe ScrollFunction) -> (!Maybe ScrollInfo,!Maybe ScrollInfo)
	validScrollInfos wMetrics wSize maybe_hScroll maybe_vScroll
		= (mapMaybe (scrollInfo hScrollRect) maybe_hScroll,mapMaybe (scrollInfo vScrollRect) maybe_vScroll)
	where
		windowRect	= SizeToRect wSize
		hasScrolls	= (isJust maybe_hScroll,isJust maybe_vScroll)
		hScrollRect	= getWindowHScrollRect wMetrics hasScrolls windowRect
		vScrollRect	= getWindowVScrollRect wMetrics hasScrolls windowRect
		
		scrollInfo :: Rect !ScrollFunction -> ScrollInfo
		scrollInfo r=:{rleft,rtop} scrollFun
			= {	scrollFunction	= scrollFun
			  ,	scrollItemPos	= {x=rleft,y=rtop}
  			  ,	scrollItemSize	= RectSize r
			  ,	scrollItemPtr	= OSNoWindowPtr
			  }

validateWindow wMetrics osdInfo wH=:{whKind=IsGameWindow,whWindowInfo} windows tb
    = (0,zero,okSize,zero,{wH & whSize=okSize},windows,tb)
where
	okSize	= (getWindowInfoGameWindowData whWindowInfo).gamewindowSize


determineRequestedSize :: Size !(Maybe Size) -> Size
determineRequestedSize size Nothing	= size
determineRequestedSize _ (Just size)= size


/*	validateWindowIndex validates the WindowIndex attribute. 
	The return Index is the validated Index. 
	The return WindowAttribute list does not contain a WindowIndex attribute.
*/
validateWindowIndex :: !WindowMode ![WindowAttribute *(.ls,.pst)] !(WindowHandles .pst)
						-> (!Index,![WindowAttribute *(.ls,.pst)], !WindowHandles .pst)
validateWindowIndex mode atts windows=:{whsWindows}
	= (okIndex,atts`,{windows & whsWindows=modal`++modeless`})
where
	(_,indexAtt,atts`)		= Remove isWindowIndex (WindowIndex 0) atts
	index					= getWindowIndexAtt indexAtt
	(modal,modeless)		= Uspan isModalWindow whsWindows
	(nrModals,modal`)		= Ulength modal
	(nrModeless,modeless`)	= Ulength modeless
	okIndex					= if (mode==Modal)
	  							 0													// Open modal windows frontmost
	  							 (SetBetween index nrModals (nrModals+nrModeless))	// Open modeless windows behind the modal windows
	
	isModalWindow :: !(WindowStateHandle .pst) -> *(!Bool,!WindowStateHandle .pst)
	isModalWindow wsH
		# (mode,wsH)	= getWindowStateHandleWindowMode wsH
		= (mode==Modal,wsH)


/*	validateWindowPos validates the WindowPos attribute.
	If no WindowPos attribute is given then Nothing is returned.
	If the WindowPos is relative, it is verified that the window relates to an existing window.
	If this is not the case, then Nothing is returned.
	The resulting attribute list does not contain the WindowPos attribute anymore.
*/
validateWindowPos :: !WindowMode ![WindowAttribute *(.ls,.pst)] !(WindowHandles .pst)
			  -> (!Maybe ItemPos,![WindowAttribute *(.ls,.pst)], !WindowHandles .pst)
validateWindowPos mode atts windows
	| not hasPosAtt
		= (Nothing,atts`,windows)
	| not isRelative
		= (Just itemPos,atts`,windows)
	| otherwise
		# (found,windows)	= hasWindowHandlesWindow (toWID relativeTo) windows
		= (if found (Just itemPos) Nothing,atts`,windows)
where
	(hasPosAtt,posAtt,atts`)= Remove isWindowPos undef atts
	itemPos					= getWindowPosAtt posAtt
	(isRelative,relativeTo)	= isRelativeItemPos itemPos


/*	The result ({corner1=A,corner2=B},_) of validateWindowDomain is such that A<B (point A lies to 
	the left of and above point B). If either A.x==B.x or A.y==B.y then the ViewDomain is illegal and 
	the computation is aborted. 
	The default ViewDomain is maximal and positive, i.e.:
		{viewDomainRange & corner1=zero}.
*/
validateWindowDomain :: ![WindowAttribute .st] -> (!ViewDomain,![WindowAttribute .st])
validateWindowDomain atts
	# (hasDomain,domainAtt,atts)= Remove isWindowViewDomain undef atts
	| not hasDomain
		= ({viewDomainRange & corner1=zero},atts)
	# domain					= getWindowViewDomainAtt domainAtt
	| IsEmptyRectangle domain
		= windowvalidateError "validateWindowDomain" "Window has illegal ViewDomain argument"
	| otherwise
		= (validateViewDomain domain,atts)

validateViewDomain :: !ViewDomain -> ViewDomain
validateViewDomain domain
	= {corner1={x=SetBetween dl rl rr,y=SetBetween dt rt rb},corner2={x=SetBetween dr rl rr,y=SetBetween db rt rb}}
where
	{rleft=dl,rtop=dt,rright=dr,rbottom=db}	= RectangleToRect domain
	{rleft=rl,rtop=rt,rright=rr,rbottom=rb}	= RectangleToRect viewDomainRange


/*	validateWindowSize wMetrics viewDomain isMDI isResizable (hasHScroll,hasVScroll) atts
		takes care that the Window(View/Outer)Size attribute fits on the current screen.
		The Boolean  isMDI should be True iff the window belongs to a MDI process.
		The Boolean  isResizable should be True iff the window is resizable. 
		The Booleans hasHScroll hasVScroll should be True iff the window has the WindowHScroll, WindowVScroll
		attribute set respectively. 
		In addition, the WindowOuterSize attribute is mapped to WindowViewSize attribute.
*/
validateWindowSize :: !OSWindowMetrics !ViewDomain !Bool !Bool !(!Bool,!Bool) ![WindowAttribute .st] !*OSToolbox
																	-> (!Size,![WindowAttribute .st],!*OSToolbox)
validateWindowSize wMetrics domain isMDI isResizable hasScrolls atts tb
	| not hasSize
		= (pictSize,[WindowViewSize pictSize:atts],tb)
	with
		domainSize		= rectangleSize domain
		pictSize		= {w=min domainSize.w maxSize.w,h=min domainSize.h maxSize.h}
	| isWindowViewSize sizeAtt
		= (size1,snd (Replace isWindowViewSize (WindowViewSize size1) atts),tb)
	with
		size			= getWindowViewSizeAtt sizeAtt
		size1			= {w=SetBetween size.w (fst minSize) maxSize.w,h=SetBetween size.h (snd minSize) maxSize.h}
	| otherwise
		# ((dw,dh),tb)	= OSstripOuterSize isMDI isResizable tb
		  (w,h)			= (outerSize.w-dw,outerSize.h-dh)
		  visScrolls	= OSscrollbarsAreVisible wMetrics (RectangleToRect domain) (w,h) hasScrolls
		  viewSize		= RectSize (getWindowContentRect wMetrics visScrolls (SizeToRect {w=w,h=h}))
		# (_,_,atts)	= Remove isWindowOuterSize undef atts
		# (_,_,atts)	= Remove isWindowViewSize  undef atts
		= (viewSize,[WindowViewSize viewSize:atts],tb)
	with
		outerSize		= getWindowOuterSizeAtt sizeAtt
where
	(hasSize,sizeAtt)	= Select (\att->isWindowViewSize att || isWindowOuterSize att) undef atts
	minSize				= OSMinWindowSize
	maxSize				= maxScrollWindowSize


/*	validateOrigin takes care that the WindowOrigin attribute is a point in the rectangle
	formed by the left top of the (validated!) ViewDomain, and the width and height of the 
	(validated!) derived size.
*/
validateOrigin :: !Size !ViewDomain ![WindowAttribute .st] -> (!Point2,![WindowAttribute .st])
validateOrigin {w,h} domain=:{corner1={x=l,y=t},corner2={x=r,y=b}} atts
	# (_,domainAtt,atts)	= Remove isWindowOrigin (WindowOrigin domain.corner1) atts
	  origin				= getWindowOriginAtt domainAtt
	= ({x=SetBetween origin.x l (max l (r-w)),y=SetBetween origin.y t (max t (b-h))},atts)


/*	validateWindow(H/V)Scroll removes the Window(H/V)Scroll attribute from the attribute list. 
*/
validateWindowHScroll :: ![WindowAttribute .st] -> (!Maybe ScrollFunction,![WindowAttribute .st])
validateWindowHScroll atts
	# (found,scrollAtt,atts)	= Remove isWindowHScroll undef atts
	| found						= (Just (getWindowHScrollFun scrollAtt),atts)
	| otherwise					= (Nothing,atts)

validateWindowVScroll :: ![WindowAttribute .st] -> (!Maybe ScrollFunction,![WindowAttribute .st])
validateWindowVScroll atts
	# (found,scrollAtt,atts)	= Remove isWindowVScroll undef atts
	| found						= (Just (getWindowVScrollFun scrollAtt),atts)
	| otherwise					= (Nothing,atts)


/*	validateWindowLook takes care that the optional WindowLook attribute is removed from the attribute list.
	If no attribute was present, then a default look is provided that paints the window with the background colour
	using standard window update mechanism.
*/
validateWindowLook :: ![WindowAttribute .st] -> (!Bool,!Look,![WindowAttribute .st])
validateWindowLook atts
	# (_,lookAtt,atts)	= Remove isWindowLook (WindowLook True defaultlook) atts
	  (sysLook,lookFun)	= getWindowLookAtt lookAtt
	= (sysLook,lookFun,atts)
where
	defaultlook :: SelectState !UpdateState !*Picture -> *Picture
	defaultlook _ {updArea} picture = StrictSeq (map unfill updArea) picture


//	Retrieve (View/Outer)Size, Margins, ItemSpaces, SelectState, and PenAttributes from the attribute list.

attrSize :: ![WindowAttribute .st] -> Maybe Size
attrSize atts
	| not hasSize			= Nothing
	| isWindowViewSize att	= Just (getWindowViewSizeAtt  att)
	| otherwise				= Just (getWindowOuterSizeAtt att)
where
	(hasSize,att)			= Select (\att->isWindowViewSize att || isWindowOuterSize att) undef atts

attrMargins :: !WindowKind !OSWindowMetrics ![WindowAttribute .st] -> (!(!Int,!Int),!(!Int,!Int))
attrMargins wKind wMetrics atts
	= (getWindowHMargins wKind wMetrics atts,getWindowVMargins wKind wMetrics atts)

attrSelectState :: ![WindowAttribute .st] -> Bool
attrSelectState atts
	= enabled (getWindowSelectStateAtt (snd (Select isWindowSelectState (WindowSelectState Able) atts)))

attrPen :: ![WindowAttribute .st] -> (![PenAttribute],![WindowAttribute .st])
attrPen atts
	# (_,penAtt,atts)	= Remove isWindowPen (WindowPen []) atts
	= (getWindowPenAtt penAtt,atts)


/*	get(Ok/Cancel)Id select the Id of the Window(Ok/Cancel) attribute, and checks
	whether this Id corresponds with a (Custom)ButtonControl.
*/
getOkId :: ![WindowAttribute *(.ls,.pst)] ![WElementHandle .ls .pst] -> (!Maybe Id,![WElementHandle .ls .pst])
getOkId atts itemHs
	| not hasid
		= (Nothing,itemHs)
	# (ok,itemHs)		= isOkOrCancelControlId id itemHs
	| ok
		= (Just id,itemHs)
	| otherwise
		= (Nothing,itemHs)
where
	(hasid,idAtt)		= Select isWindowOk undef atts
	id					= getWindowOkAtt idAtt

getCancelId :: ![WindowAttribute *(.ls,.pst)] ![WElementHandle .ls .pst] -> (!Maybe Id,![WElementHandle .ls .pst])
getCancelId atts itemHs
	| not hasid
		= (Nothing,itemHs)
	# (ok,itemHs)		= isOkOrCancelControlId id itemHs
	| ok
		= (Just id,itemHs)
	| otherwise
		= (Nothing,itemHs)
where
	(hasid,idAtt)		= Select isWindowCancel undef atts
	id					= getWindowCancelAtt idAtt

isOkOrCancelControlId :: !Id ![WElementHandle .ls .pst] -> (!Bool,![WElementHandle .ls .pst])
isOkOrCancelControlId id itemHs
	# (maybeKind,itemHs)	= getControlKind id itemHs
	| isNothing maybeKind
		= (False,itemHs)
	| otherwise
		# kind				= fromJust maybeKind
		= (kind==IsButtonControl || kind==IsCustomButtonControl,itemHs)


/*	validateWindowInitActive checks if the WindowInitActive attribute corresponds with an existing control.
	If this is not the case, the attribute is removed from the attribute list.
*/
validateWindowInitActive :: ![WindowAttribute *(.ls,.pst)] ![WElementHandle .ls .pst]
						-> (![WindowAttribute *(.ls,.pst)],![WElementHandle .ls .pst])
validateWindowInitActive atts itemHs
	| not hasAtt
		= (atts1,itemHs)
	# (kind,itemHs)		= getControlKind (getWindowInitActiveAtt att) itemHs
	| isNothing kind
		= (atts1,itemHs)
	| otherwise
		= (atts,itemHs)
where
	(hasAtt,att,atts1)	= Remove isWindowInitActive undef atts

/*	getControlKind id itemHs
		returns (Just ControlKind) of the control in the item list. 
		If no such control could be found then Nothing is returned.
*/
getControlKind :: !Id ![WElementHandle .ls .pst] -> (!Maybe ControlKind,![WElementHandle .ls .pst])
getControlKind id [itemH:itemHs]
	# (maybe,itemH)	= getControlKind` id itemH
	| isJust maybe
		= (maybe,[itemH:itemHs])
	| otherwise
		# (maybe,itemHs)	= getControlKind id itemHs
		= (maybe,[itemH:itemHs])
where
	getControlKind` :: !Id !(WElementHandle .ls .pst) -> (!Maybe ControlKind,!WElementHandle .ls .pst)
	getControlKind` id (WItemHandle itemH=:{wItemId,wItemKind,wItems=itemHs})
		| isJust wItemId && fromJust wItemId==id
			= (Just wItemKind,WItemHandle itemH)
		| otherwise
			# (kind,itemHs)	= getControlKind id itemHs
			= (kind,WItemHandle {itemH & wItems=itemHs})
	getControlKind` id (WListLSHandle itemHs)
		# (kind,itemHs)		= getControlKind id itemHs
		= (kind,WListLSHandle itemHs)
	getControlKind` id (WExtendLSHandle wExH=:{wExtendItems=itemHs})
		# (kind,itemHs)		= getControlKind id itemHs
		= (kind,WExtendLSHandle {wExH & wExtendItems=itemHs})
	getControlKind` id (WChangeLSHandle wChH=:{wChangeItems=itemHs})
		# (kind,itemHs)		= getControlKind id itemHs
		= (kind,WChangeLSHandle {wChH & wChangeItems=itemHs})
getControlKind _ _
	= (Nothing,[])


/*	exactWindowSize determines the exact size of a window.
	The size is extended to fit in sliderbars if requested (argument 4 and 5).
*/
exactWindowSize :: OSWindowMetrics ViewDomain !Size Bool Bool !WindowKind -> Size
exactWindowSize wMetrics domain wSize=:{w,h} hasHScroll hasVScroll wKind
	| wKind==IsDialog			= wSize
	| visHScroll && visVScroll	= {w=w`,h=h`}
	| visHScroll				= {wSize & h=h`}
	| visVScroll				= {wSize & w=w`}
	| otherwise					= wSize
where
	visHScroll					= hasHScroll && OSscrollbarIsVisible (minmax domain.corner1.x domain.corner2.x) w
	visVScroll					= hasVScroll && OSscrollbarIsVisible (minmax domain.corner1.y domain.corner2.y) h
	w`							= w+wMetrics.osmVSliderWidth
	h`							= h+wMetrics.osmHSliderHeight


/*	exactWindowPos determines the exact position of a window.
	The size argument must be the exact size as calculated by exactWindowSize of the window.
	The ItemPos argument must be the validated(!) ItemPos attribute of the window.
*/
exactWindowPos :: !OSWindowMetrics !Size !(Maybe ItemPos) !WindowKind !WindowMode !(WindowHandles .pst) !*OSToolbox
																	   -> (!Point2,!WindowHandles .pst, !*OSToolbox)
exactWindowPos wMetrics exactSize maybePos wKind wMode windows tb
	| wKind==IsDialog && wMode==Modal
		= (pos,windows,tb1)
	with
		(screenRect,tb1)	= OSscreenrect tb
		screenSize			= RectSize screenRect
		l					= screenRect.rleft + (screenSize.w-exactSize.w)/2
		t					= screenRect.rtop  + (screenSize.h-exactSize.h)/3
		pos					= {x=SetBetween l screenRect.rleft screenRect.rright,y=SetBetween t screenRect.rtop screenRect.rbottom}
	| isNothing maybePos
		= (zero,windows,tb)
	| otherwise
		# itemPos			= fromJust maybePos
		# (pos,windows,tb)	= getItemPosPosition wMetrics exactSize itemPos windows tb
		# (pos,tb)			= setWindowInsideScreen pos exactSize tb
		= (pos,windows,tb)
where
/*	getItemPosPosition calculates the exact position of the given window. 
	getItemPosPosition does not check whether this position will place the window off screen.
*/
	getItemPosPosition :: !OSWindowMetrics !Size !ItemPos !(WindowHandles .pst) !*OSToolbox
											   -> (!Point2,!WindowHandles .pst, !*OSToolbox)
	getItemPosPosition wMetrics size itemPos windows=:{whsWindows=wsHs} tb
		| isRelative
			# (rect,tb)					= OSscreenrect tb
			  screenDomain				= RectToRectangle rect
			  screenOrigin				= {x=rect.rleft,y=rect.rtop}
			# (before,after)			= Uspan (unidentifyWindow (toWID relativeTo)) wsHs
			  (wPtr,wsH,after)			= case after of
		  									[]                           -> windowvalidateFatalError "getItemPosPosition" "target window could not be found"
		  									[wsH=:{wshIds={wPtr}}:after] -> (wPtr,wsH,after)
			  (relativeSize,wsH)		= getWindowStateHandleSize wsH
			  windows					= {windows & whsWindows=before++[wsH:after]}
			# ((relativeX,relativeY),tb)= OSgetWindowPos wPtr tb
			/* PA: do not use OSgetWindowViewFrameSize. 
			# ((relativeW,relativeH),tb)= OSgetWindowViewFrameSize wPtr tb
			*/
			  (relativeW,relativeH)		= toTuple relativeSize
			  (exactW,exactH)			= (size.w,size.h)
			  {vx,vy}					= itemPosOffset (snd itemPos) screenDomain screenOrigin
			  pos						= case (fst itemPos) of
						  					(LeftOf  _)	-> {x=relativeX+vx-exactW,   y=relativeY+vy}
						  					(RightTo _)	-> {x=relativeX+vx+relativeW,y=relativeY+vy}
				  							(Above   _)	-> {x=relativeX+vx,          y=relativeY+vy-exactH}
			  								(Below   _)	-> {x=relativeX+vx,          y=relativeY+vy+relativeH}
			  								other       -> windowvalidateFatalError "getItemPosPosition" "unexpected ItemLoc alternative"
			= (pos,windows,tb)
	where
		(isRelative,relativeTo)		= isRelativeItemPos itemPos
		
		unidentifyWindow :: !WID !(WindowStateHandle .pst) -> *(!Bool,!WindowStateHandle .pst)
		unidentifyWindow wid wsH
			# (ids,wsH)				= getWindowStateHandleWIDS wsH
			= (not (identifyWIDS wid ids),wsH)
	getItemPosPosition _ size itemPos windows tb
		| isAbsolute
			# (rect,tb)					= OSscreenrect tb
			  screenDomain				= RectToRectangle rect
			  screenOrigin				= {x=rect.rleft,y=rect.rtop}
			= (movePoint (itemPosOffset offset screenDomain screenOrigin) zero,windows,tb)
	where
		(isAbsolute,offset)			= isAbsoluteItemPos itemPos
	getItemPosPosition _ size itemPos windows tb
		| isCornerItemPos itemPos
			# (rect,tb)					= OSscreenrect tb
			  screenDomain				= RectToRectangle rect
			  screenOrigin				= {x=rect.rleft,y=rect.rtop}
			  (exactW,exactH)			= toTuple size
			  {vx,vy}					= itemPosOffset (snd itemPos) screenDomain screenOrigin
			  pos						= case (fst itemPos) of
					  						LeftTop		-> {x=rect.rleft +vx,       y=rect.rtop   +vy}
					  						RightTop	-> {x=rect.rright+vx-exactW,y=rect.rtop   +vy}
					  						LeftBottom	-> {x=rect.rleft +vx,       y=rect.rbottom+vy-exactH}
					  						RightBottom	-> {x=rect.rright+vx-exactW,y=rect.rbottom+vy-exactH}
			= (pos,windows,tb)
	getItemPosPosition _ _ _ windows tb
		= (zero,windows,tb)
	
/*	setWindowInsideScreen makes sure that a window at the given position and given size will be on screen.
*/
	setWindowInsideScreen :: !Point2 !Size !*OSToolbox -> (!Point2,!*OSToolbox)
	setWindowInsideScreen pos=:{x,y} size=:{w,h} tb
		# (screenRect,tb)		= OSscreenrect tb
		  {w=screenW,h=screenH}	= RectSize screenRect
		  (x`,y`)				= (SetBetween x screenRect.rleft (screenRect.rright-w),SetBetween y screenRect.rtop (screenRect.rbottom-h))
		  pos					= if (w<=screenW && h<=screenH)	{x=x`,y=y`}			// window fits entirely on screen
		  						 (if (w<=screenW)				{x=x`,y=0 }			// window is to high
			  					 (if (h<=screenH)				{x=0, y=y`}			// window is to wide
			  					 (								zero)))				// window doesn't fit anyway
		= (pos,tb)



//	itemPosOffset calculates the actual offset vector of the given ItemOffset value.

itemPosOffset :: !ItemOffset ViewDomain Point2 -> Vector2
itemPosOffset NoOffset _ _
	= zero
itemPosOffset (OffsetVector v) _ _
	= v
itemPosOffset (OffsetFun i f) domain origin
	| i==1		= f (domain,origin)
	| otherwise	= windowvalidateError "calculating OffsetFun" ("illegal ParentIndex value: "+++toString i)


//	Predicates on ItemPos:
isRelativeItemPos :: !ItemPos -> (!Bool,Id)
isRelativeItemPos (LeftOf  id,_)	= (True, id)
isRelativeItemPos (RightTo id,_)	= (True, id)
isRelativeItemPos (Above   id,_)	= (True, id)
isRelativeItemPos (Below   id,_)	= (True, id)
isRelativeItemPos _					= (False,undef)

isAbsoluteItemPos :: !ItemPos -> (!Bool,ItemOffset)
isAbsoluteItemPos (Fix,offset)		= (True, offset)
isAbsoluteItemPos _					= (False,undef)

isCornerItemPos :: !ItemPos -> Bool
isCornerItemPos (LeftTop,_)			= True
isCornerItemPos (RightTop,_)		= True
isCornerItemPos (LeftBottom,_)		= True
isCornerItemPos (RightBottom,_)		= True
isCornerItemPos _					= False