implementation module mouseevent

/***
- need to incorporate input-tracking...
***/

import StdEnv, StdIO
import windowaccess,windowhandle,deviceevents,iostate
import commondef
import ostypes, oswindow, ospicture
import osutil
//import controls,events,textedit
import inputtracking,keyfocus
from controls import InButton, InCheckBox, InUpButton, InDownButton, InPageUp, InPageDown, InThumb
from controls import GetCtlValue, TestControl
from textedit import TEClick, :: TEHandle
import events
//import StdDebug, dodebug
trace_n _ f :== f
//from dodebug import trace_n`

/*
mapJust _ Nothing = Nothing
mapJust f (Just i) = Just (f i)
*/
//--

:: ControlMouseState
	= NothingState
	| SliderState SliderState
	| CustomState (Bool,OSRect,MouseState)
	| CustomButtonState OSRect
	| EditTextState OSRect
	| PopUpControlState (Index,[String],Point2,Size,Maybe OSWindowPtr,String)
	| PopUpEditState (Index,[String],Point2,Size,Maybe OSWindowPtr,String)
	| RadioControlState (Index,Index,OSWindowPtr)
	| CheckControlState MarkState
	| CompoundContentState (Bool,OSRect,MouseState)
	| CompoundScrollState (OSWindowPtr, OSRect, Direction)

isCompoundContentState (CompoundContentState _) = True
isCompoundContentState _ = False

fromCompoundContentState (CompoundContentState s) = s

isCompoundScrollState (CompoundScrollState _) = True
isCompoundScrollState _ = False

fromCompoundScrollState (CompoundScrollState s) = s

isSliderState (SliderState _) = True
isSliderState _ = False

fromSliderState (SliderState s) = s

isCustomState (CustomState _) = True
isCustomState _ = False

fromCustomState (CustomState s) = s

isCustomButtonState (CustomButtonState _) = True
isCustomButtonState _ = False

fromCustomButtonState (CustomButtonState s) = s

isEditTextState (EditTextState _) = True
isEditTextState _ = False

fromEditTextState (EditTextState e) = e

isPopUpControlState (PopUpControlState _) = True
isPopUpControlState _ = False

fromPopUpControlState (PopUpControlState p) = p

isPopUpEditState (PopUpEditState _) = True
isPopUpEditState _ = False

fromPopUpEditState (PopUpEditState p) = p

isRadioControlState (RadioControlState _) = True
isRadioControlState _ = False

fromRadioControlState (RadioControlState r) = r

isCheckControlState (CheckControlState _) = True
isCheckControlState _ = False

fromCheckControlState (CheckControlState r) = r

//--
ioStGetDevice` device pState=:{io}
	# (a,b,io) = ioStGetDevice device io
	= ((a,b),{pState & io = io})
	
controlMouseDownIO :: !OSWindowMetrics !OSWindowPtr !Point2 !Int !Int !(WindowStateHandle (PSt .l)) !(WindowHandles (PSt .l)) !(PSt .l)
  -> (!Bool,!Maybe DeviceEvent,!WindowHandles (PSt .l),!WindowStateHandle (PSt .l),!(PSt .l))
controlMouseDownIO wMetrics wPtr mousePos when mods wsH=:{wshHandle=Just {wlsState=ls,wlsHandle=wls=:{whSize}}} windows ps
	# (done,returnEvent,windows,wsH1,ps1) = inWindowScrollBars wMetrics wsH ps
	| done
		= (done,returnEvent,windows,wsH1,ps1)
	= (done,returnEvent,windows3,wsH3,ps2)
	with
		windows1			= setWindowHandlesWindow wsH2 windows
		ioState1			= ioStSetDevice (WindowSystemState windows1) ps1.io
		pState1				= {ps1 & io=ioState1}
		(done,returnEvent,wsH2,pState2)		= controlMouseDownIO` mousePos wsH1 pState1

		((_,wDevice),ps2)		= ioStGetDevice` WindowDevice pState2
		windows2				= windowSystemStateGetWindowHandles wDevice
		(_,wsH3,windows3) = getWindowHandlesWindow (toWID wPtr) windows2
		

where
//	inWindowScrollBars :: !OSWindowMetrics !(WindowStateHandle (PSt .l)) !(IOSt .l) -> (!Bool,!(WindowStateHandle (PSt .l)),!(IOSt .l))
	inWindowScrollBars wMetrics wsH=:{wshHandle=Just {wlsState=ls,wlsHandle=wls=:{whKeyFocus=kf,whSize=size,whWindowInfo=WindowInfo wdata}}} ps
		| visHScroll && pointInRect mousePos hScrollRect
			= windowScrollbarMouseDownIO mousePos wPtr Horizontal wdata hScrollRect vScrollRect wsH windows ps
		| visVScroll && pointInRect mousePos vScrollRect
			= windowScrollbarMouseDownIO mousePos wPtr Vertical wdata hScrollRect vScrollRect wsH windows ps
		= (False,Nothing,windows,wsH,ps)
	where
		(visHScroll,visVScroll)		= osScrollbarsAreVisible wMetrics wDomain (toTuple size) (hasHScroll,hasVScroll)
		wDomain						= wdata.windowDomain
		hasHScroll					= isJust wdata.windowHScroll
		hasVScroll					= isJust wdata.windowVScroll
		wRect						= sizeToRect size
		hScrollRect					= osGetWindowHScrollRect wMetrics (hasHScroll,hasVScroll) wRect	// kan je ook uit ScrollInfo halen ?!
		vScrollRect					= osGetWindowVScrollRect wMetrics (hasHScroll,hasVScroll) wRect
//		cmEvent						= {cmePtr=wPtr,cmePos=mousePos,cmeWhen=when,cmeMods=toModifiers mods}
	inWindowScrollBars wMetrics wsH ps
		= (False,Nothing,windows,wsH,ps)
/*	
	windowScrollbarMouseDownIO mousePos wPtr direction {windowDomain,windowHScroll,windowVScroll} hScrollRect vScrollRect wsH windows ps
		# (upPart,ps)			= accPIO (accIOToolbox (trackClippedControl wPtr clipRect itemPtr mousePos)) ps
		| upPart <> 0
//			# state				= fromSliderState itemType
			# (thumb,ps)		= accPIO (accIOToolbox (GetCtlValue itemPtr)) ps
			# thumb`			= thumb //fromOSscrollbarRange (sliderMin, sliderMax) thumb
			# ps = trace_n (toString thumb+++".."+++toString thumb`) ps
			// moet nu thumb nog omzetten naar slider range....???
			# move				= case upPart of
									InUpButton		-> SliderDecSmall
									InDownButton	-> SliderIncSmall
									InPageUp		-> SliderDecLarge
									InPageDown		-> SliderIncLarge
									InThumb			-> SliderThumb thumb`
			# (wids,wsH)		= getWindowStateHandleWIDS wsH
			# controlInfo		= WindowScrollAction {wsaWIDS=wids,wsaSliderMove=move,wsaDirection=direction}
			= (True,Just controlInfo,windows,wsH,ps)
		= (True,Nothing,windows,wsH,ps)
	where
		hScroll	= (fromJust windowHScroll).scrollItemPtr
		vScroll	= (fromJust windowVScroll).scrollItemPtr
		clipRect = if (direction == Horizontal) hScrollRect vScrollRect
		itemPtr = if (direction == Horizontal) hScroll vScroll
		sliderMin = if (direction == Horizontal) windowDomain.rleft windowDomain.rtop
		sliderMax = if (direction == Horizontal) windowDomain.rright windowDomain.rbottom
*/
	time	= OSTime (when + 15)
	
	windowScrollbarMouseDownIO mousePos wPtr direction {windowDomain,windowHScroll,windowVScroll} hScrollRect vScrollRect wsH windows ps
		# (upPart,ps)			= accPIO (accIOToolbox (TestControl itemPtr mousePos.x mousePos.y)) ps
		= case upPart of
			0				-> (True,Nothing,windows,wsH,ps)
			InUpButton
							#  move			= SliderDecSmall
							#  (wids,wsH)	= getWindowStateHandleWIDS wsH
							#  returnEvent	= WindowScrollAction {wsaWIDS=wids,wsaSliderMove=move,wsaDirection=direction}
							#  ps			= startTrack wPtr time 0 itemPtr upPart direction False ps
							-> (True,Just returnEvent,windows,wsH,ps)
			InDownButton
							#  move			= SliderIncSmall
							#  (wids,wsH)	= getWindowStateHandleWIDS wsH
							#  returnEvent	= WindowScrollAction {wsaWIDS=wids,wsaSliderMove=move,wsaDirection=direction}
							#  ps			= startTrack wPtr time 0 itemPtr upPart direction False ps
							-> (True,Just returnEvent,windows,wsH,ps)
			InPageUp
							#  move			= SliderDecLarge
							#  (wids,wsH)	= getWindowStateHandleWIDS wsH
							#  returnEvent	= WindowScrollAction {wsaWIDS=wids,wsaSliderMove=move,wsaDirection=direction}
							#  ps			= startTrack wPtr time 0 itemPtr upPart direction False ps
							-> (True,Just returnEvent,windows,wsH,ps)
			InPageDown
							#  move			= SliderIncLarge
							#  (wids,wsH)	= getWindowStateHandleWIDS wsH
							#  returnEvent	= WindowScrollAction {wsaWIDS=wids,wsaSliderMove=move,wsaDirection=direction}
							#  ps			= startTrack wPtr time 0 itemPtr upPart direction False ps
							-> (True,Just returnEvent,windows,wsH,ps)
			InThumb			
							# (upPart,ps)	= accPIO (accIOToolbox (trackClippedControl wPtr clipRect itemPtr mousePos)) ps
							#  (thumb,ps)	= accPIO (accIOToolbox (GetCtlValue itemPtr)) ps
							#  move			= SliderThumb thumb
							#  (wids,wsH)	= getWindowStateHandleWIDS wsH
							#  returnEvent	= WindowScrollAction {wsaWIDS=wids,wsaSliderMove=move,wsaDirection=direction}
							-> (True,Just returnEvent,windows,wsH,ps)
			_				-> abort "mouseevent: unknown slider part..."
			
	where
		hScroll	= (fromJust windowHScroll).scrollItemPtr
		vScroll	= (fromJust windowVScroll).scrollItemPtr
		clipRect = if (direction == Horizontal) hScrollRect vScrollRect
		itemPtr = if (direction == Horizontal) hScroll vScroll
			
	controlMouseDownIO` pos wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH=:{whKind,whItems,whWindowInfo}}} ps
		# windowPen = case whKind of
						IsWindow	-> (getWindowInfoWindowData whWindowInfo).windowLook.lookPen
						IsDialog	-> fst (sharePen dialogPen)
						_			-> abort "window kind not supported"
		# (_,(itemNr,itemPtr,itemContextPen,itemType),itemHs,(ls,ps))	= getControlsItemNr (when,mods,wPtr,wMetrics) windowPen pos whItems (ls,ps)
		# wsH = {wsH & wshHandle=Just {wlsState = ls,  wlsHandle={wH & whItems=itemHs}}}

		| itemNr <> 0
	//		# (updRect,ps)			= accIOToolbox (loadUpdateBBox wPtr) ps
	//		# clipRect				= IntersectRects updRect (SizeToRect whSize)
			# clipRect				= sizeToRect whSize
//			# itemRect				= posSizeToRect wItemPos wItemSize
//			# clipRect`				= intersectRects clipRect itemRect

			| isCompoundContentState itemType
				# (filtered,rect,mouseState)
										= fromCompoundContentState itemType
				| filtered
					// get current keyfocus control...
					# (kf,wsH)			= getWindowStateHandleKeyFocus wsH
					# (kfIt,kf)			= getCurrentFocusItem kf
					# (wsH,ps)			= changeFocus False kfIt (Just itemNr) wPtr clipRect wsH ps
					# wsH				= setWindowStateHandleKeyFocus kf wsH

					# (wids,wsH)		= getWindowStateHandleWIDS wsH
					# controlInfo		= case kfIt of
											Nothing	-> Just (ControlGetKeyFocus {ckfWIDS=wids,ckfItemNr=itemNr,ckfItemPtr=itemPtr})
											(Just kfItem) -> case kfItem == itemNr of
												True	-> Just (ControlMouseAction {cmWIDS=wids,cmItemNr=itemNr,cmItemPtr=itemPtr,cmMouseState=mouseState})
												_		// als oude kfItem loopt te tracken dan moeten we hem untracken...
														-> Just (ControlGetKeyFocus {ckfWIDS=wids,ckfItemNr=itemNr,ckfItemPtr=itemPtr})
					// en mousedown...etc

					# (inputTrack,ps)		= accPIO ioStGetInputTrack ps
					# inputTrack			= trackMouse wPtr itemNr inputTrack
					# ps					= appPIO (ioStSetInputTrack inputTrack) ps
					# ps					= trace_n "mouse-up in compound control" ps
					= (True,controlInfo,wsH,ps)
				# ps						= trace_n "mouse-up not in compound control" ps
				= (True,Nothing,wsH,ps)
			
			| isCompoundScrollState itemType
				# (scrollPtr,scrollRect,scrollDirection) = fromCompoundScrollState itemType
//				# (upPart,ps)			= accPIO (accIOToolbox (trackClippedControl wPtr scrollRect scrollPtr mousePos)) ps
				# (upPart,ps)			= accPIO (accIOToolbox (TestControl scrollPtr mousePos.x mousePos.y)) ps
				# ps = trace_n ("windowevent:control tracking: "+++toString itemNr +++" " +++ toString upPart) ps
				// nee nee nee: nu produceren we control selectie terwijl we nu gewoon moeten gaan tracken...
				= case upPart of
					InUpButton
						#  move			= SliderDecSmall
						#  (wids,wsH)	= getWindowStateHandleWIDS wsH
						#  returnEvent	= CompoundScrollAction {csaWIDS=wids,csaItemNr=itemNr,csaItemPtr=scrollPtr,csaSliderMove=move,csaDirection=scrollDirection}
						#  ps			= startTrack wPtr time itemNr scrollPtr upPart scrollDirection False ps
						-> (True,Just returnEvent,wsH,ps)
					InDownButton
						#  move			= SliderIncSmall
						#  (wids,wsH)	= getWindowStateHandleWIDS wsH
						#  returnEvent	= CompoundScrollAction {csaWIDS=wids,csaItemNr=itemNr,csaItemPtr=scrollPtr,csaSliderMove=move,csaDirection=scrollDirection}
						#  ps			= startTrack wPtr time itemNr scrollPtr upPart scrollDirection False ps
						-> (True,Just returnEvent,wsH,ps)
					InPageUp
						#  move			= SliderDecLarge
						#  (wids,wsH)	= getWindowStateHandleWIDS wsH
						#  returnEvent	= CompoundScrollAction {csaWIDS=wids,csaItemNr=itemNr,csaItemPtr=scrollPtr,csaSliderMove=move,csaDirection=scrollDirection}
						#  ps			= startTrack wPtr time itemNr scrollPtr upPart scrollDirection False ps
						-> (True,Just returnEvent,wsH,ps)
					InPageDown
						#  move			= SliderIncLarge
						#  (wids,wsH)	= getWindowStateHandleWIDS wsH
						#  returnEvent	= CompoundScrollAction {csaWIDS=wids,csaItemNr=itemNr,csaItemPtr=scrollPtr,csaSliderMove=move,csaDirection=scrollDirection}
						#  ps			= startTrack wPtr time itemNr scrollPtr upPart scrollDirection False ps
						-> (True,Just returnEvent,wsH,ps)
					InThumb			
						# (upPart,ps)	= accPIO (accIOToolbox (trackClippedControl wPtr scrollRect scrollPtr mousePos)) ps
						#  (thumb,ps)	= accPIO (accIOToolbox (GetCtlValue scrollPtr)) ps
						#  move			= SliderThumb thumb
						#  (wids,wsH)	= getWindowStateHandleWIDS wsH
						#  returnEvent	= CompoundScrollAction {csaWIDS=wids,csaItemNr=itemNr,csaItemPtr=scrollPtr,csaSliderMove=move,csaDirection=scrollDirection}
						-> (True,Just returnEvent,wsH,ps)
					_	-> (True,Nothing,wsH,ps)
			| isSliderState itemType	// it's a slider...
				# (upPart,ps)			= accPIO (accIOToolbox (TestControl itemPtr mousePos.x mousePos.y)) ps
				# state					= fromSliderState itemType
				= case upPart of
					InUpButton
						#  move			= SliderDecSmall
						#  (wids,wsH)	= getWindowStateHandleWIDS wsH
						#  returnEvent	= ControlSliderAction {cslWIDS=wids,cslItemNr=itemNr,cslItemPtr=itemPtr,cslSliderMove=move}
						#  ps			= startTrack wPtr time itemNr itemPtr upPart Horizontal True ps
						-> (True,Just returnEvent,wsH,ps)
					InDownButton
						#  move			= SliderIncSmall
						#  (wids,wsH)	= getWindowStateHandleWIDS wsH
						#  returnEvent	= ControlSliderAction {cslWIDS=wids,cslItemNr=itemNr,cslItemPtr=itemPtr,cslSliderMove=move}
						#  ps			= startTrack wPtr time itemNr itemPtr upPart Horizontal True ps
						-> (True,Just returnEvent,wsH,ps)
					InPageUp
						#  move			= SliderDecLarge
						#  (wids,wsH)	= getWindowStateHandleWIDS wsH
						#  returnEvent	= ControlSliderAction {cslWIDS=wids,cslItemNr=itemNr,cslItemPtr=itemPtr,cslSliderMove=move}
						#  ps			= startTrack wPtr time itemNr itemPtr upPart Horizontal True ps
						-> (True,Just returnEvent,wsH,ps)
					InPageDown
						#  move			= SliderIncLarge
						#  (wids,wsH)	= getWindowStateHandleWIDS wsH
						#  returnEvent	= ControlSliderAction {cslWIDS=wids,cslItemNr=itemNr,cslItemPtr=itemPtr,cslSliderMove=move}
						#  ps			= startTrack wPtr time itemNr itemPtr upPart Horizontal True ps
						-> (True,Just returnEvent,wsH,ps)
					InThumb			
						# (upPart,ps)	= accPIO (accIOToolbox (trackClippedControl wPtr clipRect itemPtr mousePos)) ps
						#  (thumb,ps)	= accPIO (accIOToolbox (GetCtlValue itemPtr)) ps
//						# thumb`		= fromOSscrollbarRange (state.sliderMin, state.sliderMax) thumb
//						# ps = trace_n (toString thumb+++".."+++toString thumb`) ps
						#  move			= SliderThumb thumb
						#  (wids,wsH)	= getWindowStateHandleWIDS wsH
						#  returnEvent	= ControlSliderAction {cslWIDS=wids,cslItemNr=itemNr,cslItemPtr=itemPtr,cslSliderMove=move}
						-> (True,Just returnEvent,wsH,ps)
					_	-> (True,Nothing,wsH,ps)
			| isCustomState itemType
				# (filtered,itemRect,mouseState)	= fromCustomState itemType
//				# (selected,ps)						= accPIO (accIOToolbox (trackRectArea wPtr clipRect itemRect)) ps
//				| selected
				| filtered
					# (wids,wsH)			= getWindowStateHandleWIDS wsH
					// hmmm, moeten we in modifiers niet de modifiers bij mouse-up zetten ipv die bij mouse-down?
//					# controlInfo	= Just (ControlSelection {csWIDS=wids,csItemNr=itemNr,csItemPtr=itemPtr,csMoreData=0,csModifiers=toModifiers mods})
					# controlInfo			= Just (ControlMouseAction {cmWIDS=wids,cmItemNr=itemNr,cmItemPtr=itemPtr,cmMouseState=mouseState})
					# (inputTrack,ps)		= accPIO ioStGetInputTrack ps
					# inputTrack			= trackMouse wPtr itemNr inputTrack
					# ps					= appPIO (ioStSetInputTrack inputTrack) ps
					# ps					= trace_n "mouse-up in custom control" ps
					= (True,controlInfo,wsH,ps)
				# ps						= trace_n "mouse-up outside custom control" ps
				= (True,Nothing,wsH,ps)
			
			| isCustomButtonState itemType
				# itemRect			= fromCustomButtonState itemType
				# (selected,ps)		= accPIO (accIOToolbox (trackCustomButton wPtr clipRect itemRect)) ps
				| selected
					# (wids,wsH)			= getWindowStateHandleWIDS wsH
					// hmmm, moeten we in modifiers niet de modifiers bij mouse-up zetten ipv die bij mouse-down?
					# controlInfo	= Just (ControlSelection {csWIDS=wids,csItemNr=itemNr,csItemPtr=itemPtr,csMoreData=0,csModifiers=toModifiers mods})
					# ps = trace_n "mouse-up in custom button" ps
					= (True,controlInfo,wsH,ps)
				# ps = trace_n "mouse-up outside custom button" ps
				= (True,Nothing,wsH,ps)
			
			| isEditTextState itemType
				# shift				= (toModifiers mods).shiftDown
				# editHandle		= itemPtr
				// get current keyfocus control...
				# (kf,wsH)			= getWindowStateHandleKeyFocus wsH
				# (kfIt,kf)			= getCurrentFocusItem kf
				# (wsH,ps)			= changeFocus False kfIt (Just itemNr) wPtr clipRect wsH ps
				# wsH				= setWindowStateHandleKeyFocus kf wsH
//				setWindowStateHandleKeyFocus
				// if edit control deactivate
				// activate this one...
				# ps				= appPIO (appIOToolbox (appClipport wPtr clipRect (TEClick (toTuple pos) shift editHandle /* o TEActivate editHandle*/))) ps
				# (wids,wsH)		= getWindowStateHandleWIDS wsH
				# controlInfo		= case kfIt of
										Nothing		-> Just (ControlGetKeyFocus {ckfWIDS=wids,ckfItemNr=itemNr,ckfItemPtr=itemPtr})
										Just kfItem	-> case kfItem == itemNr of
											True	-> Nothing	// ??? ControlMouseAction...
											_		-> Just (ControlGetKeyFocus {ckfWIDS=wids,ckfItemNr=itemNr,ckfItemPtr=itemPtr})
				// en mousedown...etc
				= (True,controlInfo,wsH,ps)
/*				# itemRect			= fromEditTextState itemType
				# (selected,ps)		= accPIO (accIOToolbox (trackRectArea wPtr clipRect itemRect)) ps
				| selected
					# (wids,wsH)			= getWindowStateHandleWIDS wsH
					// hmmm, moeten we in modifiers niet de modifiers bij mouse-up zetten ipv die bij mouse-down?
					# controlInfo	= Just (ControlGetKeyFocus {ckfWIDS=wids,ckfItemNr=itemNr,ckfItemPtr=itemPtr})
					# ps = trace_n "mouse-up in edit control" ps
					= (True,controlInfo,wsH,ps)
				# ps = trace_n "mouse-up outside edit control" ps
				= (True,Nothing,wsH,ps)
*/			
			| isPopUpEditState itemType
				# shift				= (toModifiers mods).shiftDown
				# (puIndex,puTexts,wItemPos,wItemSize,editHandle,editTxt)
									= fromPopUpEditState itemType
				# editHandle		= fromJust editHandle
				// get current keyfocus control...
				# (kf,wsH)			= getWindowStateHandleKeyFocus wsH
				# (kfIt,kf)			= getCurrentFocusItem kf
				# (wsH,ps)			= changeFocus False kfIt (Just itemNr) wPtr clipRect wsH ps
				# wsH				= setWindowStateHandleKeyFocus kf wsH
				# ps				= appPIO (appIOToolbox (appClipport wPtr clipRect (TEClick (toTuple pos) shift editHandle /* o TEActivate editHandle*/))) ps
				# (wids,wsH)		= getWindowStateHandleWIDS wsH
				# controlInfo		= Just (ControlGetKeyFocus {ckfWIDS=wids,ckfItemNr=itemNr,ckfItemPtr=itemPtr})
				// en mousedown...etc
				= (True,controlInfo,wsH,ps)
			| isPopUpControlState itemType
				// need to differentiate between arrow and body for editable popups,
				// already in controlHit testing...
				# (puIndex,puTexts,wItemPos,wItemSize,editPtr,editTxt)
									= fromPopUpControlState itemType
				# (newIndex,ps)		= accPIO (accIOToolbox(
										osHandlePopUpControlEvent itemPtr editPtr wPtr wItemPos wItemSize puIndex editTxt
										)) ps
				# ps = trace_n ("mouseevent",puIndex,newIndex) ps
				| newIndex  == 0 || newIndex == puIndex
					= (True,Nothing,wsH,ps)
				# newText			= if (newIndex < 0)
										(editTxt)
										(puTexts!!(newIndex-1))
				# ps				= appPIO (appIOToolbox(
										osSetPopUpControl wPtr itemPtr editPtr clipRect
											(posSizeToRect wItemPos wItemSize) puIndex newIndex newText True
										)) ps 
				# (wids,wsH)		= getWindowStateHandleWIDS wsH
				# controlInfo		= Just (ControlSelection
										{csWIDS=wids,csItemNr=itemNr,csItemPtr=itemPtr
										,csMoreData=newIndex,csModifiers = toModifiers mods
										})
				= (True,controlInfo,wsH,ps)
			
//			# ps = appPIO (appIOToolbox (settbpen wPtr itemContextPen)) ps
			# (upPart,ps)			= accPIO (accIOToolbox (trackClippedControl wPtr clipRect itemPtr mousePos)) ps
			# ps = trace_n ("windowevent`:control tracking: "+++toString itemNr +++" " +++ toString upPart) ps
			// nee nee nee: nu produceren we control selectie terwijl we nu gewoon moeten gaan tracken...
			| upPart <> 0
				# (wids,wsH)			= getWindowStateHandleWIDS wsH
				| isRadioControlState itemType
					# (newIndex,oldIndex,oldPtr)	= fromRadioControlState itemType
					# newPtr						= itemPtr
					# ps							= appPIO (appIOToolbox (osSetRadioControl wPtr oldPtr newPtr clipRect)) ps
					# controlInfo					= Just (ControlSelection {csWIDS=wids,csItemNr=itemNr,csItemPtr=itemPtr,csMoreData=0,csModifiers=toModifiers mods})
					= (True,controlInfo,wsH,ps)
				| isCheckControlState itemType
					# checkState					= fromCheckControlState itemType
					# ps							= appPIO (appIOToolbox (osSetCheckControl wPtr itemPtr clipRect (if (marked checkState) False True))) ps
					# controlInfo					= Just (ControlSelection {csWIDS=wids,csItemNr=itemNr,csItemPtr=itemPtr,csMoreData=0,csModifiers=toModifiers mods})
					= (True,controlInfo,wsH,ps)
				# controlInfo = Just (ControlSelection {csWIDS=wids,csItemNr=itemNr,csItemPtr=itemPtr,csMoreData=0,csModifiers=toModifiers mods})
				= (True,controlInfo,wsH,ps)
			= (True,Nothing,wsH,ps)

		= (False,Nothing,wsH,ps)

getControlsItemNr
	:: !(!Int,!Int,!OSWindowPtr,!OSWindowMetrics) !Pen !Point2 [WElementHandle .ls (PSt .pst)] (.ls,(PSt .pst))
	-> (!Bool,!(!Int,!Int,!Pen,!ControlMouseState),[WElementHandle .ls (PSt .pst)],(.ls,(PSt .pst)))
getControlsItemNr (when,mods,wPtr,wMetrics) pen pos [itemH:itemHs] ps
	# (found,result,itemH,ps)				= getControlItemNr pen pos itemH ps
	| found
		= (found,result,[itemH:itemHs],ps)
	| otherwise
		# (found,result,itemHs,ps)			= getControlsItemNr (when,mods,wPtr,wMetrics) pen pos itemHs ps
		= (found,result,[itemH:itemHs],ps)
where
	getControlItemNrFromwItems pen pos (WItemHandle itemH=:{wItems}) ps
		# (found,result,items,ps)	= getControlsItemNr (when,mods,wPtr,wMetrics) pen pos wItems ps
		= (found,result,WItemHandle {itemH & wItems = items},ps)

	getControlItemNr
		:: !Pen !Point2 (WElementHandle .ls (PSt .pst)) (.ls,(PSt .pst))
		-> (!Bool,!(!Int,!Int,!Pen,!ControlMouseState),(WElementHandle .ls (PSt .pst)),(.ls,(PSt .pst)))
	getControlItemNr pen pos (WItemHandle itemH=:{wItems,wItemAtts,wItemNr,wItemSelect,wItemInfo,wItemKind,wItemPtr,wItemPos,wItemSize}) ps
		| not (itemH.wItemShow && wItemSelect) = (False,(0,0,pen,NothingState),WItemHandle itemH,ps)
		= case wItemKind of
			IsButtonControl				-> case (controlHit pos wItemPos wItemSize) of
											True	-> (True,(itemNr,wItemPtr,pen,NothingState),WItemHandle itemH,ps)
											False	-> (False,(0,0,pen,NothingState),WItemHandle itemH,ps)
			IsCheckControl				-> case (checkHit pos checkInfo.checkItems) of
											(Just (checkPtr,checkMark))	-> (True,(itemNr,checkPtr,pen,CheckControlState checkMark),WItemHandle itemH,ps)
											Nothing	-> (False,(0,0,pen,NothingState),WItemHandle itemH,ps)
			IsCompoundControl			-> getCompoundItemNr pos pen (WItemHandle itemH) ps
			IsCustomButtonControl		-> case (controlHit pos wItemPos wItemSize) of
											True	-> (True,(itemNr,wItemPtr,pen,CustomButtonState itemRect),WItemHandle itemH,ps)
											False	-> (False,(0,0,pen,NothingState),WItemHandle itemH,ps)
			IsCustomControl				-> case (controlHit pos wItemPos wItemSize && okCustomMouse) of
											True	# (ls,ps) = ps
													# (cMouse,ps) = accPIO (ioStButtonFreq when pos wPtr) ps
													# customState = MouseDown (pos - wItemPos) (toModifiers mods) cMouse	// <- need to determine number down...
													# ps = (ls,ps)
													-> (True,(itemNr,wItemPtr,pen,CustomState (customFilter customState,itemRect,customState)),WItemHandle itemH,ps)
											False	-> (False,(0,0,pen,NothingState),WItemHandle itemH,ps)
			IsEditControl				-> case (controlHit pos wItemPos wItemSize) of
											True	-> (True,(itemNr,wItemPtr,pen,EditTextState itemRect),WItemHandle itemH,ps)
											False	-> (False,(0,0,pen,NothingState),WItemHandle itemH,ps)
			IsLayoutControl				-> getControlItemNrFromwItems pen pos (WItemHandle itemH) ps
			IsPopUpControl				-> case (controlHit pos popupPos popupSiz) of
											True	-> (True,(itemNr,wItemPtr,pen,popupState),WItemHandle itemH,ps)
											False	-> case (controlHit pos popupPos` popupSiz`) of
												True-> (True,(itemNr,wItemPtr,pen,popupState`),WItemHandle itemH,ps)
												False-> (False,(0,0,pen,NothingState),WItemHandle itemH,ps)
			IsRadioControl				-> case (radioHit pos radioInfo.radioItems) of
											(Just (radioPtr,newIndex))	-> (True,(itemNr,radioPtr,pen,RadioControlState (newIndex,oldIndex,oldPtr)),WItemHandle itemH,ps)
											Nothing	-> (False,(0,0,pen,NothingState),WItemHandle itemH,ps)
			IsSliderControl				-> case (controlHit pos wItemPos wItemSize) of
											True	-> (True,(itemNr,wItemPtr,pen,SliderState ((getWItemSliderInfo wItemInfo).sliderInfoState)),WItemHandle itemH,ps)
											False	-> (False,(0,0,pen,NothingState),WItemHandle itemH,ps)
			IsTextControl				-> (False,(0,0,pen,NothingState),WItemHandle itemH,ps)
			IsOtherControl _			-> (False,(0,0,pen,NothingState),WItemHandle itemH,ps)
			_							-> (False,(0,0,pen,NothingState),WItemHandle itemH,ps)

	where
		itemNr							= wItemNr
		itemRect						= posSizeToRect wItemPos wItemSize
		
		(customFilter,customSelect,customFunction)
								= getControlMouseAtt (snd (cselect isControlMouse dummyCustomMouse wItemAtts))
		dummyCustomMouse		= ControlMouse (const False) Unable undef
		okCustomMouse			= enabled customSelect

		popupInfo		= getWItemPopUpInfo wItemInfo
		popupIndex		= popupInfo.popUpInfoIndex
		popupTexts		= map fst (popupInfo.popUpInfoItems)
		popupEditP		= mapMaybe (\{popUpEditPtr}->popUpEditPtr) popupInfo.popUpInfoEdit
		popupEditT		= case popupInfo.popUpInfoEdit of
							(Just {popUpEditText}) -> popUpEditText
							_ -> ""
		popupPos		= if (isJust popupInfo.popUpInfoEdit) {wItemPos & x = wItemPos.x + wItemSize.w - 20/*16*/} wItemPos
		popupSiz		= if (isJust popupInfo.popUpInfoEdit) {wItemSize & w = 20/*16*/} wItemSize
		popupPos`		= wItemPos
		popupSiz`		= if (isJust popupInfo.popUpInfoEdit) {wItemSize & w = wItemSize.w - 22} wItemSize
		popupState		= PopUpControlState (popupIndex,popupTexts,wItemPos,wItemSize,popupEditP,popupEditT)
		popupState`		= PopUpEditState (popupIndex,popupTexts,wItemPos,wItemSize,popupEditP,popupEditT)
		
		radioInfo	= getWItemRadioInfo wItemInfo
		oldIndex	= radioInfo.radioIndex
		oldPtr		= ((radioInfo.radioItems)!!(oldIndex-1)).radioItemPtr
		
		radioHit pos [] = Nothing
		radioHit pos [{radioItemPos,radioItemSize,radioItemPtr,radioItem=(_,radioIndex,_)}:rest]
			| controlHit pos radioItemPos radioItemSize
				= Just (radioItemPtr,radioIndex)
			= radioHit pos rest
		
		checkInfo = getWItemCheckInfo wItemInfo
		
		checkHit pos [] = Nothing
		checkHit pos [{checkItemPos,checkItemSize,checkItemPtr,checkItem=(_,_,checkMark,_)}:rest]
			| controlHit pos checkItemPos checkItemSize
				= Just (checkItemPtr,checkMark)
			= checkHit pos rest
		
				
	getControlItemNr pen cPtr (WListLSHandle itemHs) ps
		# (found,itemNr,itemHs,ps)		= getControlsItemNr (when,mods,wPtr,wMetrics) pen cPtr itemHs ps
		= (found,itemNr,WListLSHandle itemHs,ps)
	
	getControlItemNr pen cPtr (WExtendLSHandle wExH=:{wExtendLS=extLS,wExtendItems=itemHs}) (ls,ps)
		# (found,itemNr,itemHs,((extLS,ls),ps))		= getControlsItemNr (when,mods,wPtr,wMetrics) pen cPtr itemHs ((extLS,ls),ps)
		= (found,itemNr,WExtendLSHandle {wExtendLS=extLS, wExtendItems=itemHs},(ls,ps))
	
	getControlItemNr pen cPtr (WChangeLSHandle wChH=:{wChangeLS=chLS,wChangeItems=itemHs}) (ls,ps)
		# (found,itemNr,itemHs,(chLS,ps))		= getControlsItemNr (when,mods,wPtr,wMetrics) pen cPtr itemHs (chLS,ps)
		= (found,itemNr,WChangeLSHandle {wChangeLS = chLS, wChangeItems=itemHs},(ls,ps))

	controlHit mPos=:{x=mx,y=my} cPos=:{x=cx,y=cy} cSize=:{w=cw,h=ch}
		= cx < mx && mx < (cx + cw) && cy < my && my < (cy + ch)

	controlHit` mPos=:{x=mx,y=my} {rleft=cx,rtop=cy,rright=cx`,rbottom=cy`}
		= cx < mx && mx < cx` && cy < my && my < cy`

	getCompoundItemNr pos pen (WItemHandle itemH=:{wItems,wItemNr,wItemSelect,wItemInfo,wItemKind,wItemPtr,wItemPos,wItemSize,wItemAtts}) (ls,ps)
		#! wItemNr = trace_n ("CompoundRects",contentRect,hScrollRect,vScrollRect,visHScroll,visVScroll) wItemNr
		| visHScroll && hScrollHit
			= (True,(wItemNr,wItemPtr,compoundPen,CompoundScrollState (hPtr, hScrollRect, Horizontal)),WItemHandle itemH,(ls,ps))
		| visVScroll && vScrollHit
			= (True,(wItemNr,wItemPtr,compoundPen,CompoundScrollState (vPtr, vScrollRect, Vertical)),WItemHandle itemH,(ls,ps))
		# (found,result,itemH,(ls,ps)) =  getControlItemNrFromwItems compoundPen pos (WItemHandle itemH) (ls,ps)
		| found			// one of the compound controls was selected
			= (found,result,itemH,(ls,ps))
		| contentHit && okControlMouse	// mouse-down in content region but not in control, mousefun enabled
			//

			# itemRect			= contentRect
			# (bMouse,ps)		= accPIO (ioStButtonFreq when pos wPtr) ps
			# mstate			= MouseDown (pos - wItemPos + compoundOrigin) (toModifiers mods) bMouse	// <- need to determine number down...
	//		#! ps				= compoundMouseIO mfilter mfunction mstate ps
			# bool = mfilter mstate
			= (True,(wItemNr,wItemPtr,compoundPen,CompoundContentState (bool,contentRect,mstate)), itemH,(ls,ps))
		= (False,(0,0,pen,NothingState), itemH,(ls,ps))
	where				
		info			= getWItemCompoundInfo wItemInfo
		compoundLook	= info.compoundLookInfo.compoundLook
		compoundPen		= compoundLook.lookPen
		compoundOrigin	= info.compoundOrigin
		
		domainRect				= info.compoundDomain
		hasScrolls				= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		visScrolls				= osScrollbarsAreVisible wMetrics domainRect (toTuple wItemSize) hasScrolls
		(visHScroll,visVScroll)	= visScrolls
		hPtr					= (fromJust info.compoundHScroll).scrollItemPtr
		vPtr					= (fromJust info.compoundVScroll).scrollItemPtr
		contentRect				= /*getCompoundContentRect wMetrics visScrolls*/ (posSizeToRect wItemPos wItemSize)
		hScrollRect				= osGetCompoundHScrollRect wMetrics visScrolls contentRect
		vScrollRect				= osGetCompoundVScrollRect wMetrics visScrolls contentRect
		hScrollHit				= controlHit` pos hScrollRect
		vScrollHit				= controlHit` pos vScrollRect
		contentHit				= controlHit` pos contentRect
		
		(mfilter,selectState,mfunction)
								= getControlMouseAtt (snd (cselect isControlMouse dummyControlMouse wItemAtts))
		dummyControlMouse		= ControlMouse (const False) Unable undef
		okControlMouse			= enabled selectState

getControlsItemNr _ pen _ _ ps
	= (False,(0,0,pen,NothingState),[],ps)

//---
/*
from quickdraw import WhiteColor, HasColorQD, BlackColor, RedColor
from quickdraw import GreenColor, BlueColor, CyanColor, MagentaColor
from quickdraw import YellowColor, QRGBForeColor, QRGBBackColor
from quickdraw import QForeColor, QBackColor, QInvertRect, RGBColor

settbpen wPtr {penForeColour,penBackColour} tb
	= appGrafport wPtr set tb
where
	set tb
		# tb = settbcolour True penForeColour tb
		# tb = settbcolour False penBackColour tb
		= tb

settbcolour fore colour tb
	| asRGB
	= setRGBColour rgbColour tb
	with
//		setRGBColour :: !RGBColour !*Picture -> *Picture
		setRGBColour rgb tb
			# (hasColorQD,tb)	= HasColorQD tb
			| hasColorQD
			= setrgb (toMacRGB rgb) tb
			= setpln WhiteColor tb
		where
			toMacRGB :: !RGBColour -> (!Int,!Int,!Int)
			toMacRGB {r,g,b}
				= (macRGB r,macRGB g,macRGB b)
			where
				macRGB :: !Int -> Int
				macRGB x
					| x>=MaxRGB	= 65535
					| x<=0		= 0
								= toInt (65535.0*((toReal x)/(toReal MaxRGB)))
	= setMacColour colour tb
	with
//		setMacColour :: !Colour !*Picture -> *Picture
		setMacColour colour tb
			= setpln color tb
		where
			color		= case colour of
							Black	-> BlackColor
							White	-> WhiteColor
							Red		-> RedColor
							Green	-> GreenColor
							Blue	-> BlueColor
							Cyan	-> CyanColor
							Magenta	-> MagentaColor
							Yellow	-> YellowColor
where
	(asRGB,rgbColour)	= case colour of
							RGB rgb		-> (True, rgb)
							DarkGrey	-> (True, {r=dark,  g=dark,  b=dark})
							Grey		-> (True, {r=medium,g=medium,b=medium})
							LightGrey	-> (True, {r=light, g=light, b=light})
							_			-> (False,WhiteRGB)
	dark	= MaxRGB/4
	medium	= MaxRGB/2
	light	= MaxRGB*3/4
	setrgb | fore = QRGBForeColor; = QRGBBackColor;
	setpln | fore = QForeColor; = QBackColor;
*/	 

/*
GetMouse` tb
	# (x,y,tb) = GetMouse tb
	= ({x=x,y=y},tb)

//

compoundMouseIO mfilter mfunction mstate=:(MouseDown pos mod num) (ls,ps)
	# (ls,ps)			= case mfilter mstate of
							True	-> trace_n "D" mfunction mstate (ls,ps)
							_		-> trace_n "d" (ls,ps)
	# (still_down,ps)	= accPIO (accIOToolbox WaitMouseUp) ps
	# (pos,ps)			= accPIO (accIOToolbox GetMouse`) ps
	| still_down
		= compoundMouseIO mfilter mfunction (MouseDrag pos mod) (ls,ps)
	= compoundMouseIO mfilter mfunction (MouseUp pos mod) (ls,ps)
		
compoundMouseIO mfilter mfunction mstate=:(MouseDrag pos mod) (ls,ps)
	# (ls,ps)			= case mfilter mstate of
							True	-> mfunction mstate (ls,ps)
							_		-> (ls,ps)
	# (still_down,ps)	= accPIO (accIOToolbox WaitMouseUp) ps
	# (pos,ps)			= accPIO (accIOToolbox GetMouse`) ps
	| still_down
		= compoundMouseIO mfilter mfunction (MouseDrag pos mod) (ls,ps)
	= compoundMouseIO mfilter mfunction (MouseUp pos mod) (ls,ps)

compoundMouseIO mfilter mfunction mstate=:(MouseUp pos mod) (ls,ps)
	# (ls,ps)			= case mfilter mstate of
							True	-> trace_n "U" mfunction mstate (ls,ps)
							_		-> trace_n "u" (ls,ps)
	= (ls,ps)
*/

//--

startTrack wPtr time itemNr itemPtr upPart direction isControl ps
	# (hilite,ps) = case upPart of
//				InUpButton		-> (True,appPIO (appIOToolbox (HiliteControl itemPtr upPart)) ps)
//				InDownButton	-> (True,appPIO (appIOToolbox (HiliteControl itemPtr upPart)) ps)
				InUpButton		-> (True,appPIO (appIOToolbox (appClipped wPtr (HiliteControl itemPtr upPart))) ps)
				InDownButton	-> (True,appPIO (appIOToolbox (appClipped wPtr (HiliteControl itemPtr upPart))) ps)
				_				-> (False,ps)
	// trackSlider...
	# (inputTrack,ps)	= accPIO ioStGetInputTrack ps
	# sinfo = {stiControl = itemPtr, stiPart = upPart, stiHilite = hilite, stiDirection = direction, stiIsControl = isControl}
	# inputTrack		= trackSlider wPtr itemNr sinfo inputTrack
	# ps				= appPIO (ioStSetInputTrack inputTrack) ps
	# ps = appPIO (appIOToolbox (startTracking time)) ps
	// ...trackSlider
	= ps

from controls import TrackControl, :: ControlHandle, HiliteControl
from quickdraw import QInvertRect

trackClippedControl :: !OSWindowPtr !OSRect !OSWindowPtr !Point2 !*OSToolbox -> (!Int,!*OSToolbox)
trackClippedControl wPtr clipRect controlH {x,y} tb
= accClipport wPtr clipRect (TrackControl controlH x y 0) tb

trackCustomButton :: !OSWindowPtr !OSRect !OSRect !*OSToolbox -> (!Bool,!*OSToolbox)
trackCustomButton wPtr clipRect itemRect tb
	= accClipport wPtr clipRect (track itemRect True o QInvertRect (OSRect2Rect itemRect)) tb
where
	track :: !OSRect !Bool !*OSToolbox -> (!Bool,!*OSToolbox)
	track itemRect selected tb
		# (x,y,tb)			= GetMouse tb
		# inside			= pointInRect {x=x,y=y} itemRect
		# (stillDown,tb)	= WaitMouseUp tb
		| stillDown && selected == inside
			= track itemRect inside tb
		| stillDown
			= track itemRect inside (QInvertRect (OSRect2Rect itemRect) tb)
		| not inside
			= (inside,tb)
		= (inside,QInvertRect (OSRect2Rect itemRect) tb)

//--

OSRect2Rect r	:== (rleft,rtop,rright,rbottom)
where
	{rleft,rtop,rright,rbottom} = r

//==

changeFocus :: !Bool !(Maybe Int) !(Maybe Int) !OSWindowPtr !OSRect !*(WindowStateHandle .a) !*(PSt .c) -> *(!*(WindowStateHandle .a),!*PSt .c)
changeFocus tabbing oldItemNr newItemNr wPtr clipRect wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH=:{whItems}}} ps
	# (found,(ptr,knd),whItems)	= getFocuseableItemPtrAndKind` oldItemNr whItems
	# ps = case found of
			True	-> setFocus knd wPtr clipRect ptr False ps
			_		-> ps
	# (found,(ptr,knd),whItems)	= getFocuseableItemPtrAndKind` newItemNr whItems
	# ps = case found of
			True	-> setFocus knd wPtr clipRect ptr True ps
			_		-> ps
	# wsH				= {wsH & wshHandle=Just {wlsState = ls,  wlsHandle={wH & whItems=whItems}}}
	= (wsH,ps)
where
	setFocus IsEditControl wPtr clipRect itemPtr focus ps
		= appPIO (appIOToolbox set) ps
		where
			set tb
				# tb = case tabbing && focus of
						True	-> osSetEditControlSelection wPtr itemPtr clipRect clipRect 0 32767 tb 
						_		-> tb
				# tb = osSetEditControlFocus wPtr itemPtr clipRect focus tb
				= tb
	setFocus IsCompoundControl wPtr clipRect itemPtr focus ps
		= ps	//appPIO (appIOToolbox (osSetCompoundControlFocus wPtr itemPtr clipRect focus)) ps
	setFocus IsCustomControl wPtr clipRect itemPtr focus ps
		= ps	//appPIO (appIOToolbox (osSetCustomControlFocus wPtr itemPtr clipRect focus)) ps
	setFocus IsPopUpControl wPtr clipRect itemPtr focus ps
		= appPIO (appIOToolbox (osSetPopUpControlFocus wPtr itemPtr clipRect focus)) ps

getFocuseableItemPtrAndKind` itemNr whItems
	= case itemNr of
		Nothing		-> (False,(OSNoWindowPtr,IsOtherControl "NoControl"),whItems)
		Just itemNr	-> getFocuseableItemPtrAndKind itemNr whItems
		
getFocuseableItemPtrAndKind :: !Int [WElementHandle .ls .ps] -> (!Bool,!(!OSWindowPtr,!ControlKind),[WElementHandle .ls .ps])
getFocuseableItemPtrAndKind itemNr []
	= (False,(OSNoWindowPtr,IsOtherControl "NoControl"),[])
getFocuseableItemPtrAndKind itemNr [itemH:itemHs]
	# (found,result,itemH)		= getControlItemPtrAndKindFromItem itemNr itemH
	| found
		= (found,result,[itemH:itemHs])
	| otherwise
		# (found,result,itemHs)	= getFocuseableItemPtrAndKind itemNr itemHs
		= (found,result,[itemH:itemHs])
where
	getControlItemPtrAndKindFromItems :: !Int (WElementHandle .ls .ps) -> (!Bool,!(!OSWindowPtr,!ControlKind),WElementHandle .ls .ps)
	getControlItemPtrAndKindFromItems itemNr (WItemHandle itemH=:{wItems})
		# (found,result,items)	= getFocuseableItemPtrAndKind itemNr wItems
		= (found,result,WItemHandle {itemH & wItems = items})

	getControlItemPtrAndKindFromItem :: !Int (WElementHandle .ls .ps) -> (!Bool,!(!OSWindowPtr,!ControlKind),(WElementHandle .ls .ps))
	getControlItemPtrAndKindFromItem itemNr (WItemHandle itemH=:{wItemNr,wItemKind,wItemPtr,wItemInfo})
		| wItemNr == itemNr	= case wItemKind of
			IsPopUpControl		-> (isJust (getWItemPopUpInfo wItemInfo).popUpInfoEdit,(wItemPtr,wItemKind),WItemHandle itemH)
			IsCompoundControl	-> (True,(wItemPtr,wItemKind),WItemHandle itemH)
			IsCustomControl		-> (True,(wItemPtr,wItemKind),WItemHandle itemH)
			IsEditControl		-> (True,(wItemPtr,wItemKind),WItemHandle itemH)
			_					-> (False,(OSNoWindowPtr,IsButtonControl),WItemHandle itemH)
		= case wItemKind of
			IsCompoundControl	-> getControlItemPtrAndKindFromItems itemNr (WItemHandle itemH)
			IsLayoutControl		-> getControlItemPtrAndKindFromItems itemNr (WItemHandle itemH)
			_					-> (False,(OSNoWindowPtr,IsButtonControl),WItemHandle itemH)

	getControlItemPtrAndKindFromItem itemNr (WListLSHandle itemHs)
		# (found,result,itemHs)		= getFocuseableItemPtrAndKind itemNr itemHs
		= (found,result,WListLSHandle itemHs)
	
	getControlItemPtrAndKindFromItem itemNr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
		# (found,result,itemHs)		= getFocuseableItemPtrAndKind itemNr itemHs
		= (found,result,WExtendLSHandle {wExH & wExtendItems=itemHs})
	
	getControlItemPtrAndKindFromItem itemNr (WChangeLSHandle wChH=:{wChangeItems=itemHs})
		# (found,result,itemHs)		= getFocuseableItemPtrAndKind itemNr itemHs
		= (found,result,WChangeLSHandle {wChH & wChangeItems=itemHs})

