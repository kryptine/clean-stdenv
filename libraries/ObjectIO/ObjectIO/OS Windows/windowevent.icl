implementation module windowevent


//	Clean Object I/O library, version 1.2

/*	windowevent defines the DeviceEventFunction for the window device.
	This function is placed in a separate module because it is platform dependent.
*/


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	clCCall_12, clCrossCall_12, windowCrossCall_12
from	ostypes				import	OSNoWindowPtr
from	oswindow			import	fromOSscrollbarRange, OSscrollbarsAreVisible
import	commondef, controlcreate, deviceevents, iostate, windowaccess
from	StdControlAttribute	import	isControlKeyboard, getControlKeyboardAtt, 
									isControlMouse,    getControlMouseAtt, 
									isControlActivate, isControlDeactivate
from	StdPSt				import	accPIO
from	StdWindowAttribute	import	isWindowKeyboard,  getWindowKeyboardAtt,
									isWindowMouse,     getWindowMouseAtt,
									isWindowCursor,    getWindowCursorAtt


windoweventFatalError :: String String -> .x
windoweventFatalError function error
	= FatalError function "windowevent" error


/*	windowEvent filters the scheduler events that can be handled by this window device.
	For the time being no timer controls are added, so these events are ignored.
	windowEvent assumes that it is not applied to an empty IOSt.
*/
windowEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
windowEvent schedulerEvent pState
	# (hasDevice,pState)	= accPIO (IOStHasDevice WindowDevice) pState
	| not hasDevice			// This condition should never occur: WindowDevice must have been 'installed'
		= windoweventFatalError "WindowFunctions.dEvent" "could not retrieve WindowSystemState from IOSt"
	| otherwise
		= windowEvent schedulerEvent pState
where
	windowEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
	windowEvent schedulerEvent=:(ScheduleOSEvent osEvent _) pState=:{io=ioState}
		| not (isWindowOSEvent osEvent.ccMsg)
			= (False,Nothing,schedulerEvent,pState)
		| otherwise
			# (_,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
			# (wMetrics, ioState)	= IOStGetOSWindowMetrics ioState
			  windows				= WindowSystemStateGetWindowHandles wDevice
			  (myEvent,replyToOS,deviceEvent,windows,ioState)
			  						= filterOSEvent wMetrics osEvent windows ioState
			# ioState				= IOStSetDevice (WindowSystemState windows) ioState
			# pState				= {pState & io=ioState}
			  schedulerEvent		= if (isJust replyToOS) (ScheduleOSEvent osEvent (fromJust replyToOS)) schedulerEvent
			= (myEvent,deviceEvent,schedulerEvent,pState)
	where
		isWindowOSEvent :: !Int -> Bool
		isWindowOSEvent CcWmACTIVATE		= True
		isWindowOSEvent CcWmBUTTONCLICKED	= True
		isWindowOSEvent CcWmCLOSE			= True
		isWindowOSEvent CcWmCOMBOSELECT		= True
		isWindowOSEvent CcWmDEACTIVATE		= True
		isWindowOSEvent CcWmDRAWCONTROL		= True
		isWindowOSEvent CcWmIDLEDIALOG		= True
		isWindowOSEvent CcWmINITDIALOG		= True
		isWindowOSEvent CcWmKEYBOARD		= True
		isWindowOSEvent CcWmKILLFOCUS		= True
		isWindowOSEvent CcWmLOSTKEY			= True
		isWindowOSEvent CcWmLOSTMOUSE		= True
		isWindowOSEvent CcWmMOUSE			= True
		isWindowOSEvent CcWmPAINT			= True
		isWindowOSEvent CcWmSCROLLBARACTION	= True
		isWindowOSEvent CcWmSETFOCUS		= True
		isWindowOSEvent CcWmSIZE			= True
		isWindowOSEvent CcWmSPECIALBUTTON	= True
		isWindowOSEvent _					= False
	
	windowEvent schedulerEvent=:(ScheduleMsgEvent msgEvent) pState=:{io=ioState}
		# (ioId,ioState)		= IOStGetIOId ioState
		| ioId<>recLoc.rlIOId || recLoc.rlDevice<>WindowDevice
			= (False,Nothing,schedulerEvent,{pState & io=ioState})
		| otherwise
			# (_,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
			  windows				= WindowSystemStateGetWindowHandles wDevice
			  (found,windows)		= hasWindowHandlesWindow (toWID recLoc.rlParentId) windows
			  deviceEvent			= if found (Just (ReceiverEvent msgEvent)) Nothing
			# ioState				= IOStSetDevice (WindowSystemState windows) ioState
			# pState				= {pState & io=ioState}
			= (found,deviceEvent,schedulerEvent,pState)
	where
		recLoc						= getMsgEventRecLoc msgEvent
	
	windowEvent schedulerEvent pState
		= (False,Nothing,schedulerEvent,pState)


/*	filterOSEvent filters the OSEvents that can be handled by this window device.
*/
filterOSEvent :: !OSWindowMetrics !OSEvent !(WindowHandles (PSt .l)) !(IOSt .l)
  -> (!Bool,!Maybe [Int],!Maybe DeviceEvent,!WindowHandles (PSt .l),  !IOSt .l)

filterOSEvent _ {ccMsg=CcWmBUTTONCLICKED,p1=wPtr,p2=cPtr,p3=mods,p4=toolbarIndex} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)			= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	| otherwise
		# (wids,  wsH)		= getWindowStateHandleWIDS wsH
		  (itemNr,wsH)		= getControlsItemNr cPtr wsH
		  controlSelectInfo	= if (itemNr==0)	// itemNrs are always > 0
	  							Nothing
	  							(Just (ControlSelection {csWIDS		= wids
														,csItemNr	= itemNr
														,csItemPtr	= cPtr
														,csMoreData	= 0
														,csModifiers= toModifiers mods
														})
								)
		= (True,Nothing,controlSelectInfo,setWindowHandlesWindow wsH windows,ioState)
where
	getControlsItemNr :: !OSWindowPtr !(WindowStateHandle .pst) -> (!Int,!WindowStateHandle .pst)
	getControlsItemNr cPtr wsH=:{wshHandle=Just {wlsHandle={whItems}}}
		= (snd (getControlsItemNr cPtr whItems),wsH)
	where
		getControlsItemNr :: !OSWindowPtr ![WElementHandle .ls .pst] -> (!Bool,!Int)
		getControlsItemNr cPtr [itemH:itemHs]
			# (found,itemNr)					= getControlItemNr cPtr itemH
			| found								= (found,itemNr)
			| otherwise							= getControlsItemNr cPtr itemHs
		where
			getControlItemNr :: !OSWindowPtr !(WElementHandle .ls .pst) -> (!Bool,!Int)
			getControlItemNr cPtr (WItemHandle itemH=:{wItemPtr})
				| cPtr==wItemPtr				= (True,itemNr)
				| itemKind==IsRadioControl		= (Contains (\{radioItemPtr}->radioItemPtr==cPtr) (getWItemRadioInfo info).radioItems,itemNr)
				| itemKind==IsCheckControl		= (Contains (\{checkItemPtr}->checkItemPtr==cPtr) (getWItemCheckInfo info).checkItems,itemNr)
				| itemSelect && itemH.wItemShow	= getControlsItemNr cPtr itemH.wItems
				| otherwise						= (False,0)
			where
				info							= itemH.wItemInfo
				itemKind						= itemH.wItemKind
				itemSelect						= itemH.wItemSelect
				itemNr							= if itemSelect itemH.wItemNr 0
			
			getControlItemNr cPtr (WListLSHandle itemHs)
				= getControlsItemNr cPtr itemHs
			
			getControlItemNr cPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				= getControlsItemNr cPtr itemHs
			
			getControlItemNr cPtr (WChangeLSHandle wExH=:{wChangeItems=itemHs})
				= getControlsItemNr cPtr itemHs
		
		getControlsItemNr _ _
			= (False,0)
	
	getControlsItemNr _ _
		= windoweventFatalError "getControlsItemNr" "window placeholder not expected"

filterOSEvent _ {ccMsg=CcWmCOMBOSELECT,p1=wPtr,p2=cPtr,p3=index} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)			= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	| otherwise
		# (wids,  wsH)		= getWindowStateHandleWIDS wsH
		  (itemNr,wsH)		= getPopUpControlItemNr cPtr wsH
		  controlSelectInfo	= if (itemNr==0)	// itemNrs are always > 0
								Nothing
								(Just (ControlSelection {csWIDS		= wids
														,csItemNr	= itemNr
														,csItemPtr	= cPtr
														,csMoreData	= index+1
														,csModifiers= NoModifiers
														})
								)
		= (True,Nothing,controlSelectInfo,setWindowHandlesWindow wsH windows,ioState)
where
	getPopUpControlItemNr :: !OSWindowPtr !(WindowStateHandle .pst) -> (!Int,!WindowStateHandle .pst)
	getPopUpControlItemNr cPtr wsH=:{wshHandle=Just {wlsHandle={whItems}}}
		= (snd (getPopUpControlsItemNr cPtr whItems),wsH)
	where
		getPopUpControlsItemNr :: !OSWindowPtr ![WElementHandle .ls .pst] -> (!Bool,!Int)
		getPopUpControlsItemNr cPtr [itemH:itemHs]
			# (found,itemNr)	= getPopUpControlItemNr cPtr itemH
			| found				= (found,itemNr)
			| otherwise			= getPopUpControlsItemNr cPtr itemHs
		where
			getPopUpControlItemNr :: !OSWindowPtr !(WElementHandle .ls .pst) -> (!Bool,!Int)
			getPopUpControlItemNr cPtr (WItemHandle itemH=:{wItemPtr})
				| cPtr==wItemPtr= (True,if (itemKind==IsPopUpControl && itemSelect && itemShow) itemNr 0)
				| itemShow		= getPopUpControlsItemNr cPtr itemH.wItems
				| otherwise		= (False,0)
			where
				itemNr			= itemH.wItemNr
				itemKind		= itemH.wItemKind
				itemSelect		= itemH.wItemSelect
				itemShow		= itemH.wItemShow
			
			getPopUpControlItemNr cPtr (WListLSHandle itemHs)
				= getPopUpControlsItemNr cPtr itemHs
			
			getPopUpControlItemNr cPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				= getPopUpControlsItemNr cPtr itemHs
			
			getPopUpControlItemNr cPtr (WChangeLSHandle wExH=:{wChangeItems=itemHs})
				= getPopUpControlsItemNr cPtr itemHs
		
		getPopUpControlsItemNr _ _
			= (False,0)
	
	getPopUpControlItemNr _ _
		= windoweventFatalError "getPopUpControlItemNr" "window placeholder not expected"

filterOSEvent _ {ccMsg=CcWmDRAWCONTROL,p1=wPtr,p2=cPtr,p3=gc} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		# (controls,wsH)	= getUpdateControls cPtr wsH
		  updateInfo		= if (isEmpty controls)
								Nothing
								(Just (WindowUpdate {updWIDS=wids,updWindowArea=zero,updControls=controls,updGContext=Just gc}))
		= (True,Nothing,updateInfo,setWindowHandlesWindow wsH windows,ioState)
where
	getUpdateControls :: !OSWindowPtr !(WindowStateHandle .pst) -> (![ControlUpdateInfo],!WindowStateHandle .pst)
	getUpdateControls cPtr wsH=:{wshHandle=Just {wlsHandle={whItems,whSize}}}
		= (snd (getUpdateControls cPtr (SizeToRect whSize) whItems),wsH)
	where
		getUpdateControls :: !OSWindowPtr !Rect ![WElementHandle .ls .pst] -> (!Bool,![ControlUpdateInfo])
		getUpdateControls cPtr clipRect [itemH:itemHs]
			# (found,controls)		= getUpdateControl cPtr clipRect itemH
			| found					= (found,controls)
			| otherwise				= getUpdateControls cPtr clipRect itemHs
		where
			getUpdateControl :: !OSWindowPtr !Rect !(WElementHandle .ls .pst) -> (!Bool,![ControlUpdateInfo])
			getUpdateControl cPtr clipRect (WItemHandle itemH=:{wItemPtr})
				| cPtr==wItemPtr	= (True,[{cuItemNr=itemH.wItemNr,cuItemPtr=wItemPtr,cuArea=clipRect1}])
				| itemH.wItemShow	= getUpdateControls cPtr clipRect1 itemH.wItems
				| otherwise			= (False,[])
			where
				clipRect1			= IntersectRects clipRect (PosSizeToRect itemH.wItemPos itemH.wItemSize)
			
			getUpdateControl cPtr clipRect (WListLSHandle itemHs)
				= getUpdateControls cPtr clipRect itemHs
			
			getUpdateControl cPtr clipRect (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				= getUpdateControls cPtr clipRect itemHs
			
			getUpdateControl cPtr clipRect (WChangeLSHandle wExH=:{wChangeItems=itemHs})
				= getUpdateControls cPtr clipRect itemHs
		
		getUpdateControls _ _ _
			= (False,[])
	
	getUpdateControls _ _
		= windoweventFatalError "getUpdateControls" "placeholder not expected"

/*	PA: CcWmIDLEDIALOG is sent after a modal dialogue and its controls have been created.
		At that moment the initialisation action can be evaluated. This is done by the 
		WindowInitialise device event. 
*/
filterOSEvent _ {ccMsg=CcWmIDLEDIALOG,p1=wPtr} windows ioState
	# (maybeWIDS,windows)		= getWindowHandlesActiveModalDialog windows
	| isNothing maybeWIDS
		= (False,Nothing,Nothing,windows,ioState)
	# wids						= fromJust maybeWIDS
	| wPtr<>wids.wPtr
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		= (True,Nothing,Just (WindowInitialise (fromJust maybeWIDS)),windows,ioState)

/*	PA:	CcWmINITDIALOG is generated for modal dialogs. It should create all the controls of the
		dialog, and return the desired position, size, and focus control of the dialog.
		In addition, the return DeviceEvent should be WindowInitialise to have the initialisation
		function evaluated.
*/
filterOSEvent wMetrics {ccMsg=CcWmINITDIALOG,p1=wPtr} windows ioState
	# (maybeWIDS,windows)		= getWindowHandlesActiveWindow windows
	| isNothing maybeWIDS
		= (False,Nothing,Nothing,windows,ioState)
	# wids						= fromJust maybeWIDS
	| wids.wPtr<>0
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		# (_,wsH,windows)		= removeWindowHandlesWindow (toWID 0) windows
		  wids					= {wids & wPtr=wPtr}
		  wsH					= (\wsH->{wsH & wshIds=wids}) wsH
		# (tb,ioState)			= getIOToolbox ioState
		# (returnOS,wsH,tb)		= createDialogControls wMetrics wsH tb
		# ioState				= setIOToolbox tb ioState
		  windows				= addWindowHandlesActiveWindow wsH windows
		= (True,Just returnOS,Nothing,windows,ioState)
where
	createDialogControls :: !OSWindowMetrics !(WindowStateHandle .pst) !*OSToolbox
								  -> (![Int], !WindowStateHandle .pst, !*OSToolbox)
	createDialogControls wMetrics wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems=itemHs,whSize={w,h}}}} tb
		# (itemHs,tb)			= createControls wMetrics whDefaultId whCancelId True wPtr itemHs tb
		# (itemPtr,wH)			= getInitActiveControl {wH & whItems=itemHs}
		  r5cci					= [-1,-1,w,h,if (itemPtr==OSNoWindowPtr) 0 itemPtr]
		= (r5cci,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	where
		whDefaultId				= wH.whDefaultId
		whCancelId				= wH.whCancelId
	createDialogControls _ _ _
		= windoweventFatalError "createDialogControls" "placeholder not expected"

filterOSEvent _ {ccMsg=CcWmSCROLLBARACTION,p1=wPtr,p2=cPtr,p3=iBar,p4=action,p5=osThumb} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)			= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		  (sliderEvent,wsH)	= getSlidersEvent wids iBar osThumb cPtr wsH
		= (True,Nothing,Just sliderEvent,setWindowHandlesWindow wsH windows,ioState)
where
	getSlidersEvent :: !WIDS !Int !Int !OSWindowPtr !(WindowStateHandle .pst) -> (!DeviceEvent,!WindowStateHandle .pst)
	getSlidersEvent wids iBar osThumb itemPtr wsH=:{wshHandle=Just {wlsHandle={whWindowInfo,whItems,whSize={w,h}}}}
		| wids.wPtr==itemPtr
			= (WindowScrollAction info,wsH)
		with
			info				= {	wsaWIDS			= wids
								  ,	wsaSliderMove	= move min max view osThumb
								  ,	wsaDirection	= if isHorizontal Horizontal Vertical
								  }
			windowInfo			= getWindowInfoWindowData whWindowInfo
			domainRect			= windowInfo.windowDomain
			isHorizontal		= iBar==SB_HORZ
			(min,max,view)		= if isHorizontal
									(domainRect.rleft,domainRect.rright, w)
									(domainRect.rtop, domainRect.rbottom,h)
		# (found,sliderEvent)	= getSlidersEvent wids iBar osThumb itemPtr whItems
		| found
			= (sliderEvent,wsH)
		| otherwise
			= windoweventFatalError "getSlidersEvent" "SliderControl could not be located"
	where
		getSlidersEvent :: !WIDS !Int !Int !OSWindowPtr ![WElementHandle .ls .pst] -> (!Bool,!DeviceEvent)
		getSlidersEvent wids iBar osThumb itemPtr [itemH:itemHs]
			# (found,sliderEvent)	= getSliderEvent wids iBar osThumb itemPtr itemH
			| found					= (found,sliderEvent)
			| otherwise				= getSlidersEvent wids iBar osThumb itemPtr itemHs
		where
			getSliderEvent :: !WIDS !Int !Int !OSWindowPtr !(WElementHandle .ls .pst) -> (!Bool,!DeviceEvent)
			getSliderEvent wids iBar osThumb itemPtr (WItemHandle itemH=:{wItemPtr,wItemKind})
				| itemPtr<>itemH.wItemPtr
					| itemH.wItemShow
						= getSlidersEvent wids iBar osThumb itemPtr itemH.wItems
					// otherwise
						= (False,ControlSliderAction dummySlidersEvent)
				| wItemKind==IsCompoundControl
					= (True,CompoundScrollAction info)
				with
					info			= {	csaWIDS			= wids
									  ,	csaItemNr		= itemH.wItemNr
									  ,	csaItemPtr		= itemPtr
									  ,	csaSliderMove	= move min max view osThumb
									  ,	csaDirection	= if isHorizontal Horizontal Vertical
									  }
					compoundSize	= itemH.wItemSize
					compoundInfo	= getWItemCompoundInfo itemH.wItemInfo
					domainRect		= compoundInfo.compoundDomain
					isHorizontal	= iBar==SB_HORZ
					(min,max,view)	= if isHorizontal
										(domainRect.rleft,domainRect.rright, compoundSize.w)
										(domainRect.rtop, domainRect.rbottom,compoundSize.h)
				| otherwise
					= (True,ControlSliderAction info)
				with
					info			= {	cslWIDS			= wids
									  ,	cslItemNr		= itemH.wItemNr
									  ,	cslItemPtr		= itemPtr
									  ,	cslSliderMove	= move sliderState.sliderMin sliderState.sliderMax 0 osThumb
									  }
					sliderInfo		= getWItemSliderInfo itemH.wItemInfo
					sliderState		= sliderInfo.sliderInfoState
			
			getSliderEvent wids iBar osThumb itemPtr (WListLSHandle itemHs)
				= getSlidersEvent wids iBar osThumb itemPtr itemHs
			
			getSliderEvent wids iBar osThumb itemPtr (WExtendLSHandle {wExtendItems=itemHs})
				= getSlidersEvent wids iBar osThumb itemPtr itemHs
			
			getSliderEvent wids iBar osThumb itemPtr (WChangeLSHandle {wChangeItems=itemHs})
				= getSlidersEvent wids iBar osThumb itemPtr itemHs
		
		getSlidersEvent _ _ _ _ _
			= (False,ControlSliderAction dummySlidersEvent)
		
		dummySlidersEvent	= { cslWIDS=wids,cslItemNr=0,cslItemPtr=0,cslSliderMove=SliderIncSmall }
	
	getSlidersEvent _ _ _ _ _
		= windoweventFatalError "getSlidersEvent" "placeholder not expected"
	
	move :: !Int !Int !Int !Int -> SliderMove
	move min max view osThumb
		= case action of
			SB_LINEUP		-> SliderDecSmall
			SB_LINEDOWN		-> SliderIncSmall
			SB_PAGEUP		-> SliderDecLarge
			SB_PAGEDOWN		-> SliderIncLarge
			SB_THUMBPOSITION-> SliderThumb (fromOSscrollbarRange (min,max) osThumb)
			SB_THUMBTRACK	-> SliderThumb (fromOSscrollbarRange (min,max) osThumb)
			SB_TOP			-> SliderThumb min
			SB_BOTTOM		-> SliderThumb (max-view)
			SB_ENDSCROLL	-> SliderThumb (fromOSscrollbarRange (min,max) osThumb)

filterOSEvent _ {ccMsg=CcWmACTIVATE,p1=wPtr} windows ioState
	# (found,wsH,windows)		= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (active,wsH)				= getWindowStateHandleActive wsH
	| active				// The window is already active, skip
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	| otherwise
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		  windows				= setWindowHandlesWindow wsH windows
		  (activeModal,windows)	= getWindowHandlesActiveModalDialog windows
		= (True,Nothing,if (isJust activeModal) (Just (WindowInitialise wids)) (Just (WindowActivation wids)),windows,ioState)

filterOSEvent _ {ccMsg=CcWmCLOSE,p1=wPtr} windows ioState
	# (found,wsH,windows)		= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		  windows				= setWindowHandlesWindow wsH windows
		= (True,Nothing,Just (WindowRequestClose wids),windows,ioState)

filterOSEvent _ {ccMsg=CcWmDEACTIVATE,p1=wPtr} windows ioState
	# (found,wsH,windows)		= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (active,wsH)				= getWindowStateHandleActive wsH
	| not active				// The window is already inactive, skip
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	| otherwise
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		  windows				= setWindowHandlesWindow wsH windows
		  (activeModal,windows)	= getWindowHandlesActiveModalDialog windows
		= (True,Nothing,if (isJust activeModal) Nothing (Just (WindowDeactivation wids)),windows,ioState)

filterOSEvent _ {ccMsg=CcWmKEYBOARD,p1=wPtr,p2=cPtr,p3=keycode,p4=state,p5=mods} windows ioState
	# (found,wsH,windows)			= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (wids,wsH)					= getWindowStateHandleWIDS wsH
	| wPtr==cPtr					// The keyboard action takes place in the window
		# (inputTrack,ioState)		= IOStGetInputTrack ioState
		  (ok,key,wsH,inputTrack)	= okWindowKeyboardState keycode state mods wsH inputTrack
		# ioState					= IOStSetInputTrack inputTrack ioState
		  deviceEvent				= if ok (Just (WindowKeyboardAction {wkWIDS=wids,wkKeyboardState=key})) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okWindowKeyboardState :: !Int !Int !Int !(WindowStateHandle .pst) !(Maybe InputTrack)
						 -> (!Bool,KeyboardState,!WindowStateHandle .pst, ! Maybe InputTrack)
		okWindowKeyboardState keycode state mods wsH=:{wshHandle=Just {wlsHandle={whKind,whWindowInfo,whAtts}}} inputTrack
			| whKind==IsDialog
				= (False,undef,wsH,inputTrack)
			| trackingKeyboard wPtr 0 inputTrack								// Window is already handle Key(Repeat/Up)
				| isDownKey														// Ignore all key down events
					= (False,undef,wsH,inputTrack)
				| pressState==KeyUp
					= (okKeyboardAtt,keystate,wsH,untrackKeyboard inputTrack)	// Clear keyboard tracking
				// otherwise
					= (okKeyboardAtt,keystate,wsH,inputTrack)
			| isDownKey
				= (okKeyboardAtt,keystate,wsH,trackKeyboard wPtr 0 inputTrack)	// Key down sets input track
			| otherwise
				= (False,undef,wsH,inputTrack)
		where
			keystate				= keyState keycode state mods
			pressState				= getKeyboardStateKeyState keystate
			isDownKey				= pressState==KeyDown False
			(filter,selectState,_)	= getWindowKeyboardAtt (snd (Select isWindowKeyboard (WindowKeyboard (const False) Unable undef) whAtts))
			okKeyboardAtt			= filter keystate && selectState==Able
		okWindowKeyboardState _ _ _ _ _
			= windoweventFatalError "okWindowKeyboardState" "placeholder not expected"
	| otherwise				// The keyboard action takes place in a control
		# (inputTrack,ioState)			= IOStGetInputTrack ioState
		  (ok,itemNr,key,wsH,inputTrack)= okControlItemsNrKeyboardState wPtr cPtr keycode state mods wsH inputTrack
		# ioState						= IOStSetInputTrack inputTrack ioState
		  info							= {	ckWIDS			= wids
										  ,	ckItemNr		= itemNr
										  ,	ckItemPtr		= cPtr
										  ,	ckKeyboardState	= key
										  }
		  deviceEvent					= if ok (Just (ControlKeyboardAction info)) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okControlItemsNrKeyboardState :: !OSWindowPtr !OSWindowPtr !Int !Int !Int !(WindowStateHandle .pst) !(Maybe InputTrack)
													  -> (!Bool,!Int,KeyboardState,!WindowStateHandle .pst, ! Maybe InputTrack)
		okControlItemsNrKeyboardState wPtr itemPtr keycode state mods wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems}}} inputTrack
			# (_,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrKeyboardState wPtr itemPtr True keycode state mods whItems inputTrack
			= (ok,itemNr,itemPos,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}},inputTrack)
		where
			okControlsItemNrKeyboardState :: !OSWindowPtr !OSWindowPtr !Bool !Int !Int !Int ![WElementHandle .ls .pst] !(Maybe InputTrack)
														 -> (!Bool,!Bool,!Int,KeyboardState,![WElementHandle .ls .pst],! Maybe InputTrack)
			okControlsItemNrKeyboardState wPtr itemPtr contextAble keycode state mods [itemH:itemHs] inputTrack
				# (found,ok,itemNr,itemPos,itemH,inputTrack)	= okControlItemNrKeyboardState wPtr itemPtr contextAble keycode state mods itemH inputTrack
				| found
					= (found,ok,itemNr,itemPos,[itemH:itemHs],inputTrack)
				| otherwise
					# (found,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrKeyboardState wPtr itemPtr contextAble keycode state mods itemHs inputTrack
					= (found,ok,itemNr,itemPos,[itemH:itemHs],inputTrack)
			where
				okControlItemNrKeyboardState :: !OSWindowPtr !OSWindowPtr !Bool !Int !Int !Int !(WElementHandle .ls .pst) !(Maybe InputTrack)
															 -> (!Bool,!Bool,!Int,KeyboardState,!WElementHandle .ls .pst, ! Maybe InputTrack)
				okControlItemNrKeyboardState wPtr itemPtr contextAble keycode state mods (WItemHandle itemH=:{wItemPtr,wItemKind,wItemSelect,wItemAtts}) inputTrack
					| itemPtr<>wItemPtr
						| not itemH.wItemShow
							= (False,False,0,undef,WItemHandle itemH,inputTrack)
						// otherwise
							# (found,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrKeyboardState wPtr itemPtr contextAble1 keycode state mods itemH.wItems inputTrack
							= (found,ok,itemNr,itemPos,WItemHandle {itemH & wItems=itemHs},inputTrack)
					| trackingKeyboard wPtr itemPtr inputTrack		// Control is already handling Key(Repeat/Up)
						| isDownKey									// Ignore all key down events
							= (True,False,0,undef,WItemHandle itemH,inputTrack)
						| pressState==KeyUp							// Clear keyboard tracking
							= (True,okKeyboardAtt,itemNr,keystate,WItemHandle itemH,untrackKeyboard inputTrack)
						// otherwise
							= (True,okKeyboardAtt,itemNr,keystate,WItemHandle itemH,inputTrack)
					| isDownKey										// Key down sets input track
						= (True,okKeyboardAtt,itemNr,keystate,WItemHandle itemH,trackKeyboard wPtr itemPtr inputTrack)
					| otherwise
						= (True,False,0,undef,WItemHandle itemH,inputTrack)
				where
					contextAble1			= contextAble && wItemSelect
  					noKeyboardAtt			= ControlKeyboard (const False) Unable undef
					(filter,selectState,_)	= getControlKeyboardAtt (snd (Select isControlKeyboard noKeyboardAtt wItemAtts))
					okKeyboardAtt			= contextAble1 && enabled selectState && filter keystate
					keystate				= keyState keycode state mods
					pressState				= getKeyboardStateKeyState keystate
					isDownKey				= pressState==KeyDown False
					itemNr					= itemH.wItemNr
				
				okControlItemNrKeyboardState wPtr itemPtr contextAble keycode state mods (WListLSHandle itemHs) inputTrack
					# (found,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrKeyboardState wPtr itemPtr contextAble keycode state mods itemHs inputTrack
					= (found,ok,itemNr,itemPos,WListLSHandle itemHs,inputTrack)
				
				okControlItemNrKeyboardState wPtr itemPtr contextAble keycode state mods (WExtendLSHandle wExH=:{wExtendItems=itemHs}) inputTrack
					# (found,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrKeyboardState wPtr itemPtr contextAble keycode state mods itemHs inputTrack
					= (found,ok,itemNr,itemPos,WExtendLSHandle {wExH & wExtendItems=itemHs},inputTrack)
				
				okControlItemNrKeyboardState wPtr itemPtr contextAble keycode state mods (WChangeLSHandle wChH=:{wChangeItems=itemHs}) inputTrack
					# (found,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrKeyboardState wPtr itemPtr contextAble keycode state mods itemHs inputTrack
					= (found,ok,itemNr,itemPos,WChangeLSHandle {wChH & wChangeItems=itemHs},inputTrack)
			
			okControlsItemNrKeyboardState _ _ _ _ _ _ itemH inputTrack
				= (False,False,0,undef,itemH,inputTrack)
		
		okControlItemsNrKeyboardState _ _ _ _ _ _ _
			= windoweventFatalError "okControlItemsNrKeyboardState" "window placeholder not expected"
where
	keyState :: !Int !Int !Int -> KeyboardState
	keyState keycode state mods
		| isSpecial			= SpecialKey special ks modifiers
		| otherwise			= CharKey (toChar keycode) ks
	where
		modifiers			= toModifiers mods
		ks					= case state of
								KEYDOWN		-> KeyDown False
								KEYREPEAT	-> KeyDown True
								KEYUP		-> KeyUp
		(isSpecial,special)	= case keycode of
								WinBackSpKey-> (True,BackSpaceKey)
								WinBeginKey	-> (True,BeginKey)
								WinDelKey	-> (True,DeleteKey)
								WinDownKey	-> (True,DownKey)
								WinEndKey	-> (True,EndKey)
								WinEscapeKey-> (True,EscapeKey)
								WinHelpKey	-> (True,HelpKey)
								WinLeftKey	-> (True,LeftKey)
								WinPgDownKey-> (True,PgDownKey)
								WinPgUpKey	-> (True,PgUpKey)
								WinReturnKey-> (True,EnterKey)
								WinRightKey	-> (True,RightKey)
								WinUpKey	-> (True,UpKey)
								WinF1Key	-> (True,F1Key)
								WinF2Key	-> (True,F2Key)
								WinF3Key	-> (True,F3Key)
								WinF4Key	-> (True,F4Key)
								WinF5Key	-> (True,F5Key)
								WinF6Key	-> (True,F6Key)
								WinF7Key	-> (True,F7Key)
								WinF8Key	-> (True,F8Key)
								WinF9Key	-> (True,F9Key)
								WinF10Key	-> (True,F10Key)
								WinF11Key	-> (True,F11Key)
								WinF12Key	-> (True,F12Key)
								_			-> (False,undef)

filterOSEvent _ {ccMsg=CcWmKILLFOCUS,p1=wPtr,p2=cPtr} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		  (itemNr,wsH)		= getControlKeyFocusItemNr False cPtr wsH
		  windows			= setWindowHandlesWindow wsH windows
		= (True,Nothing,Just (ControlLooseKeyFocus {ckfWIDS=wids,ckfItemNr=itemNr,ckfItemPtr=cPtr}),windows,ioState)

filterOSEvent _ {ccMsg=CcWmLOSTKEY,p1=wPtr,p2=cPtr} windows ioState
	# (found,wsH,windows)			= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)					= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	# (wids,wsH)					= getWindowStateHandleWIDS wsH
	| wPtr==cPtr	// The window lost the keyboard input
		# (ok,wsH)					= okWindowKeyLost wsH
		  deviceEvent				= if ok (Just (WindowKeyboardAction {wkWIDS=wids,wkKeyboardState=KeyLost})) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okWindowKeyLost :: !(WindowStateHandle .pst) -> (!Bool,!WindowStateHandle .pst)
		okWindowKeyLost wsH=:{wshHandle=Just {wlsHandle={whKind,whAtts}}}
			| whKind==IsDialog
				= (False,wsH)
			| otherwise
				= (okKeyAtt,wsH)
		where
			(filter,selectState,_)	= getWindowKeyboardAtt (snd (Select isWindowKeyboard (WindowKeyboard (const False) Unable undef) whAtts))
			okKeyAtt				= filter KeyLost && selectState==Able
		okWindowKeyLost _
			= windoweventFatalError "okWindowKeyLost" "placeholder not expected"
	| otherwise		// One of the window controls lost the keyboard input
		# (ok,itemNr,wsH)			= okControlItemNrsKeyLost cPtr wsH
		  info						= {	ckWIDS			= wids
									  ,	ckItemNr		= itemNr
									  ,	ckItemPtr		= cPtr
									  ,	ckKeyboardState	= KeyLost
									  }
		  deviceEvent				= if (ok && itemNr>0) (Just (ControlKeyboardAction info)) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okControlItemNrsKeyLost :: !OSWindowPtr !(WindowStateHandle .pst) -> (!Bool,!Int,!WindowStateHandle .pst)
		okControlItemNrsKeyLost itemPtr wsH=:{wshHandle=Just {wlsHandle={whItems}}}
			# (_,ok,itemNr)			= okControlsItemNrKeyLost True itemPtr whItems
			= (ok,itemNr,wsH)
		where
			okControlsItemNrKeyLost :: !Bool !OSWindowPtr ![WElementHandle .ls .pst] -> (!Bool,!Bool,!Int)
			okControlsItemNrKeyLost contextAble itemPtr [itemH:itemHs]
				# (found,ok,itemNr)	= okControlItemNrKeyLost contextAble itemPtr itemH
				| found				= (found,ok,itemNr)
				| otherwise			= okControlsItemNrKeyLost contextAble itemPtr itemHs
			where
				okControlItemNrKeyLost :: !Bool !OSWindowPtr !(WElementHandle .ls .pst) -> (!Bool,!Bool,!Int)
				okControlItemNrKeyLost contextAble itemPtr (WItemHandle itemH=:{wItemPtr,wItemSelect,wItemAtts})
					| itemPtr<>wItemPtr
						| itemH.wItemShow
							= okControlsItemNrKeyLost contextAble1 itemPtr itemH.wItems
						// otherwise
							= (False,False,0)
					| otherwise
						= (True,okKeyAtt,itemNr)
				where
					contextAble1= contextAble && wItemSelect
					(filter,selectState,_)
								= getControlKeyboardAtt (snd (Select isControlKeyboard (ControlKeyboard (const False) Unable undef) wItemAtts))
					okKeyAtt	= contextAble1 && enabled selectState && filter KeyLost
					itemNr		= itemH.wItemNr
									
				okControlItemNrKeyLost contextAble itemPtr (WListLSHandle itemHs)
					= okControlsItemNrKeyLost contextAble itemPtr itemHs
				
				okControlItemNrKeyLost contextAble itemPtr (WExtendLSHandle {wExtendItems=itemHs})
					= okControlsItemNrKeyLost contextAble itemPtr itemHs
				
				okControlItemNrKeyLost contextAble itemPtr (WChangeLSHandle {wChangeItems=itemHs})
					= okControlsItemNrKeyLost contextAble itemPtr itemHs
			
			okControlsItemNrKeyLost _ _ _
				= (False,False,0)
		
		okControlItemNrsKeyLost _ _
			= windoweventFatalError "okControlItemNrsKeyLost" "placeholder not expected"

filterOSEvent _ {ccMsg=CcWmLOSTMOUSE,p1=wPtr,p2=cPtr} windows ioState
	# (found,wsH,windows)			= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)					= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	# (wids,wsH)					= getWindowStateHandleWIDS wsH
	| wPtr==cPtr	// The window lost the mouse input
		# (ok,wsH)					= okWindowMouseLost wsH
		  deviceEvent				= if ok (Just (WindowMouseAction {wmWIDS=wids,wmMouseState=MouseLost})) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okWindowMouseLost :: !(WindowStateHandle .pst) -> (!Bool,!WindowStateHandle .pst)
		okWindowMouseLost wsH=:{wshHandle=Just {wlsHandle={whKind,whAtts}}}
			| whKind==IsDialog
				= (False,wsH)
			| otherwise
				= (okMouseAtt,wsH)
		where
			(filter,selectState,_)	= getWindowMouseAtt (snd (Select isWindowMouse (WindowMouse (const False) Unable undef) whAtts))
			okMouseAtt				= filter MouseLost && selectState==Able
		okWindowMouseLost _
			= windoweventFatalError "okWindowMouseLost" "placeholder not expected"
	| otherwise		// One of the window controls lost the mouse input
		# (ok,itemNr,wsH)			= okControlItemNrsMouseLost cPtr wsH
		  info						= {	cmWIDS			= wids
									  ,	cmItemNr		= itemNr
									  ,	cmItemPtr		= cPtr
									  ,	cmMouseState	= MouseLost
									  }
		  deviceEvent				= if (ok && itemNr>0) (Just (ControlMouseAction info)) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okControlItemNrsMouseLost :: !OSWindowPtr !(WindowStateHandle .pst) -> (!Bool,!Int,!WindowStateHandle .pst)
		okControlItemNrsMouseLost itemPtr wsH=:{wshHandle=Just {wlsHandle={whItems}}}
			# (_,ok,itemNr)			= okControlsItemNrMouseLost True itemPtr whItems
			= (ok,itemNr,wsH)
		where
			okControlsItemNrMouseLost :: !Bool !OSWindowPtr ![WElementHandle .ls .pst] -> (!Bool,!Bool,!Int)
			okControlsItemNrMouseLost contextAble itemPtr [itemH:itemHs]
				# (found,ok,itemNr)	= okControlItemNrMouseLost contextAble itemPtr itemH
				| found				= (found,ok,itemNr)
				| otherwise			= okControlsItemNrMouseLost contextAble itemPtr itemHs
			where
				okControlItemNrMouseLost :: !Bool !OSWindowPtr !(WElementHandle .ls .pst) -> (!Bool,!Bool,!Int)
				okControlItemNrMouseLost contextAble itemPtr (WItemHandle itemH=:{wItemPtr,wItemSelect,wItemAtts})
					| itemPtr<>wItemPtr
						| itemH.wItemShow
							= okControlsItemNrMouseLost contextAble1 itemPtr itemH.wItems
						// otherwise
							= (False,False,0)
					| otherwise
						= (True,okMouseAtt,itemNr)
				where
					contextAble1= contextAble && wItemSelect
					(filter,selectState,_)
								= getControlMouseAtt (snd (Select isControlMouse (ControlMouse (const False) Unable undef) wItemAtts))
					okMouseAtt	= contextAble1 && enabled selectState && filter MouseLost
					itemNr		= itemH.wItemNr
									
				okControlItemNrMouseLost contextAble itemPtr (WListLSHandle itemHs)
					= okControlsItemNrMouseLost contextAble itemPtr itemHs
				
				okControlItemNrMouseLost contextAble itemPtr (WExtendLSHandle {wExtendItems=itemHs})
					= okControlsItemNrMouseLost contextAble itemPtr itemHs
				
				okControlItemNrMouseLost contextAble itemPtr (WChangeLSHandle {wChangeItems=itemHs})
					= okControlsItemNrMouseLost contextAble itemPtr itemHs
			
			okControlsItemNrMouseLost _ _ _
				= (False,False,0)
		
		okControlItemNrsMouseLost _ _
			= windoweventFatalError "okControlItemNrsMouseLost" "placeholder not expected"

filterOSEvent _ {ccMsg=CcWmMOUSE,p1=wPtr,p2=cPtr,p3=action,p4=x,p5=y,p6=mods} windows ioState
	# (found,wsH,windows)			= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)					= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	# (wids,wsH)					= getWindowStateHandleWIDS wsH
	| wPtr==cPtr	// The mouse action takes place in the window
		# (inputTrack,ioState)		= IOStGetInputTrack ioState
		  (ok,mouse,wsH,inputTrack)	= okWindowMouseState action {x=x,y=y} wsH inputTrack
		  deviceEvent				= if ok (Just (WindowMouseAction {wmWIDS=wids,wmMouseState=mouse})) Nothing
		# ioState					= IOStSetInputTrack inputTrack ioState
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okWindowMouseState :: !Int !Point2 !(WindowStateHandle .pst) !(Maybe InputTrack)
					  -> (!Bool,MouseState,!WindowStateHandle .pst, ! Maybe InputTrack)
		okWindowMouseState action eventPos wsH=:{wshHandle=Just {wlsHandle={whKind,whWindowInfo,whAtts}}} inputTrack
			| whKind==IsDialog
				= (False,undef,wsH,inputTrack)
			| trackingMouse wPtr 0 inputTrack					// Window is already handling Mouse(Drag/Up)
				| isDownButton || buttonstate==ButtonStillUp	// Ignore all mouse down and mouse move events
					= (False,undef,wsH,inputTrack)
				| buttonstate==ButtonUp							// Clear mouse tracking
					= (okMouseAtt,mousestate,wsH,untrackMouse inputTrack)
				// otherwise
					= (okMouseAtt,mousestate,wsH,inputTrack)
			| isDownButton										// Mouse down event sets input track
				= (okMouseAtt,mousestate,wsH,trackMouse wPtr 0 inputTrack)
			| isMember buttonstate [ButtonStillDown,ButtonUp]	// Ignore all mouse drag and up events when not tracking
				= (False,undef,wsH,inputTrack)
			| otherwise
				= (okMouseAtt,mousestate,wsH,inputTrack)
		where
			origin					= (getWindowInfoWindowData whWindowInfo).windowOrigin
			mousestate				= mouseState action (eventPos+origin)
			buttonstate				= getMouseStateButtonState mousestate
			isDownButton			= isMember buttonstate [ButtonDown,ButtonDoubleDown,ButtonTripleDown]
			(filter,selectState,_)	= getWindowMouseAtt (snd (Select isWindowMouse (WindowMouse (const False) Unable undef) whAtts))
			okMouseAtt				= filter mousestate && selectState==Able
		okWindowMouseState _ _ _ _
			= windoweventFatalError "okWindowMouseState" "placeholder not expected"
	| otherwise				// The mouse action takes place in a control
		# (inputTrack,ioState)		= IOStGetInputTrack ioState
		  (ok,itemNr,mouse,wsH,inputTrack)
		  							= okControlItemsNrMouseState wPtr cPtr action {x=x,y=y} wsH inputTrack
		# ioState					= IOStSetInputTrack inputTrack ioState
		  info						= {	cmWIDS			= wids
									  ,	cmItemNr		= itemNr
									  ,	cmItemPtr		= cPtr
									  ,	cmMouseState	= mouse
									  }
		  deviceEvent				= if ok (Just (ControlMouseAction info)) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okControlItemsNrMouseState :: !OSWindowPtr !OSWindowPtr !Int !Point2 !(WindowStateHandle .pst) !(Maybe InputTrack)
												   -> (!Bool,!Int,MouseState,!WindowStateHandle .pst, ! Maybe InputTrack)
		okControlItemsNrMouseState wPtr itemPtr action eventPos wsH=:{wshHandle=Just {wlsHandle={whItems}}} inputTrack
			# (_,ok,itemNr,itemPos,inputTrack)
									= okControlsItemNrMouseState True wPtr itemPtr action eventPos whItems inputTrack
			= (ok,itemNr,itemPos,wsH,inputTrack)
		where
			okControlsItemNrMouseState :: !Bool !OSWindowPtr !OSWindowPtr !Int !Point2 ![WElementHandle .ls .pst] !(Maybe InputTrack)
																				  -> (!Bool,!Bool,!Int,MouseState,!Maybe InputTrack)
			okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos [itemH:itemHs] inputTrack
				# (found,ok,itemNr,itemPos,inputTrack)
										= okControlItemNrMouseState contextAble wPtr itemPtr action eventPos itemH   inputTrack
				| found					= (found,ok,itemNr,itemPos,inputTrack)
				| otherwise				= okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos itemHs inputTrack
			where
				okControlItemNrMouseState :: !Bool !OSWindowPtr !OSWindowPtr !Int !Point2 !(WElementHandle .ls .pst) !(Maybe InputTrack)
																					 -> (!Bool,!Bool,!Int,MouseState,!Maybe InputTrack)
				okControlItemNrMouseState contextAble wPtr itemPtr action eventPos (WItemHandle itemH=:{wItemPtr,wItemSelect,wItemAtts}) inputTrack
					| itemPtr<>wItemPtr
						| itemH.wItemShow
							= okControlsItemNrMouseState contextAble1 wPtr itemPtr action eventPos itemH.wItems inputTrack
						// otherwise
							= (False,False,0,undef,inputTrack)
					| trackingMouse wPtr itemPtr inputTrack				// Control is already handling Mouse(Drag/Up)
						| isDownButton || buttonstate==ButtonStillUp	// Ignore all mouse down and mouse move events
							= (True,False,0,undef,inputTrack)
						| buttonstate==ButtonUp							// Clear mouse tracking
							= (True,okMouseAtt,itemNr,mousestate,untrackMouse inputTrack)
						// otherwise
							= (True,okMouseAtt,itemNr,mousestate,inputTrack)
					| isDownButton										// Mouse down event sets input track
						= (True,okMouseAtt,itemNr,mousestate,trackMouse wPtr itemPtr inputTrack)
					| isMember buttonstate [ButtonStillDown,ButtonUp]	// Ignore all mouse drag and up events when not tracking
						= (True,False,0,undef,inputTrack)
					| otherwise
						= (True,okMouseAtt,itemNr,mousestate,inputTrack)
				where
					contextAble1= contextAble && wItemSelect
					(filter,selectState,_)
								= getControlMouseAtt (snd (Select isControlMouse (ControlMouse (const False) Unable undef) wItemAtts))
					okMouseAtt	= contextAble1 && enabled selectState && filter mousestate
					mousestate	= mouseState action (origin+eventPos)
					buttonstate	= getMouseStateButtonState mousestate
					isDownButton= isMember buttonstate [ButtonDown,ButtonDoubleDown,ButtonTripleDown]
					itemKind	= itemH.wItemKind
					itemNr		= itemH.wItemNr
					origin		= case itemKind of
									IsCustomButtonControl	-> zero
									IsCustomControl			-> zero
									IsCompoundControl		-> (getWItemCompoundInfo itemH.wItemInfo).compoundOrigin
									_						-> windoweventFatalError "okControlItemsNrMouseState" "mouse event generated for unexpected control"
									
				okControlItemNrMouseState contextAble wPtr itemPtr action eventPos (WListLSHandle itemHs) inputTrack
					= okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos itemHs inputTrack
				
				okControlItemNrMouseState contextAble wPtr itemPtr action eventPos (WExtendLSHandle {wExtendItems=itemHs}) inputTrack
					= okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos itemHs inputTrack
				
				okControlItemNrMouseState contextAble wPtr itemPtr action eventPos (WChangeLSHandle {wChangeItems=itemHs}) inputTrack
					= okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos itemHs inputTrack
			
			okControlsItemNrMouseState _ _ _ _ _ _ inputTrack
				= (False,False,0,undef,inputTrack)
		
		okControlItemsNrMouseState _ _ _ _ _ _
			= windoweventFatalError "okControlItemsNrMouseState" "placeholder not expected"
where
	modifiers				= toModifiers mods
	nrDown					= case action of
								BUTTONDOWN			-> 1
								BUTTONDOUBLEDOWN	-> 2
							 	_					-> 3
	mouseState action pos	= case action of
								BUTTONSTILLUP		-> MouseMove pos modifiers
								BUTTONUP			-> MouseUp   pos modifiers
								BUTTONSTILLDOWN		-> MouseDrag pos modifiers
								_					-> MouseDown pos modifiers nrDown

filterOSEvent _ {ccMsg=CcWmSETFOCUS,p1=wPtr,p2=cPtr} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		  (itemNr,wsH)		= getControlKeyFocusItemNr True cPtr wsH
		  windows			= setWindowHandlesWindow wsH windows
		= (True,Nothing,Just (ControlGetKeyFocus {ckfWIDS=wids,ckfItemNr=itemNr,ckfItemPtr=cPtr}),windows,ioState)

filterOSEvent wMetrics {ccMsg=CcWmSIZE,p1=wPtr,p2=w,p3=h,p4=usersizing} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (wKind,wsH)			= getWindowStateHandleWindowKind wsH
	| wKind==IsDialog		// This alternative should never occur
		= windoweventFatalError "filterOSEvent" "WindowSizeAction event generated for Dialog"
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		# (tb,ioState)		= getIOToolbox ioState
		  (info,wsH,tb)		= getWindowStateHandleSize wids w h (usersizing<>0) wsH tb
		# ioState			= setIOToolbox tb ioState
		  windows			= setWindowHandlesWindow wsH windows
		= (True,Nothing,Just (WindowSizeAction info),windows,ioState)
where
	getWindowStateHandleSize :: !WIDS !Int !Int !Bool !(WindowStateHandle .pst) !*OSToolbox -> (!WindowSizeActionInfo,!WindowStateHandle .pst,!*OSToolbox)
	getWindowStateHandleSize wids newW newH usersizing wsH=:{wshHandle=Just {wlsHandle=wH}} tb
		= ({wsWIDS=wids,wsSize={w=newW`,h=newH`},wsUpdateAll=not usersizing},wsH,tb)
	where
		windowInfo				= getWindowInfoWindowData wH.whWindowInfo
		domainRect				= windowInfo.windowDomain
		hasScrolls				= (isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll)
		(visHScroll,visVScroll)	= OSscrollbarsAreVisible wMetrics domainRect (toTuple wH.whSize) hasScrolls
		newW`					= if visVScroll (newW+wMetrics.osmVSliderWidth)  newW	// Correct newW in case of visible vertical   scrollbar
		newH`					= if visHScroll (newH+wMetrics.osmHSliderHeight) newH	// Correct newH in case of visible horizontal scrollbar
	getWindowStateHandleSize _ _ _ _ _ _
		= windoweventFatalError "getWindowStateHandleSize" "placeholder not expected"

filterOSEvent _ {ccMsg=CcWmSPECIALBUTTON,p1=wPtr,p2=okOrCancel} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS      wsH
		  (okId,wsH)		= getWindowStateHandleDefaultId wsH
		  (cancelId,wsH)	= getWindowStateHandleCancelId  wsH
		  okOrCancelEvent	= if (okOrCancel==ISOKBUTTON)     (if (isJust okId)     (Just (WindowOK     wids)) Nothing)
		  					 (if (okOrCancel==ISCANCELBUTTON) (if (isJust cancelId) (Just (WindowCANCEL wids)) Nothing)
		  					 								  (windoweventFatalError "filterOSEvent (CcWmSPECIALBUTTON)" "incorrect argument"))
		= (True,Nothing,okOrCancelEvent,setWindowHandlesWindow wsH windows,ioState)

/*	The CcWmPAINT message is generated to update the indicated rectangle of the argument window.
*/
filterOSEvent _ {ccMsg=CcWmPAINT,p1=wPtr,p2=left,p3=top,p4=right,p5=bottom,p6=gc} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		  windows			= setWindowHandlesWindow wsH windows
		  updRect			= fromTuple4 (left,top,right,bottom)
		  updateInfo		= {updWIDS=wids,updWindowArea=updRect,updControls=[],updGContext=if (gc==0) Nothing (Just gc)}
		= (True,Nothing,Just (WindowUpdate updateInfo),windows,ioState)

filterOSEvent _ _ _ _
	= windoweventFatalError "filterOSEvent" "unmatched OSEvent"


toModifiers :: !Int -> Modifiers
toModifiers i
	=	{	shiftDown	= shifton
		,	optionDown	= alton
		,	commandDown	= ctrlon
		,	controlDown	= ctrlon
		,	altDown		= alton
		}
where
	shifton	= i bitand SHIFTBIT <> 0
	alton	= i bitand ALTBIT   <> 0
	ctrlon	= i bitand CTRLBIT  <> 0

getControlKeyFocusItemNr :: !Bool !OSWindowPtr !(WindowStateHandle .pst) -> (!Int,!WindowStateHandle .pst)
getControlKeyFocusItemNr activated cPtr wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	# (itemNr,itemHs)	= getControlsKeyFocusItemNr` activated cPtr wH.whItems
	= (itemNr,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
where
	getControlsKeyFocusItemNr` :: !Bool !OSWindowPtr ![WElementHandle .ls .pst] -> (!Int,![WElementHandle .ls .pst])
	getControlsKeyFocusItemNr` activated cPtr itemHs
		| isEmpty itemHs
			= (0,itemHs)
		# (itemH,itemHs)		= HdTl itemHs
		  (itemNr,itemH)		= getControlKeyFocusItemNr` activated cPtr itemH
		| itemNr<>0
			= (itemNr,[itemH:itemHs])
		| otherwise
			# (itemNr,itemHs)	= getControlsKeyFocusItemNr` activated cPtr itemHs
			= (itemNr,[itemH:itemHs])
	where
		getControlKeyFocusItemNr` :: !Bool !OSWindowPtr !(WElementHandle .ls .pst) -> (!Int,!WElementHandle .ls .pst)
		getControlKeyFocusItemNr` activated cPtr (WItemHandle itemH)
			| cPtr==itemH.wItemPtr
				| not (isMember kind [IsCompoundControl,IsCustomControl,IsEditControl,IsPopUpControl])
					= (0,WItemHandle itemH)
				| not itemH.wItemSelect
					= (0,WItemHandle itemH)
				| Contains reqAttribute itemH.wItemAtts
					= (itemH.wItemNr,WItemHandle itemH)
				// otherwise
					= (0,WItemHandle itemH)
			| not (isRecursiveControl kind)
				= (0,WItemHandle itemH)
			| otherwise
				# (itemNr,itemHs)	= getControlsKeyFocusItemNr` activated cPtr itemH.wItems
				= (itemNr,WItemHandle {itemH & wItems=itemHs})
		where
			kind			= itemH.wItemKind
			reqAttribute	= if activated isControlActivate isControlDeactivate
		
		getControlKeyFocusItemNr` activated cPtr (WListLSHandle itemHs)
			# (itemNr,itemHs)	= getControlsKeyFocusItemNr` activated cPtr itemHs
			= (itemNr,WListLSHandle itemHs)
		
		getControlKeyFocusItemNr` activated cPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
			# (itemNr,itemHs)	= getControlsKeyFocusItemNr` activated cPtr itemHs
			= (itemNr,WExtendLSHandle {wExH & wExtendItems=itemHs})
		
		getControlKeyFocusItemNr` activated cPtr (WChangeLSHandle wChH=:{wChangeItems=itemHs})
			# (itemNr,itemHs)	= getControlsKeyFocusItemNr` activated cPtr itemHs
			= (itemNr,WChangeLSHandle {wChH & wChangeItems=itemHs})
getControlKeyFocusItemNr _ _ _
	= windoweventFatalError "getControlKeyFocusItemNr" "window placeholder not expected"


//	Access operations on InputTrack:

trackingMouse :: !OSWindowPtr !OSWindowPtr !(Maybe InputTrack) -> Bool
trackingMouse wPtr cPtr (Just {itWindow,itControl,itKind={itkMouse}})
	= wPtr==itWindow && cPtr==itControl && itkMouse
trackingMouse _ _ _
	= False

trackingKeyboard :: !OSWindowPtr !OSWindowPtr !(Maybe InputTrack) -> Bool
trackingKeyboard wPtr cPtr (Just {itWindow,itControl,itKind={itkKeyboard}})
	= wPtr==itWindow && cPtr==itControl && itkKeyboard
trackingKeyboard _ _ _
	= False

trackMouse :: !OSWindowPtr !OSWindowPtr !(Maybe InputTrack) -> Maybe InputTrack
trackMouse wPtr cPtr (Just it=:{itWindow,itControl,itKind=itk})
	| wPtr<>itWindow || cPtr<>itControl
		= windoweventFatalError "trackMouse" "incorrect window/control parameters"
	| otherwise
		= Just {it & itKind={itk & itkMouse=True}}
trackMouse wPtr cPtr nothing
	= Just {itWindow=wPtr,itControl=cPtr,itKind={itkMouse=True,itkKeyboard=False}}

untrackMouse :: !(Maybe InputTrack) -> Maybe InputTrack
untrackMouse (Just it=:{itKind=itk})
	| itk.itkKeyboard
		= Just {it & itKind={itk & itkMouse=False}}
	| otherwise
		= Nothing
untrackMouse nothing
	= nothing

untrackKeyboard :: !(Maybe InputTrack) -> Maybe InputTrack
untrackKeyboard (Just it=:{itKind=itk})
	| itk.itkMouse
		= Just {it & itKind={itk & itkKeyboard=False}}
	| otherwise
		= Nothing
untrackKeyboard nothing
	= nothing

trackKeyboard :: !OSWindowPtr !OSWindowPtr !(Maybe InputTrack) -> Maybe InputTrack
trackKeyboard wPtr cPtr (Just it=:{itWindow,itControl,itKind=itk})
	| wPtr<>itWindow || cPtr<>itControl
		= windoweventFatalError "trackKeyboard" "incorrect window/control parameters"
	| otherwise
		= Just {it & itKind={itk & itkKeyboard=True}}
trackKeyboard wPtr cPtr nothing
	= Just {itWindow=wPtr,itControl=cPtr,itKind={itkMouse=False,itkKeyboard=True}}
