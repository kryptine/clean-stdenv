implementation module windowcreate


//	Clean Object I/O library, version 1.2

//	Window creation


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	osevent, oswindow
from	ostoolbox			import OSNewToolbox
from	StdMenu				import enableMenuSystem, disableMenuSystem
from	StdPSt				import accPIO
from	StdWindowAttribute	import isWindowInit, getWindowInitFun, isWindowClose, isWindowCursor, getWindowCursorAtt
import	commondef, controlpos, iostate, scheduler, windowaccess
from	controlcreate		import createControls
from	controllayout		import getWindowContentRect
from	windowclipstate		import validateWindowClipState
from	windowupdate		import updatewindow
from	windowvalidate		import validateWindow


windowcreateFatalError :: String String -> .x
windowcreateFatalError function error
	= FatalError function "windowcreate" error

/*	Open a modal dialogue.
*/
openmodalwindow :: !Id !(WindowLSHandle .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!Maybe .ls,!PSt .l)
openmodalwindow wId {wlsState,wlsHandle} pState=:{io=ioState}
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found					// This condition should never occur: WindowDevice must have been 'installed'
		= windowcreateFatalError "openmodalwindow" "could not retrieve WindowSystemState from IOSt"
	# windows					= WindowSystemStateGetWindowHandles wDevice
	# (tb,ioState)				= getIOToolbox ioState
	# (osdinfo,ioState)			= IOStGetOSDInfo ioState
	# (wMetrics,ioState)		= IOStGetOSWindowMetrics ioState
	# (_,_,_,_,wlsHandle,windows,tb)
								= validateWindow wMetrics osdinfo wlsHandle windows tb
	  (title,closable,wlsHandle)= (\wlsH=:{whTitle,whAtts}->(whTitle,Contains isWindowClose whAtts,wlsH)) wlsHandle
	# ioState					= IOStSetOSDInfo osdinfo ioState
	  wlsH						= {wlsState=wlsState,wlsHandle=wlsHandle}
	  wIds						= {wId=wId,wPtr=0,wActive=True}				// wPtr=0 is assumed by system
	  wsH						= {wshIds=wIds,wshHandle=Just wlsH}
	  (modalWIDS,windows)		= getWindowHandlesActiveModalDialog windows
	  windows					= addWindowHandlesActiveWindow wsH windows	// frontmost position is assumed by system
	# (ioId,ioState)			= IOStGetIOId ioState
	# ioState					= IOStSetIOIsModal (Just ioId) ioState
	# ioState					= setIOToolbox tb ioState
	# ioState					= IOStSetDevice (WindowSystemState windows) ioState
	# (inputTrack,ioState)		= IOStGetInputTrack ioState
	# ioState					= IOStSetInputTrack Nothing ioState			// clear input track information
	# pState					= {pState & io=ioState}
	# (noError,pState,_)		= OScreateModalDialog closable title osdinfo (mapMaybe (\{wPtr}->wPtr) modalWIDS) handleOSEvent pState OSNewToolbox
	  (delayMouse,delayKey)		= case inputTrack of						// after handling modal dialog, generate proper (Mouse/Key)Lost events
	  								Nothing	-> ([],[])
	  								Just it=:{itWindow,itControl,itKind}
	  										-> (if itKind.itkMouse    [createOSLooseMouseEvent itWindow (if (itControl==0) itWindow itControl)] []
	  										   ,if itKind.itkKeyboard [createOSLooseKeyEvent   itWindow (if (itControl==0) itWindow itControl)] []
	  										   )
	# (osDelayEvents,ioState)	= accIOToolbox (StrictSeqList (delayMouse++delayKey)) pState.io
	# (osEvents,ioState)		= IOStGetEvents ioState
	# ioState					= IOStSetEvents (OSappendEvents osDelayEvents osEvents) ioState
	# (finalLS,ioState)			= getFinalModalDialogLS noError (toWID wId) ioState
	# pState					= {pState & io=ioState}
	= (if noError NoError (OtherError "could not create modal dialog"),finalLS,pState)
where
	handleOSEvent :: !OSEvent !(PSt .l) -> (![Int],!PSt .l)
	handleOSEvent osEvent pState = accContext (handleContextOSEvent osEvent) pState
	
/*	getFinalModalDialogLS retrieves the final local state of the modal dialog. This value has been stored in the window handles.
	This MUST have been done by disposeWindow (windowdispose). 
*/
	getFinalModalDialogLS :: !Bool !WID !(IOSt .l) -> (!Maybe .ls,!IOSt .l)
	getFinalModalDialogLS noError wid ioState
		| not noError
			= (Nothing,ioState)
		# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
		| not found
			= windowcreateFatalError "getFinalModalDialogLS" "could not retrieve WindowSystemState from IOSt"
		| otherwise
			# windows				= WindowSystemStateGetWindowHandles wDevice
			  (final,windows)		= getFinalLS wid windows
			# ioState				= IOStSetDevice (WindowSystemState windows) ioState
			= case final of
				Nothing    -> windowcreateFatalError "getFinalModalDialogLS" "final local modal dialog state not found"
				Just final -> (getFinalModalLS wid final,ioState)
	where
		getFinalLS :: !WID !(WindowHandles .pst) -> (!Maybe FinalModalLS,!WindowHandles .pst)
		getFinalLS wid windows=:{whsFinalModalLS}
			# (removed,finalLS,finalLSs)	= URemove (\fmLS=:{fmWIDS}->(identifyWIDS wid fmWIDS,fmLS)) undef whsFinalModalLS
			  windows						= {windows & whsFinalModalLS=finalLSs}
			| not removed					= (Nothing,windows)
			| otherwise						= (Just finalLS,windows)

/*	Open a modeless window/dialogue.
*/
openwindow :: !Id !(WindowLSHandle .ls (PSt .l)) !(PSt .l) -> PSt .l
openwindow wId {wlsState,wlsHandle} pState=:{io=ioState}
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found					// This condition should never occur: WindowDevice must have 'installed'
		= windowcreateFatalError "openwindow" "could not retrieve WindowSystemState from IOSt"
	| otherwise
		= pState2
	with
		windows					= WindowSystemStateGetWindowHandles wDevice
		(delayinfo,wPtr,index,wlsHandle1,windows1,ioState2)
								= openAnyWindow wId wlsHandle windows ioState
		(windowInit,wlsHandle2)	= getWindowHandleInit wlsHandle1
		wlsH					= {wlsState=ls1,wlsHandle=wlsHandle2}
		wIds					= {wId=wId,wPtr=wPtr,wActive=False}
		wsH						= {wshIds=wIds,wshHandle=Just wlsH}
		windows2				= addWindowHandlesWindow index wsH windows1
		ioState3				= IOStSetDevice (WindowSystemState windows2) ioState2
		ioState4				= bufferDelayedEvents delayinfo ioState3
		pState1					= {pState & io=ioState4}
		(ls1,pState2)			= windowInit (wlsState,pState1)
		
		getWindowHandleInit :: !(WindowHandle .ls .pst) -> (!IdFun *(.ls,.pst),!WindowHandle .ls .pst)
		getWindowHandleInit wH=:{whAtts}
			= (getWindowInitFun (snd (Select isWindowInit (WindowInit id) whAtts)),wH)
		
	/*	openAnyWindow creates a window.
			After validating the window and its controls, the window and its controls are created.
			The return OSWindowPtr is the OSWindowPtr of the newly created window.
			The return Index is the proper insert position in the WindowHandles list.
	*/
		openAnyWindow :: !Id !(WindowHandle .ls (PSt .l)) !(WindowHandles (PSt .l)) !(IOSt .l)
			-> (![DelayActivationInfo],!OSWindowPtr,!Index,!WindowHandle .ls (PSt .l),!WindowHandles (PSt .l),!IOSt .l)
		openAnyWindow wId wH windows ioState
			# (tb,ioState)			= getIOToolbox ioState
			# (osdinfo,ioState)		= IOStGetOSDInfo ioState
			# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
			# (index,pos,size,originv,wH,windows,tb)
									= validateWindow wMetrics osdinfo wH windows tb
			  (behindPtr,windows)	= getStackBehindWindow index windows
			# (delayinfo,wPtr,osdinfo,wH,tb)
									= createAnyWindow wMetrics behindPtr wId pos size originv osdinfo wH tb
			# (wH,tb)				= validateWindowClipState wMetrics True wPtr wH tb
			# ioState				= IOStSetOSDInfo osdinfo ioState
			# ioState				= setIOToolbox (OSinvalidateWindow wPtr tb) ioState
			= (delayinfo,wPtr,index,wH,windows,ioState)

createAnyWindow :: !OSWindowMetrics !OSWindowPtr !Id !Point2 !Size !Vector2 !OSDInfo !(WindowHandle .ls (PSt .l)) !*OSToolbox
								    -> (![DelayActivationInfo],!OSWindowPtr,!OSDInfo, !WindowHandle .ls (PSt .l), !*OSToolbox)
createAnyWindow wMetrics behindPtr wId {x,y} {w,h} originv osdinfo wH=:{whMode,whKind,whTitle,whWindowInfo,whAtts} tb
	| whKind==IsWindow
		# (delay_info,wPtr,hPtr,vPtr,osdinfo,wH,tb)
									= OScreateWindow wMetrics isResizable hInfo vInfo minSize (toTuple maxSize)
													 isClosable whTitle pos size getInitActiveControl (createWindowControls wMetrics)
													 (updateWindowControl wMetrics wId size)
													 osdinfo wH tb
		  windowInfo				= {	windowInfo	& windowHScroll	= setScrollInfoPtr hScroll hPtr
				  									, windowVScroll	= setScrollInfoPtr vScroll vPtr
						  			  }
		  wH						= {wH & whWindowInfo=WindowInfo windowInfo}
		# (wH,tb)					= movewindowviewframe wMetrics originv {wPtr=wPtr,wId=wId,wActive=False} wH tb	// PA: check WIDS value
		# tb						= stackWindow wPtr behindPtr tb
		# (delay_info`,tb)			= OSshowWindow wPtr False tb
		# tb						= OSsetWindowCursor wPtr (toCursorCode (getWindowCursorAtt cursorAtt)) tb
		= (delay_info++delay_info`,wPtr,osdinfo,wH,tb)
		with
			isResizable				= True
			windowInfo				= getWindowInfoWindowData whWindowInfo
			viewDomain				= windowInfo.windowDomain
			viewOrigin				= windowInfo.windowOrigin
			hScroll					= windowInfo.windowHScroll
			vScroll					= windowInfo.windowVScroll
			visScrolls				= OSscrollbarsAreVisible wMetrics viewDomain (w,h) (isJust hScroll,isJust vScroll)
			{rright=w`,rbottom=h`}	= getWindowContentRect wMetrics visScrolls (SizeToRect {w=w,h=h})
			hInfo					= toScrollbarInfo hScroll (viewDomain.rleft,viewOrigin.x,viewDomain.rright, w`)
			vInfo					= toScrollbarInfo vScroll (viewDomain.rtop, viewOrigin.y,viewDomain.rbottom,h`)
			minSize					= OSMinWindowSize
			maxSize					= RectSize viewDomain
			(_,cursorAtt)			= Select isWindowCursor (WindowCursor StandardCursor) whAtts
			
			toScrollbarInfo :: !(Maybe ScrollInfo) (Int,Int,Int,Int) -> ScrollbarInfo
			toScrollbarInfo Nothing scrollState
				= {cbiHasScroll=False,cbiPos=undef,cbiSize=undef,cbiState=undef}
			toScrollbarInfo (Just {scrollItemPos,scrollItemSize}) (min,origin,max,size)
				= {cbiHasScroll=True,cbiPos=toTuple scrollItemPos,cbiSize=toTuple scrollItemSize,cbiState=osScrollState}
			where
				osScrollState		= toOSscrollbarRange (min,origin,max) size
			
			setScrollInfoPtr :: !(Maybe ScrollInfo) !OSWindowPtr -> Maybe ScrollInfo
			setScrollInfoPtr (Just info) scrollPtr	= Just {info & scrollItemPtr=scrollPtr}
			setScrollInfoPtr nothing _				= nothing
	| whKind==IsDialog
		# (delay_info,wPtr,wH,tb)	= OScreateDialog isModal isClosable whTitle pos size behindPtr
										getInitActiveControl (createWindowControls wMetrics)
										(updateWindowControl wMetrics wId size)
										osdinfo wH tb
		= (delay_info,wPtr,osdinfo,wH,tb)
	with
		isModal		= whMode==Modal
// Mike ... //
	| whKind==IsGameWindow
		# (delay_info,wPtr,tb)		= OScreateGameWindow fullscreen (w,h) colordepth tb		// par 3, ddPtr removed
		//gwdata					= {gwindowData & gamewindowDDPtr=ddPtr}
																							//  PA: wordt gamewindowDDPtr nog wel gebruikt?? 
		= (delay_info,wPtr,osdinfo,{wH & whWindowInfo=GameWindowInfo gwdata},tb)
	with
		gwdata		= getWindowInfoGameWindowData whWindowInfo
		colordepth	= gwdata.gamewindowCDepth
		fullscreen	= gwdata.gamewindowFullScreen
// ... Mike //
where
	isClosable		= Contains isWindowClose whAtts
	pos				= (x,y)
	size			= (w,h)
	
	// createWindowControls creates the controls.
	createWindowControls :: !OSWindowMetrics !OSWindowPtr !(WindowHandle .ls (PSt .l)) !*OSToolbox -> (!WindowHandle .ls (PSt .l),!*OSToolbox)
	createWindowControls wMetrics wPtr wH=:{whDefaultId,whCancelId,whSelect,whItems=itemHs} tb
		# (itemHs,tb)	= createControls wMetrics whDefaultId whCancelId whSelect wPtr itemHs tb
		= ({wH & whItems=itemHs},tb)
	
	// updateWindowControl updates customised controls.
	updateWindowControl :: !OSWindowMetrics !Id !(!Int,!Int) !OSWindowPtr !OSWindowPtr !OSPictContext !(WindowHandle .ls (PSt .l)) !*OSToolbox
																								   -> (!WindowHandle .ls (PSt .l), !*OSToolbox)
	updateWindowControl wMetrics wId (w,h) wPtr cPtr osPict wH=:{whItems=itemHs} tb
		#! (_,controls)	= getUpdateControls cPtr (SizeToRect {w=w,h=h}) itemHs
		#! wH			= {wH & whItems=itemHs}
		# updateInfo	= {	updWIDS			= {wPtr=wPtr,wId=wId,wActive=False}	// PA: check WIDS value
						  ,	updWindowArea	= zero
						  ,	updControls		= controls
						  ,	updGContext		= Just osPict
						  }
		= updatewindow wMetrics updateInfo wH tb
	where
		getUpdateControls :: !OSWindowPtr !Rect ![WElementHandle .ls .pst] -> (!Bool,![ControlUpdateInfo])
		getUpdateControls cPtr clipRect [itemH:itemHs]
			# (found,controls)	= getUpdateControl cPtr clipRect itemH
			| found				= (found,controls)
			| otherwise			= getUpdateControls cPtr clipRect itemHs
		where
			getUpdateControl :: !OSWindowPtr !Rect !(WElementHandle .ls .pst) -> (!Bool,![ControlUpdateInfo])
			getUpdateControl cPtr clipRect (WItemHandle itemH=:{wItemPtr})
				| cPtr==wItemPtr= (True, [{cuItemNr=itemH.wItemNr,cuItemPtr=wItemPtr,cuArea=clipRect1}])
				| otherwise		= getUpdateControls cPtr clipRect1 itemH.wItems
			where
				clipRect1		= IntersectRects clipRect (PosSizeToRect itemH.wItemPos itemH.wItemSize)
			getUpdateControl cPtr clipRect (WListLSHandle itemHs)
				= getUpdateControls cPtr clipRect itemHs
			getUpdateControl cPtr clipRect (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				= getUpdateControls cPtr clipRect itemHs
			getUpdateControl cPtr clipRect (WChangeLSHandle wExH=:{wChangeItems=itemHs})
				= getUpdateControls cPtr clipRect itemHs
		getUpdateControls _ _ _
			= (False,[])

getStackBehindWindow :: !Index !(WindowHandles .pst) -> (!OSWindowPtr,!WindowHandles .pst)
getStackBehindWindow 0 wsHs
	= (OSNoWindowPtr,wsHs)
getStackBehindWindow index wsHs=:{whsWindows}
	# (before,[wlsH=:{wshIds={wPtr}}:wlsHs])	= splitAt (index-1) whsWindows
	= (wPtr,{wsHs & whsWindows=before++[wlsH:wlsHs]})

stackWindow :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
stackWindow wPtr OSNoWindowPtr tb = tb
stackWindow wPtr behindPtr     tb = OSstackWindow wPtr behindPtr tb


/*	bufferDelayedEvents buffers the events in the OSEvents environment.
*/
bufferDelayedEvents :: ![DelayActivationInfo] !(IOSt .l) -> IOSt .l
bufferDelayedEvents delayinfo ioState
	# (osEvents,ioState)	= IOStGetEvents ioState
	# (delayEvents,ioState)	= accIOToolbox (StrictSeqList (map toOSEvent delayinfo)) ioState
	  osEvents				= OSappendEvents delayEvents osEvents
	= IOStSetEvents osEvents ioState
where
	toOSEvent :: !DelayActivationInfo !*OSToolbox -> (!OSEvent,!*OSToolbox)
	toOSEvent (DelayActivatedWindow wPtr) tb
		= createOSActivateWindowEvent wPtr tb
	toOSEvent (DelayDeactivatedWindow wPtr) tb
		= createOSDeactivateWindowEvent wPtr tb
	toOSEvent (DelayActivatedControl wPtr cPtr) tb
		= createOSActivateControlEvent wPtr cPtr tb
	toOSEvent (DelayDeactivatedControl wPtr cPtr) tb
		= createOSDeactivateControlEvent wPtr cPtr tb


/*	WindowBound-checks for normal windows.
*/
checkZeroWindowBound :: !(IOSt .l) -> (!Bool,!IOSt .l)
checkZeroWindowBound ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (False,ioState)
	| otherwise
		# wHs					= WindowSystemStateGetWindowHandles wDevice
		  (isZero,wHs)			= checkZeroWindowHandlesBound wHs
		# ioState				= IOStSetDevice (WindowSystemState wHs) ioState
		= (isZero,ioState)

decreaseWindowBound :: !(IOSt .l) -> IOSt .l
decreaseWindowBound ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	| otherwise
		# wHs					= WindowSystemStateGetWindowHandles wDevice
		  wHs					= decreaseWindowHandlesBound wHs
		# ioState				= IOStSetDevice (WindowSystemState wHs) ioState
		= ioState
