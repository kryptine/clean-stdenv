implementation module windowdispose


//	Clean Object I/O library, version 1.2


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	menuevent, osmenu, oswindow
import	commondef, iostate, receiverid, scheduler, StdPSt, windowaccess, windowclipstate
from	StdMenu				import enableMenuSystem
from	StdWindowAttribute	import isWindowDeactivate
from	windowcreate		import bufferDelayedEvents


windowdisposeFatalError :: String String -> .x
windowdisposeFatalError function error
	= FatalError function "windowdispose" error


/*	disposeWindow disposes all system resources associated with the indicated window if it exists.
	Inactive modal dialogues are not removed.
	If the window belongs to an SDI process, then only the SDI client is removed, not the SDI frame.
	It removes the indicated window from the window device administration.
	Because the window may contain controls that are 'logically' disposed, but not 'physically' 
	disposeWindow also applies the init function contained in the IOSt.
*/
disposeWindow :: !WID !(PSt .l) -> PSt .l
disposeWindow wid pState=:{io=ioState}
	# (found,wDevice,ioState)		= IOStGetDevice WindowDevice ioState
	| not found
		= {pState & io=ioState}
	# windows						= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)			= getWindowHandlesWindow wid windows
	// The window could not be found
	| not found
		= {pState & io=IOStSetDevice (WindowSystemState windows) ioState}
	# (alreadyClosing,wsH)			= getWindowStateHandleClosing wsH
	// The window is already in the act of being closed
	| alreadyClosing
		= {pState & io=IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState}
	# (documentInterface,ioState)	= IOStGetDocumentInterface ioState
	  (wKind,wsH)					= getWindowStateHandleWindowKind wsH
	  (wids, wsH)					= getWindowStateHandleWIDS wsH
	// Of a SDI process, the SDI client should be closed, not the SDI frame (which is closed by closeProcess)
	| documentInterface==SDI && wKind==IsWindow
	//	= {pState & io=IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState}
		# windows					= incWindowBound windows
		= dispose wids wsH windows {pState & io=ioState}
	with
		incWindowBound :: !(WindowHandles .pst) -> WindowHandles .pst
		incWindowBound wHs=:{whsNrWindowBound}
			= {wHs & whsNrWindowBound=incBound whsNrWindowBound}
	# (wMode,wsH)					= getWindowStateHandleWindowMode wsH
	// Any modeless window can be disposed
	| wMode<>Modal
		= dispose wids wsH windows {pState & io=ioState}
	# (activeWIDS,windows)			= getWindowHandlesActiveWindow windows
	| isNothing activeWIDS
	// Incorrect situation: indicated dialog is modal while no active window could be found
		= windowdisposeFatalError "disposeWindow" "active window could not be found"
	# activeId						= fromJust activeWIDS
	// Do not dispose inactive modal windows
	| wids.wId<>activeId.wId
		= {pState & io=IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState}
	// Dispose only the active modal window
	| otherwise
		= dispose wids wsH windows {pState & io=ioState}
where
	dispose :: !WIDS !(WindowStateHandle (PSt .l)) !(WindowHandles (PSt .l)) !(PSt .l) -> PSt .l
	dispose wids=:{wId} wsH windows=:{whsFinalModalLS} pState
		# (disposeFun,pState)	= accPIO IOStGetInitIO pState
		# pState				= disposeFun pState
		# (osdinfo,ioState)		= IOStGetOSDInfo pState.io
		# (inputTrack,ioState)	= IOStGetInputTrack ioState
		  (_,_,windows)			= removeWindowHandlesWindow (toWID wId) windows
		# (windows,ioState)		= enableProperWindows windows ioState	// PA: before disposing last modal window, the window and menu system should be enabled
		# (tb,ioState)			= getIOToolbox ioState
		# pState				= {pState & io=ioState}
		# ((rids,ids,delayinfo,finalLS,inputTrack),pState,tb)
								= disposeWindowStateHandle osdinfo inputTrack wsH handleOSEvent pState tb
		# ioState				= setIOToolbox tb pState.io
		# ioState				= IOStSetInputTrack inputTrack ioState
		# ioState				= unbindRIds rids ioState				// When timers are part of windows, also unbind timers
		# (idtable,ioState)		= IOStGetIdTable ioState
		  (_,idtable)			= removeIdsFromIdTable (rids++ids) idtable
		# ioState				= IOStSetIdTable idtable ioState
		# windows				= {windows & whsFinalModalLS=finalLS++whsFinalModalLS}
		# ioState				= IOStSetDevice (WindowSystemState windows) ioState
		# ioState				= bufferDelayedEvents delayinfo ioState
		= {pState & io=ioState}
	
	handleOSEvent :: !OSEvent !(PSt .l) -> (![Int],!PSt .l)
	handleOSEvent osEvent pState = accContext (handleContextOSEvent osEvent) pState
	
	enableProperWindows :: !(WindowHandles (PSt .l)) !(IOSt .l) -> (!WindowHandles (PSt .l),!IOSt .l)
	enableProperWindows windows ioState
		# (modalWIDS,windows)	= getWindowHandlesActiveModalDialog windows
		| isJust modalWIDS		= (windows,ioState)
		| otherwise				= (windows,IOStSetIOIsModal Nothing ioState)


/*	disposeCursorInfo disposes all system resources associated with the given CursorInfo.
	PA: not yet implemented

disposeCursorInfo :: !CursorInfo !(IOSt .l) -> IOSt .l
*/


/*	disposeWindowStateHandle disposes all system resources associated with the given WindowStateHandle.
	The first  return [Id] are the Ids of receivers that should become unbound.
	The second return [Id] are the Ids of the other controls.
	The [DelayActivationInfo] are the delayed (de)activate events.
	The [FinalModalLS] is the final local state if the WindowStateHandle is a modal dialog.
	When timers are part of windows, also timer ids should be returned.
*/
disposeWindowStateHandle :: !OSDInfo !(Maybe InputTrack) !(WindowStateHandle .pst) !(OSEvent -> .s -> ([Int],.s)) .s !*OSToolbox
									  -> (!(![Id],![Id],![DelayActivationInfo],![FinalModalLS],!Maybe InputTrack),.s,!*OSToolbox)
disposeWindowStateHandle osdinfo inputTrack {wshIds=wids=:{wPtr,wId},wshHandle=Just {wlsState,wlsHandle=wH}} handleOSEvent state tb
	# isModalDialog				= whKind==IsDialog && whMode==Modal
	  (isWindowInfo,info)		= case wH.whWindowInfo of
									WindowInfo info	-> (True, info)
									_				-> (False,windowdisposeFatalError "disposeWindowStateHandle" "info unexpectedly evaluated")
	# (ids_dispose,tb)			= StateMap (disposeWElementHandle wPtr) wH.whItems tb
	  (rIdss,idss,disposeFuns)	= unzip3 ids_dispose
	# tb						= StrictSeq disposeFuns tb
	# (delayinfo,state,tb)		= OSdestroyWindow osdinfo (whMode==Modal) (whKind==IsWindow) wPtr handleOSEvent state tb
	  rids						= flatten rIdss
	  ids						= [wId:flatten idss]
	  finalModalLS				= if isModalDialog [{fmWIDS=wids,fmLS=wlsState}] []
	  inputTrack				= case inputTrack of
	  								Just {itWindow}
	  										-> if (itWindow==wPtr) Nothing inputTrack
	  								nothing -> nothing
	  result					= (rids,ids,delayinfo,finalModalLS,inputTrack)
	| isWindowInfo				= (result,state,disposeClipState info.windowClip tb)
	| otherwise					= (result,state,tb)
where
	whKind						= wH.whKind
	whMode						= wH.whMode
disposeWindowStateHandle _ _ _ _ _ _
	= windowdisposeFatalError "disposeWindowStateHandle" "window expected instead of placeholder"


/*	disposeWElementHandle (recursively) hides all system resources associated with the given 
	WElementHandle. The argument OSWindowPtr must be the parent window.
	The (IdFun *OSToolbox) function must be used to actually dispose the controls.
	It returns all freed receiver and control ids.
	When timers are part of windows, also timer ids should be returned.
*/
disposeWElementHandle :: !OSWindowPtr !(WElementHandle .ls .pst) !*OSToolbox -> (!(![Id],![Id],!IdFun *OSToolbox),!*OSToolbox)
disposeWElementHandle wPtr (WItemHandle itemH) tb
	= disposeWItemHandle wPtr itemH tb
disposeWElementHandle wPtr (WListLSHandle itemHs) tb
	# (ids_dispose,tb)	= StateMap (disposeWElementHandle wPtr) itemHs tb
	  (ridss,idss,fs)	= unzip3 ids_dispose
	= ((flatten ridss,flatten idss,StrictSeq fs),tb)
disposeWElementHandle wPtr (WExtendLSHandle {wExtendItems=itemHs}) tb
	# (ids_dispose,tb)	= StateMap (disposeWElementHandle wPtr) itemHs tb
	  (ridss,idss,fs)	= unzip3 ids_dispose
	= ((flatten ridss,flatten idss,StrictSeq fs),tb)
disposeWElementHandle wPtr (WChangeLSHandle {wChangeItems=itemHs}) tb
	# (ids_dispose,tb)	= StateMap (disposeWElementHandle wPtr) itemHs tb
	  (ridss,idss,fs)	= unzip3 ids_dispose
	= ((flatten ridss,flatten idss,StrictSeq fs),tb)


/*	disposeWItemHandle (recursively) hides all system resources associated with the given WItemHandle. 
	The OSWindowPtr argument must identify the parent window.
	The (IdFun *OSToolbox) function must be used to actually dispose the controls.
	It returns all freed receiver ids.
	When timers are part of windows, also timer ids should be returned.
*/
disposeWItemHandle :: !OSWindowPtr !(WItemHandle .ls .pst) !*OSToolbox -> (!(![Id],![Id],!IdFun *OSToolbox),!*OSToolbox)

disposeWItemHandle wPtr itemH=:{wItemKind=IsCheckControl} tb
	# checkInfo			= getWItemCheckInfo itemH.wItemInfo
	  items				= checkInfo.checkItems
	# tb				= StateMap2 (\{checkItemPtr,checkItemPos,checkItemSize}
										->OSsetCheckControlShow wPtr checkItemPtr (PosSizeToRect checkItemPos checkItemSize) False
									) items tb
	= (([],maybeToList itemH.wItemId,StateMap2 (\{checkItemPtr}->OSdestroyCheckControl checkItemPtr) items),tb)

disposeWItemHandle wPtr itemH=:{wItemKind=IsCompoundControl} tb
	# (ids_dispose,tb)	= StateMap (disposeWElementHandle wPtr) itemH.wItems tb
	  (rIdss,idss,fs)	= unzip3 ids_dispose
	  f					= OSdestroyCompoundControl itemPtr
	  rids				= flatten rIdss
	  ids				= maybeToList itemH.wItemId ++ flatten idss
	  info				= getWItemCompoundInfo itemH.wItemInfo
	# tb				= OSsetCompoundShow wPtr itemPtr (PosSizeToRect itemH.wItemPos itemH.wItemSize) False tb
	= ((rids,ids,f o disposeClipState info.compoundLookInfo.compoundClip o StrictSeq fs),tb)
where
	itemPtr				= itemH.wItemPtr

disposeWItemHandle wPtr itemH=:{wItemKind=IsLayoutControl} tb
	# (ids_dispose,tb)	= StateMap (disposeWElementHandle wPtr) itemH.wItems tb
	  (rIdss,idss,fs)	= unzip3 ids_dispose
	  rids				= flatten rIdss
	  ids				= maybeToList itemH.wItemId ++ flatten idss
	= ((rids,ids,StrictSeq fs),tb)

disposeWItemHandle wPtr itemH=:{wItemKind=IsOtherControl controltype} tb
//	The control is a receiver:
	| controltype=="Receiver" || controltype=="Receiver2"
		= ((maybeToList itemH.wItemId,[],id),tb)
/*	The control is a timer:
	| controltype=="TimerControl"
		= (([],getTimerLoc itemH,id),tb)
*/	| otherwise
		= windowdisposeFatalError "disposeWItemHandle" ("unknown control type: "+++controltype)

disposeWItemHandle wPtr itemH=:{wItemKind=IsRadioControl} tb
	# radioInfo			= getWItemRadioInfo itemH.wItemInfo
	  items				= radioInfo.radioItems
	# tb				= StateMap2 (\{radioItemPtr,radioItemPos,radioItemSize}
										->OSsetRadioControlShow wPtr radioItemPtr (PosSizeToRect radioItemPos radioItemSize) False
									) items tb
	= (([],maybeToList itemH.wItemId,StateMap2 (\{radioItemPtr}->OSdestroyRadioControl radioItemPtr) items),tb)

disposeWItemHandle wPtr itemH=:{wItemKind} tb
	# tb				= hide wPtr itemPtr (PosSizeToRect itemH.wItemPos itemH.wItemSize) False tb
	= (([],maybeToList itemH.wItemId,dispose itemPtr),tb)
where
	(hide,dispose)		= case wItemKind of
							IsPopUpControl			-> (OSsetPopUpControlShow,			OSdestroyPopUpControl)
							IsSliderControl			-> (OSsetSliderControlShow,			OSdestroySliderControl)
							IsTextControl			-> (OSsetTextControlShow,			OSdestroyTextControl)
							IsEditControl			-> (OSsetEditControlShow,			OSdestroyEditControl)
							IsButtonControl			-> (OSsetButtonControlShow,			OSdestroyButtonControl)
							IsCustomButtonControl	-> (OSsetCustomButtonControlShow,	OSdestroyCustomButtonControl)
							IsCustomControl			-> (OSsetCustomControlShow,			OSdestroyCustomControl)
							_						-> windowdisposeFatalError "disposeWItemHandle" ("unmatched ControlKind: "+++toString wItemKind)
	itemPtr				= itemH.wItemPtr

maybeToList :: !(Maybe .x) -> [.x]
maybeToList (Just x)	= [x]
maybeToList _			= []
