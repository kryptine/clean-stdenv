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
	= fatalError function "windowdispose" error


/*	disposeWindow disposes all system resources associated with the indicated window if it exists.
	Inactive modal dialogues are not removed.
	If the window belongs to an SDI process, then only the SDI client is removed, not the SDI frame.
	It removes the indicated window from the window device administration.
	Because the window may contain controls that are 'logically' disposed, but not 'physically' 
	disposeWindow also applies the init function contained in the IOSt.
*/
disposeWindow :: !WID !(PSt .l) -> PSt .l
disposeWindow wid pState=:{io=ioState}
	# (found,wDevice,ioState)		= ioStGetDevice WindowDevice ioState
	| not found
		= {pState & io=ioState}
	# windows						= windowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)			= getWindowHandlesWindow wid windows
	// The window could not be found
	| not found
		= {pState & io=ioStSetDevice (WindowSystemState windows) ioState}
	# (alreadyClosing,wsH)			= getWindowStateHandleClosing wsH
	// The window is already in the act of being closed
	| alreadyClosing
		= {pState & io=ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState}
	# (documentInterface,ioState)	= ioStGetDocumentInterface ioState
	  (wKind,wsH)					= getWindowStateHandleWindowKind wsH
	  (wids, wsH)					= getWindowStateHandleWIDS wsH
	// Of a SDI process, the SDI client should be closed, not the SDI frame (which is closed by closeProcess)
	| documentInterface==SDI && wKind==IsWindow
	//	= {pState & io=ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState}
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
		= {pState & io=ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState}
	// Dispose only the active modal window
	| otherwise
		= dispose wids wsH windows {pState & io=ioState}
where
	dispose :: !WIDS !(WindowStateHandle (PSt .l)) !(WindowHandles (PSt .l)) !(PSt .l) -> PSt .l
	dispose wids=:{wId} wsH windows pState=:{io=ioState}
		# (_,_,windows)			= removeWindowHandlesWindow (toWID wId) windows	// Remove window placeholder
		# (windows,ioState)		= enableProperWindows windows ioState			// PA: before disposing last modal window, the window and menu system should be enabled
		# ioState				= ioStSetDevice (WindowSystemState windows) ioState
		# (disposeFun,ioState)	= ioStGetInitIO ioState
		# pState				= disposeFun {pState & io=ioState}
		# (osdinfo,ioState)		= ioStGetOSDInfo pState.io
		# (inputTrack,ioState)	= ioStGetInputTrack ioState
		# (tb,ioState)			= getIOToolbox ioState
		# pState				= {pState & io=ioState}
		# ((rids,ids,delayinfo,finalLS,inputTrack),(_,pState),tb)
								= disposeWindowStateHandle osdinfo inputTrack handleOSEvent (wsH,pState) tb
		# ioState				= setIOToolbox tb pState.io
		# ioState				= ioStSetInputTrack inputTrack ioState
		# ioState				= unbindRIds rids ioState						// When timers are part of windows, also unbind timers
		# (idtable,ioState)		= ioStGetIdTable ioState
		  (_,idtable)			= removeIdsFromIdTable (rids++ids) idtable
		# ioState				= ioStSetIdTable idtable ioState
		# ioState				= addFinalLS finalLS ioState
		# ioState				= bufferDelayedEvents delayinfo ioState
		= {pState & io=ioState}
	
	handleOSEvent :: !OSEvent !(PSt .l) -> (![Int],!PSt .l)
	handleOSEvent osEvent pState = accContext (handleContextOSEvent osEvent) pState
	
	enableProperWindows :: !(WindowHandles (PSt .l)) !(IOSt .l) -> (!WindowHandles (PSt .l),!IOSt .l)
	enableProperWindows windows ioState
		# (modalWIDS,windows)	= getWindowHandlesActiveModalDialog windows
		| isJust modalWIDS		= (windows,ioState)
		| otherwise				= (windows,ioStSetIOIsModal Nothing ioState)
	
	addFinalLS :: ![FinalModalLS] !(IOSt .l) -> IOSt .l
	addFinalLS finalLS ioState
		# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
		| not found
			= windowdisposeFatalError "disposeWindow" "could not restore final local window state"
		| otherwise
			# windows				= windowSystemStateGetWindowHandles wDevice
			# windows				= {windows & whsFinalModalLS=finalLS++windows.whsFinalModalLS}
			= ioStSetDevice (WindowSystemState windows) ioState


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
disposeWindowStateHandle :: !OSDInfo !(Maybe InputTrack) !(OSEvent -> .s -> ([Int],.s)) !*(!*WindowStateHandle .pst,.s) !*OSToolbox
			-> (!(![Id],![Id],![DelayActivationInfo],![FinalModalLS],!Maybe InputTrack),!*(!*WindowStateHandle .pst,.s),!*OSToolbox)
disposeWindowStateHandle osdinfo inputTrack handleOSEvent 
						 (wsH=:{wshIds=wids=:{wPtr,wId},wshHandle=Just wlsH=:{wlsState,wlsHandle=wH=:{whWindowInfo,whItems,whKind,whMode}}},state)
						 tb
	# isModalDialog				= whKind==IsDialog && whMode==Modal
	  (isWindowInfo,info)		= case whWindowInfo of
									WindowInfo info	-> (True, info)
									_				-> (False,windowdisposeFatalError "disposeWindowStateHandle" "info unexpectedly evaluated")
//	# (ids_dispose,tb)			= stateMap (disposeWElementHandle wPtr) wH.whItems tb
//	  (rIdss,idss,disposeFuns)	= unzip3 ids_dispose
	# (rids,ids,fs,itemHs,tb)	= disposeWElementHandles wPtr whItems tb
	# tb						= fs tb//StrictSeq disposeFuns tb
	# (delayinfo,state,tb)		= osDestroyWindow osdinfo (whMode==Modal) (whKind==IsWindow) wPtr handleOSEvent state tb
//	  rids						= flatten rIdss
	  ids						= [wId:ids]//flatten idss]
	  finalModalLS				= if isModalDialog [{fmWIDS=wids,fmLS=wlsState}] []
	  inputTrack				= case inputTrack of
	  								Just {itWindow}
	  										-> if (itWindow==wPtr) Nothing inputTrack
	  								nothing -> nothing
	  result					= (rids,ids,delayinfo,finalModalLS,inputTrack)
	  wsH						= {wsH & wshHandle=Just {wlsH & wlsState=undef,wlsHandle={wH & whItems=itemHs}}}
	| isWindowInfo				= (result,(wsH,state),disposeClipState info.windowClip tb)
	| otherwise					= (result,(wsH,state),tb)
disposeWindowStateHandle _ _ _ _ _
	= windowdisposeFatalError "disposeWindowStateHandle" "window expected instead of placeholder"


/*	disposeWElementHandle(s) (recursively) hides all system resources associated with the given 
	WElementHandle(s). The argument OSWindowPtr must be the parent window.
	The (IdFun *OSToolbox) function must be used to actually dispose the controls.
	It returns all freed receiver and control ids.
	When timers are part of windows, also timer ids should be returned.
*/
disposeWElementHandles :: !OSWindowPtr !*[WElementHandle .ls .pst] !*OSToolbox 
	 -> (![Id],![Id],!IdFun *OSToolbox,!*[WElementHandle .ls .pst],!*OSToolbox)
disposeWElementHandles wPtr [itemH:itemHs] tb
	# (rids, ids, fs, itemH, tb)	= disposeWElementHandle  wPtr itemH  tb
	# (ridss,idss,fss,itemHs,tb)	= disposeWElementHandles wPtr itemHs tb
	= (rids++ridss,ids++idss,fss o fs,[itemH:itemHs],tb)
disposeWElementHandles _ [] tb
	= ([],[],id,[],tb)

disposeWElementHandle :: !OSWindowPtr !(WElementHandle .ls .pst) !*OSToolbox
	-> (![Id],![Id],!IdFun *OSToolbox, !WElementHandle .ls .pst, !*OSToolbox)
disposeWElementHandle wPtr (WItemHandle itemH) tb
	# (rids,ids,f,itemH,tb)	= disposeWItemHandle wPtr itemH tb
	= (rids,ids,f,WItemHandle itemH,tb)
disposeWElementHandle wPtr (WListLSHandle itemHs) tb
	# (rids,ids,fs,itemHs,tb)	= disposeWElementHandles wPtr itemHs tb
	= (rids,ids,fs,WListLSHandle itemHs,tb)
disposeWElementHandle wPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
	# (rids,ids,fs,itemHs,tb)	= disposeWElementHandles wPtr itemHs tb
	= (rids,ids,fs,WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
disposeWElementHandle wPtr (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
	# (rids,ids,fs,itemHs,tb)	= disposeWElementHandles wPtr itemHs tb
	= (rids,ids,fs,WChangeLSHandle {wChH & wChangeItems=itemHs},tb)


/*	disposeWItemHandle (recursively) hides all system resources associated with the given WItemHandle. 
	The OSWindowPtr argument must identify the parent window.
	The (IdFun *OSToolbox) function must be used to actually dispose the controls.
	It returns all freed receiver ids.
	When timers are part of windows, also timer ids should be returned.
*/
disposeWItemHandle :: !OSWindowPtr !(WItemHandle .ls .pst) !*OSToolbox
  -> (![Id],![Id],!IdFun *OSToolbox,!WItemHandle .ls .pst, !*OSToolbox)

disposeWItemHandle wPtr itemH=:{wItemKind=IsCheckControl,wItemInfo,wItemId} tb
	# checkInfo			= getWItemCheckInfo wItemInfo
	  items				= checkInfo.checkItems
	# tb				= stateMap2 (\{checkItemPtr,checkItemPos,checkItemSize}
										->osSetCheckControlShow wPtr checkItemPtr (posSizeToRect checkItemPos checkItemSize) False
									) items tb
	= ([],maybeToList wItemId,stateMap2 (\{checkItemPtr}->osDestroyCheckControl checkItemPtr) items,itemH,tb)

disposeWItemHandle wPtr itemH=:{wItemKind=IsCompoundControl,wItemInfo,wItems,wItemId,wItemPos,wItemSize,wItemPtr} tb
	# (rids,ids,fs,itemHs,tb)	= disposeWElementHandles wPtr wItems tb
	  f							= osDestroyCompoundControl wItemPtr
	  ids						= maybeToList wItemId ++ ids
	  info						= getWItemCompoundInfo wItemInfo
	# tb						= osSetCompoundShow wPtr wItemPtr (posSizeToRect wItemPos wItemSize) False tb
	  itemH						= {itemH & wItems=itemHs}
	= (rids,ids,f o disposeClipState info.compoundLookInfo.compoundClip o fs,itemH,tb)

disposeWItemHandle wPtr itemH=:{wItemKind=IsLayoutControl,wItems,wItemId} tb
	# (rids,ids,fs,itemHs,tb)	= disposeWElementHandles wPtr wItems tb
	  ids						= maybeToList wItemId ++ ids
	  itemH						= {itemH & wItems=itemHs}
	= (rids,ids,fs,itemH,tb)

disposeWItemHandle wPtr itemH=:{wItemKind=IsOtherControl controltype,wItemId} tb
//	The control is a receiver:
	| controltype=="Receiver" || controltype=="Receiver2"
		= (maybeToList wItemId,[],id,itemH,tb)
/*	The control is a timer:
	| controltype=="TimerControl"
		= ([],getTimerLoc itemH,id,itemH,tb)
*/	| otherwise
		= windowdisposeFatalError "disposeWItemHandle" ("unknown control type: "+++controltype)

disposeWItemHandle wPtr itemH=:{wItemKind=IsRadioControl,wItemId,wItemInfo} tb
	# radioInfo			= getWItemRadioInfo wItemInfo
	  items				= radioInfo.radioItems
	# tb				= stateMap2 (\{radioItemPtr,radioItemPos,radioItemSize}
										->osSetRadioControlShow wPtr radioItemPtr (posSizeToRect radioItemPos radioItemSize) False
									) items tb
	= ([],maybeToList wItemId,stateMap2 (\{radioItemPtr}->osDestroyRadioControl radioItemPtr) items,itemH,tb)

disposeWItemHandle wPtr itemH=:{wItemKind,wItemId,wItemPtr,wItemPos,wItemSize} tb
	# tb				= hide wPtr wItemPtr (posSizeToRect wItemPos wItemSize) False tb
	= ([],maybeToList wItemId,dispose wItemPtr,itemH,tb)
where
	(hide,dispose)		= case wItemKind of
							IsPopUpControl			-> (osSetPopUpControlShow,			osDestroyPopUpControl)
							IsSliderControl			-> (osSetSliderControlShow,			osDestroySliderControl)
							IsTextControl			-> (osSetTextControlShow,			osDestroyTextControl)
							IsEditControl			-> (osSetEditControlShow,			osDestroyEditControl)
							IsButtonControl			-> (osSetButtonControlShow,			osDestroyButtonControl)
							IsCustomButtonControl	-> (osSetCustomButtonControlShow,	osDestroyCustomButtonControl)
							IsCustomControl			-> (osSetCustomControlShow,			osDestroyCustomControl)
							_						-> windowdisposeFatalError "disposeWItemHandle" ("unmatched ControlKind: "+++toString wItemKind)

