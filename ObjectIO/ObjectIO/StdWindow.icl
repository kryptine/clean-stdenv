implementation module StdWindow


//	Clean Object I/O library, version 1.2.1


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	ossystem, ostypes, oswindow
import	StdControlClass
from	StdId				import getParentId
from	StdPSt				import appPIO, accPIO
from	StdSystem			import maxScrollWindowSize
import	commondef, controlpos, iostate, scheduler, windowaccess, windowcreate, windowdevice, windowhandle, windowupdate, wstate
from	controlinternal		import enablecontrols, disablecontrols
from	controllayout		import layoutControls
from	controlrelayout		import relayoutControls
from	controlvalidate		import controlIdsAreConsistent
from	keyfocus			import getCurrentFocusItem
from	StdWindowAttribute	import	isWindowCursor,   getWindowCursorAtt,
									isWindowId,       getWindowIdAtt, 
									isWindowHMargin,  getWindowHMarginAtt,
									isWindowItemSpace,getWindowItemSpaceAtt,
									isWindowKeyboard, getWindowKeyboardAtt,
									isWindowMouse,    getWindowMouseAtt,
									isWindowVMargin,  getWindowVMarginAtt
from	windowclipstate	import validateWindowClipState, forceValidWindowClipState
from	windowdispose	import disposeWindow
from	windowdraw		import drawinwindow, drawwindowlook
from	windowvalidate	import validateWindowId, validateViewDomain, exactWindowPos, exactWindowSize


//	General functions:

StdWindowFatalError :: String String -> .x
StdWindowFatalError function error
	= FatalError function "StdWindow" error

//	Use these two macros to identify windows and dialogues.
windowtype	:==	"Window"
dialogtype	:== "Dialog"

class Windows wdef where
	openWindow		:: .ls !(wdef .ls (PSt .l)) !(PSt .l)	-> (!ErrorReport,!PSt .l)
	getWindowType	::      (wdef .ls .pst)					-> WindowType

class Dialogs wdef where
	openDialog		:: .ls !(wdef .ls (PSt .l)) !(PSt .l)	-> (  !ErrorReport,            !PSt .l)
	openModalDialog	:: .ls !(wdef .ls (PSt .l)) !(PSt .l)	-> (!(!ErrorReport,!Maybe .ls),!PSt .l)
	getDialogType	::      (wdef .ls .pst)					-> WindowType

instance Windows (Window c) | Controls c where
	openWindow :: .ls !(Window c .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | Controls c
	openWindow ls (Window title controls atts) pState
		# pState				= WindowFunctions.dOpen pState
		# (isZero,pState)		= accPIO checkZeroWindowBound pState
		| isZero
			= (ErrorViolateDI,pState)
		# maybe_id				= getWindowIdAttribute atts
		# (maybe_okId,pState)	= accPIO (validateWindowId maybe_id) pState
		| isNothing maybe_okId
			= (ErrorIdsInUse,pState)
		# (cs,pState)			= controlToHandles controls pState
		# (rt,ioState)			= IOStGetReceiverTable pState.io
		# (it,ioState)			= IOStGetIdTable ioState
		# (ioId,ioState)		= IOStGetIOId ioState
		  itemHs				= map ControlStateToWElementHandle cs
		  okId					= fromJust maybe_okId
		  (ok,itemHs,rt,it)		= controlIdsAreConsistent ioId okId itemHs rt it
		# it					= addIdToIdTableIfConsistent ok okId ioId it
		# ioState				= IOStSetIdTable it ioState
		# ioState				= IOStSetReceiverTable rt ioState
		# pState				= {pState & io=ioState}
		| not ok
			= (ErrorIdsInUse,pState)
		| otherwise
			# wH				= initWindowHandle title Modeless IsWindow NoWindowInfo itemHs atts
			# pState			= openwindow okId {wlsState=ls,wlsHandle=wH} pState
			# pState			= appPIO decreaseWindowBound pState
			= (NoError,pState)
	
	getWindowType :: (Window c .ls .pst) -> WindowType | Controls c
	getWindowType _
		= windowtype

instance Dialogs (Dialog c) | Controls c where
	openDialog :: .ls !(Dialog c .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | Controls c
	openDialog ls (Dialog title controls atts) pState
		# pState				= WindowFunctions.dOpen pState
		# maybe_id				= getWindowIdAttribute atts
		# (maybe_okId,pState)	= accPIO (validateWindowId maybe_id) pState
		| isNothing maybe_okId
			= (ErrorIdsInUse,pState)
		# (cs,pState)			= controlToHandles controls pState
		# (rt,ioState)			= IOStGetReceiverTable pState.io
		# (it,ioState)			= IOStGetIdTable ioState
		# (ioId,ioState)		= IOStGetIOId ioState
		  itemHs				= map ControlStateToWElementHandle cs
		  okId					= fromJust maybe_okId
		  (ok,itemHs,rt,it)		= controlIdsAreConsistent ioId okId itemHs rt it
		# it					= addIdToIdTableIfConsistent ok okId ioId it
		# ioState				= IOStSetIdTable it ioState
		# ioState				= IOStSetReceiverTable rt ioState
		# pState				= {pState & io=ioState}
		| not ok
			= (ErrorIdsInUse,pState)
		| otherwise
			# wH				= initWindowHandle title Modeless IsDialog NoWindowInfo itemHs atts
			= (NoError,openwindow okId {wlsState=ls,wlsHandle=wH} pState)
	
	openModalDialog :: .ls !(Dialog c .ls (PSt .l)) !(PSt .l) -> (!(!ErrorReport,!Maybe .ls),!PSt .l) | Controls c
	openModalDialog ls (Dialog title controls atts) pState
		# pState				= WindowFunctions.dOpen pState
		# maybe_id				= getWindowIdAttribute atts
		# (maybe_okId,pState)	= accPIO (validateWindowId maybe_id) pState
		| isNothing maybe_okId
			= ((ErrorIdsInUse,Nothing),pState)
		# (cs,pState)			= controlToHandles controls pState
		# (rt,ioState)			= IOStGetReceiverTable pState.io
		# (it,ioState)			= IOStGetIdTable ioState
		# (ioId,ioState)		= IOStGetIOId ioState
		  itemHs				= map ControlStateToWElementHandle cs
		  okId					= fromJust maybe_okId
		  (ok,itemHs,rt,it)		= controlIdsAreConsistent ioId okId itemHs rt it
		# it					= addIdToIdTableIfConsistent ok okId ioId it
		# ioState				= IOStSetIdTable it ioState
		# ioState				= IOStSetReceiverTable rt ioState
		# pState				= {pState & io=ioState}
		| not ok
			= ((ErrorIdsInUse,Nothing),pState)
		| otherwise
			# wH				= initWindowHandle title Modal IsDialog NoWindowInfo itemHs atts
			# (errorReport,finalLS,pState)
								= openmodalwindow okId {wlsState=ls,wlsHandle=wH} pState
			= ((errorReport,finalLS),pState)
	
	getDialogType :: (Dialog c .ls .pst) -> WindowType | Controls c
	getDialogType _
		= dialogtype


addIdToIdTableIfConsistent :: !Bool !Id !SystemId !*IdTable -> *IdTable
addIdToIdTableIfConsistent True id ioId idTable
	= snd (addIdToIdTable id {idpIOId=ioId,idpDevice=WindowDevice,idpId=id} idTable)
addIdToIdTableIfConsistent _ _ _ idTable
	= idTable

getWindowIdAttribute :: ![WindowAttribute .pst] -> Maybe Id
getWindowIdAttribute atts
	# (hasIdAtt,idAtt)	= Select isWindowId undef atts
	| hasIdAtt			= Just (getWindowIdAtt idAtt)
	| otherwise			= Nothing


/*	closeWindow closes the indicated window.
*/
closeWindow :: !Id !(PSt .l) -> PSt .l
closeWindow id pState
	= disposeWindow (toWID id) pState

closeActiveWindow :: !(PSt .l) -> PSt .l
closeActiveWindow pState
	# (maybeId,pState)	= accPIO getActiveWindow pState
	| isNothing maybeId
		= pState
	| otherwise
		= closeWindow (fromJust maybeId) pState


/*	setActiveWindow activates the given window.
*/
setActiveWindow :: !Id !(PSt .l) -> PSt .l
setActiveWindow wId pState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice pState.io
	| not found
		= {pState & io=ioState}
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (exists,windows)			= hasWindowHandlesWindow wid windows
	| not exists				// Indicated window does not exist
		= {pState & io=IOStSetDevice (WindowSystemState windows) ioState}
	# (activeWIDS,windows)		= getWindowHandlesActiveWindow windows
	| isNothing activeWIDS		// There are no windows, so skip it
		= {pState & io=IOStSetDevice (WindowSystemState windows) ioState}
	# wids						= fromJust activeWIDS
	| exists && wids.wId==wId	// If already active, then skip
		= {pState & io=IOStSetDevice (WindowSystemState windows) ioState}
	# (wHs,windows)				= getWindowHandlesWindows windows
	  (modal,modeless)			= Uspan ismodalwindow wHs
	  (isModal,modal)			= UContains (identifyWindowStateHandle wid) modal
	| isModal					// Modal windows should not be activated
		= {pState & io=IOStSetDevice (WindowSystemState {windows & whsWindows=modal++modeless}) ioState}
	# (osdInfo,ioState)			= IOStGetOSDInfo ioState
	  isSDI						= getOSDInfoDocumentInterface osdInfo==SDI
	  (framePtr,clientPtr)		= case (getOSDInfoOSInfo osdInfo) of
	  								Just info -> (info.osFrame,info.osClient)
	  								_         -> (OSNoWindowPtr,OSNoWindowPtr)
	# (noModals,modal)			= u_isEmpty modal
	| noModals					// There are no modal windows, so put activated window in front
		# (_,wsH,others)		= URemove (identifyWindowStateHandle wid) undef modeless
		  (shown,wsH)			= getWindowStateHandleShow wsH
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		  activatePtr			= if (isSDI && wids.wPtr==clientPtr) framePtr wids.wPtr		// Do not activate SDI client, but SDI frame
		  showAction			= if shown id (snd o OSshowWindow activatePtr True)
		# ioState				= IOStSetDevice (WindowSystemState {windows & whsWindows=[wsH:others]}) ioState
//		# (delayinfo,ioState)	= accIOToolbox (OSactivateWindow osdInfo activatePtr o showAction) ioState
		# (tb,ioState)			= getIOToolbox ioState
		# pState				= {pState & io=ioState}
		# (delayinfo,pState,tb)	= OSactivateWindow osdInfo activatePtr handleOSEvent pState (showAction tb)
		# ioState				= setIOToolbox tb pState.io
		# ioState				= bufferDelayedEvents delayinfo ioState
		= {pState & io=ioState}
	| otherwise					// There are modal windows, so put activated window behind last modal
		# (befModals,lastModal)	= InitLast modal
		  (modalWIDS,lastModal)	= getWindowStateHandleWIDS lastModal
		  (_,wsH,others)		= URemove (identifyWindowStateHandle wid) undef modeless
		  (shown,wsH)			= getWindowStateHandleShow wsH
		  (modelessWIDS,wsH)	= getWindowStateHandleWIDS wsH
		  activatePtr			= if (isSDI && modelessWIDS.wPtr==clientPtr) framePtr modelessWIDS.wPtr	// Do not activate SDI client, but SDI frame
		  showAction			= if shown id (snd o OSshowWindow activatePtr True)
		# ioState				= IOStSetDevice (WindowSystemState {windows & whsWindows=befModals++[lastModal,wsH:others]}) ioState
//		# ioState				= appIOToolbox (showAction o OSstackWindow activatePtr modalWIDS.wPtr) ioState
		# (tb,ioState)			= getIOToolbox ioState
		# pState				= {pState & io=ioState}
		# (delayinfo,pState,tb)	= OSstackWindow activatePtr modalWIDS.wPtr handleOSEvent pState (showAction tb)
		# ioState				= setIOToolbox tb pState.io
		# ioState				= bufferDelayedEvents delayinfo ioState
		= {pState & io=ioState}
where
	wid							= toWID wId
	
	ismodalwindow :: !(WindowStateHandle .pst) -> *(!Bool,!WindowStateHandle .pst)
	ismodalwindow wsH
		# (mode,wsH)			= getWindowStateHandleWindowMode wsH
		= (mode==Modal,wsH)


/*	getActiveWindow returns the Id of the currently active window.
*/
getActiveWindow :: !(IOSt .l) -> (!Maybe Id, !IOSt .l)
getActiveWindow ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (activeWIDS,windows)		= getWindowHandlesActiveWindow windows
	# ioState					= IOStSetDevice (WindowSystemState windows) ioState
	= (mapMaybe (\{wId}->wId) activeWIDS,ioState)


/*	setActiveControl makes the indicated control active only if its parent window is already active.
*/
setActiveControl :: !Id !(PSt .l) -> PSt .l
setActiveControl controlId pState=:{io}
	# (parentId,ioState)		= getParentId controlId io
	| isNothing parentId
		= {pState & io=ioState}
	# (activeId,ioState)		= getActiveWindow ioState
	| isNothing activeId
		= {pState & io=ioState}
	# parentId					= fromJust parentId
	# activeId					= fromJust activeId
	| parentId<>activeId
		= {pState & io=ioState}
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= {pState & io=ioState}
	# windows					= WindowSystemStateGetWindowHandles wDevice
	# (found,wsH,windows)		= getWindowHandlesWindow (toWID activeId) windows
	| not found
		= StdWindowFatalError "setActiveControl" "parent window could not be located"
	| otherwise
		# (tb,ioState)			= getIOToolbox ioState
		# (delayinfo,wsH,tb)	= setactivecontrol controlId wsH tb
		# ioState				= setIOToolbox tb ioState
		  windows				= setWindowHandlesWindow wsH windows
		# ioState				= IOStSetDevice (WindowSystemState windows) ioState
		# ioState				= bufferDelayedEvents delayinfo ioState
		= {pState & io=ioState}
where
	setactivecontrol :: !Id !(WindowStateHandle .pst) !*OSToolbox -> (![DelayActivationInfo],!WindowStateHandle .pst,!*OSToolbox)
	setactivecontrol controlId wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH=:{whKeyFocus,whItems}}} tb
		# (found,itemNr,itemPtr,itemHs)	= getWElementHandlesItemNrPtr controlId whItems
		| not found
			= StdWindowFatalError "setActiveControl" "indicated control could not be located"
		| otherwise
		//	# keyfocus					= setNewFocusItem itemNr whKeyFocus
			# (delayinfo,tb)			= OSactivateControl wshIds.wPtr itemPtr tb
			= (delayinfo,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & /*whKeyFocus=keyfocus,*/whItems=itemHs}}},tb)
	where
		getWElementHandlesItemNrPtr :: !Id ![WElementHandle .ls .pst] -> (!Bool,!Int,!OSWindowPtr,![WElementHandle .ls .pst])
		getWElementHandlesItemNrPtr id [itemH:itemHs]
			# (found,itemNr,itemPtr,itemH)	= getWElementHandleItemNrPtr id itemH
			| found
				= (found,itemNr,itemPtr,[itemH:itemHs])
			| otherwise
				# (found,itemNr,itemPtr,itemHs)	= getWElementHandlesItemNrPtr id itemHs
				= (found,itemNr,itemPtr,[itemH:itemHs])
		where
			getWElementHandleItemNrPtr :: !Id !(WElementHandle .ls .pst) -> (!Bool,!Int,!OSWindowPtr,!WElementHandle .ls .pst)
			getWElementHandleItemNrPtr id (WItemHandle itemH=:{wItemNr,wItems,wItemId,wItemPtr})
				| isNothing wItemId || fromJust wItemId<>id
					# (found,itemNr,itemPtr,itemHs)	= getWElementHandlesItemNrPtr id wItems
					= (found,itemNr,itemPtr,WItemHandle {itemH & wItems=itemHs})
				| otherwise
					= (True,wItemNr,wItemPtr,WItemHandle itemH)
			getWElementHandleItemNrPtr itemNr (WListLSHandle itemHs)
				# (found,itemNr,itemPtr,itemHs)		= getWElementHandlesItemNrPtr id itemHs
				= (found,itemNr,itemPtr,WListLSHandle itemHs)
			getWElementHandleItemNrPtr itemNr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				# (found,itemNr,itemPtr,itemHs)		= getWElementHandlesItemNrPtr id itemHs
				= (found,itemNr,itemPtr,WExtendLSHandle {wExH & wExtendItems=itemHs})
			getWElementHandleItemNrPtr itemNr (WChangeLSHandle wChH=:{wChangeItems=itemHs})
				# (found,itemNr,itemPtr,itemHs)		= getWElementHandlesItemNrPtr id itemHs
				= (found,itemNr,itemPtr,WChangeLSHandle {wChH & wChangeItems=itemHs})
		getWElementHandlesItemNrPtr _ _
			= (False,0,OSNoWindowPtr,[])
	setactivecontrol _ _ _
		= StdWindowFatalError "setActiveControl" "unexpected window placeholder argument"

/*	getActiveControl returns the Id of the currently active control. The Bool result is True only iff the 
	control could be found. In that case, if the control had an Id attribute, then (Just id) is returned,
	Nothing otherwise.
*/
getActiveControl :: !(IOSt .l) -> (!(!Bool,!Maybe Id),!IOSt .l)
getActiveControl ioState
	# (activeId,ioState)		= getActiveWindow ioState
	| isNothing activeId
		= ((False,Nothing),ioState)
	# activeId					= fromJust activeId
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ((False,Nothing),ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	# (hasWindow,wsH,windows)	= getWindowHandlesWindow (toWID activeId) windows
	| not hasWindow
		= StdWindowFatalError "getActiveControl" "active window could not be located"
	# (keyfocus,wsH)			= getWindowStateHandleKeyFocus wsH
	# (maybeItemNr,keyfocus)	= getCurrentFocusItem keyfocus
	# wsH						= setWindowStateHandleKeyFocus keyfocus wsH
	| isNothing maybeItemNr
		# windows				= setWindowHandlesWindow wsH windows
		# ioState				= IOStSetDevice (WindowSystemState windows) ioState
		= ((False,Nothing),ioState)
	# (foundId,wsH)				= getControlIdFromItemNr (fromJust maybeItemNr) wsH
	  windows					= setWindowHandlesWindow wsH windows
	# ioState					= IOStSetDevice (WindowSystemState windows) ioState
	= (foundId,ioState)
where
	getControlIdFromItemNr :: !Int !(WindowStateHandle .pst) -> (!(!Bool,!Maybe Id),!WindowStateHandle .pst)
	getControlIdFromItemNr itemNr wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems}}}
		# (_,foundId,itemHs)	= getWElementHandlesIdFromItemNr itemNr whItems
		= (foundId,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
	where
		getWElementHandlesIdFromItemNr :: !Int ![WElementHandle .ls .pst] -> (!Bool,!(!Bool,!Maybe Id),![WElementHandle .ls .pst])
		getWElementHandlesIdFromItemNr itemNr [itemH:itemHs]
			# (found,foundId,itemH)		= getWElementHandleIdFromItemNr itemNr itemH
			| found
				= (found,foundId,[itemH:itemHs])
			| otherwise
				# (found,foundId,itemHs)= getWElementHandlesIdFromItemNr itemNr itemHs
				= (found,foundId,[itemH:itemHs])
		where
			getWElementHandleIdFromItemNr :: !Int !(WElementHandle .ls .pst) -> (!Bool,!(!Bool,!Maybe Id),!WElementHandle .ls .pst)
			getWElementHandleIdFromItemNr itemNr (WItemHandle itemH=:{wItemNr,wItems,wItemId})
				| itemNr<>wItemNr
					# (found,foundId,itemHs)	= getWElementHandlesIdFromItemNr itemNr wItems
					= (found,foundId,WItemHandle {itemH & wItems=itemHs})
				| otherwise
					= (True,(True,wItemId),WItemHandle itemH)
			getWElementHandleIdFromItemNr itemNr (WListLSHandle itemHs)
				# (found,foundId,itemHs)		= getWElementHandlesIdFromItemNr itemNr itemHs
				= (found,foundId,WListLSHandle itemHs)
			getWElementHandleIdFromItemNr itemNr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				# (found,foundId,itemHs)		= getWElementHandlesIdFromItemNr itemNr itemHs
				= (found,foundId,WExtendLSHandle {wExH & wExtendItems=itemHs})
			getWElementHandleIdFromItemNr itemNr (WChangeLSHandle wChH=:{wChangeItems=itemHs})
				# (found,foundId,itemHs)		= getWElementHandlesIdFromItemNr itemNr itemHs
				= (found,foundId,WChangeLSHandle {wChH & wChangeItems=itemHs})
		getWElementHandlesIdFromItemNr _ _
			= (False,(False,Nothing),[])
	getControlIdFromItemNr _ _
		= StdWindowFatalError "getActiveControl" "unexpected window placeholder argument"


/*	stackWindow changes the stacking order of the current windows.
*/
stackWindow :: !Id !Id !(PSt .l) -> PSt .l
stackWindow windowId behindId pState=:{io=ioState}
	| windowId==behindId			// Don't stack a window behind itself
		= pState
	# (found,wDevice,ioState)		= IOStGetDevice WindowDevice ioState
	| not found
		= {pState & io=ioState}
	# windows						= WindowSystemStateGetWindowHandles wDevice
	# (hasBehind,windows)			= hasWindowHandlesWindow behindWID windows
	| not hasBehind					// Behind window does not exist
		# ioState					= IOStSetDevice (WindowSystemState windows) ioState
		= {pState & io=ioState}
	# (hasWindow,wsH,windows)		= getWindowHandlesWindow windowWID windows
	| not hasWindow					// Stack window does not exist
		# ioState					= IOStSetDevice (WindowSystemState windows) ioState
		= {pState & io=ioState}
	# (mode,wsH)					= getWindowStateHandleWindowMode wsH
	| mode==Modal					// Stack window is modal, skip
		# ioState					= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
		= {pState & io=ioState}
	| otherwise
		# (_,_,windows)				= removeWindowHandlesWindow windowWID windows		// remove placeholder window
		# (wids,wsH)				= getWindowStateHandleWIDS wsH
		# (behindWIDS,windows)		= addBehindWindowHandlesWindow behindWID wsH windows
		# ioState					= IOStSetDevice (WindowSystemState windows) ioState
		# (tb,ioState)				= getIOToolbox ioState
		# pState					= {pState & io=ioState}
		# (delayinfo,pState,tb)		= OSstackWindow wids.wPtr behindWIDS.wPtr handleOSEvent pState tb
		# ioState					= setIOToolbox tb pState.io
		# ioState					= bufferDelayedEvents delayinfo ioState
		= {pState & io=ioState}
where
	windowWID						= toWID windowId
	behindWID						= toWID behindId

/*	handleOSEvent turns handleOneEventForDevices into the form required by OSactivateWindow and OSstackWindow.
	(Used by stackWindow, setActiveWindow.)
*/
handleOSEvent :: !OSEvent !(!PSt .l,!*OSToolbox) -> (!PSt .l,!*OSToolbox)
handleOSEvent osEvent (pState,tb)
	= (thd3 (handleOneEventForDevices (ScheduleOSEvent osEvent []) pState),tb)

getWindowStack :: !(IOSt .l) -> (![(Id,WindowType)],!IOSt .l)
getWindowStack ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ([],ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  wsHs						= windows.whsWindows
	  (id_types,wsHs)			= unzip (map getWindowIdType wsHs)
	= (id_types,IOStSetDevice (WindowSystemState {windows & whsWindows=wsHs}) ioState)
where
	getWindowIdType :: !(WindowStateHandle .pst) -> ((Id,WindowType),!WindowStateHandle .pst)
	getWindowIdType wsH
		# (wids,wsH)	= getWindowStateHandleWIDS wsH
		# (kind,wsH)	= getWindowStateHandleWindowKind wsH
		= ((wids.wId,if (kind==IsWindow) windowtype dialogtype),wsH)

getWindowsStack :: !(IOSt .l) -> (![Id],!IOSt .l)
getWindowsStack ioState
	# (id_types,ioState)	= getWindowStack ioState
	= (FilterMap (\(id,wtype)->(wtype==windowtype,id)) id_types,ioState)

getDialogsStack :: !(IOSt .l) -> (![Id],!IOSt .l)
getDialogsStack ioState
	# (id_types,ioState)	= getWindowStack ioState
	= (FilterMap (\(id,wtype)->(wtype==dialogtype,id)) id_types,ioState)


/*	Return layout attributes and default values.
*/
getDefaultHMargin :: !Bool !(IOSt .l) -> ((Int,Int),!IOSt .l)
getDefaultHMargin isWindow ioState
	| isWindow
		= ((0,0),ioState)
	| otherwise
		# ({osmHorMargin},ioState)	= IOStGetOSWindowMetrics ioState
		= ((osmHorMargin,osmHorMargin),ioState)

getDefaultVMargin :: !Bool !(IOSt .l) -> ((Int,Int),!IOSt .l)
getDefaultVMargin isWindow ioState
	| isWindow
		= ((0,0),ioState)
	| otherwise
		# ({osmVerMargin},ioState)	= IOStGetOSWindowMetrics ioState
		= ((osmVerMargin,osmVerMargin),ioState)

getDefaultItemSpace :: !Bool !(IOSt .l) -> ((Int,Int),!IOSt .l)
getDefaultItemSpace _ ioState
	# ({osmHorItemSpace,osmVerItemSpace},ioState)	= IOStGetOSWindowMetrics ioState
	= ((osmHorItemSpace,osmVerItemSpace),ioState)

getWindowHMargin :: !Id	!(IOSt .l) -> (!Maybe (Int,Int),!IOSt .l)
getWindowHMargin id ioState
	# (found,wDevice,ioState)		= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows						= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)			= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	| otherwise
		# (wMetrics,ioState)		= IOStGetOSWindowMetrics ioState
		# (marginAtt,wsH)			= gethmargin wMetrics wsH
		= (Just marginAtt,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
	gethmargin :: !OSWindowMetrics !(WindowStateHandle .pst) -> ((Int,Int),!WindowStateHandle .pst)
	gethmargin wMetrics wsH=:{wshHandle=Just {wlsHandle={whKind,whAtts}}}
		= (getWindowHMargins whKind wMetrics whAtts,wsH)
	gethmargin _ _
		= StdWindowFatalError "getWindowHMargin" "unexpected window placeholder argument"

getWindowVMargin :: !Id	!(IOSt .l) -> (!Maybe (Int,Int),!IOSt .l)
getWindowVMargin id ioState
	# (found,wDevice,ioState)		= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows						= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)			= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	| otherwise
		# (wMetrics,ioState)		= IOStGetOSWindowMetrics ioState
		# (marginAtt,wsH)			= getvmargin wMetrics wsH
		= (Just marginAtt,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
	getvmargin :: !OSWindowMetrics !(WindowStateHandle .pst) -> ((Int,Int),!WindowStateHandle .pst)
	getvmargin wMetrics wsH=:{wshHandle=Just {wlsHandle={whKind,whAtts}}}
		= (getWindowVMargins whKind wMetrics whAtts,wsH)
	getvmargin _ _
		= StdWindowFatalError "getWindowVMargin" "unexpected window placeholder argument"

getWindowItemSpace :: !Id !(IOSt .l) -> (!Maybe (Int,Int),!IOSt .l)
getWindowItemSpace id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	| otherwise
		# ({osmHorItemSpace,osmVerItemSpace},ioState)
								= IOStGetOSWindowMetrics ioState
		# (marginAtt,wsH)		= getitemspaces (osmHorItemSpace,osmVerItemSpace) wsH
		= (Just marginAtt,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
	getitemspaces :: (Int,Int) !(WindowStateHandle .pst) -> ((Int,Int),!WindowStateHandle .pst)
	getitemspaces (defHSpace,defVSpace) wsH=:{wshHandle=Just {wlsHandle={whAtts}}}
		= (getWindowItemSpaceAtt (snd (Select isWindowItemSpace (WindowItemSpace defHSpace defVSpace) whAtts)),wsH)
	getitemspaces _ _
		= StdWindowFatalError "getWindowItemSpace" "unexpected window placeholder argument"


/*	Setting the SelectState of windows.
*/
enableWindow :: !Id !(IOSt .l) -> IOSt .l
enableWindow id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	# (curSelectState,wsH)		= getWindowStateHandleSelect wsH
	| curSelectState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# (osdInfo, ioState)	= IOStGetOSDInfo ioState
		  isSDI					= getOSDInfoDocumentInterface osdInfo==SDI
		  framePtr				= case (getOSDInfoOSInfo osdInfo) of
		  							Just info -> info.osFrame
		  							_         -> OSNoWindowPtr
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		  wsH					= setWindowStateHandleSelect True wsH
		  (wids,wsH)			= getWindowStateHandleWIDS wsH
		  wPtr					= wids.wPtr
		# (wH`,wsH,tb)			= retrieveWindowHandle` wsH tb
		# (wH`,tb)				= enablecontrols [] True wMetrics wPtr wH` tb
		  wsH					= insertWindowHandle` wH` wsH
		  (windowInfo,wsH)		= getWindowStateHandleWindowInfo wsH
		  scrollInfo			= case windowInfo of
		  							WindowInfo info	-> (isJust info.windowHScroll,isJust info.windowVScroll)
		  							other			-> (False,False)
		# tb					= OSenableWindow (if isSDI framePtr wPtr) scrollInfo False tb
		# tb					= OSinvalidateWindow wPtr tb
		# ioState				= setIOToolbox tb ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState

disableWindow :: !Id !(IOSt .l) -> IOSt .l
disableWindow id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	# (curSelectState,wsH)		= getWindowStateHandleSelect wsH
	| not curSelectState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# (osdInfo, ioState)	= IOStGetOSDInfo ioState
		  isSDI					= getOSDInfoDocumentInterface osdInfo==SDI
		  framePtr				= case (getOSDInfoOSInfo osdInfo) of
		  							Just info -> info.osFrame
		  							_         -> OSNoWindowPtr
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		  wsH					= setWindowStateHandleSelect False wsH
		  (wids,wsH)			= getWindowStateHandleWIDS wsH
		  wPtr					= wids.wPtr
		# (wH`,wsH,tb)			= retrieveWindowHandle` wsH tb
		# (wH`,tb)				= disablecontrols [] True wMetrics wPtr wH` tb
		  wsH					= insertWindowHandle` wH` wsH
		  (windowInfo,wsH)		= getWindowStateHandleWindowInfo wsH
		  scrollInfo			= case windowInfo of
		  							WindowInfo info	-> (isJust info.windowHScroll,isJust info.windowVScroll)
		  							other			-> (False,False)
		# tb					= OSdisableWindow (if isSDI framePtr wPtr) scrollInfo False tb
		# tb					= OSinvalidateWindow wPtr tb
		# ioState				= setIOToolbox tb ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState

enableWindowMouse :: !Id !(IOSt .l) -> IOSt .l
enableWindowMouse id ioState
	= setWindowMouseSelectState Able id ioState

disableWindowMouse :: !Id !(IOSt .l) -> IOSt .l
disableWindowMouse id ioState
	= setWindowMouseSelectState Unable id ioState

setWindowMouseSelectState :: !SelectState !Id !(IOSt .l) -> IOSt .l
setWindowMouseSelectState selectState id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# wsH					= setMouseSelectState selectState wsH
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	setMouseSelectState :: !SelectState !(WindowStateHandle .pst) -> WindowStateHandle .pst
	setMouseSelectState selectState wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whAtts}}}
		= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whAtts=setMouseSelectStateAtt selectState whAtts}}}
	where
		setMouseSelectStateAtt :: !SelectState ![WindowAttribute .pst] -> [WindowAttribute .pst]
		setMouseSelectStateAtt selectState atts
			# (found,mouseAtt,atts)	= Remove isWindowMouse undef atts
			| not found
				= atts
			| otherwise
				# (filter,_,fun)	= getWindowMouseAtt mouseAtt
				= [WindowMouse filter selectState fun:atts]
	setMouseSelectState _ _
		= StdWindowFatalError "setWindowMouseSelectState" "unexpected window placeholder argument"

enableWindowKeyboard :: !Id !(IOSt .l) -> IOSt .l
enableWindowKeyboard id ioState
	= setWindowKeyboardSelectState Able id ioState

disableWindowKeyboard :: !Id !(IOSt .l) -> IOSt .l
disableWindowKeyboard id ioState
	= setWindowKeyboardSelectState Unable id ioState

setWindowKeyboardSelectState :: !SelectState !Id !(IOSt .l) -> IOSt .l
setWindowKeyboardSelectState selectState id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# wsH					= setKeyboardSelectState selectState wsH
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	setKeyboardSelectState :: !SelectState !(WindowStateHandle .pst) -> WindowStateHandle .pst
	setKeyboardSelectState selectState wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whAtts}}}
		= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whAtts=setKeyboardSelectStateAtt selectState whAtts}}}
	where
		setKeyboardSelectStateAtt :: !SelectState ![WindowAttribute .pst] -> [WindowAttribute .pst]
		setKeyboardSelectStateAtt selectState atts
			# (found,keyAtt,atts)	= Remove isWindowKeyboard undef atts
			| not found
				= atts
			| otherwise
				# (filter,_,fun)	= getWindowKeyboardAtt keyAtt
				= [WindowKeyboard filter selectState fun:atts]
	setKeyboardSelectState _ _
		= StdWindowFatalError "setWindowKeyboardSelectState" "unexpected window placeholder argument"

getWindowSelectState :: !Id !(IOSt .l) -> (!Maybe SelectState,!IOSt .l)
getWindowSelectState id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
	| otherwise
		# (wSelect,wsH)			= getWindowStateHandleSelect wsH
		= (Just (if wSelect Able Unable),IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowMouseSelectState :: !Id !(IOSt .l) -> (!Maybe SelectState,!IOSt .l)
getWindowMouseSelectState id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
	| otherwise
		# (found,_,select,wsH)	= getWindowMouseAttInfo wsH
		= (if found (Just select) Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowMouseAttInfo :: !(WindowStateHandle .pst) -> (!Bool,MouseStateFilter,SelectState,!WindowStateHandle .pst)
getWindowMouseAttInfo wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whAtts}}}
	# (hasMouseAtt,mouseAtt)	= Select isWindowMouse undef whAtts
	| not hasMouseAtt
		= (False,undef,undef,wsH)
	| otherwise
		# (filter,selectState,_)= getWindowMouseAtt mouseAtt
		= (True,filter,selectState,wsH)
getWindowMouseAttInfo _
	= StdWindowFatalError "getWindowMouseAttInfo" "unexpected window placeholder argument"

getWindowKeyboardSelectState :: !Id !(IOSt .l) -> (!Maybe SelectState,!IOSt .l)
getWindowKeyboardSelectState id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
	| otherwise
		# (found,_,select,wsH)	= getWindowKeyboardAttInfo wsH
		= (if found (Just select) Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowKeyboardAttInfo :: !(WindowStateHandle .pst) -> (!Bool,KeyboardStateFilter,SelectState,!WindowStateHandle .pst)
getWindowKeyboardAttInfo wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whAtts}}}
	# (hasKeyAtt,keyAtt)		= Select isWindowKeyboard undef whAtts
	| not hasKeyAtt
		= (False,undef,undef,wsH)
	| otherwise
		# (filter,selectState,_)= getWindowKeyboardAtt keyAtt
		= (True,filter,selectState,wsH)
getWindowKeyboardAttInfo _
	= StdWindowFatalError "getWindowKeyboardAttInfo" "unexpected window placeholder argument"

getWindowMouseStateFilter :: !Id !(IOSt .l) -> (!Maybe MouseStateFilter,!IOSt .l)
getWindowMouseStateFilter id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
	| otherwise
		# (found,filter,_,wsH)	= getWindowMouseAttInfo wsH
		= (if found (Just filter) Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowKeyboardStateFilter :: !Id !(IOSt .l) -> (!Maybe KeyboardStateFilter,!IOSt .l)
getWindowKeyboardStateFilter id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
	| otherwise
		# (found,filter,_,wsH)	= getWindowKeyboardAttInfo wsH
		= (if found (Just filter) Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

setWindowMouseStateFilter :: !Id !MouseStateFilter !(IOSt .l) -> IOSt .l
setWindowMouseStateFilter id filter ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# wsH					= setMouseFilter filter wsH
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	setMouseFilter :: !MouseStateFilter !(WindowStateHandle .pst) -> WindowStateHandle .pst
	setMouseFilter filter wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whAtts}}}
		= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whAtts=setMouseStateFilterAtt filter whAtts}}}
	where
		setMouseStateFilterAtt :: !MouseStateFilter ![WindowAttribute .pst] -> [WindowAttribute .pst]
		setMouseStateFilterAtt filter atts
			# (found,mouseAtt,atts)	= Remove isWindowMouse undef atts
			| not found
				= atts
			| otherwise
				# (_,select,fun)	= getWindowMouseAtt mouseAtt
				= [WindowMouse filter select fun:atts]
	setMouseFilter _ _
		= StdWindowFatalError "setWindowMouseStateFilter" "unexpected window placeholder argument"

setWindowKeyboardStateFilter :: !Id !KeyboardStateFilter !(IOSt .l) -> IOSt .l
setWindowKeyboardStateFilter id filter ioState
	# (found,wDevice,ioState)		= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows						= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)			= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wKind,wsH)					= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# wsH						= setKeyboardFilter filter wsH
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	setKeyboardFilter :: !KeyboardStateFilter !(WindowStateHandle .pst) -> WindowStateHandle .pst
	setKeyboardFilter filter wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whAtts}}}
		= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whAtts=setKeyboardStateFilterAtt filter whAtts}}}
	where
		setKeyboardStateFilterAtt :: !KeyboardStateFilter ![WindowAttribute .pst] -> [WindowAttribute .pst]
		setKeyboardStateFilterAtt filter atts
			# (found,keyAtt,atts)	= Remove isWindowKeyboard undef atts
			| not found
				= atts
			| otherwise
				# (_,select,fun)	= getWindowKeyboardAtt keyAtt
				= [WindowKeyboard filter select fun:atts]
	setKeyboardFilter _ _
		= StdWindowFatalError "setWindowKeyboardStateFilter" "unexpected window placeholder argument"


//	Operations that are concerned with the background/look of a window. 

appWindowPicture:: !Id !.(IdFun *Picture) !(IOSt .l) -> IOSt .l
appWindowPicture id drawf ioState
	= snd (drawInWindow "appWindowPicture" id (\p->(undef,drawf p)) ioState)

accWindowPicture:: !Id !.(St *Picture .x) !(IOSt .l) -> (!Maybe .x,!IOSt .l)
accWindowPicture id drawf ioState
	= drawInWindow "accWindowPicture" id drawf ioState

drawInWindow :: String !Id !.(St *Picture .x) !(IOSt .l) -> (!Maybe .x,!IOSt .l)
drawInWindow functionname id drawf ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
	| otherwise
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# ((x,wsH),ioState)		= accIOToolbox (drawinwindow` wMetrics drawf wsH) ioState
		= (Just x,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
	drawinwindow` :: !OSWindowMetrics !.(St *Picture .x) !(WindowStateHandle .pst) !*OSToolbox
												 -> (!(.x,!WindowStateHandle .pst),!*OSToolbox)
	drawinwindow` wMetrics drawf wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		# (wH,tb)			= validateWindowClipState wMetrics False wPtr wH tb
		# (x,wH,tb)			= drawinwindow wMetrics wPtr drawf wH tb
		= ((x,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}}),tb)
	drawinwindow` _ _ _ _
		= StdWindowFatalError functionname "unexpected window placeholder argument"

updateWindow :: !Id !(Maybe ViewFrame) !(IOSt .l) -> IOSt .l
updateWindow id maybeViewFrame ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (wsH,ioState)			= accIOToolbox (updateWindowBackground wMetrics maybeViewFrame wsH) ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	updateWindowBackground :: !OSWindowMetrics !(Maybe ViewFrame) !(WindowStateHandle .pst) !*OSToolbox
															   -> (!WindowStateHandle .pst, !*OSToolbox)
	updateWindowBackground wMetrics maybeViewFrame wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH=:{whSize,whWindowInfo}}} tb
		| IsEmptyRect updArea
			= (wsH,tb)
		| otherwise
			# (wH,tb)					= updatewindow wMetrics updInfo wH tb
			= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	where
		info							= getWindowInfoWindowData whWindowInfo
		(origin,domainRect,hasScrolls)	= (info.windowOrigin,info.windowDomain,(isJust info.windowHScroll,isJust info.windowVScroll))
		visScrolls						= OSscrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
		contentRect						= getWindowContentRect wMetrics visScrolls (SizeToRect whSize)
		updArea							= case maybeViewFrame of
											Nothing		-> contentRect
											Just rect	-> IntersectRects (RectangleToRect (subVector (toVector origin) rect)) contentRect
		updInfo							= {	updWIDS			= wshIds
										  ,	updWindowArea	= updArea
										  ,	updControls		= []
										  ,	updGContext		= Nothing
										  }
	updateWindowBackground _ _ _ _
		= StdWindowFatalError "updateWindow" "unexpected window placeholder argument"

setWindowLook :: !Id !Bool !(!Bool,!Look) !(IOSt .l) -> IOSt .l
setWindowLook wId redraw (sysLook,lookFun) ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID wId) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (wsH,ioState)			= accIOToolbox (setwindowlook wMetrics redraw (sysLook,lookFun) wsH) ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	setwindowlook :: !OSWindowMetrics !Bool !(!Bool,!Look) !(WindowStateHandle .pst) !*OSToolbox -> (!WindowStateHandle .pst,!*OSToolbox)
	setwindowlook wMetrics redraw (sysLook,lookFun) wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		#! lookInfo				= {lookInfo & lookFun=lookFun,lookSysUpdate=sysLook}
		#! info					= {windowInfo & windowLook=lookInfo}
		#! wH					= {wH & whWindowInfo=WindowInfo info}
		| not redraw
			= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
		| otherwise
			#! (wH,tb)			= validateWindowClipState wMetrics False wPtr wH tb
			#! (wH,tb)			= drawwindowlook wMetrics wPtr id updState wH tb
			=  ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	where
		whSize					= wH.whSize
	//	windowInfo				= fromJust wH.whWindowInfo	Mike: fromJust changed to getWindowInfoWindowData
		windowInfo				= getWindowInfoWindowData wH.whWindowInfo
		lookInfo				= windowInfo.windowLook
		domainRect				= windowInfo.windowDomain
		origin					= windowInfo.windowOrigin
		hasScrolls				= (isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll)
		visScrolls				= OSscrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
		contentRect				= getWindowContentRect wMetrics visScrolls (SizeToRect whSize)
		wFrame					= PosSizeToRectangle origin (RectSize contentRect)
		updState				= RectangleToUpdateState wFrame
	setwindowlook _ _ _ _ _
		= StdWindowFatalError "setWindowLook" "unexpected window placeholder argument"

getWindowLook :: !Id !(IOSt .l) -> (!Maybe (Bool,Look),!IOSt .l)
getWindowLook id ioState
	# (found,wDevice,ioState)		= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows						= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)			= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	# (wKind,wsH)					= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
	| otherwise
		# (windowInfo,wsH)			= getWindowStateHandleWindowInfo wsH
		  {lookFun,lookSysUpdate}	= (getWindowInfoWindowData windowInfo).windowLook
		= (Just (lookSysUpdate,lookFun),IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)


//	Operations that are concerned with the position of windows/dialogues.

setWindowPos :: !Id !ItemPos !(IOSt .l) -> IOSt .l
setWindowPos id pos ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wMode,wsH)				= getWindowStateHandleWindowMode wsH
	| wMode==Modal
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	# (okId,pos,windows)		= validateRelativeId id pos windows
	| not okId
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# (wids, wsH)			= getWindowStateHandleWIDS wsH
		  (wSize,wsH)			= getWindowStateHandleSize wsH
		  (wKind,wsH)			= getWindowStateHandleWindowKind wsH
		  windows				= setWindowHandlesWindow wsH windows
		# (osdInfo, ioState)	= IOStGetOSDInfo ioState
		  isSDI					= getOSDInfoDocumentInterface osdInfo==SDI
		  (framePtr,clientPtr)	= case (getOSDInfoOSInfo osdInfo) of
		  							Just info -> (info.osFrame,info.osClient)
		  							_         -> (OSNoWindowPtr,OSNoWindowPtr)
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		# (pos,windows,tb)		= exactWindowPos wMetrics wSize (Just pos) wKind Modeless windows tb
		# tb					= OSsetWindowPos (if (isSDI && wids.wPtr==clientPtr) framePtr wids.wPtr) (toTuple pos) True True tb
		# ioState				= setIOToolbox tb ioState
		= IOStSetDevice (WindowSystemState windows) ioState
where
	// validateRelativeId checks the validity of the ItemPos. 
	// It assumes that the WindowHandles argument is not empty (containing atleast the target window).
	validateRelativeId :: !Id !ItemPos !(WindowHandles .pst) -> (!Bool,!ItemPos,!WindowHandles .pst)
	validateRelativeId id itemPos=:(itemLoc,itemOffset) windows
		| isRelative
			# (exists,windows)	= hasWindowHandlesWindow (toWID relativeId) windows
			= (exists,itemPos,windows)
		| isRelativePrev
			# wsHs				= windows.whsWindows
			  (widsstack,wsHs)	= unzip (map getWindowStateHandleWIDS wsHs)
			  windows			= {windows & whsWindows=wsHs}
			  (found,prevId)	= findPrevId (toWID id) widsstack
			  itemLoc			= if (not found) itemLoc
			  					 (case itemLoc of
									LeftOfPrev  -> LeftOf  prevId
									RightToPrev -> RightTo prevId
									AbovePrev   -> Above   prevId
									BelowPrev   -> Below   prevId
								 )
			= (found,(itemLoc,itemOffset),windows)
			with
				findPrevId :: !WID ![WIDS] -> (!Bool,Id)
				findPrevId wid [_]
					= (False,undef)
				findPrevId wid [wid`:wids=:[wid``:_]]
					| identifyWIDS wid wid``	= (True,wid`.wId)
					| otherwise					= findPrevId wid [wid``:wids]
		| otherwise
			= (True,itemPos,windows)
	where
		(isRelative,relativeId)	= case itemLoc of
									LeftOf  id  -> (True,id)
									RightTo id  -> (True,id)
									Above   id  -> (True,id)
									Below   id  -> (True,id)
									_           -> (False,undef)
		isRelativePrev			= case itemLoc of
									LeftOfPrev  -> True
									RightToPrev -> True
									AbovePrev   -> True
									BelowPrev   -> True
									_           -> False

getWindowPos :: !Id !(IOSt .l) -> (!Maybe Vector2,!IOSt .l)
getWindowPos id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	| otherwise
		# (osdInfo,ioState)		= IOStGetOSDInfo ioState
		  di					= getOSDInfoDocumentInterface osdInfo
		  isSDI					= di==SDI
		  isMDI					= di==MDI
		  (framePtr,clientPtr,getParentPos)
		  						= case (getOSDInfoOSInfo osdInfo) of
		  							Just info -> (info.osFrame,info.osClient,if isMDI (OSgetWindowPos info.osClient) (return (0,0)))
							  		nothing   -> (OSNoWindowPtr, OSNoWindowPtr,return (0,0))
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		# (tb,ioState)			= getIOToolbox ioState
		# ((wx,wy),tb)			= OSgetWindowPos (if (isSDI && wids.wPtr==clientPtr) framePtr wids.wPtr) tb
		# ((fx,fy),tb)			= getParentPos tb
		# ioState				= setIOToolbox tb ioState
		# ioState				= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
		= (Just {vx=wx-fx,vy=wy-fy},ioState)


//	Operations that are concerned with the ViewFrame of a window.

moveWindowViewFrame :: !Id Vector2 !(IOSt .l) -> IOSt .l
moveWindowViewFrame id v ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (wsH,ioState)			= accIOToolbox (movewindowviewframe` wMetrics v wsH) ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	movewindowviewframe` :: !OSWindowMetrics !Vector2 !(WindowStateHandle .pst) !*OSToolbox -> (!WindowStateHandle .pst,!*OSToolbox)
	movewindowviewframe` wMetrics v wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		# (wH,tb)			= movewindowviewframe wMetrics v wshIds wH tb
		= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	movewindowviewframe` _ _ _ _
		= StdWindowFatalError "moveWindowViewFrame" "unexpected window placeholder argument"

getWindowViewFrame :: !Id !(IOSt .l) -> (!ViewFrame,!IOSt .l)
getWindowViewFrame id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (zero,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (zero,IOStSetDevice (WindowSystemState windows) ioState)
	| otherwise
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (viewFrame,wsH)		= getwindowviewframe wMetrics wsH
		= (viewFrame,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

//	getwindowviewframe is also used by getWindowOuterSize.
getwindowviewframe :: !OSWindowMetrics !(WindowStateHandle .pst) -> (!ViewFrame,!WindowStateHandle .pst)
getwindowviewframe wMetrics wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH}}
	| wKind==IsWindow
		= (RectToRectangle contentRect,wsH)
	| otherwise
		= (SizeToRectangle wSize,wsH)
where
	wSize		= wH.whSize
	wKind		= wH.whKind
	(origin,domainRect,hasHScroll,hasVScroll)
				= case wH.whWindowInfo of
					WindowInfo info	-> (info.windowOrigin,info.windowDomain,isJust info.windowHScroll,isJust info.windowVScroll)
					other			-> StdWindowFatalError "getWindowViewFrame" "Window has no WindowInfo"
	visScrolls	= OSscrollbarsAreVisible wMetrics domainRect (toTuple wSize) (hasHScroll,hasVScroll)
	contentRect	= getWindowContentRect wMetrics visScrolls (PosSizeToRect origin wSize)
getwindowviewframe _ _
	= StdWindowFatalError "getWindowViewFrame" "unexpected window placeholder argument"

setWindowViewSize :: !Id !Size !(IOSt .l) -> IOSt .l
setWindowViewSize wid reqSize ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID wid) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	# (wMetrics,ioState)		= IOStGetOSWindowMetrics ioState
	  (diffSize,viewSize,frameSize,wsH)
	  							= validateSize wMetrics reqSize wsH
	| not diffSize
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# (activeWIDS,windows)	= getWindowHandlesActiveWindow windows
		# (osdInfo,ioState)		= IOStGetOSDInfo ioState
		  isSDI					= getOSDInfoDocumentInterface osdInfo==SDI
		  (framePtr,clientPtr,tbHeight)
		  						= case (getOSDInfoOSInfo osdInfo) of
		  							Just info -> (info.osFrame,info.osClient,case info.osToolbar of
		  																		Just {toolbarHeight} -> toolbarHeight
		  																		_					 -> 0
		  										 )
		  							_         -> (OSNoWindowPtr,OSNoWindowPtr,0)
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		# (tb,ioState)			= getIOToolbox ioState
	//	# tb					= OSsetWindowSize wids.wPtr (toTuple reqSize) True tb
		# tb					= OSsetWindowViewFrameSize wids.wPtr (toTuple viewSize) tb
		# tb					= (if (isSDI && wids.wPtr==clientPtr)
									(OSsetWindowViewFrameSize framePtr (toTuple {viewSize & h=viewSize.h+tbHeight}))
									id
								  ) tb
		# (wsH,tb)				= windowStateSizeAction wMetrics
									(isJust activeWIDS && (fromJust activeWIDS).wId==wid)
									{wsWIDS=wids,wsSize=frameSize,wsUpdateAll=False} wsH tb
		# ioState				= setIOToolbox tb ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	validateSize :: !OSWindowMetrics !Size !(WindowStateHandle .pst) -> (!Bool,!Size,!Size,!WindowStateHandle .pst)
	validateSize wMetrics reqSize wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
		# (visHScroll,visVScroll)
							= OSscrollbarsAreVisible wMetrics domainRect (toTuple curSize) (hasHScroll,hasVScroll)
		  newW				= if visVScroll (okSize.w+wMetrics.osmVSliderWidth)  okSize.w	// Correct newW in case of visible vertical   scrollbar
		  newH				= if visHScroll (okSize.h+wMetrics.osmHSliderHeight) okSize.h	// Correct newH in case of visible horizontal scrollbar
		= (okSize<>curSize,okSize,{w=newW,h=newH},wsH)
	where
		(minWidth,minHeight)= OSMinWindowSize
		minSize				= {w=minWidth,h=minHeight}
		maxSize				= maxScrollWindowSize
		okSize				= {w=SetBetween reqSize.w minSize.w maxSize.w,h=SetBetween reqSize.h minSize.h maxSize.h}
		curSize				= wH.whSize
	//	windowInfo			= fromJust wH.whWindowInfo	Mike: fromJust changed into getWindowInfoWindowData
		windowInfo			= getWindowInfoWindowData wH.whWindowInfo
		domainRect			= windowInfo.windowDomain
		hasHScroll			= isJust windowInfo.windowHScroll
		hasVScroll			= isJust windowInfo.windowVScroll
	validateSize _ _ _
		= StdWindowFatalError "setWindowViewSize" "unexpected window placeholder argument"

getWindowViewSize :: !Id !(IOSt .l) -> (!Size,!IOSt .l)
getWindowViewSize id ioState
	# (viewFrame,ioState)	= getWindowViewFrame id ioState
	= (rectangleSize viewFrame,ioState)

setWindowOuterSize :: !Id !Size !(IOSt .l) -> IOSt .l
setWindowOuterSize id {w,h} ioState
	# (osdInfo,ioState)	= IOStGetOSDInfo ioState
	  isMDI				= getOSDInfoDocumentInterface osdInfo == MDI
	# ((dw,dh),ioState)	= accIOToolbox (OSstripOuterSize isMDI True) ioState
	= setWindowViewSize id {w=w-dw,h=h-dh} ioState

getWindowOuterSize :: !Id !(IOSt .l) -> (!Size,!IOSt .l)
getWindowOuterSize id ioState
	# (found,wDevice,ioState)		= IOStGetDevice WindowDevice ioState
	| not found
		= (zero,ioState)
	# windows						= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)			= getWindowHandlesWindow (toWID id) windows
	| not found
		= (zero,IOStSetDevice (WindowSystemState windows) ioState)
	# (wMetrics,ioState)			= IOStGetOSWindowMetrics ioState
	# (viewFrame,wsH)				= getwindowviewframe wMetrics wsH
	# viewFrameSize					= rectangleSize viewFrame
	# (wKind,wsH)					= getWindowStateHandleWindowKind wsH
	| wKind==IsDialog
		# windows					= setWindowHandlesWindow wsH windows
		  outerSize					= exactWindowSize undef undef viewFrameSize undef undef wKind
		= (outerSize,IOStSetDevice (WindowSystemState windows) ioState)
	// Mike
	| wKind==IsGameWindow
		# (wInfo,wsH)				= getWindowStateHandleWindowInfo wsH
		  info						= getWindowInfoGameWindowData wInfo
		  windows					= setWindowHandlesWindow wsH windows
		= (info.gamewindowSize,IOStSetDevice (WindowSystemState windows) ioState)
	//
	| otherwise
		# (wInfo,wsH)				= getWindowStateHandleWindowInfo wsH
		  info						= getWindowInfoWindowData wInfo
		  (hasHScroll,hasVScroll)	= (isJust info.windowHScroll,isJust info.windowVScroll)
		  domain					= RectToRectangle info.windowDomain
		# (osdInfo,ioState)			= IOStGetOSDInfo ioState
		  isMDI						= getOSDInfoDocumentInterface osdInfo == MDI
		# ((dw,dh),ioState)			= accIOToolbox (OSstripOuterSize isMDI True) ioState
		  outerSize					= exactWindowSize wMetrics domain viewFrameSize hasHScroll hasVScroll wKind
		  addOSOuterSize			= {w=outerSize.w+dw,h=outerSize.h+dh}
		  windows					= setWindowHandlesWindow wsH windows
		= (addOSOuterSize,IOStSetDevice (WindowSystemState windows) ioState)

setWindowViewDomain :: !Id ViewDomain !(IOSt .l) -> IOSt .l
setWindowViewDomain wId newDomain ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID wId) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (wsH,ioState)			= accIOToolbox (setwindowviewdomain wMetrics newDomain wsH) ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	setwindowviewdomain :: !OSWindowMetrics !ViewDomain !(WindowStateHandle .pst) !*OSToolbox -> (!WindowStateHandle .pst,!*OSToolbox)
	setwindowviewdomain wMetrics newDomain 
						wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH=:{whSelect,whShow,whDefaultId,whSize,whAtts,whWindowInfo,whItems=oldItems}}} tb
		# newDomain						= validateViewDomain newDomain
		  newDomainSize					= rectangleSize newDomain
		  newDomainRect					= RectangleToRect newDomain
		  newOrigin						= {	x = if (w>=newDomainSize.w) newDomainRect.rleft (SetBetween oldOrigin.x newDomainRect.rleft (newDomainRect.rright -w))
		  								  ,	y = if (h>=newDomainSize.h) newDomainRect.rtop  (SetBetween oldOrigin.y newDomainRect.rtop  (newDomainRect.rbottom-h))
		  								  }
		  newVisScrolls					= OSscrollbarsAreVisible wMetrics newDomainRect wSize` hasScrolls
		  newContentRect				= getWindowContentRect wMetrics newVisScrolls (SizeToRect whSize)
		  {rright=w`,rbottom=h`}		= newContentRect
		  osHState						= toOSscrollbarRange (newDomainRect.rleft,newOrigin.x,newDomainRect.rright)  w`
		  osVState						= toOSscrollbarRange (newDomainRect.rtop, newOrigin.y,newDomainRect.rbottom) h`
		# tb							= setwindowslider hasHScroll wMetrics wPtr True  osHState wSize` tb
		# tb							= setwindowslider hasVScroll wMetrics wPtr False osVState wSize` tb
		  windowInfo					= WindowInfo {windowInfo & windowDomain=newDomainRect,windowOrigin=newOrigin}
		  newViewFrameRect				= PosSizeToRect newOrigin {w=w`,h=h`}
		  newViewFrame					= RectToRectangle newViewFrameRect
		  oldViewFrame					= RectToRectangle oldViewFrameRect
		  oldDomainViewMax				= getdomainviewmax oldDomainRect oldViewFrameRect
		  newDomainViewMax				= getdomainviewmax newDomainRect newViewFrameRect
		  updArea						= if (sysLook && oldOrigin==newOrigin && oldDomainViewMax==newDomainViewMax)
		  									[]
		  									[newViewFrame]
		  updState						= {oldFrame=oldViewFrame,newFrame=newViewFrame,updArea=updArea}
		  (noControls,oldItems)			= u_isEmpty oldItems
		| noControls					// window has no controls
			# wH						= {wH & whWindowInfo=windowInfo,whItems=oldItems}
		//	# tb						= OSvalidateWindowRect wPtr (SizeToRect whSize) tb
			| isEmpty updArea			// nothing has to updated
				= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
			// otherwise
				# (wH,tb)				= drawwindowlook wMetrics wPtr id updState wH tb
				= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
		| otherwise						// window has controls
			# hMargins					= getWindowHMargins   IsWindow wMetrics whAtts
			  vMargins					= getWindowVMargins   IsWindow wMetrics whAtts
			  spaces					= getWindowItemSpaces IsWindow wMetrics whAtts
			  reqSize					= {w=w`-fst hMargins-snd hMargins,h=h`-fst vMargins-snd vMargins}
			  (oldItems`,oldItems,tb)	= getWElementHandles` wPtr oldItems tb
			# (_,newItems,tb)			= layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(newDomain,newOrigin)] oldItems tb
			  wH						= {wH & whWindowInfo=windowInfo,whItems=newItems}
			# (wH,tb)					= forceValidWindowClipState wMetrics True wPtr wH tb
			# (updRgn,newItems,tb)		= relayoutControls wMetrics whSelect whShow newContentRect newContentRect zero zero wPtr whDefaultId oldItems` wH.whItems tb
			# (wH,tb)					= drawwindowlook wMetrics wPtr id updState {wH & whItems=newItems} tb
			# (wH,tb)					= updatewindowbackgrounds wMetrics updRgn wshIds wH tb
		//	# tb						= OSvalidateWindowRect wPtr (SizeToRect whSize) tb
			= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	where
		wPtr					= wshIds.wPtr
		wSize`					= toTuple whSize
		(w,h)					= wSize`
		windowInfo				= getWindowInfoWindowData whWindowInfo
		oldDomainRect			= windowInfo.windowDomain
		oldOrigin				= windowInfo.windowOrigin
		sysLook					= windowInfo.windowLook.lookSysUpdate
		hasScrolls				= (isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll)
		(hasHScroll,hasVScroll)	= hasScrolls
		oldVisScrolls			= OSscrollbarsAreVisible wMetrics oldDomainRect wSize` hasScrolls
		oldContentRect			= getWindowContentRect wMetrics oldVisScrolls (SizeToRect whSize)
		oldViewFrameRect		= PosSizeToRect oldOrigin (RectSize oldContentRect)
		(defMinW,defMinH)		= OSMinWindowSize
		minSize					= {w=defMinW,h=defMinH}
		
		setwindowslider :: !Bool OSWindowMetrics OSWindowPtr Bool (Int,Int,Int,Int) (Int,Int) !*OSToolbox -> *OSToolbox
		setwindowslider hasScroll wMetrics wPtr isHorizontal state maxcoords tb
			| hasScroll			= OSsetWindowSlider wMetrics wPtr isHorizontal state maxcoords tb
			| otherwise			= tb
		
		getdomainviewmax :: !Rect !Rect -> Point2
		getdomainviewmax domainRect viewframeRect
			= {x=min domainRect.rright viewframeRect.rright,y=min domainRect.rbottom viewframeRect.rbottom}
	setwindowviewdomain _ _ _ _
		= StdWindowFatalError "setWindowViewDomain" "unexpected window placeholder argument"

getWindowViewDomain :: !Id !(IOSt .l) -> (!Maybe ViewDomain,!IOSt .l)
getWindowViewDomain id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
	| otherwise
		# (wInfo,wsH)			= getWindowStateHandleWindowInfo wsH
		  domain				= RectToRectangle (getWindowInfoWindowData wInfo).windowDomain
		= (Just domain,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)


//	Set and get ScrollFunctions:

setWindowScrollFunction :: !Id Direction ScrollFunction !(IOSt .l) -> IOSt .l
setWindowScrollFunction wId direction scrollFun ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID wId) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# wsH					= setwindowscrollfunction direction scrollFun wsH
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	setwindowscrollfunction :: !Direction ScrollFunction !(WindowStateHandle .pst) -> WindowStateHandle .pst
	setwindowscrollfunction direction scrollFun wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
		| direction==Horizontal && isJust hScroll
			# info			= {windowInfo & windowHScroll=mapMaybe (setScrollFun scrollFun) hScroll}
			# wH			= {wH & whWindowInfo=WindowInfo info}
			= {wsH & wshHandle=Just {wlsH & wlsHandle=wH}}
		| direction==Vertical && isJust vScroll
			# info			= {windowInfo & windowVScroll=mapMaybe (setScrollFun scrollFun) vScroll}
			# wH			= {wH & whWindowInfo=WindowInfo info}
			= {wsH & wshHandle=Just {wlsH & wlsHandle=wH}}
		| otherwise
			= wsH
	where
		windowInfo			= getWindowInfoWindowData wH.whWindowInfo
		hScroll				= windowInfo.windowHScroll
		vScroll				= windowInfo.windowVScroll
		
		setScrollFun :: !ScrollFunction !ScrollInfo -> ScrollInfo
		setScrollFun f scrollInfo
			= {scrollInfo & scrollFunction=f}
	setwindowscrollfunction _ _ _
		= StdWindowFatalError "setWindowScrollFunction" "unexpected window placeholder argument"

getWindowScrollFunction :: !Id Direction !(IOSt .l) -> (!Maybe ScrollFunction,!IOSt .l)
getWindowScrollFunction wId direction ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID wId) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
	| otherwise
		# (maybeScrollFun,wsH)	= getwindowscrollfunction direction wsH
		= (maybeScrollFun,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
	getwindowscrollfunction :: !Direction !(WindowStateHandle .pst) -> *(!Maybe ScrollFunction,!WindowStateHandle .pst)
	getwindowscrollfunction direction wsH=:{wshHandle=Just {wlsHandle=wH=:{whWindowInfo}}}
		| direction==Horizontal && isJust hScroll
			= (fst (accMaybe getScrollFun hScroll),wsH)
		| direction==Vertical && isJust vScroll
			= (fst (accMaybe getScrollFun vScroll),wsH)
		| otherwise
			= (Nothing,wsH)
	where
		windowInfo			= getWindowInfoWindowData whWindowInfo
		hScroll				= windowInfo.windowHScroll
		vScroll				= windowInfo.windowVScroll
		
		getScrollFun :: !ScrollInfo -> *(!ScrollFunction,!ScrollInfo)
		getScrollFun info=:{scrollFunction}
			= (scrollFunction,info)
	getwindowscrollfunction _ _
		= StdWindowFatalError "getWindowScrollFunction" "unexpected window placeholder argument"


//	Operations that are concerned with remaining attributes of windows.

setWindowTitle :: !Id Title !(IOSt .l) -> IOSt .l
setWindowTitle id title ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	| otherwise
		# (osdInfo,ioState)		= IOStGetOSDInfo ioState
		  isSDI					= getOSDInfoDocumentInterface osdInfo==SDI
		  (framePtr,clientPtr)	= case (getOSDInfoOSInfo osdInfo) of
									Just info -> (info.osFrame, info.osClient)
									_         -> (OSNoWindowPtr,OSNoWindowPtr)
		  (wids,wsH)			= getWindowStateHandleWIDS wsH
		  wsH					= setWindowStateHandleWindowTitle title wsH
	//	# ioState				= appIOToolbox (OSsetWindowTitle (if (isSDI && wids.wPtr==clientPtr) framePtr wids.wPtr) title) ioState
		# ioState				= appIOToolbox (OSsetWindowTitle (if isSDI framePtr wids.wPtr) title) ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState

setWindowOk :: !Id Id !(IOSt .l) -> IOSt .l
setWindowOk id okId ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	| otherwise
		# (wsH,ioState)			= accIOToolbox (setwindowok okId wsH) ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	setwindowok :: !Id !(WindowStateHandle .pst) !*OSToolbox -> (!WindowStateHandle .pst,!*OSToolbox)
	setwindowok okId wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		= (wsH,tb)			// PA: Be aware to invalidate appropriate window/compound ClipStates
	setwindowok _ _ _
		= StdWindowFatalError "setWindowOk" "unexpected window placeholder argument"

setWindowCancel :: !Id Id !(IOSt .l) -> IOSt .l
setWindowCancel id cancelId ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	| otherwise
		# (wsH,ioState)			= accIOToolbox (setwindowcancel cancelId wsH) ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	setwindowcancel :: !Id !(WindowStateHandle .pst) !*OSToolbox -> (!WindowStateHandle .pst,!*OSToolbox)
	setwindowcancel cancelId wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		= (wsH,tb)
	setwindowcancel _ _ _
		= StdWindowFatalError "setWindowCancel" "unexpected window placeholder argument"

setWindowCursor :: !Id CursorShape !(IOSt .l) -> IOSt .l
setWindowCursor id shape ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	| otherwise
		# (wsH,ioState)			= accIOToolbox (setwindowcursor shape wsH) ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	setwindowcursor :: !CursorShape !(WindowStateHandle .pst) !*OSToolbox -> (!WindowStateHandle .pst,!*OSToolbox)
	setwindowcursor shape wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		# tb				= OSsetWindowCursor wPtr (toCursorCode shape) tb
		  cursorAtt			= WindowCursor shape
		  (replaced,atts)	= Replace isWindowCursor cursorAtt wH.whAtts
		  atts				= if replaced atts [cursorAtt:atts]
		= ({wsH & wshHandle=Just {wlsH & wlsHandle={wH & whAtts=atts}}},tb)
	setwindowcursor _ _ _
		= StdWindowFatalError "setWindowCursor" "unexpected window placeholder argument"

getWindowTitle :: !Id !(IOSt .l) -> (!Maybe Title,!IOSt .l)
getWindowTitle id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	| otherwise
		# (title,wsH)			= getWindowStateHandleWindowTitle wsH
		= (Just title,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowOk :: !Id !(IOSt .l) -> (!Maybe Id,!IOSt .l)
getWindowOk id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	| otherwise
		# (okId,wsH)			= getWindowStateHandleDefaultId wsH
		= (okId,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowCancel :: !Id !(IOSt .l) -> (!Maybe Id,!IOSt .l)
getWindowCancel id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	| otherwise
		# (cancelId,wsH)		= getWindowStateHandleCancelId wsH
		= (cancelId,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowCursor :: !Id !(IOSt .l) -> (!Maybe CursorShape,!IOSt .l)
getWindowCursor id ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	| otherwise
		# (shape,wsH)			= getcursor wsH
		= (shape,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
	getcursor :: !(WindowStateHandle .pst) -> (!Maybe CursorShape,!WindowStateHandle .pst)
	getcursor wsH=:{wshHandle=Just wlsH=:{wlsHandle={whAtts}}}
		= (if hasCursorAtt (Just (getWindowCursorAtt cursorAtt)) Nothing,wsH)
	where
		(hasCursorAtt,cursorAtt)	= Select isWindowCursor undef whAtts
	getcursor _
		= StdWindowFatalError "getWindowCursor" "unexpected window placeholder argument"
