implementation module StdWindow


//	Clean Object I/O library, version 1.2


import	StdBool, StdEnum, StdFunc, StdList, StdMisc, StdTuple
import	ostypes, ossystem, oswindow
from	ostoolbox		import WorldGetToolbox, WorldSetToolbox
import	StdControlClass, StdMaybe
from	StdId			import getParentId
from	StdPSt			import appPIO, accPIO
from	StdSystem		import maxScrollWindowSize
import	commondef, controlpos, deviceevents, iostate, windowaccess, windowcontrols, windowcreate, windowdefaccess, windowdevice, wstate
from	controlinternal	import openpopupitems, closepopupitems, enablecontrols, disablecontrols
from	controllayout	import layoutControls
from	controlrelayout	import relayoutControls
from	controlvalidate	import getWElementControlIds, disjointControlIds
from	keyfocus		import getCurrentFocusItem, setNewFocusItem
from	receiverid		import unbindRIds
from	windowclipstate	import validateWindowClipState, forceValidWindowClipState
from	windowdispose	import disposeWindow
from	windowdraw		import drawinwindow, drawwindowlook
from	windowupdate	import updatewindow, updatewindowbackgrounds
from	windowvalidate	import validateWindowId, validateViewDomain, exactWindowPos, exactWindowSize


//	General functions:

StdWindowFatalError :: String String -> .x
StdWindowFatalError function error
	= FatalError function "StdWindow" error

/*	getParentWindowId controlId returns the Id of the parent window/dialog if this
	exists and belongs to the same interactive process. 
*/
getParentWindowId :: !Id !(IOSt .l) -> (!Maybe Id,!IOSt .l)
getParentWindowId controlId ioState
	# (it,ioState)		= IOStGetIdTable ioState
	  maybeParent		= getIdParent controlId it
	| isNothing maybeParent
		= (Nothing,ioState)
	# parent			= fromJust maybeParent
	| parent.idpDevice<>WindowDevice
		= (Nothing,ioState)
	# (pid,ioState)		= IOStGetIOId ioState
	| parent.idpIOId<>pid
		= (Nothing,ioState)
	| otherwise
		= (Just parent.idpId,ioState)

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
		  it					= if ok (snd (addIdToIdTable okId {idpIOId=ioId,idpDevice=WindowDevice,idpId=okId} it)) it
		# ioState				= IOStSetIdTable it ioState
		# ioState				= IOStSetReceiverTable rt ioState
		# pState				= {pState & io=ioState}
		| not ok
			= (ErrorIdsInUse,pState)
		| otherwise
			# wH				= initWindowHandle title Modeless IsWindow (WindowInfo undef) itemHs atts
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
		  it					= if ok (snd (addIdToIdTable okId {idpIOId=ioId,idpDevice=WindowDevice,idpId=okId} it)) it
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
		  it					= if ok (snd (addIdToIdTable okId {idpIOId=ioId,idpDevice=WindowDevice,idpId=okId} it)) it
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


getWindowIdAttribute :: ![WindowAttribute .pst] -> Maybe Id
getWindowIdAttribute atts
	# (hasIdAtt,idAtt)	= Select isWindowId undef atts
	| hasIdAtt			= Just (getWindowIdAtt idAtt)
	| otherwise			= Nothing


/*	controlIdsAreConsistent checks whether the WElementHandles contain (R(2))Ids that have already been
	associated with open receivers or other I/O objects and if there are no duplicate Ids. 
	The ReceiverTable is not changed if there are duplicate (R(2))Ids; otherwise all (R(2))Ids have been bound.
*/
controlIdsAreConsistent :: !SystemId !Id ![WElementHandle .ls .pst] !ReceiverTable !IdTable
							   -> (!Bool,![WElementHandle .ls .pst],!ReceiverTable,!IdTable)
controlIdsAreConsistent ioId wId itemHs rt it
	# (ids,itemHs)	= getWElementControlIds itemHs
	| not (okMembersIdTable ids it)
		= (False,itemHs,rt,it)
	# (ok,it)		= addIdsToIdTable (map (\id->(id,{idpIOId=ioId,idpDevice=WindowDevice,idpId=wId})) ids) it
	  (itemHs,rt)	= bindReceiverControlIds ioId wId itemHs rt
	| not ok
		= StdWindowFatalError "controlIdsAreConsistent" "could not add all Ids to IdTable"
	| otherwise
		= (True,itemHs,rt,it)


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


/*	closeControls closes the controls in the indicated window.
*/
closeControls :: !Id [Id] !Bool !(IOSt .l) -> IOSt .l
closeControls wId ids relayout ioState
	# (found,wDevice,ioState)					= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# wHs										= WindowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)							= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= IOStSetDevice (WindowSystemState wHs) ioState
    // Mike //
    # (wKind,wsH)								= getWindowStateHandleWindowKind wsH
    | wKind==IsGameWindow
    	= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
    ///
	| otherwise
		# (wMetrics,ioState)					= IOStGetOSWindowMetrics ioState
		# (tb,ioState)							= getIOToolbox ioState
		# (freeRIds,freeIds,disposeFun,wsH,tb)	= closecontrols wMetrics ids relayout wsH tb
		# ioState								= setIOToolbox tb ioState
		# ioState								= unbindRIds freeRIds ioState
		# (idtable,ioState)						= IOStGetIdTable ioState
		  (_,idtable)							= removeIdsFromIdTable (freeRIds++freeIds) idtable
		# ioState								= IOStSetIdTable idtable ioState
		# (f,ioState)							= IOStGetInitIO ioState
		# ioState								= IOStSetInitIO ((appPIO (appIOToolbox disposeFun)) o f) ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState


/*	closeAllControls closes all controls from the indicated window.
*/
closeAllControls :: !Id !(IOSt .l) -> IOSt .l
closeAllControls wId ioState
	# (found,wDevice,ioState)					= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# wHs										= WindowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)							= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= IOStSetDevice (WindowSystemState wHs) ioState
	| otherwise
		# (tb,ioState)							= getIOToolbox ioState
		# (freeRIds,freeIds,disposeFun,wsH,tb)	= closeallcontrols wsH tb
		# ioState								= setIOToolbox tb ioState
		# ioState								= unbindRIds freeRIds ioState
		# (idtable,ioState)						= IOStGetIdTable ioState
		  (_,idtable)							= removeIdsFromIdTable (freeRIds++freeIds) idtable
		# ioState								= IOStSetIdTable idtable ioState
		# (f,ioState)							= IOStGetInitIO ioState
		# ioState								= IOStSetInitIO ((appPIO (appIOToolbox disposeFun)) o f) ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState


/*	closePopUpControlItems closes items from the indicated PopUpControl in the indicated window/dialogue.
*/
closePopUpControlItems :: !Id ![Index] !(IOSt .l) -> IOSt .l
closePopUpControlItems popUpId indexs ioState
	| isEmpty indexs
		= ioState
	# (maybeId,ioState)			= getParentWindowId popUpId ioState
	| isNothing maybeId
		= ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# wHs						= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,wHs)			= getWindowHandlesWindow (toWID (fromJust maybeId)) wHs
	| not found
		= IOStSetDevice (WindowSystemState wHs) ioState
	| otherwise
		# (tb,ioState)			= getIOToolbox ioState
		# (wsH,tb)				= closepopupcontrolitems popUpId indexs wsH tb
		# ioState				= setIOToolbox tb ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
where
	closepopupcontrolitems :: !Id ![Index] !(WindowStateHandle (PSt .l)) !*OSToolbox
										-> (!WindowStateHandle (PSt .l), !*OSToolbox)
	closepopupcontrolitems popUpId indexs wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		# (wH,tb)		= closepopupitems popUpId indexs wPtr wH tb
		= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	closepopupcontrolitems _ _ _ _
		= StdWindowFatalError "closepopupcontrolitems" "unexpected window placeholder argument"


/*	getWindowStateHandleIds returns all Ids of the controls in this window.
	This function is used by open(Compound)Controls.
*/
getWindowStateHandleIds :: !(WindowStateHandle .pst) -> (![Id],!WindowStateHandle .pst)
getWindowStateHandleIds wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems}}}
	# (ids,itemHs)	= getWElementControlIds whItems
	= (ids,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
getWindowStateHandleIds _
	= StdWindowFatalError "getWindowStateHandleIds" "unexpected window placeholder argument"

/*	openControls adds controls to the indicated window.
*/
openControls :: !Id .ls (cdef .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | Controls cdef
openControls wId ls newControls pState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice pState.io
	| not found
		= (ErrorUnknownObject,{pState & io=ioState})
	# wHs						= WindowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)			= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= (ErrorUnknownObject,{pState & io=IOStSetDevice (WindowSystemState wHs) ioState})
    // Mike //
    # (wKind,wsH)				= getWindowStateHandleWindowKind wsH
    | wKind==IsGameWindow
    	= (OtherError "WrongObject",{pState & io=IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState})
    ///
	# (cs,pState)				= controlToHandles newControls {pState & io=ioState}
	# newItemHs					= map ControlStateToWElementHandle cs
	  (currentIds,wsH)			= getWindowStateHandleIds wsH
	  (disjoint,newItemHs)		= disjointControlIds currentIds newItemHs
	| not disjoint
		= (ErrorIdsInUse,appPIO (IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs))) pState)
	# (rt,ioState)				= IOStGetReceiverTable pState.io
	# (it,ioState)				= IOStGetIdTable ioState
	# (ioId,ioState)			= IOStGetIOId ioState
	  (ok,newItemHs,rt,it)		= controlIdsAreConsistent ioId wId newItemHs rt it
	# ioState					= IOStSetIdTable it ioState
	# ioState					= IOStSetReceiverTable rt ioState
	| not ok
		# ioState				= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
		# pState				= {pState & io=ioState}
		= (ErrorIdsInUse,pState)
	| otherwise
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (wsH,ioState)			= accIOToolbox (opencontrols wMetrics ls newItemHs wsH) ioState
		# ioState				= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
		# pState				= {pState & io=ioState}
		= (NoError,pState)

/*	openCompoundControls adds controls to the indicated CompoundControl of the indicated window.
*/
openCompoundControls :: !Id .ls (cdef .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | Controls cdef
openCompoundControls cId ls newControls pState=:{io=ioState}
	# (maybeId,ioState)			= getParentWindowId cId ioState
	| isNothing maybeId
		= (ErrorUnknownObject,{pState & io=ioState})
	# wId						= fromJust maybeId
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (ErrorUnknownObject,{pState & io=ioState})
	# wHs						= WindowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)			= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= (ErrorUnknownObject,{pState & io=IOStSetDevice (WindowSystemState wHs) ioState})
    // Mike //
    # (wKind,wsH)				= getWindowStateHandleWindowKind wsH
    | wKind==IsGameWindow
		= (OtherError "WrongObject",{pState & io=IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState})
    ///
	# (cs,pState)				= controlToHandles newControls {pState & io=ioState}
	# newItemHs					= map ControlStateToWElementHandle cs
	  (currentIds,wsH)			= getWindowStateHandleIds wsH
	  (disjoint,newItemHs)		= disjointControlIds currentIds newItemHs
	| not disjoint
		= (ErrorIdsInUse,appPIO (IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs))) pState)
	# (rt,ioState)				= IOStGetReceiverTable pState.io
	# (it,ioState)				= IOStGetIdTable ioState
	# (ioId,ioState)			= IOStGetIOId ioState
	  (ok,newItemHs,rt,it)		= controlIdsAreConsistent ioId wId newItemHs rt it
	# ioState					= IOStSetIdTable it ioState
	# ioState					= IOStSetReceiverTable rt ioState
	| not ok
		# ioState				= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
		# pState				= {pState & io=ioState}
		= (ErrorIdsInUse,pState)
	| otherwise
		# (osdInfo, ioState)	= IOStGetOSDInfo ioState
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		# (ok,wsH,tb)			= opencompoundcontrols osdInfo wMetrics cId ls newItemHs wsH tb
		# ioState				= setIOToolbox tb ioState
		# ioState				= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
		# pState				= {pState & io=ioState}
		= (if ok NoError ErrorUnknownObject,pState)


/*	openPopUpControlItems opens items to the PopUpControl of the indicated window/dialogue.
*/
openPopUpControlItems :: !Id !Index ![PopUpControlItem (PSt .l)] !(IOSt .l) -> IOSt .l
openPopUpControlItems popUpId index items ioState
	| isEmpty items
		= ioState
	# (maybeId,ioState)			= getParentWindowId popUpId ioState
	| isNothing maybeId
		= ioState
	# wId						= fromJust maybeId
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# wHs						= WindowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)			= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= IOStSetDevice (WindowSystemState wHs) ioState
    // Mike //
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind==IsGameWindow
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
    ///
	| otherwise
		# (tb,ioState)			= getIOToolbox ioState
		# (wsH,tb)				= openpopupcontrolitems popUpId index items wsH tb
		# ioState				= setIOToolbox tb ioState
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
where
	openpopupcontrolitems :: !Id !Index ![PopUpControlItem (PSt .l)] !(WindowStateHandle (PSt .l)) !*OSToolbox
																	 -> (!WindowStateHandle (PSt .l), !*OSToolbox)
	openpopupcontrolitems popUpId index items wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		# (wH,tb)		= openpopupitems popUpId index items wPtr wH tb
		= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	openpopupcontrolitems _ _ _ _ _
		= StdWindowFatalError "openPopUpControlItems" "unexpected window placeholder argument"


/*	setControlPos changes the position of the indicated control.
*/
setControlPos :: !Id ![(Id,ItemPos)] !(IOSt .l) -> (!Bool,!IOSt .l)
setControlPos wId newPoss ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (False,ioState)
	# wHs						= WindowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)			= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= (False,IOStSetDevice (WindowSystemState wHs) ioState)
	// Mike //
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind==IsGameWindow
		= (False,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState)
	///
	| otherwise
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		# (ok,wsH,tb)			= setcontrolpositions wMetrics newPoss wsH tb
		# ioState				= setIOToolbox tb ioState
		  wHs					= setWindowHandlesWindow wsH wHs
		= (ok,IOStSetDevice (WindowSystemState wHs) ioState)


/*	controlSize calculates the size of the given control.
*/
controlSize :: !(cdef .ls (PSt .l)) !Bool !(Maybe (Int,Int)) !(Maybe (Int,Int)) !(Maybe (Int,Int)) !(PSt .l)
			-> (!Size,!PSt .l) | Controls cdef
controlSize cdef isWindow hMargins vMargins itemSpaces pState
	# (cs,pState)		= controlToHandles cdef pState
	  itemHs			= map ControlStateToWElementHandle cs
	# (tb,ioState)		= getIOToolbox pState.io
	# (wMetrics,ioState)= IOStGetOSWindowMetrics ioState
	  hMargins			= case hMargins of
		  					(Just (left,right))	-> (max 0 left,max 0 right)
		  					_					-> if isWindow (0,0) (wMetrics.osmHorMargin,wMetrics.osmHorMargin)
	  vMargins			= case vMargins of
		  					(Just (top,bottom))	-> (max 0 top,max 0 bottom)
		  					_					-> if isWindow (0,0) (wMetrics.osmVerMargin,wMetrics.osmVerMargin)
	  itemSpaces		= case itemSpaces of
		  					(Just (hor,vert))	-> (max 0 hor,max 0 vert)
		  					_					-> (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)
	  domain			= {viewDomainRange & corner1=zero}
	# (derSize,_,tb)	= layoutControls wMetrics hMargins vMargins itemSpaces zero zero [(domain,zero)] itemHs tb
	# ioState			= setIOToolbox tb ioState
	# pState			= {pState & io=ioState}
	= (derSize,pState)


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
	# wHs						= windows.whsWindows
	  (modal,modeless)			= Uspan ismodalwindow wHs
	  (isModal,modal)			= UContains (identifyWindowStateHandle wid) modal
	| isModal					// Modal windows should be activated
		= {pState & io=IOStSetDevice (WindowSystemState {windows & whsWindows=modal++modeless}) ioState}
	# (osdInfo,ioState)			= IOStGetOSDInfo ioState
	  isSDI						= getOSDInfoDocumentInterface osdInfo==SDI
	  (framePtr,clientPtr)		= case (getOSDInfoOSInfo osdInfo) of
	  								Just info -> (info.osFrame,info.osClient)
	  								_         -> (OSNoWindowPtr,OSNoWindowPtr)
	| isEmpty modal				// There are no modal windows, so put activated window in front
		# (_,wsH,others)		= URemove (identifyWindowStateHandle wid) undef modeless
		  (shown,wsH)			= getWindowStateHandleShow wsH
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		  activatePtr			= if (isSDI && wids.wPtr==clientPtr) framePtr wids.wPtr		// Do not activate SDI client, but SDI frame
		  showAction			= if shown id (snd o OSshowWindow activatePtr True)
		# ioState				= IOStSetDevice (WindowSystemState {windows & whsWindows=[wsH:others]}) ioState
		# (delayinfo,ioState)	= accIOToolbox (OSactivateWindow osdInfo activatePtr o showAction) ioState
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
		# ioState				= appIOToolbox (showAction o OSstackWindow activatePtr modalWIDS.wPtr) ioState
		= {pState & io=ioState}
where
	wid							= toWID wId
	
	ismodalwindow :: !(WindowStateHandle .pst) -> (!Bool,!WindowStateHandle .pst)
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
	  maybeItemNr				= getCurrentFocusItem keyfocus
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
stackWindow :: !Id !Id !(IOSt .l) -> IOSt .l
stackWindow windowId behindId ioState
	| windowId==behindId	// Don't stack a window behind itself
		= ioState
	# (found,wDevice,ioState)		= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows						= WindowSystemStateGetWindowHandles wDevice
	# (hasBehind,windows)			= hasWindowHandlesWindow (toWID behindId) windows
	| not hasBehind			// Behind window does not exist
		= IOStSetDevice (WindowSystemState windows) ioState
	# (hasWindow,wsH,windows)		= getWindowHandlesWindow (toWID windowId) windows
	| not hasWindow			// Stack window does not exist
		= IOStSetDevice (WindowSystemState windows) ioState
	# (mode,wsH)					= getWindowStateHandleWindowMode wsH
	| mode==Modal			// Stack window is modal, skip
		= IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# (_,_,windows)				= removeWindowHandlesWindow (toWID windowId) windows		// remove placeholder window
		# (wids,wsH)				= getWindowStateHandleWIDS wsH
		# (osdInfo,ioState)			= IOStGetOSDInfo ioState
		  isSDI						= getOSDInfoDocumentInterface osdInfo==SDI
		  (framePtr,clientPtr)		= case (getOSDInfoOSInfo osdInfo) of
	  									Just info -> (info.osFrame,info.osClient)
	  									_         -> (OSNoWindowPtr,OSNoWindowPtr)
		  wPtr						= if (isSDI && wids.wPtr==clientPtr) framePtr wids.wPtr
		# (tb,ioState)				= getIOToolbox ioState
		# (windows,tb)				= stackwindows wsH wPtr behindId windows tb
		# ioState					= setIOToolbox tb ioState
		= IOStSetDevice (WindowSystemState windows) ioState
where
/*	stackwindows stackwindow stackptr behindId
		places stackwindow behind the window identified by behindId.
*/
	stackwindows :: !(WindowStateHandle .pst) !OSWindowPtr !Id !(WindowHandles .pst) !*OSToolbox
															-> (!WindowHandles .pst, !*OSToolbox)
	stackwindows wsH wPtr behindId windows=:{whsWindows=wsHs} tb
		# (wsHs,tb)		= stackBehind wsH wPtr behindId wsHs tb
		= ({windows & whsWindows=wsHs},tb)
	where
		stackBehind :: !(WindowStateHandle .pst) !OSWindowPtr !Id ![WindowStateHandle .pst] !*OSToolbox
															  -> (![WindowStateHandle .pst],!*OSToolbox)
		stackBehind wsH wPtr behindId [wsH`:wsHs] tb
			# (wids`,wsH`)	= getWindowStateHandleWIDS wsH`
			| behindId<>wids`.wId
				# (wsHs,tb) = stackBehind wsH wPtr behindId wsHs tb
				= ([wsH`:wsHs],tb)
			# (mode`,wsH`)	= getWindowStateHandleWindowMode wsH`
			| mode`==Modal
				# (wsHs,tb)	= stackBehindLastModal wsH wPtr wids`.wPtr wsHs tb
				= ([wsH`:wsHs],tb)
				with
					stackBehindLastModal :: !(WindowStateHandle .pst) !OSWindowPtr !OSWindowPtr ![WindowStateHandle .pst] !*OSToolbox
																							-> (![WindowStateHandle .pst],!*OSToolbox)
					stackBehindLastModal wsH wPtr behindPtr [wsH`:wsHs] tb
						# (wids`,wsH`)	= getWindowStateHandleWIDS wsH`
						# (mode`,wsH`)	= getWindowStateHandleWindowMode wsH`
						| mode`==Modal
							# (wsHs,tb) = stackBehindLastModal wsH wPtr wids`.wPtr wsHs tb
							= ([wsH`:wsHs],tb)
						| otherwise
							= ([wsH,wsH`:wsHs],OSstackWindow wPtr behindPtr tb)
					stackBehindLastModal wsH wPtr behindPtr _ tb
						= ([wsH],OSstackWindow wPtr behindPtr tb)
			| otherwise
				= ([wsH`,wsH:wsHs],OSstackWindow wPtr wids`.wPtr tb)
		stackBehind _ _ _ _ _
			= StdWindowFatalError "stackBehind" "this alternative should not be reached"

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
		# ({osmHorMargin},ioState)	= IOStGetOSWindowMetrics ioState
		# (marginAtt,wsH)			= gethmargin osmHorMargin wsH
		= (Just marginAtt,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
	gethmargin :: Int !(WindowStateHandle .pst) -> ((Int,Int),!WindowStateHandle .pst)
	gethmargin defHMargin wsH=:{wshHandle=Just {wlsHandle={whAtts}}}
		= (getWindowHMarginAtt (snd (Select isWindowHMargin (WindowHMargin defHMargin defHMargin) whAtts)),wsH)
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
		# ({osmVerMargin},ioState)	= IOStGetOSWindowMetrics ioState
		# (marginAtt,wsH)			= getvmargin osmVerMargin wsH
		= (Just marginAtt,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
	getvmargin :: Int !(WindowStateHandle .pst) -> ((Int,Int),!WindowStateHandle .pst)
	getvmargin defVMargin wsH=:{wshHandle=Just {wlsHandle={whAtts}}}
		= (getWindowVMarginAtt (snd (Select isWindowVMargin (WindowVMargin defVMargin defVMargin) whAtts)),wsH)
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
		/* Mike
		  (maybeWindowInfo,wsH)	= getWindowStateHandleWindowInfo wsH
		  scrollInfo			= case maybeWindowInfo of
		  							Nothing		-> (False,False)
		  							Just info	-> (isJust info.windowHScroll,isJust info.windowVScroll)
		*/
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
	updateWindowBackground wMetrics maybeViewFrame wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		| IsEmptyRect updArea
			= (wsH,tb)
		| otherwise
			# (wH,tb)					= updatewindow wMetrics updInfo wH tb
			= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	where
		info							= getWindowInfoWindowData wH.whWindowInfo
		(origin,domainRect,hasScrolls)	= (info.windowOrigin,info.windowDomain,(isJust info.windowHScroll,isJust info.windowVScroll))
		visScrolls						= OSscrollbarsAreVisible wMetrics domainRect (toTuple wH.whSize) hasScrolls
		contentRect						= getWindowContentRect wMetrics visScrolls (SizeToRect wH.whSize)
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
	//	  {lookFun,lookSysUpdate}	= (fromJust windowInfo).windowLook	Mike: fromJust changed into getWindowInfoWindowData
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
	setwindowviewdomain wMetrics domain wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		# domain				= validateViewDomain domain
		  domainSize			= rectangleSize domain
		  domainRect			= RectangleToRect domain
		  newOrigin				= {	x = if (w>=domainSize.w) domain.corner1.x (SetBetween oldOrigin.x domain.corner1.x (domain.corner2.x-w))
		  						  ,	y = if (h>=domainSize.h) domain.corner1.y (SetBetween oldOrigin.y domain.corner1.y (domain.corner2.y-h))
		  						  }
		  visScrolls			= OSscrollbarsAreVisible wMetrics domainRect (toTuple wSize) (hasHScroll,hasVScroll)
		  wFrame				= getWindowContentRect wMetrics visScrolls (SizeToRect wSize)
		  {rright=w`,rbottom=h`}= wFrame
		  osHState				= toOSscrollbarRange (domain.corner1.x,newOrigin.x,domain.corner2.x) w`
		  osVState				= toOSscrollbarRange (domain.corner1.y,newOrigin.y,domain.corner2.y) h`
		# tb					= setwindowslider hasHScroll wMetrics wPtr True  osHState (toTuple wSize) tb
		# tb					= setwindowslider hasVScroll wMetrics wPtr False osVState (toTuple wSize) tb
	//	  windowInfo			= Just {windowInfo & windowDomain=domainRect,windowOrigin=newOrigin}	Mike: Just changed to WindowInfo
		  windowInfo			= WindowInfo {windowInfo & windowDomain=domainRect,windowOrigin=newOrigin}
		  (defHSpace,defVSpace)	= (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)
		  hMargins				= getWindowHMarginAtt   (snd (Select isWindowHMargin   (WindowHMargin 0 0) atts))
		  vMargins				= getWindowVMarginAtt   (snd (Select isWindowVMargin   (WindowVMargin 0 0) atts))
		  spaces				= getWindowItemSpaceAtt (snd (Select isWindowItemSpace (WindowItemSpace defHSpace defVSpace) atts))
		  reqSize				= {w=w`-fst hMargins-snd hMargins,h=h`-fst vMargins-snd vMargins}
		# (_,newItems,tb)		= layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(domain,newOrigin)] oldItems tb
		  wH					= {wH & whWindowInfo=windowInfo,whItems=newItems}
		# (wH,tb)				= forceValidWindowClipState wMetrics True wPtr wH tb
		# (updRgn,tb)			= relayoutControls wMetrics whSelect wFrame wFrame zero zero wPtr whDefaultId oldItems wH.whItems tb
		  viewFrame				= PosSizeToRectangle newOrigin {w=w`,h=h`}
		  updState				= RectangleToUpdateState viewFrame
		# (wH,tb)				= drawwindowlook wMetrics wPtr id updState wH tb
		# (wH,tb)				= updatewindowbackgrounds wMetrics updRgn wshIds wH tb
		# tb					= OSvalidateWindowRect wPtr (SizeToRect wSize) tb
		= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	where
		wPtr					= wshIds.wPtr
		atts					= wH.whAtts
		wSize					= wH.whSize
		(w,h)					= toTuple wSize
		whSelect				= wH.whSelect
		whDefaultId				= wH.whDefaultId
	//	windowInfo				= fromJust wH.whWindowInfo	Mike: fromJust changed to getWindowInfoWindowData
		windowInfo				= getWindowInfoWindowData wH.whWindowInfo
		oldOrigin				= windowInfo.windowOrigin
		oldItems				= wH.whItems
		(hasHScroll,hasVScroll)	= (isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll)
		(defMinW,defMinH)		= OSMinWindowSize
		minSize					= {w=defMinW,h=defMinH}
		
		setwindowslider :: !Bool OSWindowMetrics OSWindowPtr Bool (Int,Int,Int,Int) (Int,Int) !*OSToolbox -> *OSToolbox
		setwindowslider hasScroll wMetrics wPtr isHorizontal state maxcoords tb
			| hasScroll			= OSsetWindowSlider wMetrics wPtr isHorizontal state maxcoords tb
			| otherwise			= tb
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
	//	  domain				= RectToRectangle (fromJust wInfo).windowDomain	Mike: fromJust changed into getWindowInfoWindowData
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
	getwindowscrollfunction :: !Direction !(WindowStateHandle .pst) -> (!Maybe ScrollFunction,!WindowStateHandle .pst)
	getwindowscrollfunction direction wsH=:{wshHandle=Just {wlsHandle=wH}}
		| direction==Horizontal && isJust hScroll
			= (fst (accMaybe getScrollFun hScroll),wsH)
		| direction==Vertical && isJust vScroll
			= (fst (accMaybe getScrollFun vScroll),wsH)
		| otherwise
			= (Nothing,wsH)
	where
		windowInfo			= getWindowInfoWindowData wH.whWindowInfo
		hScroll				= windowInfo.windowHScroll
		vScroll				= windowInfo.windowVScroll
		
		getScrollFun :: !ScrollInfo -> (!ScrollFunction,!ScrollInfo)
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
		# ioState				= appIOToolbox (OSsetWindowTitle (if (isSDI && wids.wPtr==clientPtr) framePtr wids.wPtr) title) ioState
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
	# windows				= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)	= getWindowHandlesWindow (toWID id) windows
	| not found
		= (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
	| otherwise
		# (cancelId,wsH)	= getWindowStateHandleCancelId wsH
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
