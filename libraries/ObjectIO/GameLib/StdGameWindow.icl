implementation module StdWindow


//  Clean Object I/O library, version 1.0.1

import  StdBool, StdEnum, StdList, StdMisc, StdTuple
import  ospicture, oswindow
import  StdControlClass, StdMaybe, StdPSt, StdSystem
import  commondef, controlinternal, controllayout, controlpos, controlvalidate, iostate, receiverid,
        windowaccess, windowclipstate, windowcontrols, windowcreate, windowdefaccess, windowdevice, windowdispose, windowdraw, windowupdate, windowvalidate


StdWindowFatalError :: String String -> .x
StdWindowFatalError function error
    = FatalError function "StdWindow" error

//  Use these two macros to identify windows and dialogues.
windowtype  :== "Window"
dialogtype  :== "Dialog"

class Windows wdef
where
    openWindow      :: .ls !(wdef .ls (PSt .l .p)) !(PSt .l .p) -> (!ErrorReport,!PSt .l .p)
    getWindowType   ::      (wdef .ls .ps)                      -> WindowType

class Dialogs wdef
where
    openDialog      :: .ls !(wdef .ls (PSt .l .p)) !(PSt .l .p) -> (!ErrorReport,!PSt .l .p)
    openModalDialog :: .ls !(wdef .ls (PSt .l .p)) !(PSt .l .p) -> (!ErrorReport,!PSt .l .p)
    getDialogType   ::      (wdef .ls .ps)                      -> WindowType


// Mike //
OpenGameWindow :: !Id !Size !Int !Bool !(PSt .l .p) -> (!ErrorReport, !PSt .l .p)
OpenGameWindow id gamewindowsize bitsperpixel fullscreen pState
    # (isZero, pState)      = accPIO checkZeroWindowBound pState
    | isZero
    = (ErrorViolateDI, pState)
    # maybe_id              = Just id
    # (validId, okId, ioState) = validateWindowId maybe_id pState.io
    | not validId
    = (ErrorIdsInUse, {pState & io=ioState})
    # pState                = {pState & io=ioState}
    # wH                    = initWindowHandle "" Modeless IsGameWindow []
                                    [WindowId id,
                                     WindowSize gamewindowsize,
                                     WindowColorDepth bitsperpixel,
                                     WindowFullScreen fullscreen]
    = (NoError, openwindow okId {wlsState=undef, wlsHandle=wH} pState)
///



instance Windows (Window c) | Controls c where
    openWindow :: .ls !(Window c .ls (PSt .l .p)) !(PSt .l .p) -> (!ErrorReport,!PSt .l .p) | Controls c
    openWindow ls (Window title controls atts) pState
        # (isZero,pState)       = accPIO checkZeroWindowBound pState
        | isZero
        = (ErrorViolateDI,pState)
        # maybe_id              = getWindowIdAttribute atts
        # (validId,okId,ioState)= validateWindowId maybe_id pState.io
        | not validId
        = (ErrorIdsInUse,{pState & io=ioState})
        # cStates               = controlToHandles controls
        # (rt,ioState)          = IOStGetReceiverTable ioState
        # (ioId,ioState)        = IOStGetIOId ioState
        # itemHs                = map ControlStateToWElementHandle cStates
        # (ok,itemHs,rt)        = controlIdsAreConsistent ioId okId itemHs rt
        # ioState               = IOStSetReceiverTable rt ioState
        # pState                = {pState & io=ioState}
        | not ok
        = (ErrorIdsInUse,pState)
        # wH                    = initWindowHandle title Modeless IsWindow itemHs atts
        = (NoError,openwindow okId {wlsState=ls,wlsHandle=wH} pState)

    getWindowType :: (Window c .ls .ps) -> WindowType | Controls c
    getWindowType _
        = windowtype

instance Dialogs (Dialog c) | Controls c where
    openDialog :: .ls !(Dialog c .ls (PSt .l .p)) !(PSt .l .p) -> (!ErrorReport,!PSt .l .p) | Controls c
    openDialog ls (Dialog title controls atts) pState
        # maybe_id              = getWindowIdAttribute atts
        # (validId,okId,ioState)= validateWindowId maybe_id pState.io
        | not validId
        = (ErrorIdsInUse,{pState & io=ioState})
        # cStates               = controlToHandles controls
        # (rt,ioState)          = IOStGetReceiverTable ioState
        # (ioId,ioState)        = IOStGetIOId ioState
        # itemHs                = map ControlStateToWElementHandle cStates
        # (ok,itemHs,rt)        = controlIdsAreConsistent ioId okId itemHs rt
        # ioState               = IOStSetReceiverTable rt ioState
        # pState                = {pState & io=ioState}
        | not ok
        = (ErrorIdsInUse,pState)
        # wH                    = initWindowHandle title Modeless IsDialog itemHs atts
        = (NoError,openwindow okId {wlsState=ls,wlsHandle=wH} pState)

    openModalDialog :: .ls !(Dialog c .ls (PSt .l .p)) !(PSt .l .p) -> (!ErrorReport,!PSt .l .p) | Controls c
    openModalDialog ls (Dialog title controls atts) pState
        # maybe_id              = getWindowIdAttribute atts
        # (validId,okId,ioState)= validateWindowId maybe_id pState.io
        | not validId
        = (ErrorIdsInUse,{pState & io=ioState})
        # cStates               = controlToHandles controls
        # (rt,ioState)          = IOStGetReceiverTable ioState
        # (ioId,ioState)        = IOStGetIOId ioState
        # itemHs                = map ControlStateToWElementHandle cStates
        # (ok,itemHs,rt)        = controlIdsAreConsistent ioId okId itemHs rt
        # ioState               = IOStSetReceiverTable rt ioState
        # pState                = {pState & io=ioState}
        | not ok
        = (ErrorIdsInUse,pState)
        # wH                    = initWindowHandle title Modal IsDialog itemHs atts
        = (NoError,openmodalwindow okId {wlsState=ls,wlsHandle=wH} pState)

    getDialogType :: (Dialog c .ls .ps) -> WindowType | Controls c
    getDialogType _
        = dialogtype

initWindowHandle :: !Title !WindowMode !WindowKind ![WElementHandle .ls .ps] ![WindowAttribute *(.ls,.ps)] -> WindowHandle .ls .ps
initWindowHandle title mode kind itemHs atts
    = { whMode       = mode
      , whKind       = kind
      , whTitle      = title
      , whItemNrs    = [1..]
      , whKeyFocus   = {kfItem=Nothing,kfItems=[]}
       //,  whWindowInfo = Nothing
       // Mike //
      , whWindowInfo = undef
       ///
      , whItems      = itemHs
      , whShow       = True
      , whSelect     = True
      , whAtts       = atts
      , whDefaultId  = Nothing
      , whCancelId   = Nothing
      , whSize       = zero
      }


/*  WindowBound checks for normal windows.
*/
checkZeroWindowBound :: !(IOSt .l .p) -> (!Bool,!IOSt .l .p)
checkZeroWindowBound ioState
    # (wDevice,ioState) = IOStGetDevice WindowDevice ioState
      wHs               = WindowSystemStateGetWindowHandles wDevice
      (bound,wHs)       = (\wHs=:{whsNrWindowBound}->(whsNrWindowBound,wHs)) wHs
    # ioState           = IOStSetDevice (WindowSystemState wHs) ioState
    = (zeroBound bound,ioState)

decreaseWindowBound :: !Bool !(IOSt .l .p) -> IOSt .l .p
decreaseWindowBound decNrWindowBound ioState                            /// warning: not used
    # (wDevice,ioState) = IOStGetDevice WindowDevice ioState
      wHs               = WindowSystemStateGetWindowHandles wDevice
      wHs               = decWindowBound decNrWindowBound wHs
    # ioState           = IOStSetDevice (WindowSystemState wHs) ioState
    = ioState
where
    decWindowBound :: !Bool !(WindowHandles .ps) -> WindowHandles .ps
    decWindowBound decNrWindowBound wHs=:{whsNrWindowBound}             /// warning: not used
        | decNrWindowBound
        = {wHs & whsNrWindowBound=decBound whsNrWindowBound}
        = wHs

getWindowIdAttribute :: ![WindowAttribute .ps] -> Maybe Id
getWindowIdAttribute atts
    # (hasIdAtt,idAtt)  = Select iswindowid undef atts
    | hasIdAtt
    = Just (getwindowid idAtt)
    = Nothing


/*  controlIdsAreConsistent checks whether the WElementHandles contain R(2)Ids that have already been
    associated with open receivers and if there are no duplicate Ids.
    The ReceiverTable is not changed if there are duplicate R(2)Ids; otherwise all R(2)Ids have been bound.
*/
controlIdsAreConsistent :: !SystemId !Id ![WElementHandle .ls .ps] !ReceiverTable -> (!Bool,![WElementHandle .ls .ps],!ReceiverTable)
controlIdsAreConsistent ioId wId itemHs rt
    | not (noDuplicates ids)
    = (False, itemHs1,rt)
    = (okRIds,itemHs2,if okRIds rt1 rt)
where
    (ids,   itemHs1)    = getWElementControlIds itemHs
    (okRIds,itemHs2,rt1)= bindReceiverControlIds ioId wId itemHs1 rt


/*  closeWindow closes the indicated window.
*/
closeWindow :: !Id !(PSt .l .p) -> PSt .l .p
closeWindow id pState
    = appPIO (disposeWindow (toWID id)) pState


/*  closeControls closes the controls in the indicated window.
*/
closeControls :: !Id [Id] !Bool !(IOSt .l .p) -> IOSt .l .p
closeControls wId ids relayout ioState
    # (wDevice,ioState) = IOStGetDevice WindowDevice ioState
    # wHs               = WindowSystemStateGetWindowHandles wDevice
    # (found,wsH,wHs)   = getWindowHandlesWindow (toWID wId) wHs
    | not found
    = IOStSetDevice (WindowSystemState wHs) ioState
    // Mike //
    # (wKind,wsH)       = getWindowStateHandleWindowKind wsH
    | wKind==IsGameWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
    ///
    # (tb,ioState)      = getIOToolbox ioState
    # (rids,wsH,tb)     = closecontrols ids relayout wsH tb
    # ioState           = setIOToolbox tb ioState
    # ioState           = unbindRIds rids ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState


/*  getWindowStateHandleIds returns all Ids of the controls in this window.
    This function is used by open(Compound)Controls.
*/
getWindowStateHandleIds :: !(WindowStateHandle .ps) -> (![Id],!WindowStateHandle .ps)
getWindowStateHandleIds wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems}}}
    # (ids,itemHs)  = getWElementControlIds whItems
    = (ids,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
getWindowStateHandleIds _
    = StdWindowFatalError "getWindowStateHandleIds" "unexpected window placeholder argument"

/*  openControls adds controls to the indicated window.
*/
openControls :: !Id .ls (cdef .ls (PSt .l .p)) !(IOSt .l .p) -> (!ErrorReport,!IOSt .l .p) | Controls cdef
openControls wId ls newControls ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
    # wHs                   = WindowSystemStateGetWindowHandles wDevice
    # (found,wsH,wHs)       = getWindowHandlesWindow (toWID wId) wHs
    | not found
    = (ErrorUnknownObject,IOStSetDevice (WindowSystemState wHs) ioState)
    // Mike //
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind==IsGameWindow
    = (ErrorWrongObject,IOStSetDevice (WindowSystemState wHs) ioState)
    ///
    # newItemHs             = map ControlStateToWElementHandle (controlToHandles newControls)
      (currentIds,wsH)      = getWindowStateHandleIds wsH
      (disjoint,newItemHs)  = disjointControlIds currentIds newItemHs
    | not disjoint
    = (ErrorIdsInUse,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState)
    # (rt,ioState)          = IOStGetReceiverTable ioState
    # (ioId,ioState)        = IOStGetIOId ioState
      (ok,newItemHs,rt)     = controlIdsAreConsistent ioId wId newItemHs rt
    # ioState               = IOStSetReceiverTable rt ioState
    | not ok
    = (ErrorIdsInUse,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState)
    # (wsH,ioState)         = accIOToolbox (opencontrols ls newItemHs wsH) ioState
    = (NoError,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState)

openCompoundControls :: !Id !Id .ls (cdef .ls (PSt .l .p)) !(IOSt .l .p) -> (!ErrorReport,!IOSt .l .p) | Controls cdef
openCompoundControls wId cId ls newControls ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
    # wHs                   = WindowSystemStateGetWindowHandles wDevice
    # (found,wsH,wHs)       = getWindowHandlesWindow (toWID wId) wHs
    | not found
    = (ErrorUnknownObject,IOStSetDevice (WindowSystemState wHs) ioState)
    // Mike //
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind==IsGameWindow
    = (ErrorWrongObject,IOStSetDevice (WindowSystemState wHs) ioState)
    ///
    # newItemHs             = map ControlStateToWElementHandle (controlToHandles newControls)
      (currentIds,wsH)      = getWindowStateHandleIds wsH
      (disjoint,newItemHs)  = disjointControlIds currentIds newItemHs
    | not disjoint
    = (ErrorIdsInUse,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState)
    # (rt,ioState)          = IOStGetReceiverTable ioState
    # (ioId,ioState)        = IOStGetIOId ioState
      (ok,newItemHs,rt)     = controlIdsAreConsistent ioId wId newItemHs rt
    # ioState               = IOStSetReceiverTable rt ioState
    | not ok
    = (ErrorIdsInUse,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState)
    # (tb,ioState)          = getIOToolbox ioState
    # (ok,wsH,tb)           = opencompoundcontrols cId ls newItemHs wsH tb
    # ioState               = setIOToolbox tb ioState
      error                 = if ok NoError ErrorUnknownObject
    = (error,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState)


/*  setControlPos changes the position of the indicated control.
*/
setControlPos :: !Id !Id !ItemPos !(IOSt .l .p) -> (!Bool,!IOSt .l .p)
setControlPos wId cId newPos ioState
    # (wDevice,ioState) = IOStGetDevice WindowDevice ioState
    # wHs               = WindowSystemStateGetWindowHandles wDevice
    # (found,wsH,wHs)   = getWindowHandlesWindow (toWID wId) wHs
    | not found
    = (False,IOStSetDevice (WindowSystemState wHs) ioState)
    // Mike //
    # (wKind,wsH)       = getWindowStateHandleWindowKind wsH
    | wKind==IsGameWindow
    = (False,IOStSetDevice (WindowSystemState wHs) ioState)
    ///
    # (tb,ioState)      = getIOToolbox ioState
    # (ok,wsH,tb)       = setcontrolpos cId newPos wsH tb
    # ioState           = setIOToolbox tb ioState
      wHs               = setWindowHandlesWindow wsH wHs
    = (ok,IOStSetDevice (WindowSystemState wHs) ioState)


/*  controlSize calculates the size of the given control.
*/
controlSize :: !(cdef .ls (PSt .l .p)) !(Maybe (Int,Int)) !(Maybe (Int,Int)) !(Maybe (Int,Int)) !(IOSt .l .p)
            -> (!Size,!IOSt .l .p) | Controls cdef
controlSize cdef hMargins vMargins itemSpaces ioState
    # (tb,ioState)      = getIOToolbox ioState
    # (wMetrics,tb)     = OSDefaultWindowMetrics tb
      hMargins          = case hMargins of
                            (Just (left,right)) -> (max 0 left,max 0 right)
                            _                   -> (wMetrics.osmHorMargin,wMetrics.osmHorMargin)
      vMargins          = case vMargins of
                            (Just (top,bottom)) -> (max 0 top,max 0 bottom)
                            _                   -> (wMetrics.osmVerMargin,wMetrics.osmVerMargin)
      itemSpaces        = case itemSpaces of
                            (Just (hor,vert))   -> (max 0 hor,max 0 vert)
                            _                   -> (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)
      itemHs            = map ControlStateToWElementHandle (controlToHandles cdef)
    # (derSize,_,tb)    = layoutControls wMetrics hMargins vMargins itemSpaces zero zero zero zero itemHs tb
    # ioState           = setIOToolbox tb ioState
    = (derSize,ioState)


/*  (hide/show)Windows hide/show the indicated windows.
    get(Hidden/Shown)Windows return the Ids of the hidden/shown windows.
*/
//  getWindowWIDSShow is used by (hide/show)Windows and get(Hidden/Shown)Windows.
getWindowWIDSShow :: !(WindowStateHandle .ps) -> ((WIDS,Bool),!WindowStateHandle .ps)
getWindowWIDSShow wsH
    # (wids,wsH)    = getWindowStateHandleWIDS wsH
    # (show,wsH)    = getWindowStateHandleShow wsH
    = ((wids,show),wsH)

//  setWindowsShow is used by (hide/show)Windows.
setWindowsShow :: !Bool ![WIDS] ![WindowStateHandle .ps] !*OSToolbox -> (![WIDS],![WindowStateHandle .ps],!*OSToolbox)
setWindowsShow show wids wsHs tb
    | isEmpty wids || isEmpty wsHs
    = (wids,wsHs,tb)
    # (wsH,wsHs)    = HdTl wsHs
    # (wids,wsH,tb) = setWindowShow  show wids wsH  tb
    # (wids,wsHs,tb)= setWindowsShow show wids wsHs tb
    = (wids,[wsH:wsHs],tb)
where
    setWindowShow :: !Bool ![WIDS] !(WindowStateHandle .ps) !*OSToolbox -> (![WIDS],!WindowStateHandle .ps,!*OSToolbox)
    setWindowShow show wids wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH}} tb
        # (found,wids)  = RemoveCheck wshIds wids
        | not found
        = (wids,wsH,tb)
        | wH.whMode==Modal      // Do not hide/show modal dialogues
        = (wids,wsH,tb)
        # wsH           = {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whShow=show}}}
        | show
        = (wids,wsH,OSshowWindow wshIds.wPtr False tb)
        = (wids,wsH,OShideWindow wshIds.wPtr tb)
    setWindowShow _ _ _ _
        = StdWindowFatalError "setWindowShow" "unexpected window placeholder argument"

hideWindows :: ![Id] !(IOSt .l .p) -> IOSt .l .p
hideWindows ids ioState
    | isEmpty ids
    = ioState
    # (wDevice,ioState) = IOStGetDevice WindowDevice ioState
      windows           = WindowSystemStateGetWindowHandles wDevice
      wHs               = windows.whsWindows
      (widsshows,wHs)   = unzip (map getWindowWIDSShow wHs)
      widsshown         = filter (\({wId},show)->show && isMember wId ids) widsshows
    | isEmpty widsshown
    = IOStSetDevice (WindowSystemState {windows & whsWindows=wHs}) ioState
    # (tb,ioState)      = getIOToolbox ioState
    # (_,wHs,tb)        = setWindowsShow False (fst (unzip widsshown)) wHs tb
    # ioState           = setIOToolbox tb ioState
    = IOStSetDevice (WindowSystemState {windows & whsWindows=wHs}) ioState

showWindows :: ![Id] !(IOSt .l .p) -> IOSt .l .p
showWindows ids ioState
    | isEmpty ids
    = ioState
    # (wDevice,ioState) = IOStGetDevice WindowDevice ioState
      windows           = WindowSystemStateGetWindowHandles wDevice
      wHs               = windows.whsWindows
      (widsshows,wHs)   = unzip (map getWindowWIDSShow wHs)
      widshidden        = filter (\({wId},show)->not show && isMember wId ids) widsshows
    | isEmpty widshidden
    = IOStSetDevice (WindowSystemState {windows & whsWindows=wHs}) ioState
    # (tb,ioState)      = getIOToolbox ioState
    # (_,wHs,tb)        = setWindowsShow True (fst (unzip widshidden)) wHs tb
    | any snd widsshows
    = IOStSetDevice (WindowSystemState {windows & whsWindows=wHs}) (setIOToolbox tb ioState)
    # newTopMost        = (fst (hd widshidden)).wPtr
    # tb                = OSactivateWindow newTopMost tb
    = IOStSetDevice (WindowSystemState {windows & whsWindows=wHs}) (setIOToolbox tb ioState)

getHiddenWindows :: !(IOSt .l .p) -> (![Id],!IOSt .l .p)
getHiddenWindows ioState
    # (wDevice,ioState) = IOStGetDevice WindowDevice ioState
      windows           = WindowSystemStateGetWindowHandles wDevice
      wHs               = windows.whsWindows
      (widsshows,wHs)   = unzip (map getWindowWIDSShow wHs)
      hiddenIds         = FilterMap (\({wId},show)->(not show,wId)) widsshows
    # ioState           = IOStSetDevice (WindowSystemState {windows & whsWindows=wHs}) ioState
    = (hiddenIds,ioState)

getShownWindows :: !(IOSt .l .p) -> (![Id],!IOSt .l .p)
getShownWindows ioState
    # (wDevice,ioState) = IOStGetDevice WindowDevice ioState
      windows           = WindowSystemStateGetWindowHandles wDevice
      wHs               = windows.whsWindows
      (widsshows,wHs)   = unzip (map getWindowWIDSShow wHs)
      shownIds          = FilterMap (\({wId},show)->(show,wId)) widsshows
    # ioState           = IOStSetDevice (WindowSystemState {windows & whsWindows=wHs}) ioState
    = (shownIds,ioState)


/*  activateWindow activates the given window.
*/
activateWindow :: !Id !(IOSt .l .p) -> IOSt .l .p
activateWindow id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (exists,windows)      = hasWindowHandlesWindow wid windows
    | not exists
    = IOStSetDevice (WindowSystemState windows) ioState
    # wHs                   = windows.whsWindows
      (modal,modeless)      = Uspan ismodalwindow wHs
      (isModal,modal)       = UContains (identifyWindowStateHandle wid) modal
    | isModal
    = IOStSetDevice (WindowSystemState {windows & whsWindows=modal++modeless}) ioState
    | isEmpty modal
        # (_,wsH,others)    = URemove (identifyWindowStateHandle wid) undef modeless
        # (wids,wsH)        = getWindowStateHandleWIDS wsH
        # ioState           = appIOToolbox (OSactivateWindow wids.wPtr) ioState
        = IOStSetDevice (WindowSystemState {windows & whsWindows=[wsH:others]}) ioState
    // otherwise
    # (befModals,lastModal) = InitLast modal
      (modalWIDS,lastModal) = getWindowStateHandleWIDS lastModal
      (_,wsH,others)        = URemove (identifyWindowStateHandle wid) undef modeless
      (modelessWIDS,wsH)    = getWindowStateHandleWIDS wsH
    # ioState               = appIOToolbox (OSstackWindow modelessWIDS.wPtr modalWIDS.wPtr) ioState
    = IOStSetDevice (WindowSystemState {windows & whsWindows=befModals++[lastModal,wsH:others]}) ioState
where
    wid                     = toWID id

    ismodalwindow :: !(WindowStateHandle .ps) -> (!Bool,!WindowStateHandle .ps)
    ismodalwindow wsH
        # (mode,wsH)        = getWindowStateHandleWindowMode wsH
        = (mode==Modal,wsH)

/*  getActiveWindow returns the Id of the currently active window.
*/
getActiveWindow :: !(IOSt .l .p) -> (!Maybe Id, !IOSt .l .p)
getActiveWindow ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wids,windows)  = getWindowHandlesActiveWindow windows
    # ioState               = IOStSetDevice (WindowSystemState windows) ioState
    = (if found (Just wids.wId) Nothing,ioState)


/*  stackWindow changes the stacking order of the current windows.
*/
stackWindow :: !Id !Id !(IOSt .l .p) -> IOSt .l .p
stackWindow windowId behindId ioState
    | windowId==behindId
    = ioState
    # (wDevice,ioState)         = IOStGetDevice WindowDevice ioState
      windows                   = WindowSystemStateGetWindowHandles wDevice
    # (hasBehind,windows)       = hasWindowHandlesWindow (toWID behindId) windows
    | not hasBehind
    = IOStSetDevice (WindowSystemState windows) ioState
    # (hasWindow,wsH,windows)   = getWindowHandlesWindow (toWID windowId) windows
    | not hasWindow
    = IOStSetDevice (WindowSystemState windows) ioState
    # (mode,wsH)                = getWindowStateHandleWindowMode wsH
    | mode==Modal
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (_,_,windows)             = removeWindowHandlesWindow (toWID windowId) windows        // remove placeholder window
    # (tb,ioState)              = getIOToolbox ioState
    # (windows,tb)              = stackwindows wsH behindId windows tb
    # ioState                   = setIOToolbox tb ioState
    = IOStSetDevice (WindowSystemState windows) ioState
where
    stackwindows :: !(WindowStateHandle .ps) !Id !(WindowHandles .ps) !*OSToolbox -> (!WindowHandles .ps,!*OSToolbox)
    stackwindows wsH behindId windows=:{whsWindows=wsHs} tb
        # (wids,wsH)    = getWindowStateHandleWIDS wsH
        # (wsHs,tb)     = stackBehind wsH wids.wPtr behindId wsHs tb
        = ({windows & whsWindows=wsHs},tb)
    where
        stackBehind :: (WindowStateHandle .ps) !OSWindowPtr !Id ![WindowStateHandle .ps] !*OSToolbox
                                                            -> (![WindowStateHandle .ps],!*OSToolbox)
        stackBehind wsH wPtr behindId [wsH`:wsHs] tb
            # (wids`,wsH`)  = getWindowStateHandleWIDS wsH`
            | behindId<>wids`.wId
                # (wsHs,tb) = stackBehind wsH wPtr behindId wsHs tb
                = ([wsH`:wsHs],tb)
            # (mode`,wsH`)  = getWindowStateHandleWindowMode wsH`
            | mode`==Modal
                # (wsHs,tb) = stackBehindLastModal wsH wPtr wids`.wPtr wsHs tb
                = ([wsH`:wsHs],tb)
                with
                    stackBehindLastModal :: (WindowStateHandle .ps) !OSWindowPtr !OSWindowPtr ![WindowStateHandle .ps] !*OSToolbox
                                                                                          -> (![WindowStateHandle .ps],!*OSToolbox)
                    stackBehindLastModal wsH wPtr behindPtr [wsH`:wsHs] tb
                        # (wids`,wsH`)  = getWindowStateHandleWIDS wsH`
                        # (mode`,wsH`)  = getWindowStateHandleWindowMode wsH`
                        | mode`==Modal
                            # (wsHs,tb) = stackBehindLastModal wsH wPtr wids`.wPtr wsHs tb
                            = ([wsH`:wsHs],tb)
                        // otherwise
                        = ([wsH,wsH`:wsHs],OSstackWindow wPtr behindPtr tb)
                    stackBehindLastModal wsH wPtr behindPtr _ tb
                        = ([wsH],OSstackWindow wPtr behindPtr tb)
            // otherwise
            = ([wsH`,wsH:wsHs],OSstackWindow wPtr wids`.wPtr tb)
        stackBehind _ _ _ _ _
            = StdWindowFatalError "stackBehind" "this alternative should not be reached"

getWindowStack :: !(IOSt .l .p) -> (![(Id,WindowType)],!IOSt .l .p)
getWindowStack ioState
    # (wDevice,ioState) = IOStGetDevice WindowDevice ioState
      windows           = WindowSystemStateGetWindowHandles wDevice
      wsHs              = windows.whsWindows
      (id_types,wsHs)   = unzip (map getWindowIdType wsHs)
    = (id_types,IOStSetDevice (WindowSystemState {windows & whsWindows=wsHs}) ioState)
where
    getWindowIdType :: !(WindowStateHandle .ps) -> ((Id,WindowType),!WindowStateHandle .ps)
    getWindowIdType wsH
        # (wids,wsH)    = getWindowStateHandleWIDS wsH
        # (kind,wsH)    = getWindowStateHandleWindowKind wsH
        = ((wids.wId,if (kind==IsWindow) windowtype dialogtype),wsH)

getWindowsStack :: !(IOSt .l .p) -> (![Id],!IOSt .l .p)
getWindowsStack ioState
    # (id_types,ioState)    = getWindowStack ioState
    = (FilterMap (\(id,wtype)->(wtype==windowtype,id)) id_types,ioState)

getDialogsStack :: !(IOSt .l .p) -> (![Id],!IOSt .l .p)
getDialogsStack ioState
    # (id_types,ioState)    = getWindowStack ioState
    = (FilterMap (\(id,wtype)->(wtype==dialogtype,id)) id_types,ioState)


/*  Return layout attributes and default values.
*/
getDefaultHMargin :: !(IOSt .l .p) -> ((Int,Int),!IOSt .l .p)
getDefaultHMargin ioState
    # ({osmHorMargin},ioState)  = accIOToolbox OSDefaultWindowMetrics ioState
    = ((osmHorMargin,osmHorMargin),ioState)

getDefaultVMargin :: !(IOSt .l .p) -> ((Int,Int),!IOSt .l .p)
getDefaultVMargin ioState
    # ({osmVerMargin},ioState)  = accIOToolbox OSDefaultWindowMetrics ioState
    = ((osmVerMargin,osmVerMargin),ioState)

getDefaultItemSpace :: !(IOSt .l .p) -> ((Int,Int),!IOSt .l .p)
getDefaultItemSpace ioState
    # ({osmHorItemSpace,osmVerItemSpace},ioState)   = accIOToolbox OSDefaultWindowMetrics ioState
    = ((osmHorItemSpace,osmVerItemSpace),ioState)

getWindowHMargin :: !Id !(IOSt .l .p) -> (!Maybe (Int,Int),!IOSt .l .p)
getWindowHMargin id ioState
    # (wDevice,ioState)         = IOStGetDevice WindowDevice ioState
      windows                   = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)       = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # ({osmHorMargin},ioState)  = accIOToolbox OSDefaultWindowMetrics ioState
    # (marginAtt,wsH)           = gethmargin osmHorMargin wsH
    = (Just marginAtt,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
    gethmargin :: Int !(WindowStateHandle .ps) -> ((Int,Int),!WindowStateHandle .ps)
    gethmargin defHMargin wsH=:{wshHandle=Just {wlsHandle={whAtts}}}
        = (getwindowhmargin (snd (Select iswindowhmargin (WindowHMargin defHMargin defHMargin) whAtts)),wsH)
    gethmargin _ _
        = StdWindowFatalError "getWindowHMargin" "unexpected window placeholder argument"

getWindowVMargin :: !Id !(IOSt .l .p) -> (!Maybe (Int,Int),!IOSt .l .p)
getWindowVMargin id ioState
    # (wDevice,ioState)         = IOStGetDevice WindowDevice ioState
      windows                   = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)       = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # ({osmVerMargin},ioState)  = accIOToolbox OSDefaultWindowMetrics ioState
    # (marginAtt,wsH)           = getvmargin osmVerMargin wsH
    = (Just marginAtt,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
    getvmargin :: Int !(WindowStateHandle .ps) -> ((Int,Int),!WindowStateHandle .ps)
    getvmargin defVMargin wsH=:{wshHandle=Just {wlsHandle={whAtts}}}
        = (getwindowvmargin (snd (Select iswindowvmargin (WindowVMargin defVMargin defVMargin) whAtts)),wsH)
    getvmargin _ _
        = StdWindowFatalError "getWindowVMargin" "unexpected window placeholder argument"

getWindowItemSpace :: !Id !(IOSt .l .p) -> (!Maybe (Int,Int),!IOSt .l .p)
getWindowItemSpace id ioState
    # (wDevice,ioState)         = IOStGetDevice WindowDevice ioState
      windows                   = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)       = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # ({osmHorItemSpace,osmVerItemSpace},ioState)
                                = accIOToolbox OSDefaultWindowMetrics ioState
    # (marginAtt,wsH)           = getitemspaces (osmHorItemSpace,osmVerItemSpace) wsH
    = (Just marginAtt,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
    getitemspaces :: (Int,Int) !(WindowStateHandle .ps) -> ((Int,Int),!WindowStateHandle .ps)
    getitemspaces (defHSpace,defVSpace) wsH=:{wshHandle=Just {wlsHandle={whAtts}}}
        = (getwindowitemspace (snd (Select iswindowitemspace (WindowItemSpace defHSpace defVSpace) whAtts)),wsH)
    getitemspaces _ _
        = StdWindowFatalError "getWindowItemSpace" "unexpected window placeholder argument"


/*  Setting the SelectState of windows.
*/
enableWindow :: !Id !(IOSt .l .p) -> IOSt .l .p
enableWindow id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (curSelectState,wsH)  = getWindowStateHandleSelect wsH
    | curSelectState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (tb,ioState)          = getIOToolbox ioState
      wsH                   = setWindowStateHandleSelect True wsH
      (wids,wsH)            = getWindowStateHandleWIDS wsH
      wPtr                  = wids.wPtr
    # (wH`,wsH,tb)          = retrieveWindowHandle` wsH tb
    # (wH`,tb)              = enablecontrols [] True wPtr wH` tb
      wsH                   = insertWindowHandle` wH` wsH
       //(maybeWindowInfo,wsH)  = getWindowStateHandleWindowInfo wsH
       //scrollInfo         = case maybeWindowInfo of
       //                           Nothing     -> (False,False)
       //                           Just info   -> (isJust info.windowHScroll,isJust info.windowVScroll)
      (XWindowInfo info,wsH)    = getWindowStateHandleWindowInfo wsH
      HScroll               = info.windowHScroll
      VScroll               = info.windowVScroll
      scrollInfo            = (isJust HScroll,isJust VScroll)
    # tb                    = OSenableWindow wPtr scrollInfo False tb
    # tb                    = OSinvalidateWindow wPtr tb
    # ioState               = setIOToolbox tb ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState

disableWindow :: !Id !(IOSt .l .p) -> IOSt .l .p
disableWindow id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (curSelectState,wsH)  = getWindowStateHandleSelect wsH
    | not curSelectState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (tb,ioState)          = getIOToolbox ioState
      wsH                   = setWindowStateHandleSelect False wsH
      (wids,wsH)            = getWindowStateHandleWIDS wsH
      wPtr                  = wids.wPtr
    # (wH`,wsH,tb)          = retrieveWindowHandle` wsH tb
    # (wH`,tb)              = disablecontrols [] True wPtr wH` tb
      wsH                   = insertWindowHandle` wH` wsH
       //(maybeWindowInfo,wsH)  = getWindowStateHandleWindowInfo wsH
       //scrollInfo         = case maybeWindowInfo of
       //                           Nothing     -> (False,False)
       //                           Just info   -> (isJust info.windowHScroll,isJust info.windowVScroll)
       // Mike //
      (XWindowInfo info,wsH)    = getWindowStateHandleWindowInfo wsH
      scrollInfo            = (isJust info.windowHScroll,isJust info.windowVScroll)
       ///
    # tb                    = OSdisableWindow wPtr scrollInfo False tb
    # tb                    = OSinvalidateWindow wPtr tb
    # ioState               = setIOToolbox tb ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState

enableWindowMouse :: !Id !(IOSt .l .p) -> IOSt .l .p
enableWindowMouse id ioState
    = setWindowMouseSelectState Able id ioState

disableWindowMouse :: !Id !(IOSt .l .p) -> IOSt .l .p
disableWindowMouse id ioState
    = setWindowMouseSelectState Unable id ioState

setWindowMouseSelectState :: !SelectState !Id !(IOSt .l .p) -> IOSt .l .p
setWindowMouseSelectState selectState id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # wsH                   = setMouseSelectState selectState wsH
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    setMouseSelectState :: !SelectState !(WindowStateHandle .ps) -> WindowStateHandle .ps
    setMouseSelectState selectState wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whAtts}}}
        = {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whAtts=setMouseSelectStateAtt selectState whAtts}}}
    where
        setMouseSelectStateAtt :: !SelectState ![WindowAttribute .ps] -> [WindowAttribute .ps]
        setMouseSelectStateAtt selectState atts
            # (found,mouseAtt,atts) = Remove iswindowmouse undef atts
            | not found
            = atts
            # (filter,_,fun)        = getwindowmouseinfo mouseAtt
            = [WindowMouse filter selectState fun:atts]
    setMouseSelectState _ _
        = StdWindowFatalError "setWindowMouseSelectState" "unexpected window placeholder argument"

enableWindowKeyboard :: !Id !(IOSt .l .p) -> IOSt .l .p
enableWindowKeyboard id ioState
    = setWindowKeyboardSelectState Able id ioState

disableWindowKeyboard :: !Id !(IOSt .l .p) -> IOSt .l .p
disableWindowKeyboard id ioState
    = setWindowKeyboardSelectState Unable id ioState

setWindowKeyboardSelectState :: !SelectState !Id !(IOSt .l .p) -> IOSt .l .p
setWindowKeyboardSelectState selectState id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # wsH                   = setKeyboardSelectState selectState wsH
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    setKeyboardSelectState :: !SelectState !(WindowStateHandle .ps) -> WindowStateHandle .ps
    setKeyboardSelectState selectState wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whAtts}}}
        = {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whAtts=setKeyboardSelectStateAtt selectState whAtts}}}
    where
        setKeyboardSelectStateAtt :: !SelectState ![WindowAttribute .ps] -> [WindowAttribute .ps]
        setKeyboardSelectStateAtt selectState atts
            # (found,keyAtt,atts)   = Remove iswindowkeyboard undef atts
            | not found
            = atts
            # (filter,_,fun)        = getwindowkeyboardinfo keyAtt
            = [WindowKeyboard filter selectState fun:atts]
    setKeyboardSelectState _ _
        = StdWindowFatalError "setWindowKeyboardSelectState" "unexpected window placeholder argument"

getWindowSelectState :: !Id !(IOSt .l .p) -> (!Maybe SelectState,!IOSt .l .p)
getWindowSelectState id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
    # (wSelect,wsH)         = getWindowStateHandleSelect wsH
    = (Just (if wSelect Able Unable),IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowMouseSelectState :: !Id !(IOSt .l .p) -> (!Maybe SelectState,!IOSt .l .p)
getWindowMouseSelectState id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
    # (found,_,select,wsH)  = getWindowMouseAttInfo wsH
    = (if found (Just select) Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowMouseAttInfo :: !(WindowStateHandle .ps) -> (!Bool,MouseStateFilter,SelectState,!WindowStateHandle .ps)
getWindowMouseAttInfo wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whAtts}}}
    # (hasMouseAtt,mouseAtt)= Select iswindowmouse undef whAtts
    | not hasMouseAtt
    = (False,undef,undef,wsH)
    # (filter,selectState,_)= getwindowmouseinfo mouseAtt
    = (True,filter,selectState,wsH)
getWindowMouseAttInfo _
    = StdWindowFatalError "getWindowMouseAttInfo" "unexpected window placeholder argument"

getWindowKeyboardSelectState :: !Id !(IOSt .l .p) -> (!Maybe SelectState,!IOSt .l .p)
getWindowKeyboardSelectState id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
    # (found,_,select,wsH)  = getWindowKeyboardAttInfo wsH
    = (if found (Just select) Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowKeyboardAttInfo :: !(WindowStateHandle .ps) -> (!Bool,KeyboardStateFilter,SelectState,!WindowStateHandle .ps)
getWindowKeyboardAttInfo wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whAtts}}}
    # (hasKeyAtt,keyAtt)    = Select iswindowkeyboard undef whAtts
    | not hasKeyAtt
    = (False,undef,undef,wsH)
    # (filter,selectState,_)= getwindowkeyboardinfo keyAtt
    = (True,filter,selectState,wsH)
getWindowKeyboardAttInfo _
    = StdWindowFatalError "getWindowKeyboardAttInfo" "unexpected window placeholder argument"

getWindowMouseStateFilter :: !Id !(IOSt .l .p) -> (!Maybe MouseStateFilter,!IOSt .l .p)
getWindowMouseStateFilter id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
    # (found,filter,_,wsH)  = getWindowMouseAttInfo wsH
    = (if found (Just filter) Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowKeyboardStateFilter :: !Id !(IOSt .l .p) -> (!Maybe KeyboardStateFilter,!IOSt .l .p)
getWindowKeyboardStateFilter id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
    # (found,filter,_,wsH)  = getWindowKeyboardAttInfo wsH
    = (if found (Just filter) Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

setWindowMouseStateFilter :: !Id !MouseStateFilter !(IOSt .l .p) -> IOSt .l .p
setWindowMouseStateFilter id filter ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # wsH                   = setMouseFilter filter wsH
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    setMouseFilter :: !MouseStateFilter !(WindowStateHandle .ps) -> WindowStateHandle .ps
    setMouseFilter filter wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whAtts}}}
        = {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whAtts=setMouseStateFilterAtt filter whAtts}}}
    where
        setMouseStateFilterAtt :: !MouseStateFilter ![WindowAttribute .ps] -> [WindowAttribute .ps]
        setMouseStateFilterAtt filter atts
            # (found,mouseAtt,atts) = Remove iswindowmouse undef atts
            | not found
            = atts
            # (_,select,fun)        = getwindowmouseinfo mouseAtt
            = [WindowMouse filter select fun:atts]
    setMouseFilter _ _
        = StdWindowFatalError "setWindowMouseStateFilter" "unexpected window placeholder argument"

setWindowKeyboardStateFilter :: !Id !KeyboardStateFilter !(IOSt .l .p) -> IOSt .l .p
setWindowKeyboardStateFilter id filter ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # wsH                   = setKeyboardFilter filter wsH
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    setKeyboardFilter :: !KeyboardStateFilter !(WindowStateHandle .ps) -> WindowStateHandle .ps
    setKeyboardFilter filter wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whAtts}}}
        = {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whAtts=setKeyboardStateFilterAtt filter whAtts}}}
    where
        setKeyboardStateFilterAtt :: !KeyboardStateFilter ![WindowAttribute .ps] -> [WindowAttribute .ps]
        setKeyboardStateFilterAtt filter atts
            # (found,keyAtt,atts)   = Remove iswindowkeyboard undef atts
            | not found
            = atts
            # (_,select,fun)        = getwindowkeyboardinfo keyAtt
            = [WindowKeyboard filter select fun:atts]
    setKeyboardFilter _ _
        = StdWindowFatalError "setWindowKeyboardStateFilter" "unexpected window placeholder argument"


//  Operations that are concerned with the background/look of a window.

drawInWindow :: !Id ![DrawFunction] !(IOSt .l .p) -> IOSt .l .p
drawInWindow _ [] ioState
    = ioState
drawInWindow id drawfs ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (wsH,ioState)         = accIOToolbox (drawinwindow` drawfs wsH) ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    drawinwindow` :: ![DrawFunction] !(WindowStateHandle .ps) !*OSToolbox -> (!WindowStateHandle .ps,!*OSToolbox)
    drawinwindow` drawfs wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH}} tb
        # (wH,tb)           = validateWindowClipState wPtr wH tb
        # (wH,tb)           = drawinwindow wPtr drawfs wH tb
        = ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
    drawinwindow` _ _ _
        = StdWindowFatalError "drawInWindow" "unexpected window placeholder argument"

updateWindow :: !Id !(Maybe ViewFrame) !(IOSt .l .p) -> IOSt .l .p
updateWindow id maybeViewFrame ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (wsH,ioState)         = accIOToolbox (updateWindowBackground maybeViewFrame wsH) ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    updateWindowBackground :: !(Maybe ViewFrame) !(WindowStateHandle .ps) !*OSToolbox -> (!WindowStateHandle .ps,!*OSToolbox)
    updateWindowBackground maybeViewFrame wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH}} tb
        | IsEmptyRect updArea
        = (wsH,tb)
        # (wH,tb)   = updatewindow info wH tb
        = ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
    where
        windowArea  = PosSizeToRect zero wH.whSize
        updArea     = case maybeViewFrame of
                        Nothing     -> windowArea
                        Just rect   -> IntersectRects (RectangleToRect rect) windowArea
        info        = { updWIDS         = wshIds
                      , updWindowArea   = updArea
                      , updControls     = []
                      , updGContext     = Nothing
                      }
    updateWindowBackground _ _ _
        = StdWindowFatalError "updateWindow" "unexpected window placeholder argument"

setWindowLook :: !Id !Bool !Look !(IOSt .l .p) -> IOSt .l .p
setWindowLook id redraw look ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (wsH,ioState)         = accIOToolbox (setwindowlook redraw look wsH) ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    setwindowlook :: !Bool !Look !(WindowStateHandle .ps) !*OSToolbox -> (!WindowStateHandle .ps,!*OSToolbox)
    setwindowlook redraw look wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH}} tb
        # wH                = {wH & whWindowInfo=XWindowInfo {windowInfo & windowLook={lookInfo & lookFun=look}}}
        | not redraw
        = ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
        # (wH,tb)           = validateWindowClipState wPtr wH tb
        # (wH,tb)           = drawwindowlook wPtr wH tb
        = ({wsH & wshHandle = Just {wlsH & wlsHandle=wH}},tb)
    where
         // Mike //
        windowInfo          = getWindowInfoWindowData wH.whWindowInfo
         ///
        lookInfo            = windowInfo.windowLook
    setwindowlook _ _ _ _
        = StdWindowFatalError "setWindowLook" "unexpected window placeholder argument"

getWindowLook :: !Id !(IOSt .l .p) -> (!Maybe Look,!IOSt .l .p)
getWindowLook id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
    # (XWindowInfo windowInfo,wsH)      = getWindowStateHandleWindowInfo wsH
     // # look                  = (fromJust windowInfo).windowLook.lookFun
     // Mike //
    # look                  = windowInfo.windowLook.lookFun
     ///
    = (Just look,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)


//  Operations that are concerned with the position of windows/dialogues.

setWindowPos :: !Id !ItemPos !(IOSt .l .p) -> IOSt .l .p
setWindowPos id pos ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wMode,wsH)           = getWindowStateHandleWindowMode wsH
    | wMode==Modal
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (okId,pos,windows)    = validateRelativeId id pos windows
    | not okId
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (wids, wsH)           = getWindowStateHandleWIDS wsH
      (wSize,wsH)           = getWindowStateHandleSize wsH
      (wKind,wsH)           = getWindowStateHandleWindowKind wsH
      windows               = setWindowHandlesWindow wsH windows
    # (tb,ioState)          = getIOToolbox ioState
    # (pos,windows,tb)      = exactWindowPos wSize (Just pos) wKind Modeless windows tb
    # tb                    = OSsetWindowPos wids.wPtr (PointToTuple pos) tb
    # ioState               = setIOToolbox tb ioState
    = IOStSetDevice (WindowSystemState windows) ioState
where
    // validateRelativeId checks the validity of the ItemPos.
    // It assumes that the WindowHandles argument is not empty (containing atleast the target window).
    validateRelativeId :: !Id !ItemPos !(WindowHandles .ps) -> (!Bool,!ItemPos,!WindowHandles .ps)
    validateRelativeId id itemPos=:(itemLoc,itemOffset) windows
        | isRelative
            # (exists,windows)  = hasWindowHandlesWindow (toWID relativeId) windows
            = (exists,itemPos,windows)
        | isRelativePrev
            # wsHs              = windows.whsWindows
              (widsstack,wsHs)  = unzip (map getWindowStateHandleWIDS wsHs)
              windows           = {windows & whsWindows=wsHs}
              (found,prevId)    = findPrevId (toWID id) widsstack
              itemLoc           = if (not found) itemLoc
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
                    | identifyWIDS wid wid``
                    = (True,wid`.wId)
                    = findPrevId wid [wid``:wids]
        // otherwise
            = (True,itemPos,windows)
    where
        (isRelative,relativeId) = case itemLoc of
                                    LeftOf  id  -> (True,id)
                                    RightTo id  -> (True,id)
                                    Above   id  -> (True,id)
                                    Below   id  -> (True,id)
                                    _           -> (False,undef)
        isRelativePrev          = case itemLoc of
                                    LeftOfPrev  -> True
                                    RightToPrev -> True
                                    AbovePrev   -> True
                                    BelowPrev   -> True
                                    _           -> False

getWindowPos :: !Id !(IOSt .l .p) -> (!Maybe ItemOffset,!IOSt .l .p)
getWindowPos id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # (wids,wsH)            = getWindowStateHandleWIDS wsH
    # ((x,y),ioState)       = accIOToolbox (OSgetWindowPos wids.wPtr) ioState
    = (Just {vx=x,vy=y},IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)


//  Operations that are concerned with the ViewFrame of a window.

moveWindowViewFrame :: !Id Vector !(IOSt .l .p) -> IOSt .l .p
moveWindowViewFrame id v ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (wsH,ioState)         = accIOToolbox (movewindowviewframe v wsH) ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    movewindowviewframe :: !Vector !(WindowStateHandle .ps) !*OSToolbox -> (!WindowStateHandle .ps,!*OSToolbox)
    movewindowviewframe v wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH}} tb
        # (wMetrics,tb)     = OSDefaultWindowMetrics tb
          contentRect       = getWindowContentRect wMetrics hasHScroll hasVScroll (SizeToRect wSize)
          contentSize       = RectSize contentRect
          (minx,maxx,viewx) = (domain.corner1.x,domain.corner2.x,contentSize.w)
          (miny,maxy,viewy) = (domain.corner1.y,domain.corner2.y,contentSize.h)
          newOrigin         = {x=SetBetween (oldOrigin.x+v.vx) minx (maxx-viewx),y=SetBetween (oldOrigin.y+v.vy) miny (maxy-viewy)}
          v                 = toVector (newOrigin-oldOrigin)
        | v==zero
        = (wsH,tb)
        # tb                = setsliderthumb hasHScroll wPtr True  (minx,newOrigin.x,maxx) viewx tb
        # tb                = setsliderthumb hasVScroll wPtr False (miny,newOrigin.y,maxy) viewy tb
        # (itemHs,tb)       = movecontrolspos (~v) wPtr zero wH.whItems tb
        # tb                = OSinvalidateWindow wPtr tb
          wH                = {wH & whItems=itemHs,whWindowInfo=XWindowInfo windowInfo}
          wH                = invalidateWindowClipState wH
        = ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
    where
        wPtr                = wshIds.wPtr
        wSize               = wH.whSize
         // Mike //
        (XWindowInfo windowInfo)            = wH.whWindowInfo
        domain              = windowInfo.windowDomain
        oldOrigin           = windowInfo.windowOrigin
        hasHScroll          = isJust windowInfo.windowHScroll
        hasVScroll          = isJust windowInfo.windowVScroll
         ///

        setsliderthumb :: !Bool OSWindowPtr Bool (Int,Int,Int) Int !*OSToolbox -> *OSToolbox
        setsliderthumb hasScroll wPtr isHScroll scrollValues viewSize tb
            | hasScroll
            = OSsetWindowSliderThumb wPtr isHScroll osThumb True tb
            = tb
        where
            (_,osThumb,_,_) = toOSscrollbarRange scrollValues viewSize
    movewindowviewframe _ _ _
        = StdWindowFatalError "moveWindowViewFrame" "unexpected window placeholder argument"

getWindowViewFrame :: !Id !(IOSt .l .p) -> (!ViewFrame,!IOSt .l .p)
getWindowViewFrame id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (zero,IOStSetDevice (WindowSystemState windows) ioState)
    # (viewFrame,wsH)       = getwindowviewframe wsH
    = (viewFrame,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
    getwindowviewframe :: !(WindowStateHandle .ps) -> (!ViewFrame,!WindowStateHandle .ps)
    getwindowviewframe wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
        | wKind==IsDialog
        = (SizeToRectangle wSize,wsH)
        = (PosSizeToRectangle origin wSize,wsH)
    where
        wSize   = wH.whSize
        wKind   = wH.whKind
         // Mike //
        origin  = (getWindowInfoWindowData wH.whWindowInfo).windowOrigin
         ///
    getwindowviewframe _
        = StdWindowFatalError "getWindowViewFrame" "unexpected window placeholder argument"

setWindowViewSize :: !Id Size !(IOSt .l .p) -> IOSt .l .p
setWindowViewSize id reqSize ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (wMetrics,ioState)    = accIOToolbox OSDefaultWindowMetrics ioState
      (diffSize,reqSize,wsH)= validateSize wMetrics reqSize wsH
    | not diffSize
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (wids,wsH)            = getWindowStateHandleWIDS wsH
    # (tb,ioState)          = getIOToolbox ioState
    # tb                    = OSsetWindowSize wids.wPtr (SizeToTuple reqSize) tb
    # (wsH,tb)              = windowStateSizeAction {wsWIDS=wids,wsSize=reqSize} wsH tb
    # ioState               = setIOToolbox tb ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    validateSize :: !OSWindowMetrics !Size !(WindowStateHandle .ps) -> (!Bool,!Size,!WindowStateHandle .ps)
    validateSize wMetrics reqSize wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
        # (visHScroll,visVScroll)
                            = OSscrollbarsAreVisible wMetrics (RectangleToRect domain) (SizeToTuple curSize) (hasHScroll,hasVScroll)
          newW              = if visVScroll (okSize.w+wMetrics.osmVSliderWidth)  okSize.w   // Correct newW in case of visible vertical   scrollbar
          newH              = if visHScroll (okSize.h+wMetrics.osmHSliderHeight) okSize.h   // Correct newH in case of visible horizontal scrollbar
        = (okSize<>curSize,{w=newW,h=newH},wsH)
    where
        (minWidth,minHeight)= OSMinWindowSize
        minSize             = getwindowminimumsize (snd (Select iswindowminimumsize (WindowMinimumSize {w=minWidth,h=minHeight}) wH.whAtts))
        maxSize             = maxScrollWindowSize
        okSize              = {w=SetBetween reqSize.w minSize.w maxSize.w,h=SetBetween reqSize.h minSize.h maxSize.h}
        curSize             = wH.whSize
        // Mike //
        windowInfo          = getWindowInfoWindowData wH.whWindowInfo
        ///
        domain              = windowInfo.windowDomain
        hasHScroll          = isJust windowInfo.windowHScroll
        hasVScroll          = isJust windowInfo.windowVScroll
    validateSize _ _ _
        = StdWindowFatalError "setWindowViewSize" "unexpected window placeholder argument"

getWindowViewSize :: !Id !(IOSt .l .p) -> (!Size,!IOSt .l .p)
getWindowViewSize id ioState
    # (viewFrame,ioState)   = getWindowViewFrame id ioState
    = (rectangleSize viewFrame,ioState)

setWindowViewDomain :: !Id ViewDomain !(IOSt .l .p) -> IOSt .l .p
setWindowViewDomain id newDomain ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
    # (wsH,ioState)         = accIOToolbox (setwindowviewdomain newDomain wsH) ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    setwindowviewdomain :: !ViewDomain !(WindowStateHandle .ps) !*OSToolbox -> (!WindowStateHandle .ps,!*OSToolbox)
    setwindowviewdomain domain wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH}} tb
        # domain            = validateViewDomain domain
          domainSize        = rectangleSize domain
          newOrigin         = { x = if (wSize.w>=domainSize.w) domain.corner1.x (SetBetween oldOrigin.x domain.corner1.x (domain.corner2.x-wSize.w))
                              , y = if (wSize.h>=domainSize.h) domain.corner1.y (SetBetween oldOrigin.y domain.corner1.y (domain.corner2.y-wSize.h))
                              }
          osHState          = toOSscrollbarRange (domain.corner1.x,newOrigin.x,domain.corner2.x) wSize.w
          osVState          = toOSscrollbarRange (domain.corner1.y,newOrigin.y,domain.corner2.y) wSize.h
        # tb                = setwindowslider hasHScroll wPtr True  osHState tb
        # tb                = setwindowslider hasVScroll wPtr False osVState tb
        # tb                = OSinvalidateWindow wPtr tb
          windowInfo        = XWindowInfo {windowInfo & windowDomain=domain,windowOrigin=newOrigin}
        | oldOrigin==newOrigin
            # wH            = {wH & whWindowInfo=windowInfo}
              wH            = invalidateWindowClipState wH
            = ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
        # v                 = toVector (newOrigin-oldOrigin)
        # (itemHs,tb)       = movecontrolspos (~v) wPtr zero wH.whItems tb
          wH                = {wH & whWindowInfo=windowInfo,whItems=itemHs}
          wH                = invalidateWindowClipState wH
        = ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
    where
        wPtr                = wshIds.wPtr
        wSize               = wH.whSize
        // Mike //
        windowInfo          = getWindowInfoWindowData wH.whWindowInfo
        ///
        oldOrigin           = windowInfo.windowOrigin
        hasHScroll          = isJust windowInfo.windowHScroll
        hasVScroll          = isJust windowInfo.windowVScroll

        setwindowslider :: !Bool OSWindowPtr Bool (Int,Int,Int,Int) !*OSToolbox -> *OSToolbox
        setwindowslider hasScroll wPtr isHorizontal state tb
            | hasScroll
            = OSsetWindowSlider wPtr isHorizontal state tb
            = tb
    setwindowviewdomain _ _ _
        = StdWindowFatalError "setWindowViewDomain" "unexpected window placeholder argument"

getWindowViewDomain :: !Id !(IOSt .l .p) -> (!Maybe ViewDomain,!IOSt .l .p)
getWindowViewDomain id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # (wKind,wsH)           = getWindowStateHandleWindowKind wsH
    | wKind<>IsWindow
    = (Nothing,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
    # (wInfo,wsH)           = getWindowStateHandleWindowInfo wsH
      // Mike //
      domain                = (getWindowInfoWindowData wInfo).windowDomain
      ///
    = (Just domain,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)


//  Operations that are concerned with remaining attributes of windows.

setWindowTitle :: !Id Title !(IOSt .l .p) -> IOSt .l .p
setWindowTitle id title ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wids,wsH)            = getWindowStateHandleWIDS wsH
    # ioState               = appIOToolbox (OSsetWindowTitle wids.wPtr title) ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState

setWindowOk :: !Id Id !(IOSt .l .p) -> IOSt .l .p
setWindowOk id okId ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wsH,ioState)         = accIOToolbox (setwindowok okId wsH) ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    setwindowok :: !Id !(WindowStateHandle .ps) !*OSToolbox -> (!WindowStateHandle .ps,!*OSToolbox)
    setwindowok okId wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}} tb
        = (wsH,tb)          // PA: Be aware to invalidate appropriate window/compound ClipStates
    setwindowok _ _ _
        = StdWindowFatalError "setWindowOk" "unexpected window placeholder argument"

setWindowCancel :: !Id Id !(IOSt .l .p) -> IOSt .l .p
setWindowCancel id cancelId ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wsH,ioState)         = accIOToolbox (setwindowcancel cancelId wsH) ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    setwindowcancel :: !Id !(WindowStateHandle .ps) !*OSToolbox -> (!WindowStateHandle .ps,!*OSToolbox)
    setwindowcancel cancelId wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}} tb
        = (wsH,tb)
    setwindowcancel _ _ _
        = StdWindowFatalError "setWindowCancel" "unexpected window placeholder argument"

setWindowCursor :: !Id CursorShape !(IOSt .l .p) -> IOSt .l .p
setWindowCursor id shape ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = IOStSetDevice (WindowSystemState windows) ioState
    # (wsH,ioState)         = accIOToolbox (setwindowcursor shape wsH) ioState
    = IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
    setwindowcursor :: !CursorShape !(WindowStateHandle .ps) !*OSToolbox -> (!WindowStateHandle .ps,!*OSToolbox)
    setwindowcursor shape wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}} tb
        # (_,_,atts)        = Remove iswindowcursor undef wH.whAtts
        = ({wsH & wshHandle=Just {wlsH & wlsHandle={wH & whAtts=[WindowCursor shape:atts]}}},tb)
    setwindowcursor _ _ _
        = StdWindowFatalError "setWindowCursor" "unexpected window placeholder argument"

getWindowTitle :: !Id !(IOSt .l .p) -> (!Maybe Title,!IOSt .l .p)
getWindowTitle id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # (title,wsH)           = getWindowStateHandleWindowTitle wsH
    = (Just title,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowOk :: !Id !(IOSt .l .p) -> (!Maybe Id,!IOSt .l .p)
getWindowOk id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # (okId,wsH)            = getWindowStateHandleDefaultId wsH
    = (okId,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowCancel :: !Id !(IOSt .l .p) -> (!Maybe Id,!IOSt .l .p)
getWindowCancel id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # (cancelId,wsH)        = getWindowStateHandleCancelId wsH
    = (cancelId,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)

getWindowCursor :: !Id !(IOSt .l .p) -> (!Maybe CursorShape,!IOSt .l .p)
getWindowCursor id ioState
    # (wDevice,ioState)     = IOStGetDevice WindowDevice ioState
      windows               = WindowSystemStateGetWindowHandles wDevice
      (found,wsH,windows)   = getWindowHandlesWindow (toWID id) windows
    | not found
    = (Nothing,IOStSetDevice (WindowSystemState windows) ioState)
    # (shape,wsH)           = getcursor wsH
    = (shape,IOStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState)
where
    getcursor :: !(WindowStateHandle .ps) -> (!Maybe CursorShape,!WindowStateHandle .ps)
    getcursor wsH=:{wshHandle=Just wlsH=:{wlsHandle={whAtts}}}
        = (if hasCursorAtt (Just (getwindowcursor cursorAtt)) Nothing,wsH)
    where
        (hasCursorAtt,cursorAtt)    = Select iswindowcursor undef whAtts
    getcursor _
        = StdWindowFatalError "getWindowCursor" "unexpected window placeholder argument"
