implementation module windowaccess


//	Object I/O library, version 1.2

//	Access operations to Window(State)Handle(s).


import	StdBool, StdEnum, StdInt, StdList, StdMisc
import	oswindow
from	windowCrossCall_12	import CURSARROW, CURSBUSY, CURSCROSS, CURSFATCROSS, CURSHIDDEN, CURSIBEAM
import	commondef, devicesystemstate, windowhandle
from	StdControlAttribute	import isControlKeyboard
from	StdWindowAttribute	import isWindowInitActive, getWindowInitActiveAtt


windowaccessFatalError :: String String -> .x
windowaccessFatalError function error
	= FatalError function "windowaccess" error


//	Dummy values for window handles.

dummyWindowHandles :: WindowHandles .pst
dummyWindowHandles
	= {	whsWindows		= undef
//	  ,	whsCursorInfo	= undef
	  ,	whsNrWindowBound= undef
	  ,	whsModal		= undef
	  ,	whsFinalModalLS	= undef
	  }

dummyWindowStateHandle :: WindowStateHandle .pst
dummyWindowStateHandle
	= {	wshIds			= undef
	  ,	wshHandle		= undef
	  }

dummyWindowLSHandle :: WindowLSHandle .ls .pst
dummyWindowLSHandle
	= {	wlsState		= undef
	  ,	wlsHandle		= undef
	  }

dummyWindowHandle :: WindowHandle .ls .pst
dummyWindowHandle
	= {	whMode			= undef
	  ,	whKind			= undef
	  ,	whTitle			= undef
	  ,	whItemNrs		= undef
	  ,	whKeyFocus		= undef
	  ,	whWindowInfo	= undef
	  ,	whItems			= undef
	  ,	whShow			= undef
	  ,	whSelect		= undef
	  ,	whAtts			= undef
	  ,	whDefaultId		= undef
	  ,	whCancelId		= undef
	  ,	whSize			= undef
	  ,	whClosing		= undef
	  }

initWindowHandle :: !Title !WindowMode !WindowKind !WindowInfo ![WElementHandle .ls .pst] ![WindowAttribute *(.ls,.pst)] -> WindowHandle .ls .pst
initWindowHandle title mode kind info itemHs atts
	= {	whMode		= mode
	  ,	whKind		= kind
	  ,	whTitle		= title
	  ,	whItemNrs	= [1..]
	  ,	whKeyFocus	= {kfItem=Nothing,kfItems=[]}
//	  , whWindowInfo= Nothing	Mike: changed Nothing into argument
      , whWindowInfo= info
	  ,	whItems		= itemHs
	  ,	whShow		= True
	  ,	whSelect	= True
	  ,	whAtts		= atts
	  ,	whDefaultId	= Nothing
	  ,	whCancelId	= Nothing
	  ,	whSize		= zero
	  ,	whClosing	= False
	  }

// Mike //
/*  Access to the particular WindowInfo alternatives (partial functions!).
*/
getWindowInfoWindowData :: !WindowInfo -> WindowData
getWindowInfoWindowData (WindowInfo wData) = wData

getWindowInfoGameWindowData :: !WindowInfo -> GameWindowData
getWindowInfoGameWindowData (GameWindowInfo gwData) = gwData
///

//	Access to the additional WItemInfo field of a WItemHandle (partial functions!).

getWItemRadioInfo :: !(WItemInfo .ls .pst) -> RadioInfo *(.ls,.pst)
getWItemRadioInfo (RadioInfo info) = info

getWItemCheckInfo :: !(WItemInfo .ls .pst) -> CheckInfo *(.ls,.pst)
getWItemCheckInfo (CheckInfo info) = info

getWItemPopUpInfo :: !(WItemInfo .ls .pst) -> PopUpInfo *(.ls,.pst)
getWItemPopUpInfo (PopUpInfo info) = info

getWItemSliderInfo :: !(WItemInfo .ls .pst) -> SliderInfo *(.ls,.pst)
getWItemSliderInfo (SliderInfo info) = info

getWItemTextInfo :: !(WItemInfo .ls .pst) -> TextInfo
getWItemTextInfo (TextInfo info) = info

getWItemEditInfo :: !(WItemInfo .ls .pst) -> EditInfo
getWItemEditInfo (EditInfo info) = info

getWItemButtonInfo :: !(WItemInfo .ls .pst) -> ButtonInfo
getWItemButtonInfo (ButtonInfo info) = info

getWItemCustomButtonInfo :: !(WItemInfo .ls .pst) -> CustomButtonInfo
getWItemCustomButtonInfo (CustomButtonInfo info) = info

getWItemCustomInfo :: !(WItemInfo .ls .pst) -> CustomInfo
getWItemCustomInfo (CustomInfo info) = info

getWItemCompoundInfo :: !(WItemInfo .ls .pst) -> CompoundInfo
getWItemCompoundInfo (CompoundInfo info) = info

getWItemReceiverInfo :: !(WItemInfo .ls .pst) -> ReceiverHandle .ls .pst
getWItemReceiverInfo (ReceiverInfo info) = info


//	For internal identification of windows/dialogs Id and OSWindowPtr (Integer) can be used.

::	WID							// Identify a window/dialog either
	=	ById  !Id				// by its Id, or
	|	ByPtr !OSWindowPtr		// by its OSWindowPtr

class toWID x :: !x -> WID

instance toWID Id where
	toWID :: !Id -> WID
	toWID id = ById id
instance toWID Int where
	toWID :: !Int -> WID
	toWID wPtr = ByPtr wPtr
instance toWID WIDS where
	toWID :: !WIDS -> WID
	toWID {wPtr} = ByPtr wPtr

WIDbyId :: !WID -> Bool
WIDbyId (ById _)	= True
WIDbyId _			= False

WIDbyPtr :: !WID -> Bool
WIDbyPtr (ByPtr _)	= True
WIDbyPtr _			= False

WIDgetId :: !WID -> Id
WIDgetId (ById id)	= id

WIDgetPtr :: !WID -> OSWindowPtr
WIDgetPtr (ByPtr ptr) = ptr

//identifyWindowIds :: !WID !WIDS -> Bool
identifyWIDS :: !WID !WIDS -> Bool
identifyWIDS (ById  id)  {wId}  = id==wId
identifyWIDS (ByPtr ptr) {wPtr} = ptr==wPtr

identifyMaybeId :: !Id !(Maybe Id) -> Bool
identifyMaybeId id (Just id`) = id==id`
identifyMaybeId _ _ = False


//	Transforming CursorShape to OS cursor codes:
toCursorCode :: !CursorShape -> Int
toCursorCode StandardCursor	= CURSARROW
toCursorCode BusyCursor		= CURSBUSY
toCursorCode IBeamCursor	= CURSIBEAM
toCursorCode CrossCursor	= CURSCROSS
toCursorCode FatCrossCursor	= CURSFATCROSS
toCursorCode ArrowCursor	= CURSARROW
toCursorCode HiddenCursor	= CURSHIDDEN


//	Access operations on WindowStateHandles:

getWindowStateHandleWIDS :: !(WindowStateHandle .pst) -> (!WIDS,!WindowStateHandle .pst)
getWindowStateHandleWIDS wsH=:{wshIds}
	= (wshIds,wsH)

getWindowStateHandleWindowMode :: !(WindowStateHandle .pst) -> (!WindowMode,!WindowStateHandle .pst)
getWindowStateHandleWindowMode wsH=:{wshHandle=Just {wlsHandle={whMode}}}
	= (whMode,wsH)

getWindowStateHandleWindowKind :: !(WindowStateHandle .pst) -> (!WindowKind,!WindowStateHandle .pst)
getWindowStateHandleWindowKind wsH=:{wshHandle=Just {wlsHandle={whKind}}}
	= (whKind,wsH)

getWindowStateHandleWindowTitle :: !(WindowStateHandle .pst) -> (!Title,!WindowStateHandle .pst)
getWindowStateHandleWindowTitle wsH=:{wshHandle=Just {wlsHandle={whTitle}}}
	= (whTitle,wsH)

getWindowStateHandleItemNrs :: !(WindowStateHandle .pst) -> (![Int],!WindowStateHandle .pst)
getWindowStateHandleItemNrs wsH=:{wshHandle=Just {wlsHandle={whItemNrs}}}
	= (whItemNrs,wsH)

getWindowStateHandleKeyFocus :: !(WindowStateHandle .pst) -> (!KeyFocus,!WindowStateHandle .pst)
getWindowStateHandleKeyFocus wsH=:{wshHandle=Just {wlsHandle={whKeyFocus}}}
	= (whKeyFocus,wsH)

/* Mike
getWindowStateHandleWindowInfo :: !(WindowStateHandle .pst) -> (!Maybe WindowInfo,!WindowStateHandle .pst)
*/
getWindowStateHandleWindowInfo :: !(WindowStateHandle .pst) -> (!WindowInfo,!WindowStateHandle .pst)
getWindowStateHandleWindowInfo wsH=:{wshHandle=Just {wlsHandle={whWindowInfo}}}
	= (whWindowInfo,wsH)

getWindowStateHandleShow :: !(WindowStateHandle .pst) -> (!Bool,!WindowStateHandle .pst)
getWindowStateHandleShow wsH=:{wshHandle=Just {wlsHandle={whShow}}}
	= (whShow,wsH)

getWindowStateHandleSelect :: !(WindowStateHandle .pst) -> (!Bool,!WindowStateHandle .pst)
getWindowStateHandleSelect wsH=:{wshHandle=Just {wlsHandle={whSelect}}}
	= (whSelect,wsH)

getWindowStateHandleActive :: !(WindowStateHandle .pst) -> (!Bool,!WindowStateHandle .pst)
getWindowStateHandleActive wsH=:{wshIds={wActive}}
	= (wActive,wsH)

getWindowStateHandleDefaultId :: !(WindowStateHandle .pst) -> (!Maybe Id,!WindowStateHandle .pst)
getWindowStateHandleDefaultId wsH=:{wshHandle=Just {wlsHandle={whDefaultId}}}
	= (whDefaultId,wsH)

getWindowStateHandleCancelId :: !(WindowStateHandle .pst) -> (!Maybe Id,!WindowStateHandle .pst)
getWindowStateHandleCancelId wsH=:{wshHandle=Just {wlsHandle={whCancelId}}}
	= (whCancelId,wsH)

getWindowStateHandleSize :: !(WindowStateHandle .pst) -> (!Size,!WindowStateHandle .pst)
getWindowStateHandleSize wsH=:{wshHandle=Just {wlsHandle={whSize}}}
	= (whSize,wsH)

getWindowStateHandleClosing :: !(WindowStateHandle .pst) -> (!Bool,!WindowStateHandle .pst)
getWindowStateHandleClosing wsH=:{wshHandle=Just {wlsHandle={whClosing}}}
	= (whClosing,wsH)

isWindowStateHandlePlaceHolder :: !(WindowStateHandle .pst) -> (!Bool,!WindowStateHandle .pst)
isWindowStateHandlePlaceHolder wsH=:{wshHandle=Nothing}
	= (True,wsH)
isWindowStateHandlePlaceHolder wsH
	= (False,wsH)

identifyWindowStateHandle :: !WID !(WindowStateHandle .pst) -> (!Bool,!WindowStateHandle .pst)
identifyWindowStateHandle wid wsH
	# (wids,wsH)	= getWindowStateHandleWIDS wsH
	= (identifyWIDS wid wids,wsH)

setWindowStateHandleWindowTitle :: !Title !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleWindowTitle title wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whTitle=title}}}

setWindowStateHandleItemNrs :: ![Int] !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleItemNrs itemNrs wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItemNrs=itemNrs}}}

setWindowStateHandleKeyFocus :: !KeyFocus !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleKeyFocus keyFocus wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whKeyFocus=keyFocus}}}

/* Mike
setWindowStateHandleWindowInfo :: !(Maybe WindowInfo) !(WindowStateHandle .pst) -> WindowStateHandle .pst
*/
setWindowStateHandleWindowInfo :: !WindowInfo !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleWindowInfo windowInfo wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whWindowInfo=windowInfo}}}

setWindowStateHandleShow :: !Bool !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleShow show wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whShow=show}}}

setWindowStateHandleSelect :: !Bool !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleSelect select wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whSelect=select}}}

setWindowStateHandleActive :: !Bool !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleActive active wsH=:{wshIds=wids}
	= {wsH & wshIds={wids & wActive=active}}

setWindowStateHandleDefaultId :: !(Maybe Id) !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleDefaultId defaultId wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whDefaultId=defaultId}}}

setWindowStateHandleCancelId :: !(Maybe Id) !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleCancelId cancelId wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whCancelId=cancelId}}}

setWindowStateHandleSize :: !Size !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleSize size wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whSize=size}}}

setWindowStateHandleClosing :: !Bool !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleClosing closing wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whClosing=closing}}}


//	Search, get, and set WindowStateHandles.

getWindowHandlesActiveWindow :: !(WindowHandles .pst) -> (!Maybe WIDS,!WindowHandles .pst)
getWindowHandlesActiveWindow wHs=:{whsWindows=wsHs}
	# (found,wids,wsHs)	= Access get_active_wids undef wsHs
	  wHs				= {wHs & whsWindows=wsHs}
	| found				= (Just wids,wHs)
	| otherwise			= (Nothing,  wHs)
where
	get_active_wids :: !(WindowStateHandle .pst) -> (!(!Bool,WIDS),!WindowStateHandle .pst)
	get_active_wids wsH
		# (wids,wsH)	= getWindowStateHandleWIDS wsH
		= ((wids.wActive,wids),wsH)

//	getWindowHandlesActiveModalDialog assumes that all modal dialogues are at the front of the list
getWindowHandlesActiveModalDialog :: !(WindowHandles .pst) -> (!Maybe WIDS,!WindowHandles .pst)
getWindowHandlesActiveModalDialog wHs=:{whsWindows=wsHs}
	| isEmpty wsHs
		= (Nothing,wHs)
	# (wsH,wsHs)	= HdTl wsHs
	# (mode,wsH)	= getWindowStateHandleWindowMode wsH
	| mode<>Modal
		= (Nothing,{wHs & whsWindows=[wsH:wsHs]})
	| otherwise
		# (wids,wsH)= getWindowStateHandleWIDS wsH
		= (Just wids,{wHs & whsWindows=[wsH:wsHs]})

hasWindowHandlesWindow :: !WID !(WindowHandles .pst) -> (!Bool,!WindowHandles .pst)
hasWindowHandlesWindow wid wHs=:{whsWindows}
	# (found,windows)	= haswindow wid whsWindows
	= (found,{wHs & whsWindows=windows})
where
	haswindow :: !WID ![WindowStateHandle .pst] -> (!Bool,![WindowStateHandle .pst])
	haswindow wid [wsH:wsHs]
		# (wIds,wsH)		= getWindowStateHandleWIDS wsH
		| identifyWIDS wid wIds
			= (True, [wsH:wsHs])
		| otherwise
			# (found,wsHs)	= haswindow wid wsHs
			= (found,[wsH:wsHs])
	haswindow _ _
		= (False,[])

getWindowHandlesWindow :: !WID !(WindowHandles .pst) -> (!Bool,!WindowStateHandle .pst,!WindowHandles .pst)
getWindowHandlesWindow wid wHs=:{whsWindows}
	# (ok,wsH,wsHs)	= getwindow wid whsWindows
	= (ok,wsH,{wHs & whsWindows=wsHs})
where
	getwindow :: !WID ![WindowStateHandle .pst] -> (!Bool,!WindowStateHandle .pst,![WindowStateHandle .pst])
	getwindow wid [wsH:wsHs]
		# (wIds,wsH)	= getWindowStateHandleWIDS wsH
		| identifyWIDS wid wIds
			= (True, wsH, [{wshIds=wIds,wshHandle=Nothing}:wsHs])
		| otherwise
			# (found,wsH`,wsHs) = getwindow wid wsHs
			= (found,wsH`,[wsH:wsHs])
	getwindow _ _
		= (False,dummyWindowStateHandle,[])

removeWindowHandlesWindow :: !WID !(WindowHandles .pst) -> (!Bool,!WindowStateHandle .pst,!WindowHandles .pst)
removeWindowHandlesWindow wid wHs=:{whsWindows}
	# (ok,wsH,wsHs)	= URemove (identifyWindowStateHandle wid) dummyWindowStateHandle whsWindows
	= (ok,wsH,{wHs & whsWindows=wsHs})
where
	identifyWindowStateHandle :: !WID !(WindowStateHandle .pst) -> (!Bool,!WindowStateHandle .pst)
	identifyWindowStateHandle wid wsH
		# (windowIds,wsH)	= getWindowStateHandleWIDS wsH
		= (identifyWIDS wid windowIds,wsH)

setWindowHandlesWindow :: !(WindowStateHandle .pst) !(WindowHandles .pst) -> WindowHandles .pst
setWindowHandlesWindow wsH wHs=:{whsWindows}
	# (isPlaceHolder,wsH)	= isWindowStateHandlePlaceHolder wsH
	| isPlaceHolder
		= windowaccessFatalError "setWindowHandlesWindow" "WindowStateHandle argument should not be a place holder"
	| otherwise
		#  (wIds,wsH)		= getWindowStateHandleWIDS wsH
		#! wsHs				= setwindow wIds wsH whsWindows		// PA: strictness added
		=  {wHs & whsWindows=wsHs}
where
	setwindow :: !WIDS !(WindowStateHandle .pst) ![WindowStateHandle .pst] -> [WindowStateHandle .pst]
	setwindow wids wsH [wsH`:wsHs]
		# (wids`,wsH`)		= getWindowStateHandleWIDS wsH`
		| wids<>wids`
			#! wsHs			= setwindow wids wsH wsHs
			= [wsH`:wsHs]
		# (isPlaceHolder,_)	= isWindowStateHandlePlaceHolder wsH`
		| isPlaceHolder
			= [wsH:wsHs]
		| otherwise
			= windowaccessFatalError "setWindowHandlesWindow" "place holder expected instead of WindowStateHandle"
	setwindow _ _ _
		= windowaccessFatalError "setWindowHandlesWindow" "place holder not found"

addWindowHandlesWindow :: !Index !(WindowStateHandle .pst) !(WindowHandles .pst) -> WindowHandles .pst
addWindowHandlesWindow index wsH wHs=:{whsWindows}
	#! wsHs	= insert (max 0 index) wsH whsWindows
	= {wHs & whsWindows=wsHs}
where
	insert :: !Index !.x ![.x] -> [.x]
	insert 0 x ys
		= [x:ys]
	insert i x [y:ys]
		#! ys = insert (i-1) x ys
		= [y:ys]
	insert _ x _
		= [x]

addWindowHandlesActiveWindow :: !(WindowStateHandle .pst) !(WindowHandles .pst) -> WindowHandles .pst
addWindowHandlesActiveWindow wsH wHs=:{whsWindows}
	# (mode,wsH)	= getWindowStateHandleWindowMode wsH
	| mode==Modal
		= {wHs & whsWindows=[wsH:whsWindows]}
	| otherwise
		#! wsHs		= addBehindLastModal wsH whsWindows
		= {wHs & whsWindows=wsHs}
	with
		addBehindLastModal :: !(WindowStateHandle .pst) ![WindowStateHandle .pst] -> [WindowStateHandle .pst]
		addBehindLastModal wsH [wsH`:wsHs]
			# (mode`,wsH`)	= getWindowStateHandleWindowMode wsH`
			| mode`==Modal
				#! wsHs		= addBehindLastModal wsH wsHs
				= [wsH`:wsHs]
			| otherwise
				= [wsH,wsH`:wsHs]
		addBehindLastModal wsH _
			= [wsH]


/*	disableWindowSystem disables all current windows.
	The return WIDS is the WIDS of the topmost active modal dialogue, if present. 
*/
disableWindowSystem :: !(WindowHandles .pst) !*OSToolbox -> (!(!Maybe WIDS,!WindowHandles .pst),!*OSToolbox)
disableWindowSystem windows=:{whsModal,whsWindows} tb
	| not whsModal
		# (wHs,tb)	= StateMap disablewindow whsWindows tb
		= ((Nothing,{windows & whsModal=True,whsWindows=wHs}),tb)
	# (activeWIDS,windows)	= getWindowHandlesActiveWindow windows
	| isNothing activeWIDS
		= windowaccessFatalError "disableWindowSystem" "no active window found"
	| otherwise
		= ((activeWIDS,windows),OSdisableWindow (fromJust activeWIDS).wPtr (False,False) True tb)
where
	disablewindow :: !(WindowStateHandle .pst) !*OSToolbox -> (!WindowStateHandle .pst,!*OSToolbox)
	/* Mike
	disablewindow wsH tb
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		# (maybeWindowInfo,wsH)	= getWindowStateHandleWindowInfo wsH
		# scrollInfo			= case maybeWindowInfo of
									Nothing		-> (False,False)
									Just info	-> (isJust info.windowHScroll,isJust info.windowVScroll)
		= (wsH,OSdisableWindow wids.wPtr scrollInfo True tb)
	*/
	disablewindow wsH tb
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		# (windowInfo,wsH)	= getWindowStateHandleWindowInfo wsH
		# scrollInfo		= case windowInfo of
								WindowInfo info	-> (isJust info.windowHScroll,isJust info.windowVScroll)
								other			-> (False,False)
		= (wsH,OSdisableWindow wids.wPtr scrollInfo True tb)

/*	enableWindowSystem Nothing re-enables all current windows.
	enableWindowSystem (Just wids) re-enables the modal dialogue indicated by wids.
*/
enableWindowSystem :: !(Maybe WIDS) !(WindowHandles .pst) !*OSToolbox -> (!WindowHandles .pst,!*OSToolbox)
enableWindowSystem Nothing windows=:{whsWindows} tb
	# (wHs,tb)	= StateMap enablewindow whsWindows tb
	= ({windows & whsModal=False,whsWindows=wHs},tb)
where
	enablewindow :: !(WindowStateHandle .pst) !*OSToolbox -> (!WindowStateHandle .pst,!*OSToolbox)
	enablewindow wsH tb
		# (whSelect,wsH)		= getWindowStateHandleSelect wsH
		| not whSelect
			= (wsH,tb)
		| otherwise
			# (wids,wsH)		= getWindowStateHandleWIDS wsH
			/* Mike
			# (maybeWindowInfo,wsH)	= getWindowStateHandleWindowInfo wsH
			  scrollInfo			= case maybeWindowInfo of
			  							Nothing		-> (False,False)
			  							Just info	-> (isJust info.windowHScroll,isJust info.windowVScroll)
			*/
			# (windowInfo,wsH)	= getWindowStateHandleWindowInfo wsH
			  scrollInfo		= case windowInfo of
			  						WindowInfo info	-> (isJust info.windowHScroll,isJust info.windowVScroll)
			  						other			-> (False,False)
			= (wsH,OSenableWindow wids.wPtr scrollInfo True tb)
enableWindowSystem (Just wids) windows tb
	= (windows,OSenableWindow wids.wPtr (False,False) True tb)


/*	Checking WindowBounds:
*/
checkZeroWindowHandlesBound :: !(WindowHandles .pst) -> (!Bool,!WindowHandles .pst)
checkZeroWindowHandlesBound wHs=:{whsNrWindowBound}
	= (zeroBound whsNrWindowBound,wHs)

decreaseWindowHandlesBound :: !(WindowHandles .pst) -> WindowHandles .pst
decreaseWindowHandlesBound wHs=:{whsNrWindowBound}
	= {wHs & whsNrWindowBound=decBound whsNrWindowBound}


/*	Bind all free R(2)Ids that are contained in the WElementHandles.
	It assumes that it has already been checked that no R(2)Id is already bound in the ReceiverTable.
*/
bindReceiverControlIds :: !SystemId !Id ![WElementHandle .ls .pst] !ReceiverTable -> (![WElementHandle .ls .pst],!ReceiverTable)
bindReceiverControlIds ioId wId [itemH:itemHs] rt
	# (itemH, rt) = bindReceiverControlIds` ioId wId itemH rt
	# (itemHs,rt) = bindReceiverControlIds ioId wId itemHs rt
	= ([itemH:itemHs],rt)
where
	bindReceiverControlIds` :: !SystemId !Id !(WElementHandle .ls .pst) !ReceiverTable
										  -> (!WElementHandle .ls .pst, !ReceiverTable)
	bindReceiverControlIds` ioId wId (WItemHandle itemH=:{wItemKind,wItemInfo}) rt
		| not (isReceiverControl wItemKind)
			# (itemHs,rt1)	= bindReceiverControlIds ioId wId itemH.wItems rt
			  itemH1		= {itemH & wItems=itemHs}
			= (WItemHandle itemH1,rt1)
		| otherwise
			# recLoc		= {rlIOId=ioId,rlDevice=WindowDevice,rlParentId=wId,rlReceiverId=id}
			# rte			= {rteLoc=recLoc,rteSelectState=if itemH.wItemSelect Able Unable,rteASMCount=0}
			# (_,rt)		= addReceiverToReceiverTable rte rt
			= (WItemHandle itemH,rt)
	where
		rH					= getWItemReceiverInfo wItemInfo
		id					= rH.rId
		
		isReceiverControl :: !ControlKind -> Bool
		isReceiverControl (IsOtherControl type)	= type=="Receiver" || type=="Receiver2"
		isReceiverControl _						= False
	
	bindReceiverControlIds` ioId wId (WListLSHandle itemHs) rt
		# (itemHs,rt)	= bindReceiverControlIds ioId wId itemHs rt
		= (WListLSHandle itemHs,rt)
	
	bindReceiverControlIds` ioId wId (WExtendLSHandle wExH=:{wExtendItems}) rt
		# (itemHs,rt)	= bindReceiverControlIds ioId wId wExtendItems rt
		= (WExtendLSHandle {wExH & wExtendItems=itemHs},rt)
	
	bindReceiverControlIds` ioId wId (WChangeLSHandle wChH=:{wChangeItems}) rt
		# (itemHs,rt)	= bindReceiverControlIds ioId wId wChangeItems rt
		= (WChangeLSHandle {wChH & wChangeItems=itemHs},rt)

bindReceiverControlIds _ _ itemHs rt
	= (itemHs,rt)


/*	getInitActiveControl retrieves the OSWindowPtr of the control that has the initial input focus.
	It is assumed that the control identified by the WindowInitActive attribute exists.
*/
getInitActiveControl :: !(WindowHandle .ls .pst) -> (!OSWindowPtr,!WindowHandle .ls .pst)
getInitActiveControl wH=:{whItems=itemHs,whAtts}
	# (found,itemPtr,itemHs)			= getFocusWElementHandles initActiveId itemHs
	= (if found itemPtr OSNoWindowPtr,{wH & whItems=itemHs})
where
	(hasInitActiveAtt,initActiveAtt)	= Select isWindowInitActive undef whAtts
	initActiveId						= if hasInitActiveAtt (Just (getWindowInitActiveAtt initActiveAtt)) Nothing
	
	getFocusWElementHandles :: !(Maybe Id) ![WElementHandle .ls .pst] -> (!Bool,!OSWindowPtr,![WElementHandle .ls .pst])
	getFocusWElementHandles initActiveId [itemH:itemHs]
		# (found,itemPtr,itemH)		= getFocusWElementHandle initActiveId itemH
		| found
			= (found,itemPtr,[itemH:itemHs])
		| otherwise
			# (found,itemPtr,itemHs)= getFocusWElementHandles initActiveId itemHs
			= (found,itemPtr,[itemH:itemHs])
	where
		getFocusWElementHandle :: !(Maybe Id) !(WElementHandle .ls .pst) -> (!Bool,!OSWindowPtr,!WElementHandle .ls .pst)
		getFocusWElementHandle initActiveId (WItemHandle itemH=:{wItemId,wItemKind,wItemPtr,wItems})
			| isJust initActiveId && initActiveId==wItemId
				= (True,wItemPtr,WItemHandle itemH)
			| wItemKind==IsEditControl && isNothing initActiveId
				= (True,wItemPtr,WItemHandle itemH)
			| otherwise
				# (found,itemPtr,itemHs)	= getFocusWElementHandles initActiveId wItems
				= (found,itemPtr,WItemHandle {itemH & wItems=itemHs})
		getFocusWElementHandle initActiveId (WListLSHandle itemHs)
			# (found,itemPtr,itemHs)	= getFocusWElementHandles initActiveId itemHs
			= (found,itemPtr,WListLSHandle itemHs)
		getFocusWElementHandle initActiveId (WExtendLSHandle wExH=:{wExtendItems=itemHs})
			# (found,itemPtr,itemHs)	= getFocusWElementHandles initActiveId itemHs
			= (found,itemPtr,WExtendLSHandle {wExH & wExtendItems=itemHs})
		getFocusWElementHandle initActiveId (WChangeLSHandle wChH=:{wChangeItems=itemHs})
			# (found,itemPtr,itemHs)	= getFocusWElementHandles initActiveId itemHs
			= (found,itemPtr,WChangeLSHandle {wChH & wChangeItems=itemHs})
	getFocusWElementHandles _ _
		= (False,OSNoWindowPtr,[])


/*	Determine the list of window items that can obtain the keyboard input focus.
*/
getWElementKeyFocusIds :: !Bool ![WElementHandle .ls .pst] -> (![FocusItem],![WElementHandle .ls .pst])
getWElementKeyFocusIds shownContext [itemH:itemHs]
	# (ids1,itemH)	= getWElementKeyFocusIds` shownContext itemH
	  (ids2,itemHs)	= getWElementKeyFocusIds  shownContext itemHs
	= (ids1++ids2,[itemH:itemHs])
where
	getWElementKeyFocusIds` :: !Bool !(WElementHandle .ls .pst) -> (![FocusItem],!WElementHandle .ls .pst)
	getWElementKeyFocusIds` shownContext (WItemHandle itemH)
		# (ids,itemH)	= getWItemKeyFocusIds itemH
		= (ids,WItemHandle itemH)
	where
		getWItemKeyFocusIds :: !(WItemHandle .ls .pst) -> (![FocusItem],!WItemHandle .ls .pst)
		getWItemKeyFocusIds itemH=:{wItemNr,wItemKind,wItemShow,wItemAtts,wItems}
			| wItemKind==IsEditControl
				= (focus,itemH)
			| keySensitive && hasKeyAtt
				= (focus,itemH)
			| otherwise
				# (focus,itemHs)= getWElementKeyFocusIds (shownContext && wItemShow) wItems
				  itemH			= {itemH & wItems=itemHs}
				= (focus,itemH)
		where
			focus				= [{focusNr=wItemNr,focusShow=shownContext}]
			hasKeyAtt			= Contains isControlKeyboard wItemAtts
			keySensitive		= wItemKind==IsCustomControl
	
	getWElementKeyFocusIds` shownContext (WListLSHandle itemHs)
		# (ids,itemHs)	= getWElementKeyFocusIds shownContext itemHs
		= (ids,WListLSHandle itemHs)
	
	getWElementKeyFocusIds` shownContext (WExtendLSHandle wExH=:{wExtendItems=itemHs})
		# (ids,itemHs)	= getWElementKeyFocusIds shownContext itemHs
		= (ids,WExtendLSHandle {wExH & wExtendItems=itemHs})
	
	getWElementKeyFocusIds` shownContext (WChangeLSHandle wChH=:{wChangeItems=itemHs})
		# (ids,itemHs)	= getWElementKeyFocusIds shownContext itemHs
		= (ids,WChangeLSHandle {wChH & wChangeItems=itemHs})

getWElementKeyFocusIds _ _
	= ([],[])


/*	Generate internal numbers for all WElementHandles which wItemNr==0.
*/
genWElementItemNrs :: ![Int] ![WElementHandle .ls .pst] -> (![Int],![WElementHandle .ls .pst])
genWElementItemNrs nrs [itemH:itemHs]
	# (nrs,itemH)	= genWElementNrs  nrs itemH
	  (nrs,itemHs)	= genWElementItemNrs nrs itemHs
	= (nrs,[itemH:itemHs])
where
	genWElementNrs :: ![Int] !(WElementHandle .ls .pst) -> (![Int],!WElementHandle .ls .pst)
	genWElementNrs nrs wItemH=:(WItemHandle itemH=:{wItemNr,wItemKind,wItems})
		# (nrs,itemHs)	= genWElementItemNrs nrs wItems
		| wItemNr<>0
			= (nrs,WItemHandle {itemH & wItems=itemHs})
		| otherwise
			# (nr,nrs)	= HdTl nrs
			= (nrs,WItemHandle {itemH & wItemNr=nr,wItems=itemHs})
	
	genWElementNrs nrs (WListLSHandle itemHs)
		# (nrs,itemHs)	= genWElementItemNrs nrs itemHs
		= (nrs,WListLSHandle itemHs)
	
	genWElementNrs nrs (WExtendLSHandle wExH=:{wExtendItems=itemHs})
		# (nrs,itemHs)	= genWElementItemNrs nrs itemHs
		= (nrs,WExtendLSHandle {wExH & wExtendItems=itemHs})
	
	genWElementNrs nrs (WChangeLSHandle wChH=:{wChangeItems=itemHs})
		# (nrs,itemHs)	= genWElementItemNrs nrs itemHs
		= (nrs,WChangeLSHandle {wChH & wChangeItems=itemHs})

genWElementItemNrs nrs _
	= (nrs,[])


/*	Cast contains abc code because it can't be typed conventionally.
	The function Cast is required to break the Existential Type abstraction needed
	for retrieving the final modal dialog local state. This code should be replaced as soon as
	unique messages can be passed using receivers.
*/
Cast :: !.a -> .b
Cast a
	= code
		{
			pop_a 0
		}

getFinalModalLS :: !WID FinalModalLS -> Maybe .ls
getFinalModalLS wid {fmWIDS,fmLS}
	| identifyWIDS wid fmWIDS
		= Just (Cast fmLS)
	| otherwise
		= Nothing
