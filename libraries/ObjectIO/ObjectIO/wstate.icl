implementation module wstate


//	Clean Object I/O library, version 1.2


import	StdInt, StdList, StdTuple, StdFunc
import	oswindow
import	commondef, windowhandle, windowdefaccess, controldefaccess


wstateFatalError :: String String -> .x
wstateFatalError rule error
	= FatalError rule "wstate" error


/*	The WindowHandle` data type.
	This type is a subtype of the WindowHandle data type. The WindowHandle` data type 
	takes the projection of those fields of the (WindowHandle ls pst) data type that 
	do not depend on the type variables {ls,pst}.
*/
::	WindowHandle`
	=	{	whMode`				:: WindowMode					// The window mode (Modal or Modeless)
		,	whKind`				:: WindowKind					// The window kind (Window or Dialog)
		,	whTitle`			:: Title						// The window title
		,	whItemNrs`			:: [Int]						// The list of free system item numbers for all controls
		,	whKeyFocus`			:: KeyFocus						// The item that has the keyboard input focus
		,	whWindowInfo`		:: WindowInfo					// Additional information about the Window (Nothing for Dialogs)
		,	whItems`			:: [WElementHandle`]			// The window controls
		,	whShow`				:: Bool							// The visibility of the window (True iff visible)
		,	whSelect`			:: Bool							// The WindowSelect==Able (by default True)
		,	whAtts`				:: [WindowAttribute`]			// The window attributes
		,	whDefaultId`		:: Maybe Id						// The Id of the optional default button
		,	whCancelId`			:: Maybe Id						// The Id of the optional cancel  button
		,	whSize`				:: Size							// The exact size of the window
		,	whClosing`			:: Bool							// Flag: the window is being closed (True)
		}
::	WElementHandle`
	=	WItemHandle`			WItemHandle`
	|	WRecursiveHandle`		[WElementHandle`] WRecursiveKind
::	WRecursiveKind
	=	IsWListLSHandle
	|	IsWExtendLSHandle
	|	IsWChangeLSHandle
::	WItemHandle`
	=	{	wItemId`			:: Maybe Id						// If the control has a (ControlId id) attribute, then Just id; Nothing
		,	wItemNr`			:: Int							// The internal nr of this control  (generated from whIds)
		,	wItemKind`			:: ControlKind					// The sort of control
		,	wItemShow`			:: Bool							// The visibility of the control (True iff visible)
		,	wItemSelect`		:: Bool							// The ControlSelectState==Able  (by default True)
		,	wItemInfo`			:: WItemInfo`					// Additional information of the control
		,	wItemAtts`			:: [ControlAttribute`]			// The control attributes
		,	wItems`				:: [WElementHandle`]			// In case of	CompoundControl	: its control elements
																//				Otherwise		: []
		,	wItemVirtual`		:: Bool							// The control is virtual (True) and should not be layn out
		,	wItemPos`			:: !Point2						// The exact position of the item
		,	wItemSize`			:: Size							// The exact size of the item
		,	wItemPtr`			:: OSWindowPtr					// The ptr to the item (OSNoWindowPtr if no handle)
		,	wItemLayoutInfo`	:: LayoutInfo					// Additional information on layout
		}
::	WItemInfo`
	=	RadioInfo`				RadioInfo`						// In case of	RadioControl		: the radio items information
	|	CheckInfo`				CheckInfo`						// In case of	CheckControl		: the check items information
	|	PopUpInfo`				PopUpInfo`						// In case of	PopUpControl		: the pop up information
	|	SliderInfo`				SliderInfo`						// In case of	SliderControl		: the slider information
	|	TextInfo`				TextInfo						// In case of	TextControl			: the text information
	|	EditInfo`				EditInfo						// In case of	EditControl			: the edit text information
	|	ButtonInfo`				ButtonInfo						// In case of	ButtonControl		: the button information
	|	CustomButtonInfo`		CustomButtonInfo				// In case of	CustomButtonControl	: the custom button information
	|	CustomInfo`				CustomInfo						// In case of	CustomControl		: the custom information
	|	CompoundInfo`			CompoundInfo					// In case of	CompoundControl		: the compound control information
	|	NoWItemInfo`											// No additional information
::	RadioInfo`
	=	{	radioItems`			:: [RadioItemInfo`]				// The radio items and their exact position (initially zero)
		,	radioLayout`		:: RowsOrColumns				// The layout of the radio items
		,	radioIndex`			:: Int							// The currently selected radio item (1<=radioIndex<=length radioItems)
		}
::	RadioItemInfo`
	=	{	radioItem`			:: (String,Int)					// The text of the item
		,	radioItemPos`		:: !Point2						// The exact position of the item
		,	radioItemSize`		:: Size							// The exact size of the item
		,	radioItemPtr`		:: OSWindowPtr					// The OSWindowPtr of the item
		}
::	CheckInfo`
	=	{	checkItems`			:: [CheckItemInfo`]				// The check items and their exact position (initially zero)
		,	checkLayout`		:: RowsOrColumns				// The layout of the check items
		}
::	CheckItemInfo`
	=	{	checkItem`			:: (String,Int,MarkState)		// The text and mark of the item
		,	checkItemPos`		:: !Point2						// The exact position of the item
		,	checkItemSize`		:: Size							// The exact size of the item
		,	checkItemPtr`		:: OSWindowPtr					// The OSWindowPtr of the item
		}
::	PopUpInfo`
	=	{	popUpInfoItems`		:: [String]						// The pop up items
		,	popUpInfoIndex`		:: Index						// The currently selected pop up item (1<=popUpInfoIndex<=length popUpInfoItems)
		,	popUpInfoEdit`		:: Maybe PopUpEditInfo			// If the pop up is editable: the PopUpEditInfo, otherwise Nothing
		}
::	SliderInfo`
	=	{	sliderInfoDir`		:: Direction					// The direction of the slider
		,	sliderInfoLength`	:: Int							// The length (in pixels) of the slider
		,	sliderInfoState`	:: SliderState					// The current slider state
		}
::	WindowAttribute`
	=	WindowActivate`
	|	WindowCancel`		Id
	|	WindowClose`
	| 	WindowCursor`		CursorShape
	|	WindowDeactivate`
	|	WindowHMargin`		Int Int
	|	WindowHScroll`		ScrollFunction
	|	WindowId`			Id
	|	WindowIndex`		Int
	|	WindowInit`
	|	WindowInitActive`	Id
	|	WindowItemSpace`	Int Int
	|	WindowKeyboard`		SelectState
	|	WindowLook`			Bool Look
	|	WindowMouse`		SelectState
	|	WindowOk`			Id
	|	WindowOrigin`		Point2
	|	WindowPen`			[PenAttribute]
	|	WindowPos`			ItemPos
	|	WindowSelectState`	SelectState
	|	WindowViewDomain`	ViewDomain
	|	WindowViewSize`		Size
	|	WindowVMargin`		Int Int
	|	WindowVScroll`		ScrollFunction
::	ControlAttribute`
	=	ControlActivate`
	|	ControlDeactivate`
	|	ControlFunction`
	|	ControlHide`
	|	ControlHMargin`		Int Int
	|	ControlHScroll`		ScrollFunction
	|	ControlId`			Id
	|	ControlItemSpace`	Int Int
	|	ControlKeyboard`	SelectState
	|	ControlLook`		Bool Look
	|	ControlMinimumSize`	Size
	|	ControlModsFunction`
	|	ControlMouse`		SelectState
	|	ControlOrigin`		Point2
	|	ControlOuterSize`	Size
	|	ControlPen`			[PenAttribute]
	|	ControlPos`			ItemPos
	|	ControlResize`		ControlResizeFunction
	|	ControlSelectState`	SelectState
	|	ControlTip`			String
	|	ControlViewDomain`	ViewDomain
	|	ControlViewSize`	Size
	|	ControlVMargin`		Int Int
	|	ControlVScroll`		ScrollFunction
	|	ControlWidth`		ControlWidth


retrieveWindowHandle` :: !u:(WindowStateHandle .pst) !*OSToolbox -> (!WindowHandle`,!u:WindowStateHandle .pst,!*OSToolbox)
retrieveWindowHandle` wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle}} tb
	# (wH`,wH,tb)	= getWindowHandle` wPtr wlsHandle tb
	= (wH`,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
retrieveWindowHandle` _ _
	= wstateFatalError "retrieveWindowHandle`" "unexpected window placeholder argument"

insertWindowHandle` :: !WindowHandle` !u:(WindowStateHandle .pst) -> u:WindowStateHandle .pst
insertWindowHandle` wH` wsH=:{wshHandle=Just wlsH=:{wlsHandle}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle=setWindowHandle` wH` wlsHandle}}
insertWindowHandle` _ _
	= wstateFatalError "insertWindowHandle`" "unexpected window placeholder argument"

getWindowHandle` :: !OSWindowPtr !u:(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle`,!u:WindowHandle .ls .pst,!*OSToolbox)
getWindowHandle` wPtr wH=:{	whMode
						  ,	whKind
						  ,	whTitle
						  ,	whItemNrs
						  ,	whKeyFocus
						  ,	whWindowInfo
						  ,	whItems=items
						  ,	whShow
						  ,	whSelect
						  ,	whAtts
						  ,	whDefaultId
						  ,	whCancelId
						  ,	whSize
						  ,	whClosing
						  } tb
	#! (items`,items,tb)	= getWElementHandles` wPtr items tb
	= (	{	whMode`			= whMode
		,	whKind`			= whKind
		,	whTitle`		= whTitle
		,	whItemNrs`		= whItemNrs
		,	whKeyFocus`		= whKeyFocus
		,	whWindowInfo`	= whWindowInfo
		,	whItems`		= items`
		,	whShow`			= whShow
		,	whSelect`		= whSelect
		,	whAtts`			= map getWAtt whAtts
		,	whDefaultId`	= whDefaultId
		,	whCancelId`		= whCancelId
		,	whSize`			= whSize
		,	whClosing`		= whClosing
		}
	  ,	{wH & whItems=items}
	  ,	tb
	  )
where
	getWAtt :: !(WindowAttribute .st) -> WindowAttribute`
	getWAtt (WindowId          id)			= WindowId`          id
	getWAtt (WindowPos         pos)			= WindowPos`         pos
	getWAtt (WindowIndex       index)		= WindowIndex`       index
	getWAtt (WindowViewSize    size)		= WindowViewSize`    size
	getWAtt (WindowHMargin     l r)			= WindowHMargin`     l r
	getWAtt (WindowVMargin     t b)			= WindowVMargin`     t b
	getWAtt (WindowItemSpace   h v)			= WindowItemSpace`   h v
	getWAtt (WindowOk          id)			= WindowOk`          id
	getWAtt (WindowCancel      id)			= WindowCancel`      id
	getWAtt (WindowClose       _)			= WindowClose`
	getWAtt (WindowInit        _)			= WindowInit`
	getWAtt (WindowSelectState select)		= WindowSelectState` select
	getWAtt (WindowLook        sysLook look)= WindowLook`        sysLook look
	getWAtt (WindowPen         pen)			= WindowPen`         pen
	getWAtt (WindowViewDomain  domain)		= WindowViewDomain`  domain
	getWAtt (WindowOrigin      origin)		= WindowOrigin`      origin
	getWAtt (WindowHScroll     f)			= WindowHScroll`     f
	getWAtt (WindowVScroll     f)			= WindowVScroll`     f
	getWAtt (WindowActivate    _)			= WindowActivate`
	getWAtt (WindowDeactivate  _)			= WindowDeactivate`
	getWAtt (WindowInitActive  id)			= WindowInitActive`  id
	getWAtt (WindowMouse       _ select _)	= WindowMouse`       select
	getWAtt (WindowKeyboard    _ select _)	= WindowKeyboard`    select
	getWAtt (WindowCursor      shape)		= WindowCursor`      shape

getWElementHandles` :: !OSWindowPtr ![WElementHandle .ls .pst] !*OSToolbox -> (![WElementHandle`],![WElementHandle .ls .pst],!*OSToolbox)
getWElementHandles` wPtr [itemH:itemHs] tb
	#! (itemH`, itemH, tb)	= getWElementHandle`  wPtr itemH  tb
	#! (itemHs`,itemHs,tb)	= getWElementHandles` wPtr itemHs tb
	= ([itemH`:itemHs`],[itemH:itemHs],tb)
where
	getWElementHandle` :: !OSWindowPtr !(WElementHandle .ls .pst) !*OSToolbox -> (!WElementHandle`,!WElementHandle .ls .pst,!*OSToolbox)
	getWElementHandle` wPtr (WItemHandle itemH) tb
		#! (itemH`,itemH,tb)	= getWItemHandle` wPtr itemH tb
		= (WItemHandle` itemH`,WItemHandle itemH,tb)
	getWElementHandle` wPtr (WListLSHandle itemHs) tb
		#! (itemHs`,itemHs,tb)	= getWElementHandles` wPtr itemHs tb
		= (WRecursiveHandle` itemHs` IsWListLSHandle,WListLSHandle itemHs,tb)
	getWElementHandle` wPtr (WExtendLSHandle exH=:{wExtendItems=itemHs}) tb
		#! (itemHs`,itemHs,tb)	= getWElementHandles` wPtr itemHs tb
		= (WRecursiveHandle` itemHs` IsWExtendLSHandle,WExtendLSHandle {exH & wExtendItems=itemHs},tb)
	getWElementHandle` wPtr (WChangeLSHandle chH=:{wChangeItems=itemHs}) tb
		#! (itemHs`,itemHs,tb)	= getWElementHandles` wPtr itemHs tb
		= (WRecursiveHandle` itemHs` IsWChangeLSHandle,WChangeLSHandle {chH & wChangeItems=itemHs},tb)
getWElementHandles` _ _ tb
	= ([],[],tb)

getWItemHandle` :: !OSWindowPtr !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle`,!WItemHandle .ls .pst,!*OSToolbox)
getWItemHandle` wPtr itemH=:{	wItemId
							,	wItemNr
							,	wItemKind
							,	wItemShow
							,	wItemSelect
							,	wItemInfo
							,	wItemAtts
							,	wItems
							,	wItemVirtual
							,	wItemPos
							,	wItemSize
							,	wItemPtr
							,	wItemLayoutInfo
							} tb
	#! (itemHs`,itemHs,tb)	= getWElementHandles` wPtr wItems tb
	#! (info`,info,tb)		= getWItemInfo` wPtr wItemPtr wItemInfo tb
	= (	{	wItemId`		= wItemId
		,	wItemNr`		= wItemNr
		,	wItemKind`		= wItemKind
		,	wItemShow`		= wItemShow
		,	wItemSelect`	= wItemSelect
		,	wItemInfo`		= info`
		,	wItemAtts`		= map getWItemAtt` wItemAtts
		,	wItems`			= itemHs`
		,	wItemVirtual`	= wItemVirtual
		,	wItemPos`		= wItemPos
		,	wItemSize`		= wItemSize
		,	wItemPtr`		= wItemPtr
		,	wItemLayoutInfo`= wItemLayoutInfo
		}
	  ,	{itemH & wItems=itemHs,wItemInfo=info}
	  ,	tb
	  )
where
	getWItemAtt` :: !(ControlAttribute .st) -> ControlAttribute`
	getWItemAtt` (ControlId           id)			= ControlId`          id
	getWItemAtt` (ControlPos          pos)			= ControlPos`         pos
	getWItemAtt` (ControlViewSize     size)			= ControlViewSize`    size
	getWItemAtt` (ControlOuterSize    size)			= ControlOuterSize`   size
	getWItemAtt` (ControlMinimumSize  size)			= ControlMinimumSize` size
	getWItemAtt` (ControlWidth        width)		= ControlWidth`       width
	getWItemAtt` (ControlResize       f)			= ControlResize`      f
	getWItemAtt` (ControlSelectState  select)		= ControlSelectState` select
	getWItemAtt`  ControlHide						= ControlHide`
	getWItemAtt` (ControlFunction     _)			= ControlFunction`
	getWItemAtt` (ControlModsFunction _)			= ControlModsFunction`
	getWItemAtt` (ControlActivate     _)			= ControlActivate`
	getWItemAtt` (ControlDeactivate   _)			= ControlDeactivate`
	getWItemAtt` (ControlMouse        _ select _)	= ControlMouse`       select
	getWItemAtt` (ControlKeyboard     _ select _)	= ControlKeyboard`    select
	getWItemAtt` (ControlTip		  tip)			= ControlTip`		  tip
	getWItemAtt` (ControlPen          pen)			= ControlPen`         pen
	getWItemAtt` (ControlItemSpace    h v)			= ControlItemSpace`   h v
	getWItemAtt` (ControlHMargin      l r)			= ControlHMargin`     l r
	getWItemAtt` (ControlVMargin      t b)			= ControlVMargin`     t b
	getWItemAtt` (ControlLook         sysLook look)	= ControlLook`        sysLook look
	getWItemAtt` (ControlViewDomain   domain)		= ControlViewDomain`  domain
	getWItemAtt` (ControlOrigin       origin)		= ControlOrigin`      origin
	getWItemAtt` (ControlHScroll      f)			= ControlHScroll`     f
	getWItemAtt` (ControlVScroll      f)			= ControlVScroll`     f
	
	getWItemInfo` :: !OSWindowPtr !OSWindowPtr !(WItemInfo .ls .pst) !*OSToolbox -> !(WItemInfo`,!WItemInfo .ls .pst,!*OSToolbox)
	getWItemInfo` wPtr itemPtr info=:(RadioInfo {radioItems,radioLayout,radioIndex}) tb
		= (	RadioInfo` { radioItems`  = map getRadioInfo` radioItems
					   , radioLayout` = radioLayout
					   , radioIndex`  = radioIndex
					   }
		  ,	info
		  ,	tb
		  )
	where
		getRadioInfo` :: !(RadioItemInfo .st) -> RadioItemInfo`
		getRadioInfo` {radioItem=(text,width,_),radioItemPos,radioItemSize,radioItemPtr}
			= {	radioItem`     = (text,width)
			  ,	radioItemPos`  = radioItemPos
			  ,	radioItemSize` = radioItemSize
			  ,	radioItemPtr`  = radioItemPtr
			  }
	getWItemInfo` wPtr itemPtr info=:(CheckInfo {checkItems,checkLayout}) tb
		= (	CheckInfo` { checkItems`  = map getCheckInfo` checkItems
					   , checkLayout` = checkLayout
					   }
		  ,	info
		  ,	tb
		  )
	where
		getCheckInfo` :: !(CheckItemInfo .st) -> CheckItemInfo`
		getCheckInfo` {checkItem=(text,width,mark,_),checkItemPos,checkItemSize,checkItemPtr}
			= {	checkItem`     = (text,width,mark)
			  ,	checkItemPos`  = checkItemPos
			  ,	checkItemSize` = checkItemSize
			  ,	checkItemPtr`  = checkItemPtr
			  }
	getWItemInfo` wPtr itemPtr info=:(PopUpInfo {popUpInfoItems,popUpInfoIndex,popUpInfoEdit}) tb
		# (infoEdit,tb)	= getPopUpInfoEdit` popUpInfoEdit tb
		= (	PopUpInfo` { popUpInfoItems` = map fst popUpInfoItems
					   , popUpInfoIndex` = popUpInfoIndex
					   , popUpInfoEdit`  = infoEdit
					   }
		  ,	info
		  ,	tb
		  )
	where
		getPopUpInfoEdit` :: !(Maybe PopUpEditInfo) !*OSToolbox -> (!Maybe PopUpEditInfo,!*OSToolbox)
		getPopUpInfoEdit` Nothing tb
			= (Nothing,tb)
		getPopUpInfoEdit` (Just info=:{popUpEditPtr}) tb
			# (content,tb)	= OSgetEditControlText wPtr popUpEditPtr tb
			= (Just {info & popUpEditText=content},tb)
	getWItemInfo` wPtr itemPtr info=:(SliderInfo {sliderInfoDir,sliderInfoLength,sliderInfoState}) tb
		= (	SliderInfo` { sliderInfoDir`    = sliderInfoDir
						, sliderInfoLength` = sliderInfoLength
						, sliderInfoState`  = sliderInfoState
						}
		  ,	info
		  ,	tb
		  )
	getWItemInfo` wPtr itemPtr info=:(TextInfo textInfo) tb
		= (TextInfo` textInfo,info,tb)
	getWItemInfo` wPtr itemPtr info=:(EditInfo editInfo) tb
		# (content,tb)	= OSgetEditControlText wPtr itemPtr tb
		#! editInfo		= {editInfo & editInfoText=content}
		= (EditInfo` editInfo,info,tb)
	getWItemInfo` wPtr itemPtr info=:(ButtonInfo buttonInfo) tb
		= (ButtonInfo` buttonInfo,info,tb)
	getWItemInfo` wPtr itemPtr info=:(CustomButtonInfo customButtonInfo) tb
		= (CustomButtonInfo` customButtonInfo,info,tb)
	getWItemInfo` wPtr itemPtr info=:(CustomInfo customInfo) tb
		= (CustomInfo` customInfo,info,tb)
	getWItemInfo` wPtr itemPtr info=:(CompoundInfo compoundInfo) tb
		= (CompoundInfo` compoundInfo,info,tb)
	getWItemInfo` wPtr itemPtr info=:(ReceiverInfo _) tb
		= (NoWItemInfo`,info,tb)
	getWItemInfo` wPtr itemPtr info=:NoWItemInfo tb
		= (NoWItemInfo`,info,tb)

setWindowHandle` :: !WindowHandle` !u:(WindowHandle .ls .pst) -> u:WindowHandle .ls .pst
setWindowHandle` wH`=:{whTitle`,whItemNrs`,whKeyFocus`,whWindowInfo`,whItems`,whShow`,whSelect`,whAtts`,whSize`,whClosing`}
				 wH =:{whItems,whAtts}
	#! itemHs	= setWElementHandles` whItems` whItems
	#! atts		= setWAtts whAtts` whAtts
	= {	wH	& whTitle		= whTitle`
			, whItemNrs		= whItemNrs`
			, whKeyFocus	= whKeyFocus`
			, whWindowInfo	= whWindowInfo`
			, whItems		= itemHs
			, whShow		= whShow`
			, whSelect		= whSelect`
			, whAtts		= atts
			, whSize		= whSize`
			, whClosing		= whClosing`
	  }
where
	setWAtts :: ![WindowAttribute`] ![WindowAttribute .st] -> [WindowAttribute .st]
	setWAtts [att`:atts`] [att:atts]
		#! att	= setWAtt att` att
		#! atts	= setWAtts atts` atts
		= [att:atts]
	setWAtts [] []
		= []
	setWAtts _ _
		= wstateFatalError "setWindowHandle`" "incompatible number of WindowAttributes"
	
	setWAtt :: !WindowAttribute` !(WindowAttribute .st) -> WindowAttribute .st
	setWAtt (WindowId`          _)				att=:(WindowId           _)	= att
	setWAtt (WindowPos`         pos)			att=:(WindowPos          _)	= WindowPos         pos
	setWAtt (WindowIndex`       index)			att=:(WindowIndex        _)	= WindowIndex       index
	setWAtt (WindowViewSize`    size)			att=:(WindowViewSize     _)	= WindowViewSize    size
	setWAtt (WindowHMargin`     _ _)			att=:(WindowHMargin    _ _)	= att
	setWAtt (WindowVMargin`     _ _)			att=:(WindowVMargin    _ _)	= att
	setWAtt (WindowItemSpace`   _ _)			att=:(WindowItemSpace  _ _)	= att
	setWAtt (WindowOk`          okId)			att=:(WindowOk           _)	= WindowOk          okId
	setWAtt (WindowCancel`      cancelId)		att=:(WindowCancel       _)	= WindowCancel      cancelId
	setWAtt  WindowClose`						att=:(WindowClose        _)	= att
	setWAtt  WindowInit`						att=:(WindowInit         _)	= att
	setWAtt (WindowSelectState`	select)			att=:(WindowSelectState  _)	= WindowSelectState select
	setWAtt (WindowLook`        sysLook look)	att=:(WindowLook       _ _)	= WindowLook        sysLook look
	setWAtt (WindowPen`         pen)			att=:(WindowPen          _) = WindowPen         pen
	setWAtt (WindowViewDomain`  domain)			att=:(WindowViewDomain   _)	= WindowViewDomain  domain
	setWAtt (WindowOrigin`      origin)			att=:(WindowOrigin       _)	= WindowOrigin      origin
	setWAtt (WindowHScroll`     scroll)			att=:(WindowHScroll      _)	= WindowHScroll		scroll
	setWAtt (WindowVScroll`     scroll)			att=:(WindowVScroll      _)	= WindowVScroll     scroll
	setWAtt  WindowActivate`					att=:(WindowActivate     _)	= att
	setWAtt  WindowDeactivate`					att=:(WindowDeactivate   _)	= att
	setWAtt (WindowInitActive`  id)				att=:(WindowInitActive   _)	= WindowInitActive id
	setWAtt (WindowMouse`       select)			att=:(WindowMouse    s _ f)	= WindowMouse    s select f
	setWAtt (WindowKeyboard`    select)			att=:(WindowKeyboard s _ f)	= WindowKeyboard s select f
	setWAtt (WindowCursor`      cursor)			att=:(WindowCursor       _)	= WindowCursor      cursor
	setWAtt _ _
		= wstateFatalError "setWindowHandle`" "WindowAttributes do not match pairwise"
	
setWElementHandles` :: ![WElementHandle`] ![WElementHandle .ls .pst] -> [WElementHandle .ls .pst]
setWElementHandles` [itemH`:itemHs`] [itemH:itemHs]
	#! itemH	= setWElement`  itemH`  itemH
	#! itemHs	= setWElementHandles` itemHs` itemHs
	= [itemH:itemHs]
where
	setWElement` :: !WElementHandle` !(WElementHandle .ls .pst) -> WElementHandle .ls .pst
	setWElement` (WItemHandle` itemH`) (WItemHandle itemH)
		#! itemH	= setWItemHandle` itemH` itemH
		=  WItemHandle itemH
	setWElement` (WRecursiveHandle` itemHs` IsWListLSHandle) (WListLSHandle itemHs)
		#! itemHs	= setWElementHandles` itemHs` itemHs
		=  WListLSHandle itemHs
	setWElement` (WRecursiveHandle` itemHs` IsWExtendLSHandle) (WExtendLSHandle exH=:{wExtendItems=itemHs})
		#! itemHs	= setWElementHandles` itemHs` itemHs
		=  WExtendLSHandle {exH & wExtendItems=itemHs}
	setWElement` (WRecursiveHandle` itemHs` IsWChangeLSHandle) (WChangeLSHandle chH=:{wChangeItems=itemHs})
		#! itemHs	= setWElementHandles` itemHs` itemHs
		=  WChangeLSHandle {chH & wChangeItems=itemHs}
	setWElement` _ _
		= wstateFatalError "setWElementHandles`" "WElementHandles do not match pairwise"
setWElementHandles` [] []
	= []
setWElementHandles` _ _
	= wstateFatalError "setWElementHandles`" "incompatible number of WElementHandles"

setWItemHandle` :: !WItemHandle` !(WItemHandle .ls .pst) -> WItemHandle .ls .pst
setWItemHandle` itemH`=:{	wItemNr`
						,	wItemShow`
						,	wItemSelect`
						,	wItemInfo`
						,	wItemAtts`	= atts`
						,	wItems`		= itemHs`
						,	wItemVirtual`
						,	wItemPos`
						,	wItemSize`
						,	wItemLayoutInfo`
						}
				itemH =:{	wItemInfo	= info
						,	wItemAtts	= atts
						,	wItems		= itemHs
						}
	#! info1	= setWItemInfo` wItemInfo` info
	   atts1	= setWItemAtts` atts` atts
	   itemHs1	= setWElementHandles` itemHs` itemHs
	= {	itemH	& wItemNr			= wItemNr`
				, wItemShow			= wItemShow`
				, wItemSelect		= wItemSelect`
				, wItemInfo			= info1
				, wItemAtts			= atts1
				, wItems			= itemHs1
				, wItemVirtual		= wItemVirtual`
				, wItemPos			= wItemPos`
				, wItemSize			= wItemSize`
				, wItemLayoutInfo	= wItemLayoutInfo`
	  }
where
	setWItemAtts` :: ![ControlAttribute`] ![ControlAttribute .st] -> [ControlAttribute .st]
	setWItemAtts` [att`:atts`] [att:atts]
		#! att	= setWItemAtt` att` att
		#! atts	= setWItemAtts` atts` atts
		= [att:atts]
	setWItemAtts` [] []
		= []
	setWItemAtts` _ _
		= wstateFatalError "setWItemHandle`" "incompatible number of ControlAttributes"
	
	setWItemAtt` :: !ControlAttribute` !(ControlAttribute .st) -> ControlAttribute .st
	setWItemAtt` (ControlId`           _)				att=:(ControlId           _)	= att
	setWItemAtt` (ControlPos`          pos)				att=:(ControlPos          _)	= ControlPos  pos
	setWItemAtt` (ControlViewSize`     size)			att=:(ControlViewSize     _)	= ControlViewSize size
	setWItemAtt` (ControlOuterSize`    size)			att=:(ControlOuterSize    _)	= ControlOuterSize size
	setWItemAtt` (ControlMinimumSize`  _)				att=:(ControlMinimumSize  _)	= att
	setWItemAtt` (ControlWidth`        width)			att=:(ControlWidth        _)	= ControlWidth width
	setWItemAtt` (ControlResize`       f)				att=:(ControlResize       _)	= ControlResize f
	setWItemAtt` (ControlSelectState`  select)			att=:(ControlSelectState  _)	= ControlSelectState select
	setWItemAtt`  ControlHide`							att=: ControlHide				= att
	setWItemAtt`  ControlFunction`						att=:(ControlFunction     _)	= att
	setWItemAtt`  ControlModsFunction`					att=:(ControlModsFunction _)	= att
	setWItemAtt`  ControlActivate`						att=:(ControlActivate     _)	= att
	setWItemAtt`  ControlDeactivate`					att=:(ControlDeactivate   _)	= att
	setWItemAtt` (ControlMouse`        select)			att=:(ControlMouse    s _ f)	= ControlMouse    s select f
	setWItemAtt` (ControlKeyboard`     select)			att=:(ControlKeyboard s _ f)	= ControlKeyboard s select f
	setWItemAtt` (ControlTip`		   tip)				att=:(ControlTip		  _)	= ControlTip      tip
	setWItemAtt` (ControlPen`          pen)				att=:(ControlPen          _)	= ControlPen      pen
	setWItemAtt` (ControlItemSpace`    _ _)				att=:(ControlItemSpace  _ _)	= att
	setWItemAtt` (ControlHMargin`      _ _)				att=:(ControlHMargin    _ _)	= att
	setWItemAtt` (ControlVMargin`      _ _)				att=:(ControlVMargin    _ _)	= att
	setWItemAtt` (ControlLook`         sysLook look)	att=:(ControlLook       _ _)	= ControlLook       sysLook look
	setWItemAtt` (ControlViewDomain`   domain)			att=:(ControlViewDomain   _)	= ControlViewDomain domain
	setWItemAtt` (ControlOrigin`       origin)			att=:(ControlOrigin       _)	= ControlOrigin     origin
	setWItemAtt` (ControlHScroll`      scroll)			att=:(ControlHScroll      _)	= ControlHScroll    scroll
	setWItemAtt` (ControlVScroll`      scroll)			att=:(ControlVScroll      _)	= ControlVScroll    scroll
	setWItemAtt` att` att
		= wstateFatalError "setWItemHandle`"
			("ControlAttributes do not match pairwise ("+++toString att`+++"vs. "+++toString att+++")")
	
	setWItemInfo` :: !WItemInfo` !(WItemInfo .ls .pst) -> WItemInfo .ls .pst
	setWItemInfo` (RadioInfo` {radioItems`,radioIndex`}) (RadioInfo radio=:{radioItems,radioIndex})
		= RadioInfo {radio & radioItems=setRadioInfos radioItems` radioItems,radioIndex=radioIndex`}
	where
		setRadioInfos :: ![RadioItemInfo`] ![RadioItemInfo .st] -> [RadioItemInfo .st]
		setRadioInfos [info`:infos`] [info:infos]
			= [setRadioInfo info` info:setRadioInfos infos` infos]
		where
			setRadioInfo :: !RadioItemInfo` !(RadioItemInfo .st) -> RadioItemInfo .st
			setRadioInfo {radioItem`=(item`,s`),radioItemPos`,radioItemSize`} info=:{radioItem=(_,_,f)}
				= {info & radioItem=(item`,s`,f),radioItemPos=radioItemPos`,radioItemSize=radioItemSize`}
		setRadioInfos [] []
			= []
		setRadioInfos _ _
			= wstateFatalError "setWindowHandle`" "incompatible RadioInfo"
	setWItemInfo` (CheckInfo` {checkItems`}) (CheckInfo check=:{checkItems})
		= CheckInfo {check & checkItems=setCheckInfos checkItems` checkItems}
	where
		setCheckInfos :: ![CheckItemInfo`] ![CheckItemInfo .st] -> [CheckItemInfo .st]
		setCheckInfos [info`:infos`] [info:infos]
			= [setCheckInfo info` info:setCheckInfos infos` infos]
		where
			setCheckInfo :: !CheckItemInfo` !(CheckItemInfo .st) -> CheckItemInfo .st
			setCheckInfo {checkItem`=(text`,s`,mark`),checkItemPos`,checkItemSize`} info=:{checkItem=(_,_,_,f)}
				= {info & checkItem=(text`,s`,mark`,f),checkItemPos=checkItemPos`,checkItemSize=checkItemSize`}
		setCheckInfos [] []
			= []
		setCheckInfos _ _
			= wstateFatalError "setWindowHandle`" "incompatible CheckInfo"
	setWItemInfo` (PopUpInfo` {popUpInfoItems`=texts`,popUpInfoIndex`=i,popUpInfoEdit`}) (PopUpInfo popup=:{popUpInfoItems=items})
		= PopUpInfo {popup & popUpInfoItems=setpopuptexts texts` items,popUpInfoIndex=i,popUpInfoEdit=popUpInfoEdit`}
	where
		setpopuptexts :: ![String] ![PopUpControlItem .st] -> [PopUpControlItem .st]
		setpopuptexts [text:texts] [(_,f):items]
			= [(text,f):setpopuptexts texts items]
		setpopuptexts [] []
			= []
		setpopuptexts _ _
			= wstateFatalError "setWindowHandle`" "incompatible PopUpInfo"
	setWItemInfo` (SliderInfo` {sliderInfoDir`=dir,sliderInfoLength`=length,sliderInfoState`=state}) (SliderInfo slider)
		= SliderInfo {slider & sliderInfoDir=dir,sliderInfoLength=length,sliderInfoState=state}
	setWItemInfo` (TextInfo` info) (TextInfo _)
		= TextInfo info
	setWItemInfo` (EditInfo` info) (EditInfo _)
		= EditInfo info
	setWItemInfo` (ButtonInfo` info) (ButtonInfo _)
		= ButtonInfo info
	setWItemInfo` (CustomButtonInfo` info) (CustomButtonInfo _)
		= CustomButtonInfo info
	setWItemInfo` (CustomInfo` info) (CustomInfo _)
		= CustomInfo info
	setWItemInfo` (CompoundInfo` info) (CompoundInfo _)
		= CompoundInfo info
	setWItemInfo` NoWItemInfo` info
		= info
	setWItemInfo` _ _
		= wstateFatalError "setWindowHandle`" "incompatible WItemInfo"

instance toString ControlAttribute` where
	toString (ControlId`			_) = "ControlId`"
	toString (ControlPos`			_) = "ControlPos`"
	toString (ControlViewSize`		_) = "ControlViewSize`"
	toString (ControlOuterSize`     _) = "ControlOuterSize`"
	toString (ControlMinimumSize`	_) = "ControlMinimumSize`"
	toString (ControlWidth`         _) = "ControlWidth"
	toString (ControlResize`		_) = "ControlResize`"
	toString (ControlSelectState`	_) = "ControlSelectState`"
	toString  ControlHide`			   = "ControlHide`"
	toString  ControlFunction`		   = "ControlFunction`"
	toString  ControlModsFunction`	   = "ControlModsFunction`"
	toString  ControlActivate`         = "ControlActivate`"
	toString  ControlDeactivate`       = "ControlDeactivate`"
	toString (ControlMouse`			_) = "ControlMouse`"
	toString (ControlKeyboard`		_) = "ControlKeyboard`"
	toString (ControlTip`			_) = "ControlTip`"
	toString (ControlPen`           _) = "ControlPen`"
	toString (ControlItemSpace`	  _ _) = "ControlItemSpace`"
	toString (ControlHMargin`	  _ _) = "ControlHMargin`"
	toString (ControlVMargin`	  _ _) = "ControlVMargin`"
	toString (ControlLook`		  _	_) = "ControlLook`"
	toString (ControlViewDomain`	_) = "ControlViewDomain`"
	toString (ControlOrigin`		_) = "ControlOrigin`"
	toString (ControlHScroll`		_) = "ControlHScroll`"
	toString (ControlVScroll`		_) = "ControlVScroll`"
instance toString (ControlAttribute .st) where
	toString (ControlId				_) = "ControlId"
	toString (ControlPos			_) = "ControlPos"
	toString (ControlViewSize		_) = "ControlViewSize"
	toString (ControlMinimumSize	_) = "ControlMinimumSize"
	toString (ControlResize			_) = "ControlResize"
	toString (ControlSelectState	_) = "ControlSelectState"
	toString  ControlHide			   = "ControlHide"
	toString (ControlFunction		_) = "ControlFunction"
	toString (ControlModsFunction	_) = "ControlModsFunction"
	toString (ControlActivate       _) = "ControlActivate"
	toString (ControlDeactivate     _) = "ControlDeactivate"
	toString (ControlMouse		_ _ _) = "ControlMouse"
	toString (ControlKeyboard	_ _ _) = "ControlKeyboard"
	toString (ControlTip			_) = "ControlTip"
	toString (ControlPen            _) = "ControlPen"
	toString (ControlItemSpace	  _ _) = "ControlItemSpace"
	toString (ControlHMargin	  _ _) = "ControlHMargin"
	toString (ControlVMargin	  _ _) = "ControlVMargin"
	toString (ControlLook		  _	_) = "ControlLook"
	toString (ControlViewDomain		_) = "ControlViewDomain"
	toString (ControlOrigin			_) = "ControlOrigin"
	toString (ControlHScroll		_) = "ControlHScroll"
	toString (ControlVScroll		_) = "ControlVScroll"
