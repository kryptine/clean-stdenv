implementation module StdControlClass


//	Clean Object I/O library, version 1.2

//	Definition of the Controls class for controls.


import	StdBool, StdFunc, StdInt, StdList, StdMisc, StdTuple
import	commondef, controldefaccess, controlvalidate, id, iostate, StdControlDef, StdPSt, windowhandle, windowvalidate
import	ospicture, ossystem, oswindow


class Controls cdef where
	controlToHandles:: !(cdef .ls (PSt .l)) !(PSt .l)	-> (![ControlState .ls (PSt .l)],!PSt .l)
	getControlType	::  (cdef .ls .pst)					-> ControlType


/*	Translating control elements with local state into the internal representation.
	Note that no additional information is generated yet.
	Attributes that can be placed in the relevant record fields 
		wItemId		- ControlId
		wItemShow	- ControlHide
		wItemSelect	- ControlSelectState
		wItemLook	- ControlLook
		wItemInfo	- ControlDomain
	are removed from the attribute list. 
	The remaining attribute list is copied to wItemAtts.
*/

instance Controls (AddLS c) | Controls c where
	controlToHandles :: !(AddLS c .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)	| Controls c
	controlToHandles {addLS,addDef} pState
		# (cs,pState)	= controlToHandles addDef pState
		= (	[WElementHandleToControlState
				(WExtendLSHandle {	wExtendLS		= addLS
								 ,	wExtendItems	= map ControlStateToWElementHandle cs
								 }
				)
			]
		  ,	pState
		  )
	getControlType _
		= ""

instance Controls (NewLS c) | Controls c where
	controlToHandles :: !(NewLS c .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)	| Controls c
	controlToHandles {newLS,newDef} pState
		# (cs,pState)	= controlToHandles newDef pState
		= (	[WElementHandleToControlState
				(WChangeLSHandle {	wChangeLS		= newLS
								 ,	wChangeItems	= map ControlStateToWElementHandle cs
								 }
				)
			]
		  ,	pState
		  )
	getControlType _
		= ""

instance Controls (ListLS c) | Controls c where
	controlToHandles :: !(ListLS c .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)	| Controls c
	controlToHandles (ListLS cDefs) pState
		# (css,pState)	= StateMap controlToHandles cDefs pState
		= ([WElementHandleToControlState (WListLSHandle (map ControlStateToWElementHandle (flatten css)))],pState)
	getControlType _
		= ""

instance Controls NilLS where
	controlToHandles :: !(NilLS .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	controlToHandles NilLS pState
		= ([WElementHandleToControlState (WListLSHandle [])],pState)
	getControlType _
		= ""

instance Controls ((:+:) c1 c2)	| Controls c1 & Controls c2 where
	controlToHandles :: !((:+:) c1 c2 .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)	| Controls c1 & Controls c2
	controlToHandles (c1:+:c2) pState
		# (cs1,pState)	= controlToHandles c1 pState
		# (cs2,pState)	= controlToHandles c2 pState
		= (cs1++cs2,pState)
	getControlType _
		= ""

instance Controls RadioControl where
	controlToHandles :: !(RadioControl .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	controlToHandles (RadioControl items layout index atts) pState
		# (wMetrics, ioState)		= IOStGetOSWindowMetrics pState.io
		  (nrItems,items)			= Ulength items
		# (infoItems,ioState)		= StateMap (radioItemToInfo wMetrics) items ioState
		= (	[WElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsRadioControl
				,	wItemShow		= not (Contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= RadioInfo 
										{	radioItems = infoItems
										,	radioLayout= validateLayout nrItems layout
										,	radioIndex = SetBetween index 1 nrItems
										}
				,	wItemAtts		= filter (not o redundantAttribute) atts
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= zero
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		radioItemToInfo :: !OSWindowMetrics !(RadioControlItem *(.ls,PSt .l)) !(IOSt .l)
											-> (!RadioItemInfo *(.ls,PSt .l), ! IOSt .l)
		radioItemToInfo wMetrics (text,Just (PixelWidth reqW),f) ioState
			# wOK				= max (OSgetRadioControlItemMinWidth wMetrics) reqW
			# hOK				= OSgetRadioControlItemHeight wMetrics
			= ({radioItem=(text,wOK,f),radioItemSize={w=wOK,h=hOK},radioItemPos=zero,radioItemPtr=OSNoWindowPtr},ioState)
		radioItemToInfo wMetrics (text,Just (TextWidth wtext),f) ioState
			# (w,ioState)		= getDialogFontTextWidth wtext ioState
			  wOK				= max (OSgetRadioControlItemMinWidth wMetrics) w
			# hOK				= OSgetRadioControlItemHeight wMetrics
			= ({radioItem=(text,wOK,f),radioItemSize={w=wOK,h=hOK},radioItemPos=zero,radioItemPtr=OSNoWindowPtr},ioState)
		radioItemToInfo wMetrics (text,Just (ContentWidth wtext),f) ioState
			# ((w,hOK),ioState)	= accIOToolbox (OSgetRadioControlItemSize wMetrics wtext) ioState
			  wOK				= max (OSgetRadioControlItemMinWidth wMetrics) w
			= ({radioItem=(text,wOK,f),radioItemSize={w=wOK,h=hOK},radioItemPos=zero,radioItemPtr=OSNoWindowPtr},ioState)
		radioItemToInfo wMetrics (text,Nothing,f) ioState
			# ((w,hOK),ioState)	= accIOToolbox (OSgetRadioControlItemSize wMetrics text) ioState
			  wOK				= max (OSgetRadioControlItemMinWidth wMetrics) w
			= ({radioItem=(text,wOK,f),radioItemSize={w=wOK,h=hOK},radioItemPos=zero,radioItemPtr=OSNoWindowPtr},ioState)
	getControlType _
		= "RadioControl"

instance Controls CheckControl where
	controlToHandles :: !(CheckControl .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	controlToHandles (CheckControl items layout atts) pState
		# (wMetrics,ioState)		= IOStGetOSWindowMetrics pState.io
		  (nrItems,items)			= Ulength items
		# (infoItems,ioState)		= StateMap (checkItemToInfo wMetrics) items ioState
		= (	[WElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsCheckControl
				,	wItemShow		= not (Contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= CheckInfo
										{	checkItems = infoItems
										,	checkLayout= validateLayout nrItems layout
										}
				,	wItemAtts		= filter (not o redundantAttribute) atts
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= zero
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		checkItemToInfo :: !OSWindowMetrics !(CheckControlItem *(.ls,PSt .l)) !(IOSt .l)
											-> (!CheckItemInfo *(.ls,PSt .l), ! IOSt .l)
		checkItemToInfo wMetrics (text,Just (PixelWidth reqW),mark,f) ioState
			# wOK				= max (OSgetCheckControlItemMinWidth wMetrics) reqW
			# hOK				= OSgetCheckControlItemHeight wMetrics
			= ({checkItem=(text,wOK,mark,f),checkItemSize={w=wOK,h=hOK},checkItemPos=zero,checkItemPtr=OSNoWindowPtr},ioState)
		checkItemToInfo wMetrics (text,Just (TextWidth wtext),mark,f) ioState
			# (w,ioState)		= getDialogFontTextWidth wtext ioState
			  wOK				= max (OSgetCheckControlItemMinWidth wMetrics) w
			# hOK				= OSgetCheckControlItemHeight wMetrics
			= ({checkItem=(text,wOK,mark,f),checkItemSize={w=wOK,h=hOK},checkItemPos=zero,checkItemPtr=OSNoWindowPtr},ioState)
		checkItemToInfo wMetrics (text,Just (ContentWidth wtext),mark,f) ioState
			# ((w,hOK),ioState)	= accIOToolbox (OSgetCheckControlItemSize wMetrics wtext) ioState
			  wOK				= max (OSgetCheckControlItemMinWidth wMetrics) w
			= ({checkItem=(text,wOK,mark,f),checkItemSize={w=wOK,h=hOK},checkItemPos=zero,checkItemPtr=OSNoWindowPtr},ioState)
		checkItemToInfo wMetrics (text,Nothing,mark,f) ioState
			# ((w,hOK),ioState)	= accIOToolbox (OSgetCheckControlItemSize wMetrics text) ioState
			  wOK				= max (OSgetCheckControlItemMinWidth wMetrics) w
			= ({checkItem=(text,wOK,mark,f),checkItemSize={w=wOK,h=hOK},checkItemPos=zero,checkItemPtr=OSNoWindowPtr},ioState)
	getControlType _
		= "CheckControl"

instance Controls PopUpControl where
	controlToHandles :: !(PopUpControl .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	controlToHandles (PopUpControl popUpItems index atts) pState
		# (wMetrics,ioState)		= IOStGetOSWindowMetrics pState.io
		# (size,ioState)			= getPopUpSize wMetrics (map fst popUpItems) cWidth ioState
		# nrItems					= length popUpItems
		= (	[WElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsPopUpControl
				,	wItemShow		= not (Contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= PopUpInfo 
										{	popUpInfoItems = popUpItems
										,	popUpInfoIndex = validatePopUpIndex nrItems index
										,	popUpInfoEdit  = Nothing
										}
				,	wItemAtts		= filter (not o redundantAttribute) atts
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		cWidth						= getControlWidthAttribute atts
		
		getPopUpSize :: !OSWindowMetrics [String] !(Maybe ControlWidth) !(IOSt .l) -> (!Size,!IOSt .l)
		getPopUpSize wMetrics _ (Just (PixelWidth reqW)) ioState
			# wOK					= max (OSgetPopUpControlMinWidth wMetrics) reqW
			  hOK					= OSgetPopUpControlHeight wMetrics
			= ({w=wOK,h=hOK},ioState)
		getPopUpSize wMetrics _ (Just (TextWidth wtext)) ioState
			# (w,ioState)			= getDialogFontTextWidth wtext ioState
			  wOK					= max (OSgetPopUpControlMinWidth wMetrics) w
			  hOK					= OSgetPopUpControlHeight wMetrics
			= ({w=wOK,h=hOK},ioState)
		getPopUpSize wMetrics _ (Just (ContentWidth wtext)) ioState
			# ((w,hOK),ioState)		= accIOToolbox (OSgetPopUpControlSize wMetrics [wtext]) ioState
			  wOK					= max (OSgetPopUpControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
		getPopUpSize wMetrics itemtexts Nothing ioState
			# ((w,hOK),ioState)		= accIOToolbox (OSgetPopUpControlSize wMetrics (if (isEmpty itemtexts) ["MMMMMMMMMM"] itemtexts)) ioState
			  wOK					= max (OSgetPopUpControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
	getControlType _
		= "PopUpControl"

instance Controls SliderControl where
	controlToHandles :: !(SliderControl .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	controlToHandles (SliderControl direction cWidth sliderState action atts) pState
		# (wMetrics,ioState)		= IOStGetOSWindowMetrics pState.io
		# (size,ioState)			= getSliderSize wMetrics isHorizontal cWidth ioState
		= (	[WElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsSliderControl
				,	wItemShow		= not (Contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= SliderInfo 
				 						{	sliderInfoDir	= direction
				 						,	sliderInfoLength= if isHorizontal size.w size.h		// PA: maybe this field is now redundant
			 							,	sliderInfoState	= validateSliderState sliderState
			 							,	sliderInfoAction= action
			 							}
				,	wItemAtts		= filter (not o redundantAttribute) atts
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		isHorizontal				= direction == Horizontal
		
		getSliderSize :: !OSWindowMetrics !Bool !ControlWidth !(IOSt .l) -> (!Size,!IOSt .l)
		getSliderSize wMetrics isHorizontal (PixelWidth reqW) ioState
			# (wOK,hOK)				= OSgetSliderControlSize wMetrics isHorizontal reqW
			= ({w=wOK,h=hOK},ioState)
		getSliderSize wMetrics isHorizontal (TextWidth wtext) ioState
			# (w,ioState)			= getDialogFontTextWidth wtext ioState
			  (wOK,hOK)				= OSgetSliderControlSize wMetrics isHorizontal w
			= ({w=wOK,h=hOK},ioState)
		getSliderSize wMetrics isHorizontal (ContentWidth wtext) ioState
			# (w,ioState)			= getDialogFontTextWidth wtext ioState
			  (wOK,hOK)				= OSgetSliderControlSize wMetrics isHorizontal w
			= ({w=wOK,h=hOK},ioState)
	getControlType _
		= "SliderControl"

instance Controls TextControl where
	controlToHandles :: !(TextControl .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	controlToHandles (TextControl textLine atts) pState
		# (wMetrics,ioState)		= IOStGetOSWindowMetrics pState.io
		# (size,ioState)			= getTextSize wMetrics textLine cWidth ioState
		= (	[WElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsTextControl
				,	wItemShow		= not (Contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= TextInfo {textInfoText=textLine}
				,	wItemAtts		= filter (not o redundantAttribute) atts
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		cWidth						= getControlWidthAttribute atts
		
		getTextSize :: !OSWindowMetrics !String !(Maybe ControlWidth) !(IOSt .l) -> (!Size,!IOSt .l)
		getTextSize wMetrics _ (Just (PixelWidth reqW)) ioState
			# wOK					= max (OSgetTextControlMinWidth wMetrics) reqW
			  hOK					= OSgetTextControlHeight wMetrics
			= ({w=wOK,h=hOK},ioState)
		getTextSize wMetrics _ (Just (TextWidth wtext)) ioState
			# (w,ioState)			= getDialogFontTextWidth wtext ioState
			  wOK					= max (OSgetTextControlMinWidth wMetrics) w
			  hOK					= OSgetTextControlHeight wMetrics
			= ({w=wOK,h=hOK},ioState)
		getTextSize wMetrics _ (Just (ContentWidth wtext)) ioState
			# ((w,hOK),ioState)	= accIOToolbox (OSgetTextControlSize wMetrics wtext) ioState
			  wOK				= max (OSgetTextControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
		getTextSize wMetrics text Nothing ioState
			# ((w,hOK),ioState)	= accIOToolbox (OSgetTextControlSize wMetrics text) ioState
			  wOK				= max (OSgetTextControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
	getControlType _
		= "TextControl"

instance Controls EditControl where
	controlToHandles :: !(EditControl .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	controlToHandles (EditControl textLine cWidth nrLines atts) pState
		# (wMetrics,ioState)		= IOStGetOSWindowMetrics pState.io
		# (size,ioState)			= getEditSize wMetrics nrLines cWidth ioState
		= (	[WElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsEditControl
				,	wItemShow		= not (Contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= EditInfo
										{	editInfoText	= textLine
										,	editInfoWidth	= size.w			// PA: this field might have become redundant
										,	editInfoNrLines	= nrLines
										}
				,	wItemAtts		= filter (not o redundantAttribute) atts
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		getEditSize :: !OSWindowMetrics Int !ControlWidth !(IOSt .l) -> (!Size,!IOSt .l)
		getEditSize wMetrics nrLines (PixelWidth reqW) ioState
			# ((w,hOK),ioState)		= accIOToolbox (OSgetEditControlSize wMetrics reqW nrLines) ioState
			# wOK					= max (OSgetEditControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
		getEditSize wMetrics nrLines (TextWidth wtext) ioState
			# (w,ioState)			= getDialogFontTextWidth wtext ioState
			# ((w,hOK),ioState)		= accIOToolbox (OSgetEditControlSize wMetrics w nrLines) ioState
			  wOK					= max (OSgetEditControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
		getEditSize wMetrics nrLines _ ioState
			= ({w=100,h=OSgetEditControlHeight wMetrics nrLines},ioState)
	getControlType _
		= "EditControl"

instance Controls ButtonControl where
	controlToHandles :: !(ButtonControl .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	controlToHandles (ButtonControl textLine atts) pState
		# (wMetrics,ioState)		= IOStGetOSWindowMetrics pState.io
		# (size,ioState)			= getButtonSize wMetrics textLine cWidth ioState
		= (	[WElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsButtonControl
				,	wItemShow		= not (Contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= ButtonInfo {buttonInfoText=textLine}
				,	wItemAtts		= filter (not o redundantAttribute) atts
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		cWidth						= getControlWidthAttribute atts
		
		getButtonSize :: !OSWindowMetrics String !(Maybe ControlWidth) !(IOSt .l) -> (!Size,!IOSt .l)
		getButtonSize wMetrics _ (Just (PixelWidth reqW)) ioState
			# wOK					= max (OSgetButtonControlMinWidth wMetrics) reqW
			# hOK					= OSgetButtonControlHeight wMetrics
			= ({w=wOK,h=hOK},ioState)
		getButtonSize wMetrics _ (Just (TextWidth wtext)) ioState
			# (w,ioState)			= getDialogFontTextWidth wtext ioState
			  wOK					= max (OSgetButtonControlMinWidth wMetrics) w
			  hOK					= OSgetButtonControlHeight wMetrics
			= ({w=wOK,h=hOK},ioState)
		getButtonSize wMetrics _ (Just (ContentWidth wtext)) ioState
			# ((w,hOK),ioState)		= accIOToolbox (OSgetButtonControlSize wMetrics wtext) ioState
			  wOK					= max (OSgetButtonControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
		getButtonSize wMetrics text Nothing ioState
			# ((w,hOK),ioState)		= accIOToolbox (OSgetButtonControlSize wMetrics text) ioState
			  wOK					= max (OSgetButtonControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
	getControlType _
		= "ButtonControl"

instance Controls CustomButtonControl where
	controlToHandles :: !(CustomButtonControl .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	controlToHandles (CustomButtonControl {w,h} controlLook atts) pState
		# size	= {w=max 0 w,h=max 0 h}
		= (	[WElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsCustomButtonControl
				,	wItemShow		= not (Contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= CustomButtonInfo {cButtonInfoLook={lookFun=controlLook,lookPen=pen,lookSysUpdate=True}}
				,	wItemAtts		= filter (not o redundantAttribute) atts
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	pState
		  )
	where
		(_,pen)						= getInitialPen atts
	getControlType _
		= "CustomButtonControl"

instance Controls CustomControl where
	controlToHandles :: !(CustomControl .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	controlToHandles (CustomControl {w,h} controlLook atts) pState
		# size	= {w=max 0 w,h=max 0 h}
		= (	[WElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsCustomControl
				,	wItemShow		= not (Contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= CustomInfo {customInfoLook={lookFun=controlLook,lookPen=pen,lookSysUpdate=True}}
				,	wItemAtts		= filter (not o redundantAttribute) atts
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	pState
		  )
	where
		(_,pen)						= getInitialPen atts
	getControlType _
		= "CustomControl"

instance Controls (CompoundControl c)	| Controls c where
	controlToHandles :: !(CompoundControl c .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)	| Controls c
	controlToHandles (CompoundControl controls atts) pState
		# (cs,pState)	= controlToHandles controls pState
		= (	[WElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsCompoundControl
				,	wItemShow		= not (Contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= CompoundInfo
										{	compoundDomain	= RectangleToRect domain
										,	compoundOrigin	= origin
										,	compoundHScroll	= if hasHScroll (Just hScrollInfo) Nothing
										,	compoundVScroll	= if hasVScroll (Just vScrollInfo) Nothing
										,	compoundLookInfo= if (hasLook || hasPenAtts)
																(Just {compoundLook={	lookFun			= lookFun
																					,	lookPen			= pen
																					,	lookSysUpdate	= sysLook
																					}
																	  ,compoundClip={clipRgn=0,clipOk=False}
																	  }
																)
																Nothing
										}
				,	wItemAtts		= filter (not o redundantAttribute) atts
				,	wItems			= map ControlStateToWElementHandle cs
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= zero
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	pState
		  )
	where
		(hasHScroll,hScrollAtt)		= Select isControlHScroll undef atts
		(hasVScroll,vScrollAtt)		= Select isControlVScroll undef atts
		(hasLook,lookAtt)			= Select isControlLook (ControlLook True (\_ _ p->p)) atts
		(sysLook,lookFun)			= getControlLookAtt lookAtt
		defaultDomain				= ControlViewDomain {viewDomainRange & corner1=zero}
		(_,domainAtt)				= Select isControlViewDomain defaultDomain atts
		domain						= validateViewDomain (getControlViewDomainAtt domainAtt)
		(_,originAtt)				= Select isControlOrigin (ControlOrigin domain.corner1) atts
		origin						= validateOrigin domain (getControlOriginAtt originAtt)
		hScrollInfo					= {	scrollFunction	= getControlHScrollFun hScrollAtt
									  ,	scrollItemPos	= zero
									  ,	scrollItemSize	= zero
									  ,	scrollItemPtr	= OSNoWindowPtr
									  }
		vScrollInfo					= {	scrollFunction	= getControlVScrollFun vScrollAtt
									  ,	scrollItemPos	= zero
									  ,	scrollItemSize	= zero
									  ,	scrollItemPtr	= OSNoWindowPtr
									  }
		(hasPenAtts,pen)			= getInitialPen atts
	getControlType _
		= "CompoundControl"


//	Additional functions:

getDialogFontTextWidth :: !String !*env -> (!Int,!*env) | accScreenPicture env
getDialogFontTextWidth s env
	= accScreenPicture getTextWidth env
where
	getTextWidth :: !*Picture -> (!Int,!*Picture)
	getTextWidth picture
		# (dialogFont,picture)	= openDialogFont picture
		= getFontStringWidth dialogFont s picture

getIdAttribute :: ![ControlAttribute .st] -> Maybe Id
getIdAttribute atts
	| hasId			= Just (getControlIdAtt idAtt)
	| otherwise		= Nothing
where
	(hasId,idAtt)	= Select isControlId undef atts

getControlWidthAttribute :: ![ControlAttribute .st] -> Maybe ControlWidth
getControlWidthAttribute atts
	| hasControlWidth			= Just (getControlWidthAtt widthAtt)
	| otherwise					= Nothing
where
	(hasControlWidth,widthAtt)	= Select isControlWidth undef atts

getSelectStateAttribute :: ![ControlAttribute .st] -> Bool
getSelectStateAttribute atts
	= enabled (getControlSelectStateAtt (snd (Select isControlSelectState (ControlSelectState Able) atts)))

redundantAttribute :: !(ControlAttribute .st) -> Bool
redundantAttribute (ControlId _)			= True
redundantAttribute ControlHide				= True
redundantAttribute (ControlSelectState _)	= True
redundantAttribute (ControlLook _ _)		= True
redundantAttribute (ControlViewDomain _)	= True
redundantAttribute _						= False

getInitialPen :: ![ControlAttribute .st] -> (!Bool,!Pen)
getInitialPen atts
	= (hasPenAtts,pen)
where
	(hasPenAtts,penAttsAtt)	= Select isControlPen undef atts
	pen						= if hasPenAtts (StateMap2 setPenAttribute (reverse (getControlPenAtt penAttsAtt)) defaultPen) defaultPen

validateLayout :: !Int !RowsOrColumns -> RowsOrColumns
validateLayout nrItems (Rows    n) = Rows    (SetBetween n 1 nrItems)
validateLayout nrItems (Columns n) = Columns (SetBetween n 1 nrItems)

validatePopUpIndex :: !Int !Index -> Index
validatePopUpIndex nrItems index
	| IsBetween index 1 nrItems	= index
	| otherwise					= 1

validateOrigin :: !ViewDomain !Point2 -> Point2
validateOrigin domain origin
	= {	x=SetBetween origin.x domain.corner1.x domain.corner2.x
	  ,	y=SetBetween origin.y domain.corner1.y domain.corner2.y
	  }
