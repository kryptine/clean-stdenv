implementation module wstateaccess


//	Clean Object I/O library, version 1.2

import	StdInt, StdBool, StdList
import	commondef, wstate


//	Higher order access functions on [WElementHandle`]

setWElements :: ([arg] WItemHandle` .s -> ([arg],WItemHandle`,.s)) ![arg] ![WElementHandle`] !.s
															   -> (![arg],![WElementHandle`],!.s)
setWElements f args itemHs s
	| isEmpty args || isEmpty itemHs
		= (args,itemHs,s)
	| otherwise
		# (itemH,itemHs)	= HdTl itemHs
		# (args,itemH, s)	= setWElements` f args itemH  s
		# (args,itemHs,s)	= setWElements  f args itemHs s
		= (args,[itemH:itemHs],s)
where
	setWElements` :: ([arg] WItemHandle` .s -> ([arg],WItemHandle`,.s)) ![arg] !WElementHandle` !.s
																	-> (![arg],!WElementHandle`,!.s)
	setWElements` f args (WItemHandle` itemH) s
		# (args,itemH,s)	= f args itemH s
		= (args,WItemHandle` itemH,s)
	setWElements` f args (WRecursiveHandle` itemHs dRecKind) s
		# (args,itemHs,s)	= setWElements f args itemHs s
		= (args,WRecursiveHandle` itemHs dRecKind,s)

setAllWElements :: (WItemHandle` .s -> (WItemHandle`,.s)) ![WElementHandle`] !.s
													  -> (![WElementHandle`],!.s)
setAllWElements f [itemH:itemHs] s
	# (itemH, s)	= setWElement     f itemH  s
	# (itemHs,s)	= setAllWElements f itemHs s
	= ([itemH:itemHs],s)
where
	setWElement :: (WItemHandle` .s -> (WItemHandle`,.s)) !WElementHandle` !.s -> (!WElementHandle`,!.s)
	setWElement f (WItemHandle` itemH) s
		# (itemH,s)		= f itemH s
		= (WItemHandle` itemH,s)
	setWElement f (WRecursiveHandle` itemHs wRecKind) s
		# (itemHs,s)	= setAllWElements f itemHs s
		= (WRecursiveHandle` itemHs wRecKind,s)
setAllWElements _ _ s
	= ([],s)

setWElement :: (Id WItemHandle` .s -> (Bool,WItemHandle`,.s)) !Id ![WElementHandle`] !.s -> (!Bool,![WElementHandle`],!.s)
setWElement f id itemHs s
	| isEmpty itemHs
		= (False,itemHs,s)
	# (itemH,itemHs)		= HdTl itemHs
	# (done,itemH,s)		= setWElement` f id itemH  s
	| done
		= (done,[itemH:itemHs],s)
	| otherwise
		# (done,itemHs,s)	= setWElement  f id itemHs s
		= (done,[itemH:itemHs],s)
where
	setWElement` :: (Id WItemHandle` .s -> (Bool,WItemHandle`,.s)) !Id !WElementHandle` !.s -> (!Bool,!WElementHandle`,!.s)
	setWElement` f id (WItemHandle` itemH) s
		# (done,itemH,s)	= f id itemH s
		= (done,WItemHandle` itemH,s)
	setWElement` f id (WRecursiveHandle` itemHs dRecKind) s
		# (done,itemHs,s)	= setWElement f id itemHs s
		= (done,WRecursiveHandle` itemHs dRecKind,s)

setWItemHandle :: (WItemHandle` .s -> (Bool,WItemHandle`,.s)) ![WElementHandle`] !.s -> (!Bool,![WElementHandle`],!.s)
setWItemHandle f itemHs s
	| isEmpty itemHs
		= (False,itemHs,s)
	# (itemH,itemHs)		= HdTl itemHs
	# (done,itemH,s)		= setWItemHandle` f itemH  s
	| done
		= (done,[itemH:itemHs],s)
	| otherwise
		# (done,itemHs,s)	= setWItemHandle  f itemHs s
		= (done,[itemH:itemHs],s)
where
	setWItemHandle` :: (WItemHandle` .s -> (Bool,WItemHandle`,.s)) !WElementHandle` !.s -> (!Bool,!WElementHandle`,!.s)
	setWItemHandle` f (WItemHandle` itemH) s
		# (done,itemH,s)	= f itemH s
		= (done,WItemHandle` itemH,s)
	setWItemHandle` f (WRecursiveHandle` itemHs dRecKind) s
		# (done,itemHs,s)	= setWItemHandle f itemHs s
		= (done,WRecursiveHandle` itemHs dRecKind,s)


//	Determine the list of window items that can obtain the keyboard input focus.

getWElementKeyFocusIds` :: !Bool ![WElementHandle`] -> [FocusItem]
getWElementKeyFocusIds` shownContext [itemH:itemHs]
	= getWElementKeyFocusIds`` shownContext itemH++getWElementKeyFocusIds` shownContext itemHs
where
	getWElementKeyFocusIds`` :: !Bool !WElementHandle` -> [FocusItem]
	getWElementKeyFocusIds`` shownContext (WItemHandle` {wItemNr`,wItemKind`,wItemShow`,wItemAtts`,wItems`})
		| wItemKind`==IsEditControl	= focus
		| keySensitive && hasKeyAtt	= focus
		| otherwise					= getWElementKeyFocusIds` (shownContext && wItemShow`) wItems`
	where
		focus						= [{focusNr=wItemNr`,focusShow=shownContext}]
		hasKeyAtt					= Contains iscontrolkeyboard` wItemAtts`
		keySensitive				= wItemKind`==IsCustomControl
	getWElementKeyFocusIds`` shownContext (WRecursiveHandle` itemHs _)
		= getWElementKeyFocusIds` shownContext itemHs
getWElementKeyFocusIds` _ _
	= []


instance == WRecursiveKind where
	(==) :: !WRecursiveKind !WRecursiveKind -> Bool
	(==) IsWListLSHandle	wRecKind	= case wRecKind of
											IsWListLSHandle		-> True
											_					-> False
	(==) IsWExtendLSHandle	wRecKind	= case wRecKind of
											IsWExtendLSHandle	-> True
											_					-> False
	(==) IsWChangeLSHandle	wRecKind	= case wRecKind of
											IsWChangeLSHandle	-> True
											_					-> False


//	Access to the additional WItemInfo` field of a WItemHandle` (partial functions!).

getWItemRadioInfo` :: !WItemInfo` -> RadioInfo`
getWItemRadioInfo` (RadioInfo` info) = info

getWItemCheckInfo` :: !WItemInfo` -> CheckInfo`
getWItemCheckInfo` (CheckInfo` info) = info

getWItemPopUpInfo` :: !WItemInfo` -> PopUpInfo`
getWItemPopUpInfo` (PopUpInfo` info) = info

getWItemSliderInfo` :: !WItemInfo` -> SliderInfo`
getWItemSliderInfo` (SliderInfo` info) = info

getWItemTextInfo` :: !WItemInfo` -> TextInfo
getWItemTextInfo` (TextInfo` info) = info

getWItemEditInfo` :: !WItemInfo` -> EditInfo
getWItemEditInfo` (EditInfo` info) = info

getWItemButtonInfo` :: !WItemInfo` -> ButtonInfo
getWItemButtonInfo` (ButtonInfo` info) = info

getWItemCustomButtonInfo` :: !WItemInfo` -> CustomButtonInfo
getWItemCustomButtonInfo` (CustomButtonInfo` info) = info

getWItemCustomInfo` :: !WItemInfo` -> CustomInfo
getWItemCustomInfo` (CustomInfo` info) = info

getWItemCompoundInfo` :: !WItemInfo` -> CompoundInfo
getWItemCompoundInfo` (CompoundInfo` info) = info


//	General access rules for (Window/Control)Attribute`:

iswindowitemspace` :: !WindowAttribute` -> Bool
iswindowitemspace` (WindowItemSpace` _ _)			= True
iswindowitemspace` _								= False

iswindowhmargin` :: !WindowAttribute` -> Bool
iswindowhmargin` (WindowHMargin` _ _)				= True
iswindowhmargin` _									= False

iswindowvmargin` :: !WindowAttribute` -> Bool
iswindowvmargin` (WindowVMargin` _ _)				= True
iswindowvmargin` _									= False

getwindowhmargin` :: !WindowAttribute` -> (Int,Int)
getwindowhmargin` (WindowHMargin` l r)				= (l,r)

getwindowvmargin` :: !WindowAttribute` -> (Int,Int)
getwindowvmargin` (WindowVMargin` t b)				= (t,b)

getwindowitemspace` :: !WindowAttribute` -> (Int,Int)
getwindowitemspace` (WindowItemSpace` hspace vspace)= (hspace,vspace)


iscontrolid` :: !ControlAttribute` -> Bool
iscontrolid` (ControlId` _)		= True
iscontrolid` _					= False

iscontrolpos` :: !ControlAttribute` -> Bool
iscontrolpos` (ControlPos` _)	= True
iscontrolpos` _					= False

iscontrolviewsize` :: !ControlAttribute` -> Bool
iscontrolviewsize` (ControlViewSize` _)	= True
iscontrolviewsize` _					= False

iscontroloutersize` :: !ControlAttribute` -> Bool
iscontroloutersize` (ControlOuterSize` _)	= True
iscontroloutersize` _						= False

iscontrolminimumsize` :: !ControlAttribute` -> Bool
iscontrolminimumsize` (ControlMinimumSize` _)	= True
iscontrolminimumsize` _							= False

iscontrolresize` :: !ControlAttribute` -> Bool
iscontrolresize` (ControlResize` _)	= True
iscontrolresize` _					= False

iscontrolselectstate` :: !ControlAttribute` -> Bool
iscontrolselectstate` (ControlSelectState` _)	= True
iscontrolselectstate` _							= False

iscontrolkeyboard` :: !ControlAttribute` -> Bool
iscontrolkeyboard` (ControlKeyboard` _ )	= True
iscontrolkeyboard` _						= False

iscontrolitemspace` :: !ControlAttribute` -> Bool
iscontrolitemspace` (ControlItemSpace` _ _)	= True
iscontrolitemspace` _						= False

iscontrolhmargin` :: !ControlAttribute` -> Bool
iscontrolhmargin` (ControlHMargin` _ _)	= True
iscontrolhmargin` _						= False

iscontrolvmargin` :: !ControlAttribute` -> Bool
iscontrolvmargin` (ControlVMargin` _ _)	= True
iscontrolvmargin` _						= False

iscontrolhscroll` :: !ControlAttribute` -> Bool
iscontrolhscroll` (ControlHScroll` _)	= True
iscontrolhscroll` _						= False

iscontrolvscroll` :: !ControlAttribute` -> Bool
iscontrolvscroll` (ControlVScroll` _)	= True
iscontrolvscroll` _						= False


getcontrolid` :: !ControlAttribute` -> Id
getcontrolid` (ControlId` id)	= id

getcontrolpos` :: !ControlAttribute` -> ItemPos
getcontrolpos` (ControlPos` pos)	= pos

getcontrolviewsize` :: !ControlAttribute` -> Size
getcontrolviewsize` (ControlViewSize` size) = size

getcontroloutersize` :: !ControlAttribute` -> Size
getcontroloutersize` (ControlOuterSize` size) = size

getcontrolminimumsize` :: !ControlAttribute` -> Size
getcontrolminimumsize` (ControlMinimumSize` size) = size

getcontrolresize` :: !ControlAttribute` -> ControlResizeFunction
getcontrolresize` (ControlResize` f)	= f

getcontrolselectstate` :: !ControlAttribute` -> SelectState
getcontrolselectstate` (ControlSelectState` select) = select

getcontrolitemspace` :: !ControlAttribute` -> (Int,Int)
getcontrolitemspace` (ControlItemSpace` hspace vspace)	= (hspace,vspace)

getcontrolhmargin` :: !ControlAttribute` -> (Int,Int)
getcontrolhmargin` (ControlHMargin` l r)	= (l,r)

getcontrolvmargin` :: !ControlAttribute` -> (Int,Int)
getcontrolvmargin` (ControlVMargin` t b)	= (t,b)

getcontrolhscrollfunction` :: !ControlAttribute` -> ScrollFunction
getcontrolhscrollfunction` (ControlHScroll` f)	= f

getcontrolvscrollfunction` :: !ControlAttribute` -> ScrollFunction
getcontrolvscrollfunction` (ControlVScroll` f)	= f
