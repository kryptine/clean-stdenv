definition module wstateaccess


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Access operations on WItemHandle`
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


import	wstate


setWElements				:: (WItemHandle` *([arg],.s) -> (WItemHandle`,*([arg],.s)))    ![WElementHandle`] !*(![arg],!.s)
																					  -> *(![WElementHandle`],!*(![arg],!.s))
setAllWElements				:: (      WItemHandle` .s -> *(      WItemHandle`,.s))        ![WElementHandle`] !.s
																			  -> *(       ![WElementHandle`],!.s)
setWElement					:: (  Id  WItemHandle` .s -> *( Bool,WItemHandle`,.s))   !Id  ![WElementHandle`] !.s
																			  -> *(!Bool, ![WElementHandle`],!.s)
setWItemHandle				:: (      WItemHandle` .s -> *( Bool,WItemHandle`,.s))        ![WElementHandle`] !.s
																			  -> *(!Bool, ![WElementHandle`],!.s)
getWElementKeyFocusIds`		:: !Bool ![WElementHandle`] -> [FocusItem]


instance == WRecursiveKind


//	Access to the additional WItemInfo` field of a WItemHandle` (partial functions!).
getWItemRadioInfo`			:: !WItemInfo`			-> RadioInfo`
getWItemCheckInfo`			:: !WItemInfo`			-> CheckInfo`
getWItemPopUpInfo`			:: !WItemInfo`			-> PopUpInfo`
getWItemSliderInfo`			:: !WItemInfo`			-> SliderInfo`
getWItemTextInfo`			:: !WItemInfo`			-> TextInfo
getWItemEditInfo`			:: !WItemInfo`			-> EditInfo
getWItemButtonInfo`			:: !WItemInfo`			-> ButtonInfo
getWItemCustomButtonInfo`	:: !WItemInfo`			-> CustomButtonInfo
getWItemCustomInfo`			:: !WItemInfo`			-> CustomInfo
getWItemCompoundInfo`		:: !WItemInfo`			-> CompoundInfo


//	General functions on WindowAttribute`:
iswindowitemspace`			:: !WindowAttribute`	-> Bool
iswindowhmargin`			:: !WindowAttribute`	-> Bool
iswindowvmargin`			:: !WindowAttribute`	-> Bool
getwindowhmargin`			:: !WindowAttribute`	-> (Int,Int)
getwindowvmargin`			:: !WindowAttribute`	-> (Int,Int)
getwindowitemspace`			:: !WindowAttribute`	-> (Int,Int)


//	General functions on ControlAttribute`:
iscontrolid`				:: !ControlAttribute`	-> Bool
iscontrolpos`				:: !ControlAttribute`	-> Bool
iscontrolviewsize`			:: !ControlAttribute`	-> Bool
iscontroloutersize`			:: !ControlAttribute`	-> Bool
iscontrolminimumsize`		:: !ControlAttribute`	-> Bool
iscontrolresize`			:: !ControlAttribute`	-> Bool
iscontrolselectstate`		:: !ControlAttribute`	-> Bool
iscontrolkeyboard`			:: !ControlAttribute`	-> Bool
iscontrolitemspace`			:: !ControlAttribute`	-> Bool
iscontrolhmargin`			:: !ControlAttribute`	-> Bool
iscontrolvmargin`			:: !ControlAttribute`	-> Bool
iscontrolhscroll`			:: !ControlAttribute`	-> Bool
iscontrolvscroll`			:: !ControlAttribute`	-> Bool
getcontrolid`				:: !ControlAttribute`	-> Id
getcontrolpos`				:: !ControlAttribute`	-> ItemPos
getcontrolviewsize`			:: !ControlAttribute`	-> Size
getcontroloutersize`		:: !ControlAttribute`	-> Size
getcontrolminimumsize`		:: !ControlAttribute`	-> Size
getcontrolresize`			:: !ControlAttribute`	-> ControlResizeFunction
getcontrolselectstate`		:: !ControlAttribute`	-> SelectState
getcontrolitemspace`		:: !ControlAttribute`	-> (Int,Int)
getcontrolhmargin`			:: !ControlAttribute`	-> (Int,Int)
getcontrolvmargin`			:: !ControlAttribute`	-> (Int,Int)
getcontrolhscrollfunction`	:: !ControlAttribute`	-> ScrollFunction
getcontrolvscrollfunction`	:: !ControlAttribute`	-> ScrollFunction
