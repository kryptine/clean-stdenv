implementation module controlrelayout


//	Clean Object I/O library, version 1.2


import StdList, StdMisc
import relayout, windowaccess, windowclipstate, wstateaccess


/*	relayoutControls(`) wMetrics isAble oldFrame newFrame oldParentPos newParentPos parentPtr defaultId old new
	resizes, moves, and updates changed WElementHandle(`)s. 
		isAble							is True iff the parent window/compound is Able.
		oldFrame						is the clipping rect of the parent window/compound at the original location and size.
		newFrame						is the clipping rect of the parent window/compound at the new location and size.
		oldParentPos and newParentPos	are the positions of the respective parent window/compound of the elements.
		parentPtr						is the parent window/dialog.
		defaultId						is the optional Id of the default control.
		old								contains the elements at their original location and size.
		new								contains the elements at their new location and size.
	relayoutControls(`) assumes that the two lists contain elements that are identical except for size and position.
		If this is not the case, a runtime error will occur.
	relayoutControls(`) assumes that the ClipStates of all compound elements are valid.
	The return OSRgnHandle is the area of the window that requires to be updated (use updatewindowbackgrounds [windowupdate] for this purpose).
*/
relayoutControls :: !OSWindowMetrics !Bool !Rect !Rect !Point2 !Point2 !OSWindowPtr !(Maybe Id)
					![WElementHandle .ls .pst] ![WElementHandle .ls .pst] !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
relayoutControls wMetrics isAble oldFrame newFrame oldParentPos newParentPos wPtr defaultId oldHs newHs tb
	= relayoutItems wMetrics isAble oldFrame newFrame oldParentPos newParentPos wPtr
					(WElementHandlesToRelayoutItems oldHs [])
					(WElementHandlesToRelayoutItems newHs [])
					tb
where
	WElementHandlesToRelayoutItems :: ![WElementHandle .ls .pst] ![RelayoutItem] -> [RelayoutItem]
	WElementHandlesToRelayoutItems [itemH:itemHs] items
		= WElementHandleToRelayoutItems itemH (WElementHandlesToRelayoutItems itemHs items)
	where
		WElementHandleToRelayoutItems :: !(WElementHandle .ls .pst) ![RelayoutItem] -> [RelayoutItem]
		WElementHandleToRelayoutItems (WItemHandle itemH=:{wItemKind}) items
			= WItemHandleToRelayoutItems wItemKind itemH items
		where
			WItemHandleToRelayoutItems :: !ControlKind !(WItemHandle .ls .pst) ![RelayoutItem] -> [RelayoutItem]
			WItemHandleToRelayoutItems controlKind=:IsRadioControl itemH=:{wItemSelect,wItemShow} items
				= RadioItemToRelayoutItems wItemSelect wItemShow (getWItemRadioInfo itemH.wItemInfo).radioItems items
			where
				RadioItemToRelayoutItems :: !Bool !Bool ![RadioItemInfo *(.ls,.pst)] ![RelayoutItem] -> [RelayoutItem]
				RadioItemToRelayoutItems isAble isVisible [radio:radios] items
					#! item	= RadioItemInfoToRelayoutItem isAble isVisible radio
					=  [item:RadioItemToRelayoutItems isAble isVisible radios items]
				where
					RadioItemInfoToRelayoutItem :: !Bool !Bool !(RadioItemInfo .pst) -> RelayoutItem
					RadioItemInfoToRelayoutItem isAble isVisible {radioItemPos,radioItemSize,radioItemPtr}
						= {	rliItemKind		= controlKind
						  ,	rliItemPtr		= radioItemPtr
						  ,	rliItemPos		= radioItemPos
						  ,	rliItemSize		= radioItemSize
						  ,	rliItemSelect	= isAble
						  ,	rliItemShow		= isVisible
						  ,	rliItemInfo		= undef
						  ,	rliItemLook		= undef
						  ,	rliItems		= []
						  }
				RadioItemToRelayoutItems _ _ _ items
					= items
			
			WItemHandleToRelayoutItems controlKind=:IsCheckControl itemH=:{wItemSelect,wItemShow} items
				= CheckItemToRelayoutItems wItemSelect wItemShow (getWItemCheckInfo itemH.wItemInfo).checkItems items
			where
				CheckItemToRelayoutItems :: !Bool !Bool ![CheckItemInfo *(.ls,.pst)] ![RelayoutItem] -> [RelayoutItem]
				CheckItemToRelayoutItems isAble isVisible [check:checks] items
					#! item	= CheckItemInfoToRelayoutItem isAble isVisible check
					=  [item:CheckItemToRelayoutItems isAble isVisible checks items]
				where
					CheckItemInfoToRelayoutItem :: !Bool !Bool !(CheckItemInfo .pst) -> RelayoutItem
					CheckItemInfoToRelayoutItem isAble isVisible {checkItemPos,checkItemSize,checkItemPtr}
						= {	rliItemKind		= controlKind
						  ,	rliItemPtr		= checkItemPtr
						  ,	rliItemPos		= checkItemPos
						  ,	rliItemSize		= checkItemSize
						  ,	rliItemSelect	= isAble
						  ,	rliItemShow		= isVisible
						  ,	rliItemInfo		= undef
						  ,	rliItemLook		= undef
						  ,	rliItems		= []
						  }
				CheckItemToRelayoutItems _ _ _ items
					= items
			
			WItemHandleToRelayoutItems controlKind itemH=:{wItemPtr,wItemPos,wItemSize,wItemSelect,wItemShow} items
				#! item		=	{	rliItemKind		= controlKind
								,	rliItemPtr		= wItemPtr
								,	rliItemPos		= wItemPos
								,	rliItemSize		= wItemSize
								,	rliItemSelect	= wItemSelect
								,	rliItemShow		= wItemShow
								,	rliItemInfo		= info
								,	rliItemLook		= look
								,	rliItems		= items`
								}
				= [item:items]
			where
				(info,look,items`)	= getinfo controlKind itemH
				
				getinfo :: !ControlKind !(WItemHandle .ls .pst) -> (CompoundInfo,LookInfo,![RelayoutItem])
				getinfo IsCompoundControl {wItemInfo,wItems}
					= (info,look,WElementHandlesToRelayoutItems wItems [])
				where
					info	= getWItemCompoundInfo wItemInfo
					look	= case info.compoundLookInfo of
								Nothing		-> undef
								Just info	-> info.compoundLook
				getinfo IsCustomButtonControl {wItemInfo}
					= (undef,(getWItemCustomButtonInfo wItemInfo).cButtonInfoLook,[])
				getinfo IsCustomControl {wItemInfo}
					= (undef,(getWItemCustomInfo wItemInfo).customInfoLook,[])
				getinfo _ _
					= (undef,undef,[])
		
		WElementHandleToRelayoutItems (WListLSHandle itemHs) items
			= WElementHandlesToRelayoutItems itemHs items
		
		WElementHandleToRelayoutItems (WExtendLSHandle {wExtendItems=itemHs}) items
			= WElementHandlesToRelayoutItems itemHs items
		
		WElementHandleToRelayoutItems (WChangeLSHandle {wChangeItems=itemHs}) items
			= WElementHandlesToRelayoutItems itemHs items
	
	WElementHandlesToRelayoutItems _ items
		= items


relayoutControls` :: !OSWindowMetrics !Bool !Rect !Rect !Point2 !Point2 !OSWindowPtr !(Maybe Id)
					 ![WElementHandle`] ![WElementHandle`] !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
relayoutControls` wMetrics isAble oldFrame newFrame oldParentPos newParentPos wPtr defaultId oldHs newHs tb
	= relayoutItems wMetrics isAble oldFrame newFrame oldParentPos newParentPos wPtr
					(WElementHandles`ToRelayoutItems oldHs [])
					(WElementHandles`ToRelayoutItems newHs [])
					tb
where
	WElementHandles`ToRelayoutItems :: ![WElementHandle`] ![RelayoutItem] -> [RelayoutItem]
	WElementHandles`ToRelayoutItems [itemH:itemHs] items
		= WElementHandle`ToRelayoutItems itemH (WElementHandles`ToRelayoutItems itemHs items)
	where
		WElementHandle`ToRelayoutItems :: !WElementHandle` ![RelayoutItem] -> [RelayoutItem]
		WElementHandle`ToRelayoutItems (WItemHandle` itemH=:{wItemKind`}) items
			= WItemHandle`ToRelayoutItems wItemKind` itemH items
		where
			WItemHandle`ToRelayoutItems :: !ControlKind !WItemHandle` ![RelayoutItem] -> [RelayoutItem]
			WItemHandle`ToRelayoutItems controlKind=:IsRadioControl itemH=:{wItemSelect`,wItemShow`} items
				= RadioItem`ToRelayoutItems wItemSelect` wItemShow` (getWItemRadioInfo` itemH.wItemInfo`).radioItems` items
			where
				RadioItem`ToRelayoutItems :: !Bool !Bool ![RadioItemInfo`] ![RelayoutItem] -> [RelayoutItem]
				RadioItem`ToRelayoutItems isAble isVisible [radio:radios] items
					#! item	= RadioItemInfo`ToRelayoutItem isAble isVisible radio
					=  [item:RadioItem`ToRelayoutItems isAble isVisible radios items]
				where
					RadioItemInfo`ToRelayoutItem :: !Bool !Bool !RadioItemInfo` -> RelayoutItem
					RadioItemInfo`ToRelayoutItem isAble isVisible {radioItemPos`,radioItemSize`,radioItemPtr`}
						= {	rliItemKind		= controlKind
						  ,	rliItemPtr		= radioItemPtr`
						  ,	rliItemPos		= radioItemPos`
						  ,	rliItemSize		= radioItemSize`
						  ,	rliItemSelect	= isAble
						  ,	rliItemShow		= isVisible
						  ,	rliItemInfo		= undef
  						  ,	rliItemLook		= undef
						  ,	rliItems		= []
						  }
				RadioItem`ToRelayoutItems _ _ _ items
					= items
			
			WItemHandle`ToRelayoutItems controlKind=:IsCheckControl itemH=:{wItemSelect`,wItemShow`} items
				= CheckItem`ToRelayoutItems wItemSelect` wItemShow` (getWItemCheckInfo` itemH.wItemInfo`).checkItems` items
			where
				CheckItem`ToRelayoutItems :: !Bool !Bool ![CheckItemInfo`] ![RelayoutItem] -> [RelayoutItem]
				CheckItem`ToRelayoutItems isAble isVisible [check:checks] items
					#! item	= CheckItemInfo`ToRelayoutItem isAble isVisible check
					=  [item:CheckItem`ToRelayoutItems isAble isVisible checks items]
				where
					CheckItemInfo`ToRelayoutItem :: !Bool !Bool !CheckItemInfo` -> RelayoutItem
					CheckItemInfo`ToRelayoutItem isAble isVisible {checkItemPos`,checkItemSize`,checkItemPtr`}
						= {	rliItemKind		= controlKind
						  ,	rliItemPtr		= checkItemPtr`
						  ,	rliItemPos		= checkItemPos`
						  ,	rliItemSize		= checkItemSize`
						  ,	rliItemSelect	= isAble
						  ,	rliItemShow		= isVisible
						  ,	rliItemInfo		= undef
						  ,	rliItemLook		= undef
						  ,	rliItems		= []
						  }
				CheckItem`ToRelayoutItems _ _ _ items
					= items
			
			WItemHandle`ToRelayoutItems controlKind itemH=:{wItemPtr`,wItemPos`,wItemSize`,wItemSelect`,wItemShow`} items
				#! item		=	{	rliItemKind		= controlKind
								,	rliItemPtr		= wItemPtr`
								,	rliItemPos		= wItemPos`
								,	rliItemSize		= wItemSize`
								,	rliItemSelect	= wItemSelect`
								,	rliItemShow		= wItemShow`
								,	rliItemInfo		= info
								,	rliItemLook		= look
								,	rliItems		= items`
								}
				= [item:items]
			where
				(info,look,items`)	= getinfo controlKind itemH
				
				getinfo :: !ControlKind !WItemHandle` -> (CompoundInfo,LookInfo,![RelayoutItem])
				getinfo IsCompoundControl {wItemInfo`,wItems`}
					= (info,look,WElementHandles`ToRelayoutItems wItems` [])
				where
					info	= getWItemCompoundInfo` wItemInfo`
					look	= case info.compoundLookInfo of
								Nothing		-> undef
								Just info	-> info.compoundLook
				getinfo IsCustomButtonControl {wItemInfo`}
					= (undef,(getWItemCustomButtonInfo` wItemInfo`).cButtonInfoLook,[])
				getinfo IsCustomControl {wItemInfo`}
					= (undef,(getWItemCustomInfo` wItemInfo`).customInfoLook,[])
				getinfo _ _
					= (undef,undef,[])
		
		WElementHandle`ToRelayoutItems (WRecursiveHandle` itemHs _) items
			= WElementHandles`ToRelayoutItems itemHs items
	
	WElementHandles`ToRelayoutItems _ items
		= items
