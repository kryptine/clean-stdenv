implementation module controlrelayout


//	Clean Object I/O library, version 1.2


import StdBool, StdList, StdMisc
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
relayoutControls :: !OSWindowMetrics !Bool !Bool !Rect !Rect !Point2 !Point2 !OSWindowPtr !(Maybe Id) ![WElementHandle`]
					 !*[WElementHandle .ls .pst] !*OSToolbox
	-> (!OSRgnHandle,!*[WElementHandle .ls .pst],!*OSToolbox)
relayoutControls wMetrics isAble isVisible oldFrame newFrame oldParentPos newParentPos wPtr defaultId oldHs newHs tb
	# oldRelayoutItems			= WElementHandles`ToRelayoutItems isAble isVisible oldHs []
	# (newHs,newRelayoutItems)	= WElementHandlesToRelayoutItems  isAble isVisible newHs []
	# (updRgn,tb)				= relayoutItems wMetrics oldFrame newFrame oldParentPos newParentPos wPtr oldRelayoutItems newRelayoutItems tb
	= (updRgn,newHs,tb)
where
	WElementHandlesToRelayoutItems :: !Bool !Bool ![WElementHandle .ls .pst] ![RelayoutItem] -> (![WElementHandle .ls .pst],![RelayoutItem])
	WElementHandlesToRelayoutItems isAble isVisible [itemH:itemHs] items
		# (itemHs,items)	= WElementHandlesToRelayoutItems isAble isVisible itemHs items
		# (itemH, items)	= WElementHandleToRelayoutItems  isAble isVisible itemH  items
		= ([itemH:itemHs],items)
	where
		WElementHandleToRelayoutItems :: !Bool !Bool !(WElementHandle .ls .pst) ![RelayoutItem] -> (!WElementHandle .ls .pst,![RelayoutItem])
		WElementHandleToRelayoutItems isAble isVisible (WItemHandle itemH=:{wItemKind}) items
			# (itemH,items)	= WItemHandleToRelayoutItems wItemKind isAble isVisible itemH items
			= (WItemHandle itemH,items)
		where
			WItemHandleToRelayoutItems :: !ControlKind !Bool !Bool !(WItemHandle .ls .pst) ![RelayoutItem] -> (!WItemHandle .ls .pst,![RelayoutItem])
			WItemHandleToRelayoutItems controlKind=:IsRadioControl isAble isVisible itemH=:{wItemSelect,wItemShow,wItemInfo} items
				= (itemH,RadioItemToRelayoutItems (isAble && wItemSelect) (isVisible && wItemShow) (getWItemRadioInfo wItemInfo).radioItems items)
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
				RadioItemToRelayoutItems _ _ _ _
					= []
			
			WItemHandleToRelayoutItems controlKind=:IsCheckControl isAble isVisible itemH=:{wItemSelect,wItemShow,wItemInfo} items
				= (itemH,CheckItemToRelayoutItems (isAble && wItemSelect) (isVisible && wItemShow) (getWItemCheckInfo wItemInfo).checkItems items)
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
				CheckItemToRelayoutItems _ _ _ _
					= []
			
			WItemHandleToRelayoutItems controlKind isAble isVisible itemH=:{wItemPtr,wItemPos,wItemSize,wItemSelect,wItemShow} items
				#! (info,look,items`,itemH)	= getinfo controlKind isAble` isVisible` itemH
				#! item						= {	rliItemKind		= controlKind
											  ,	rliItemPtr		= wItemPtr
											  ,	rliItemPos		= wItemPos
											  ,	rliItemSize		= wItemSize
											  ,	rliItemSelect	= isAble`
											  ,	rliItemShow		= isVisible`
											  ,	rliItemInfo		= info
											  ,	rliItemLook		= look
											  ,	rliItems		= items`
											  }
				= (itemH,[item:items])
			where
				isAble`						= isAble    && wItemSelect
				isVisible`					= isVisible && wItemShow
				
				getinfo :: !ControlKind !Bool !Bool !*(WItemHandle .ls .pst) -> (CompoundInfo,LookInfo,![RelayoutItem],!*WItemHandle .ls .pst)
				getinfo IsCompoundControl isAble isVisible itemH=:{wItemInfo,wItems=itemHs}
					# (itemHs,items)		= WElementHandlesToRelayoutItems isAble isVisible itemHs []
					= (info,info.compoundLookInfo.compoundLook,items,{itemH & wItems=itemHs})
				where
					info					= getWItemCompoundInfo wItemInfo
				getinfo IsCustomButtonControl _ _ itemH=:{wItemInfo}
					= (undef,(getWItemCustomButtonInfo wItemInfo).cButtonInfoLook,[],itemH)
				getinfo IsCustomControl _ _ itemH=:{wItemInfo}
					= (undef,(getWItemCustomInfo wItemInfo).customInfoLook,[],itemH)
				getinfo IsLayoutControl isAble isVisible itemH=:{wItems=itemHs}
					# (itemHs,items)		= WElementHandlesToRelayoutItems isAble isVisible itemHs []
					= (undef,undef,items,{itemH & wItems=itemHs})
				getinfo _ _ _ itemH
					= (undef,undef,[],itemH)
		
		WElementHandleToRelayoutItems isAble isVisible (WListLSHandle itemHs) items
			# (itemHs,items)	= WElementHandlesToRelayoutItems isAble isVisible itemHs items
			= (WListLSHandle itemHs,items)
		
		WElementHandleToRelayoutItems isAble isVisible (WExtendLSHandle wExH=:{wExtendItems=itemHs}) items
			# (itemHs,items)	= WElementHandlesToRelayoutItems isAble isVisible itemHs items
			= (WExtendLSHandle {wExH & wExtendItems=itemHs},items)
		
		WElementHandleToRelayoutItems isAble isVisible (WChangeLSHandle wChH=:{wChangeItems=itemHs}) items
			# (itemHs,items)	= WElementHandlesToRelayoutItems isAble isVisible itemHs items
			= (WChangeLSHandle {wChH & wChangeItems=itemHs},items)
	
	WElementHandlesToRelayoutItems _ _ [] items
		= ([],items)


relayoutControls` :: !OSWindowMetrics !Bool !Bool !Rect !Rect !Point2 !Point2 !OSWindowPtr !(Maybe Id) ![WElementHandle`] ![WElementHandle`] !*OSToolbox
																															-> (!OSRgnHandle,!*OSToolbox)
relayoutControls` wMetrics isAble isVisible oldFrame newFrame oldParentPos newParentPos wPtr defaultId oldHs newHs tb
	= relayoutItems wMetrics oldFrame newFrame oldParentPos newParentPos wPtr
					(WElementHandles`ToRelayoutItems isAble isVisible oldHs [])
					(WElementHandles`ToRelayoutItems isAble isVisible newHs [])
					tb

WElementHandles`ToRelayoutItems :: !Bool !Bool ![WElementHandle`] ![RelayoutItem] -> [RelayoutItem]
WElementHandles`ToRelayoutItems isAble isVisible [itemH:itemHs] items
	= WElementHandle`ToRelayoutItems isAble isVisible itemH (WElementHandles`ToRelayoutItems isAble isVisible itemHs items)
where
	WElementHandle`ToRelayoutItems :: !Bool !Bool !WElementHandle` ![RelayoutItem] -> [RelayoutItem]
	WElementHandle`ToRelayoutItems isAble isVisible (WItemHandle` itemH=:{wItemKind`}) items
		= WItemHandle`ToRelayoutItems wItemKind` isAble isVisible itemH items
	where
		WItemHandle`ToRelayoutItems :: !ControlKind !Bool !Bool !WItemHandle` ![RelayoutItem] -> [RelayoutItem]
		WItemHandle`ToRelayoutItems controlKind=:IsRadioControl isAble isVisible itemH=:{wItemSelect`,wItemShow`} items
			= RadioItem`ToRelayoutItems (isAble && wItemSelect`) (isVisible && wItemShow`) (getWItemRadioInfo` itemH.wItemInfo`).radioItems` items
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
		
		WItemHandle`ToRelayoutItems controlKind=:IsCheckControl isAble isVisible itemH=:{wItemSelect`,wItemShow`} items
			= CheckItem`ToRelayoutItems (isAble && wItemSelect`) (isVisible && wItemShow`) (getWItemCheckInfo` itemH.wItemInfo`).checkItems` items
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
		
		WItemHandle`ToRelayoutItems controlKind isAble isVisible itemH=:{wItemPtr`,wItemPos`,wItemSize`,wItemSelect`,wItemShow`} items
			#! item		=	{	rliItemKind		= controlKind
							,	rliItemPtr		= wItemPtr`
							,	rliItemPos		= wItemPos`
							,	rliItemSize		= wItemSize`
							,	rliItemSelect	= isAble`
							,	rliItemShow		= isVisible`
							,	rliItemInfo		= info
							,	rliItemLook		= look
							,	rliItems		= items`
							}
			= [item:items]
		where
			isAble`				= isAble    && wItemSelect`
			isVisible`			= isVisible && wItemShow`
			(info,look,items`)	= getinfo controlKind isAble` isVisible` itemH
			
			getinfo :: !ControlKind !Bool !Bool !WItemHandle` -> (CompoundInfo,LookInfo,![RelayoutItem])
			getinfo IsCompoundControl isAble isVisible {wItemInfo`,wItems`}
				= (info,info.compoundLookInfo.compoundLook,WElementHandles`ToRelayoutItems isAble isVisible wItems` [])
			where
				info	= getWItemCompoundInfo` wItemInfo`
			getinfo IsCustomButtonControl _ _ {wItemInfo`}
				= (undef,(getWItemCustomButtonInfo` wItemInfo`).cButtonInfoLook,[])
			getinfo IsCustomControl _ _ {wItemInfo`}
				= (undef,(getWItemCustomInfo` wItemInfo`).customInfoLook,[])
			getinfo IsLayoutControl isAble isVisible {wItems`}
				= (undef,undef,WElementHandles`ToRelayoutItems isAble isVisible wItems` [])
			getinfo _ _ _ _
				= (undef,undef,[])
	
	WElementHandle`ToRelayoutItems isAble isVisible (WRecursiveHandle` itemHs _) items
		= WElementHandles`ToRelayoutItems isAble isVisible itemHs items

WElementHandles`ToRelayoutItems _ _ _ items
	= items
