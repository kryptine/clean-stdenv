implementation module controlinternal


//	Clean Object I/O library, version 1.2


import	StdBool, StdEnum, StdList, StdMisc, StdTuple
import	osrgn, oswindow
from	ospicture			import defaultPen
from	ostooltip			import osAddControlToolTip
import	commondef, wstateaccess
from	StdControlAttribute	import isControlTip, getControlTipAtt, isControlKeyboard
from	controldraw			import drawCustomButtonLook`, drawCustomLook`, drawCompoundLook`,
									drawInCustomButton`,  drawInCustom`,   drawInCompound`
from	controllayout		import layoutControls`
from	controlrelayout		import relayoutControls`
from	controlvalidate		import validateControlTitle, validateSliderState
from	windowaccess		import identifyMaybeId, getWItemPopUpInfo, getWindowInfoWindowData, getCompoundContentRect, getWindowContentRect
from	windowclipstate		import validateCompoundClipState`, forceValidCompoundClipState`, invalidateCompoundClipState`
from	windowupdate		import updatewindowbackgrounds`
from	windowvalidate		import validateViewDomain


//	General occurrence tests on Id.

maybeRemoveCheck :: !(Maybe x) [x] -> (!Bool,![x])	| Eq x
maybeRemoveCheck (Just id) ids
	= removeCheck id ids
maybeRemoveCheck nothing ids
	= (False,ids)

removeOnIdOfPair :: !(Maybe Id) ![(Id,x)] -> (!Bool,(Id,x),![(Id,x)])
removeOnIdOfPair (Just id) id_args
	= remove (\(id`,_)->id`==id) undef id_args
removeOnIdOfPair nothing id_args
	= (False,undef,id_args)

removeOnIdOfTriple :: !(Maybe Id) ![(Id,x,y)] -> (!Bool,(Id,x,y),![(Id,x,y)])
removeOnIdOfTriple (Just id) id_args
	= remove (\(id`,_,_)->id`==id) undef id_args
removeOnIdOfTriple nothing id_args
	= (False,undef,id_args)


/*	getContentRect returns the content rect of the window. 
*/
getContentRect :: !OSWindowMetrics !WindowInfo !Size -> Rect
getContentRect wMetrics (WindowInfo wInfo) size
	= getWindowContentRect wMetrics visScrolls (sizeToRect size)
where
	domainRect	= wInfo.windowDomain
	hasScrolls	= (isJust wInfo.windowHScroll,isJust wInfo.windowVScroll)
	visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple size) hasScrolls
getContentRect _ (GameWindowInfo gwInfo) _
	= sizeToRect gwInfo.gamewindowSize
getContentRect _ NoWindowInfo size
	= sizeToRect size

/*	Calculate the intersection of the given Rect with the content of a CompoundControl.
*/
intersectRectContent :: !OSWindowMetrics !Rect !CompoundInfo !Point2 !Size -> Rect
intersectRectContent wMetrics clipRect info itemPos itemSize
	= intersectRects clipRect contentRect
where
	hasScrolls	= (isJust info.compoundHScroll,isJust info.compoundVScroll)
	domainRect	= info.compoundDomain
	itemRect	= posSizeToRect itemPos itemSize
	visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
	contentRect	= getCompoundContentRect wMetrics visScrolls itemRect


/*	Enable the controls and provide proper feedback.
	The [Id] argument contains the Ids of the controls that should be enabled.
	The Boolean argument controls the new SelectState. 
		If the Boolean argument is False, then SelectState is the new SelectState of the indicated controls.
		If the Boolean argument is True,  then SelectState is the new SelectState of the indicated controls 
										  and  all other controls. 
*/
enablecontrols :: ![Id] !Bool !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
enablecontrols ids overrule wMetrics wPtr wH=:{whItems`,whShow`,whSelect`,whSize`,whDefaultId`} tb
	# (itemHs,(_,tb))	= setAllWElements (enableWItemHandle` wMetrics wPtr whDefaultId` overrule whSelect` whShow` clipRect) whItems` (ids,tb)
	= ({wH & whItems`=itemHs},tb)
where
	clipRect			= getContentRect wMetrics wH.whWindowInfo` whSize`
	
	enableWItemHandle` :: !OSWindowMetrics !OSWindowPtr !(Maybe Id) !Bool !Bool !Bool !Rect !WItemHandle` !(![Id],!*OSToolbox)
																						-> (!WItemHandle`,!(![Id],!*OSToolbox))
	
	enableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH=:{wItemKind`} (ids,tb)
		| systemControl
			# (found,ids)	= maybeRemoveCheck itemH.wItemId` ids
			| found
				= ({itemH & wItemSelect`=True},(ids,systemOSAction contextSelect tb))
			| overrule
				= (itemH,(ids,systemOSAction (contextSelect && itemSelect) tb))
			// otherwise
				= (itemH,(ids,tb))
	where
		itemPtr		= itemH.wItemPtr`
		itemSelect	= itemH.wItemSelect`
		radioItems	= (getWItemRadioInfo` itemH.wItemInfo`).radioItems`
		checkItems	= (getWItemCheckInfo` itemH.wItemInfo`).checkItems`
		(systemControl,systemOSAction)
					= case wItemKind` of
						IsRadioControl	-> (True,\able->stateMap2 (\{radioItemPtr`}->osSetRadioControlSelect wPtr radioItemPtr` clipRect able) radioItems)
						IsCheckControl	-> (True,\able->stateMap2 (\{checkItemPtr`}->osSetCheckControlSelect wPtr checkItemPtr` clipRect able) checkItems)
  						IsPopUpControl	-> (True,osSetPopUpControlSelect  wPtr itemPtr clipRect)
  						IsSliderControl	-> (True,osSetSliderControlSelect wPtr itemPtr clipRect)
  						IsTextControl	-> (True,osSetTextControlSelect   wPtr itemPtr clipRect)
  						IsEditControl	-> (True,osSetEditControlSelect   wPtr itemPtr clipRect)
  						IsButtonControl	-> (True,osSetButtonControlSelect wPtr itemPtr clipRect)
  						_				-> (False,undef)
	
	enableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH=:{wItemKind`} (ids,tb)
		| customControl
			# (found,ids)	= maybeRemoveCheck itemH.wItemId` ids
			| found
				# itemH		= {itemH & wItemSelect`=True}
				# tb		= customOSAction contextSelect tb
				# (itemH,tb)= customDraw contextSelect wPtr clipRect itemH tb
				= (itemH,(ids,tb))
			| overrule
				# select	= contextSelect && itemSelect
				# (itemH,tb)= customDraw select wPtr clipRect itemH tb
				= (itemH,(ids,customOSAction select tb))
			// otherwise
				= (itemH,(ids,tb))
	where
		itemPtr				= itemH.wItemPtr`
		itemSelect			= itemH.wItemSelect`
		(customControl,customDraw,customOSAction)
							= case wItemKind` of
		  						IsCustomButtonControl	-> (True,drawCustomButtonLook`,osSetCustomButtonControlSelect wPtr itemPtr clipRect)
		  						IsCustomControl			-> (True,drawCustomLook`,      osSetCustomControlSelect       wPtr itemPtr clipRect)
		  						_						-> (False,undef,undef)
	
	enableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH=:{wItemKind`=IsCompoundControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| found
			# (itemH,tb)		= validateCompoundClipState` wMetrics False wPtr defId contextShow itemH tb
			# (itemH,tb)		= drawCompoundLook` wMetrics contextSelect wPtr clipRect1 itemH tb
			# (itemHs,(ids,tb))	= setAllWElements (enableWItemHandle` wMetrics wPtr defId True contextSelect contextShow1 clipRect1) itemH.wItems` (ids,tb)
			# tb				= osSetCompoundSelect wPtr itemPtr clipRect scrollInfo contextSelect tb
			  itemH				= {itemH & wItemSelect`=True,wItems`=itemHs}
			= (itemH,(ids,tb))
		| overrule
			# (itemH,tb)		= validateCompoundClipState` wMetrics False wPtr defId contextShow itemH tb
			# (itemH,tb)		= drawCompoundLook` wMetrics contextSelect1 wPtr clipRect1 itemH tb
			# (itemHs,(ids,tb))	= setAllWElements (enableWItemHandle` wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1) itemH.wItems` (ids,tb)
			# tb				= osSetCompoundSelect wPtr itemPtr clipRect scrollInfo contextSelect1 tb
			  itemH				= {itemH & wItems`=itemHs}
			= (itemH,(ids,tb))
		| otherwise
			# (itemHs,(ids,tb))	= setAllWElements (enableWItemHandle` wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1) itemH.wItems` (ids,tb)
			  itemH				= {itemH & wItems`=itemHs}
			= (itemH,(ids,tb))
	where
		itemPtr					= itemH.wItemPtr`
		itemSelect				= itemH.wItemSelect`
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		scrollInfo				= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		itemSize				= itemH.wItemSize`
		contextSelect1			= contextSelect && itemSelect
		contextShow1			= contextShow && itemH.wItemShow`
		clipRect1				= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemSize
	
	enableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH=:{wItemKind`=IsLayoutControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		  contextSelect1		= if found contextSelect (contextSelect && itemSelect)
		  (itemHs,(ids,tb))		= setAllWElements (enableWItemHandle` wMetrics wPtr defId (overrule || found) contextSelect1 contextShow1 clipRect1) itemH.wItems` (ids,tb)
		  itemH					= {itemH & wItemSelect`=found || itemSelect,wItems`=itemHs}
		= (itemH,(ids,tb))
	where
		itemSelect				= itemH.wItemSelect`
		contextShow1			= contextShow && itemH.wItemShow`
		clipRect1				= intersectRects clipRect (posSizeToRect itemH.wItemPos` itemH.wItemSize`)
	
	enableWItemHandle` _ _ _ _ _ _ _ itemH=:{wItemKind`=IsOtherControl _} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| found					= ({itemH & wItemSelect`=True},(ids,tb))
		| otherwise				= (itemH,(ids,tb))


/*	Disable the controls and provide proper feedback.
	The [Id] argument contains the Ids of the controls that should be (dis/en)abled.
	The Boolean argument controls the new SelectState. 
		If the Boolean argument is False, then SelectState is the new SelectState of the indicated controls.
		If the Boolean argument is True,  then SelectState is the new SelectState of the indicated controls 
										  and  all other controls. 
*/
disablecontrols :: ![Id] !Bool !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
disablecontrols ids overrule wMetrics wPtr wH=:{whItems`,whShow`,whSelect`,whSize`,whDefaultId`} tb
	# (itemHs,(_,tb))	= setAllWElements (disableWItemHandle` wMetrics wPtr whDefaultId` overrule whSelect` whShow` clipRect) whItems` (ids,tb)
	= ({wH & whItems`=itemHs},tb)
where
	clipRect			= getContentRect wMetrics wH.whWindowInfo` whSize`
	
	disableWItemHandle` :: !OSWindowMetrics !OSWindowPtr !(Maybe Id) !Bool !Bool !Bool !Rect !WItemHandle` !(![Id],!*OSToolbox)
																						 -> (!WItemHandle`,!(![Id],!*OSToolbox))
	
	disableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH=:{wItemKind`} (ids,tb)
		| systemControl
			# (found,ids)	= maybeRemoveCheck itemH.wItemId` ids
			| found
				= ({itemH & wItemSelect`=False},(ids,systemOSAction False tb))
			| overrule
				= (itemH,(ids,systemOSAction (contextSelect && itemSelect) tb))
			// otherwise
				= (itemH,(ids,tb))
	where
		itemPtr		= itemH.wItemPtr`
		itemSelect	= itemH.wItemSelect`
		radioItems	= (getWItemRadioInfo` itemH.wItemInfo`).radioItems`
		checkItems	= (getWItemCheckInfo` itemH.wItemInfo`).checkItems`
		(systemControl,systemOSAction)
					= case wItemKind` of
						IsRadioControl	-> (True,\able->stateMap2 (\{radioItemPtr`}->osSetRadioControlSelect wPtr radioItemPtr` clipRect able) radioItems)
						IsCheckControl	-> (True,\able->stateMap2 (\{checkItemPtr`}->osSetCheckControlSelect wPtr checkItemPtr` clipRect able) checkItems)
						IsPopUpControl	-> (True,osSetPopUpControlSelect  wPtr itemPtr clipRect)
						IsSliderControl	-> (True,osSetSliderControlSelect wPtr itemPtr clipRect)
						IsTextControl	-> (True,osSetTextControlSelect   wPtr itemPtr clipRect)
						IsEditControl	-> (True,osSetEditControlSelect   wPtr itemPtr clipRect)
						IsButtonControl	-> (True,osSetButtonControlSelect wPtr itemPtr clipRect)
						_				-> (False,undef)
	
	disableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH=:{wItemKind`} (ids,tb)
		| customControl
			# (found,ids)	= maybeRemoveCheck itemH.wItemId` ids
			| found
				# itemH		= {itemH & wItemSelect`=False}
				# tb		= customOSAction False tb
				# (itemH,tb)= customDraw False wPtr clipRect itemH tb
				= (itemH,(ids,tb))
			| overrule
				# select	= contextSelect && itemSelect
				# (itemH,tb)= customDraw select wPtr clipRect itemH tb
				= (itemH,(ids,customOSAction select tb))
			// otherwise
				= (itemH,(ids,tb))
	where
		itemPtr				= itemH.wItemPtr`
		itemSelect			= itemH.wItemSelect`
		(customControl,customDraw,customOSAction)
							= case wItemKind` of
								IsCustomButtonControl	-> (True,drawCustomButtonLook`,osSetCustomButtonControlSelect wPtr itemPtr clipRect)
								IsCustomControl			-> (True,drawCustomLook`,      osSetCustomControlSelect       wPtr itemPtr clipRect)
								_						-> (False,undef,undef)
	
	disableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH=:{wItemKind`=IsCompoundControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| found
			# itemH				= {itemH & wItemSelect`=False}
			# (itemH,tb)		= validateCompoundClipState` wMetrics False wPtr defId contextShow itemH tb
			# (itemH,tb)		= drawCompoundLook` wMetrics False wPtr clipRect1 itemH tb
			# (itemHs,(ids,tb))	= setAllWElements (disableWItemHandle` wMetrics wPtr defId True False contextShow1 clipRect1) itemH.wItems` (ids,tb)
			# tb				= osSetCompoundSelect wPtr itemPtr clipRect scrollInfo False tb
			  itemH				= {itemH & wItems`=itemHs}
			= (itemH,(ids,tb))
		| overrule
			# (itemH,tb)		= validateCompoundClipState` wMetrics False wPtr defId contextShow itemH tb
			# (itemH,tb)		= drawCompoundLook` wMetrics contextSelect1 wPtr clipRect1 itemH tb
			# (itemHs,(ids,tb))	= setAllWElements (disableWItemHandle` wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1) itemH.wItems` (ids,tb)
			# tb				= osSetCompoundSelect wPtr itemPtr clipRect scrollInfo contextSelect1 tb
			  itemH				= {itemH & wItems`=itemHs}
			= (itemH,(ids,tb))
		| otherwise
			# (itemHs,(ids,tb))	= setAllWElements (disableWItemHandle` wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1) itemH.wItems` (ids,tb)
			  itemH				= {itemH & wItems`=itemHs}
			= (itemH,(ids,tb))
	where
		itemPtr					= itemH.wItemPtr`
		itemSelect				= itemH.wItemSelect`
		itemSize				= itemH.wItemSize`
		contextSelect1			= contextSelect && itemSelect
		contextShow1			= contextShow && itemH.wItemShow`
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		scrollInfo				= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		clipRect1				= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemSize
	
	disableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH=:{wItemKind`=IsLayoutControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		  contextSelect1		= if found False (contextSelect && itemSelect)
		# (itemHs,(ids,tb))		= setAllWElements (disableWItemHandle` wMetrics wPtr defId (found || overrule) contextSelect1 contextShow1 clipRect1) itemH.wItems` (ids,tb)
		  itemH					= {itemH & wItemSelect`=if found False itemSelect,wItems`=itemHs}
		= (itemH,(ids,tb))
	where
		itemSelect				= itemH.wItemSelect`
		contextShow1			= contextShow && itemH.wItemShow`
		clipRect1				= intersectRects clipRect (posSizeToRect itemH.wItemPos` itemH.wItemSize`)
	
	disableWItemHandle` _ _ _ _ _ _ _ itemH=:{wItemKind`=IsOtherControl _} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| found					= ({itemH & wItemSelect`=False},(ids,tb))
		| otherwise				= (itemH,(ids,tb))


/*	Set the show state of the controls and provide proper feedback.
*/
setcontrolsshowstate :: ![Id] !Bool !OSWindowMetrics !WIDS !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontrolsshowstate ids itemsShow wMetrics wids=:{wPtr} wH=:{whItems`,whSelect`,whSize`,whWindowInfo`} tb
	# (itemHs,(_,tb))	= setWElements (setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect) whItems` (ids,tb)
	= ({wH & whItems`=itemHs},tb)
where
	clipRect			= getContentRect wMetrics whWindowInfo` whSize`
	overrule			= False
	contextShow			= True
	contextSelect		= if whSelect` Able Unable
	
	setWItemShowStates :: !OSWindowMetrics !OSWindowPtr !Bool !Bool !Bool !SelectState !Rect !WItemHandle` !(![Id],!*OSToolbox)
																						 -> (!WItemHandle`,!(![Id],!*OSToolbox))
	
	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect itemH=:{wItemKind`=IsRadioControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| not found && not overrule
			= (itemH,(ids,tb))
		| otherwise
			# osShow			= if overrule contextShow (contextShow && itemsShow)
			  itemH				= if found {itemH & wItemShow`=itemsShow} itemH
			  info				= getWItemRadioInfo` itemH.wItemInfo`
			# tb				= stateMap2 (setradio osShow clipRect) info.radioItems` tb
			= (itemH,(ids,tb))
	where
		setradio :: !Bool !Rect !RadioItemInfo` !*OSToolbox -> *OSToolbox
		setradio osShow clipRect {radioItemPtr`,radioItemPos`,radioItemSize`} tb
			= osSetRadioControlShow wPtr radioItemPtr` clipRect osShow tb
	
	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect itemH=:{wItemKind`=IsCheckControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| not found && not overrule
			= (itemH,(ids,tb))
		| otherwise
			# osShow			= if overrule contextShow (contextShow && itemsShow)
			  itemH				= if found {itemH & wItemShow`=itemsShow} itemH
			  info				= getWItemCheckInfo` itemH.wItemInfo`
			# tb				= stateMap2 (setcheck osShow clipRect) info.checkItems` tb
			= (itemH,(ids,tb))
	where
		setcheck :: !Bool !Rect !CheckItemInfo` !*OSToolbox -> *OSToolbox
		setcheck osShow clipRect {checkItemPtr`,checkItemPos`,checkItemSize`} tb
			= osSetRadioControlShow wPtr checkItemPtr` clipRect osShow tb
	
	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect itemH=:{wItemKind`} (ids,tb)
		| osControl
			# (found,ids)		= maybeRemoveCheck itemH.wItemId` ids
			| not found && not overrule
				= (itemH,(ids,tb))
			// otherwise
				# osShow		= if overrule contextShow (contextShow && itemsShow)
				  itemH			= if found {itemH & wItemShow`=itemsShow} itemH
				# tb			= osAction wPtr itemH.wItemPtr` clipRect osShow tb
				= (itemH,(ids,tb))
	where
		(osControl,osAction)	= case wItemKind` of
					  				IsPopUpControl			-> (True,osSetPopUpControlShow)
					  				IsSliderControl			-> (True,osSetSliderControlShow)
					  				IsTextControl			-> (True,osSetTextControlShow)
					  				IsEditControl			-> (True,osSetEditControlShow)
					  				IsButtonControl			-> (True,osSetButtonControlShow)
					  				_						-> (False,undef)
	
	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect itemH=:{wItemKind`} (ids,tb)
		| isCustom
			# (found,ids)		= maybeRemoveCheck itemH.wItemId` ids
			| not found && not overrule
				= (itemH,(ids,tb))
			// otherwise
				# osShow		= if overrule contextShow (contextShow && itemsShow)
				  itemH			= if found {itemH & wItemShow`=itemsShow} itemH
				  customDraw	= if osShow customDraw (\_ _ _ itemH tb -> (itemH,tb))
				# tb			= osAction wPtr itemH.wItemPtr` clipRect osShow tb
				# (itemH,tb)	= customDraw itemH.wItemSelect` wPtr clipRect itemH tb
				= (itemH,(ids,tb))
	where
	  (isCustom,customDraw,osAction)
				  				= case wItemKind` of
				  					IsCustomButtonControl	-> (True,drawCustomButtonLook`,osSetCustomButtonControlShow)
				  					IsCustomControl			-> (True,drawCustomLook`,      osSetCustomControlShow)
				  					_						-> (False,undef,undef)
	
	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect itemH=:{wItemKind`=IsCompoundControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		  contextShow1			= contextShow && (if found itemsShow itemH.wItemShow`)
		  overrule1				= overrule || found && itemH.wItemShow`<>itemsShow
		# (itemHs,(ids,tb))		= setAllWElements (setWItemShowStates wMetrics wPtr overrule1 itemsShow contextShow1 contextSelect1 clipRect1) itemH.wItems` (ids,tb)
						// PA: setAllWElements was setWElements
		  itemH					= {itemH & wItems`=itemHs}
		| not found && not overrule
			= (itemH,(ids,tb))
		| otherwise
			# itemH				= if found {itemH & wItemShow`=itemsShow} itemH
			# tb				= osSetCompoundShow wPtr itemH.wItemPtr` clipRect itemsShow tb
			# itemH				= invalidateCompoundClipState` itemH		// PA: added
			= (itemH,(ids,tb))
	where
		contextSelect1			= if (enabled contextSelect) (if itemH.wItemSelect` Able Unable) contextSelect
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		itemSize				= itemH.wItemSize`
		clipRect1				= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemSize
	
	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect itemH=:{wItemKind`=IsLayoutControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		  contextShow1			= contextShow && (if found itemsShow itemShow)
		  overrule1				= overrule || found && itemShow<>itemsShow
		# (itemHs,(ids,tb))		= setAllWElements (setWItemShowStates wMetrics wPtr overrule1 itemsShow contextShow1 contextSelect1 clipRect1) itemH.wItems` (ids,tb)
						// PA: setAllWElements was setWElements
		  itemH					= {itemH & wItemShow`=if found itemsShow itemShow,wItems`=itemHs}
		= (itemH,(ids,tb))
	where
		itemShow				= itemH.wItemShow`
		contextSelect1			= if (enabled contextSelect) (if itemH.wItemSelect` Able Unable) contextSelect
		clipRect1				= clipRect//intersectRects clipRect (posSizeToRect itemH.wItemPos` itemH.wItemSize`)
	
	setWItemShowStates _ _ _ _ _ _ _ itemH=:{wItemKind`=IsOtherControl _} (ids,tb)
		# (_,ids)				= maybeRemoveCheck itemH.wItemId` ids
		= (itemH,(ids,tb))


//	Set the MarkState of the controls and provide proper feedback.

setcontrolsmarkstate :: !Id !MarkState ![Index] !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontrolsmarkstate id mark indexs wMetrics wPtr wH=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)					= setWElement (setWItemMarks wMetrics wPtr mark clipRect indexs) id whItems` tb
	= ({wH & whItems`=itemHs},tb)
where
	clipRect						= getContentRect wMetrics whWindowInfo` whSize`
	
	setWItemMarks :: !OSWindowMetrics !OSWindowPtr !MarkState !Rect ![Index] !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	
	setWItemMarks wMetrics wPtr mark clipRect indexs id itemH=:{wItemKind`=IsCheckControl} tb
		| identifyMaybeId id itemH.wItemId`
			# info					= getWItemCheckInfo` itemH.wItemInfo`
			  checkItems			= info.checkItems`
			  nrCheckItems			= length checkItems
			  indexs				= filter (\index->isBetween index 1 nrCheckItems) indexs
			| isEmpty indexs
				= (True,itemH,tb)
			// otherwise
				# (checkItems,tb)	= stateMap2 (setCheckMark wPtr clipRect mark) indexs (checkItems,tb)
				  itemH				= {itemH & wItemInfo`=CheckInfo` {info & checkItems`=checkItems}}
				= (True,itemH,tb)
		| otherwise
			= (False,itemH,tb)
	where
		setCheckMark :: !OSWindowPtr !Rect !MarkState !Index !(![CheckItemInfo`],!*OSToolbox) -> (![CheckItemInfo`],!*OSToolbox)
		setCheckMark wPtr clipRect mark index (checkItems,tb)
			# (before,[item:after])	= split (index-1) checkItems
			  (title,width,_)		= item.checkItem`
			  checkItems			= before++[{item & checkItem`=(title,width,mark)}:after]
			= (checkItems,osSetCheckControl wPtr item.checkItemPtr` clipRect (marked mark) tb)
	
	setWItemMarks wMetrics wPtr mark clipRect indexs id itemH=:{wItemKind`} tb
		| wItemKind`==IsCompoundControl
			# (found,itemHs,tb)		= setWElement (setWItemMarks wMetrics wPtr mark clipRect1 indexs) id itemH.wItems` tb
			= (found,{itemH & wItems`=itemHs},tb)
		with
			info					= getWItemCompoundInfo` itemH.wItemInfo`
			clipRect1				= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemH.wItemSize`
		| wItemKind`==IsLayoutControl
			# (found,itemHs,tb)		= setWElement (setWItemMarks wMetrics wPtr mark clipRect1 indexs) id itemH.wItems` tb
			= (found,{itemH & wItems`=itemHs},tb)
		with
			clipRect1				= intersectRects clipRect (posSizeToRect itemH.wItemPos` itemH.wItemSize`)
		| otherwise
			= (False,itemH,tb)


//	Set the text of the controls and provide proper feedback.

setcontroltexts :: ![(Id,String)] !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontroltexts id_texts wMetrics wPtr wH`=:{whItems`,whSize`,whWindowInfo`} tb
	# (itemHs,(_,tb))	= setWElements (setControlText wMetrics wPtr True clipRect) whItems` (id_texts,tb)
	= ({wH` & whItems`=itemHs},tb)
where
	clipRect		= getContentRect wMetrics whWindowInfo` whSize`
	
	setControlText :: !OSWindowMetrics !OSWindowPtr !Bool !Rect !WItemHandle` !(![(Id,String)],!*OSToolbox)
															-> (!WItemHandle`,!(![(Id,String)],!*OSToolbox))
	
	setControlText wMetrics wPtr shownContext clipRect itemH=:{wItemKind`=IsEditControl} (id_texts,tb)
		# (found,id_text,id_texts)	= removeOnIdOfPair itemH.wItemId` id_texts
		| not found
			= (itemH,(id_texts,tb))
		| otherwise
			# shownContext1			= shownContext && itemH.wItemShow`
			  (_,text)				= id_text
			  editInfo				= getWItemEditInfo` itemH.wItemInfo`
			  itemH					= {itemH & wItemInfo`=EditInfo` {editInfo & editInfoText=text}}
			  itemRect				= posSizeToRect itemH.wItemPos` itemH.wItemSize`
			# tb					= osSetEditControlText wPtr itemH.wItemPtr` clipRect itemRect shownContext1 text tb
			= (itemH,(id_texts,tb))
	
	setControlText wMetrics wPtr shownContext clipRect itemH=:{wItemKind`=IsTextControl} (id_texts,tb)
		# (found,id_text,id_texts)	= removeOnIdOfPair itemH.wItemId` id_texts
		| not found
			= (itemH,(id_texts,tb))
		| otherwise
			# (_,text)				= id_text
			  shownContext1			= shownContext && itemH.wItemShow`
			  textInfo				= getWItemTextInfo` itemH.wItemInfo`
			  itemH					= {itemH & wItemInfo`=TextInfo` {textInfo & textInfoText=text}}
			  itemRect				= posSizeToRect itemH.wItemPos` itemH.wItemSize`
			# tb					= osSetTextControlText wPtr itemH.wItemPtr` clipRect itemRect shownContext1 text tb
			= (itemH,(id_texts,tb))
	
	setControlText wMetrics wPtr _ clipRect itemH=:{wItemKind`=IsButtonControl} (id_texts,tb)
		# (found,id_text,id_texts)	= removeOnIdOfPair itemH.wItemId` id_texts
		| not found
			= (itemH,(id_texts,tb))
		| otherwise
			# (_,text)				= id_text
			  buttonInfo			= getWItemButtonInfo` itemH.wItemInfo`
			  itemH					= {itemH & wItemInfo`=ButtonInfo` {buttonInfo & buttonInfoText=text}}
			# tb					= osSetButtonControlText wPtr itemH.wItemPtr` clipRect (validateControlTitle text) tb
			= (itemH,(id_texts,tb))
	
	setControlText wMetrics wPtr shownContext clipRect itemH=:{wItemKind`=IsCompoundControl} s
		# (itemHs,s)				= setWElements (setControlText wMetrics wPtr shownContext1 clipRect1) itemH.wItems` s
		= ({itemH & wItems`=itemHs},s)
	where
		info						= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1					= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemH.wItemSize`
		shownContext1				= shownContext && itemH.wItemShow`
	
	setControlText wMetrics wPtr shownContext clipRect itemH=:{wItemKind`=IsLayoutControl} s
		# (itemHs,s)	= setWElements (setControlText wMetrics wPtr shownContext1 clipRect1) itemH.wItems` s
		= ({itemH & wItems`=itemHs},s)
	where
		clipRect1					= intersectRects clipRect (posSizeToRect itemH.wItemPos` itemH.wItemSize`)
		shownContext1				= shownContext && itemH.wItemShow`
	
	setControlText _ _ _ _ itemH s
		= (itemH,s)


//	Set the cursor position of an EditControl, and handle proper feedback.

seteditcontrolcursor :: !Id !Int !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
seteditcontrolcursor id pos wMetrics wPtr wH`=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)			= setWElement (setEditCursor wMetrics wPtr True clipRect pos) id whItems` tb
	= ({wH` & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	setEditCursor :: !OSWindowMetrics !OSWindowPtr !Bool !Rect !Int !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	
	setEditCursor wMetrics wPtr shownContext clipRect pos id itemH=:{wItemKind`=IsEditControl} tb
		| not (identifyMaybeId id itemH.wItemId`)
			= (False,itemH,tb)
		| otherwise
			# itemRect		= posSizeToRect itemH.wItemPos` itemH.wItemSize`
			# tb			= osSetEditControlCursor wPtr itemH.wItemPtr` clipRect itemRect pos tb
			= (True,itemH,tb)
	
	setEditCursor wMetrics wPtr shownContext clipRect pos id itemH=:{wItemKind`=IsCompoundControl} tb
		# (found,itemHs,tb)	= setWElement (setEditCursor wMetrics wPtr shownContext1 clipRect1 pos) id itemH.wItems` tb
		= (found,{itemH & wItems`=itemHs},tb)
	where
		info				= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1			= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemH.wItemSize`
		shownContext1		= shownContext && itemH.wItemShow`
	
	setEditCursor wMetrics wPtr shownContext clipRect pos id itemH=:{wItemKind`=IsLayoutControl} tb
		# (found,itemHs,tb)	= setWElement (setEditCursor wMetrics wPtr shownContext1 clipRect1 pos) id itemH.wItems` tb
		= (found,{itemH & wItems`=itemHs},tb)
	where
		clipRect1			= intersectRects clipRect (posSizeToRect itemH.wItemPos` itemH.wItemSize`)
		shownContext1		= shownContext && itemH.wItemShow`
	
	setEditCursor _ _ _ _ _ _ itemH tb
		= (False,itemH,tb)


//	Set the look of a control, and handle proper feedback.

setcontrolslook :: ![(Id,Bool,(Bool,Look))] !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontrolslook looks wMetrics wPtr wH=:{whItems`,whSelect`,whDefaultId`,whSize`,whWindowInfo`} tb
	# (itemHs,(_,tb))	= setWElements (setWItemLook wMetrics wPtr whSelect` True resizeable whDefaultId` clipRect) whItems` (looks,tb)
	= ({wH & whItems`=itemHs},tb)
where
	clipRect			= getContentRect wMetrics whWindowInfo` whSize`
	resizeable			= True
	
	setWItemLook :: !OSWindowMetrics !OSWindowPtr !Bool !Bool !Bool (Maybe Id) !Rect !WItemHandle` !(![(Id,Bool,(Bool,Look))],!*OSToolbox)
																				 -> (!WItemHandle`,!(![(Id,Bool,(Bool,Look))],!*OSToolbox))
	
	setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect itemH=:{wItemId`,wItemKind`=IsCompoundControl,wItemSize`} (looks,tb)
		# (found,look,looks)		= removeOnIdOfTriple wItemId` looks
		# (itemHs,looks_tb)			= setWElements (setWItemLook wMetrics wPtr ableContext1 shownContext1 resizeable defId clipRect1) itemH.wItems` (looks,tb)
		  itemH						= {itemH & wItems`=itemHs}
		| not found
			= (itemH,looks_tb)
		# (_,redraw,(sysLook,cLook))= look
		  info						= {info & compoundLookInfo={lookInfo & compoundLook = {	lookFun			= cLook
		  																				  ,	lookPen			= pen
		  																				  ,	lookSysUpdate	= sysLook
		  																				  }}}
		  itemH						= {itemH & wItemInfo`=CompoundInfo` info}
		| not redraw || not shownContext1
			= (itemH,looks_tb)
		| otherwise
			# (looks,tb)			= looks_tb
			# (itemH,tb)			= validateCompoundClipState` wMetrics False wPtr defId shownContext itemH tb
			# (itemH,tb)			= drawCompoundLook` wMetrics ableContext1 wPtr clipRect1 itemH tb
			= (itemH,(looks,tb))
	where
		info						= getWItemCompoundInfo` itemH.wItemInfo`
		hasScrolls					= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		compoundLookInfo			= info.compoundLookInfo
		lookInfo					= compoundLookInfo
		pen							= compoundLookInfo.compoundLook.lookPen
		visScrolls					= osScrollbarsAreVisible wMetrics info.compoundDomain (toTuple wItemSize`) hasScrolls
		itemRect					= posSizeToRect itemH.wItemPos` wItemSize`
		contentRect					= getCompoundContentRect wMetrics visScrolls itemRect
		clipRect1					= intersectRects clipRect contentRect
		ableContext1				= ableContext  && itemH.wItemSelect`
		shownContext1				= shownContext && itemH.wItemShow`
	
	setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect itemH=:{wItemId`,wItemKind`=IsCustomButtonControl} (looks,tb)
		# (found,look,looks)		= removeOnIdOfTriple wItemId` looks
		| not found
			= (itemH,(looks,tb))
		# (_,redraw,(sysLook,cLook))= look
		  info						= {info & cButtonInfoLook={itemLook & lookFun=cLook,lookSysUpdate=sysLook}}
		  itemH						= {itemH & wItemInfo`=CustomButtonInfo` info}
		| not redraw || not shownContext1
			= (itemH,(looks,tb))
		| otherwise
			# (itemH,tb)			= drawCustomButtonLook` ableContext1 wPtr clipRect itemH tb
			= (itemH,(looks,tb))
	where
		info						= getWItemCustomButtonInfo` itemH.wItemInfo`
		itemLook					= info.cButtonInfoLook
		ableContext1				= ableContext  && itemH.wItemSelect`
		shownContext1				= shownContext && itemH.wItemShow`
	
	setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect itemH=:{wItemId`,wItemKind`=IsCustomControl} (looks,tb)
		# (found,look,looks)		= removeOnIdOfTriple wItemId` looks
		| not found
			= (itemH,(looks,tb))
		# (_,redraw,(sysLook,cLook))= look
		# info						= {info & customInfoLook={itemLook & lookFun=cLook,lookSysUpdate=sysLook}}
		# itemH						= {itemH & wItemInfo`=CustomInfo` info}
		| not redraw || not shownContext1
			= (itemH,(looks,tb))
		| otherwise
			# (itemH,tb)			= drawCustomLook` ableContext1 wPtr clipRect itemH tb
			= (itemH,(looks,tb))
	where
		info						= getWItemCustomInfo` itemH.wItemInfo`
		itemLook					= info.customInfoLook
		ableContext1				= ableContext  && itemH.wItemSelect`
		shownContext1				= shownContext && itemH.wItemShow`
	
	setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect itemH=:{wItemId`,wItemKind`=IsLayoutControl} (looks,tb)
		# (_,_,looks)				= removeOnIdOfTriple wItemId` looks
		# (itemHs,looks_tb)			= setWElements (setWItemLook wMetrics wPtr ableContext1 shownContext1 resizeable defId clipRect1) itemH.wItems` (looks,tb)
		  itemH						= {itemH & wItems`=itemHs}
		= (itemH,looks_tb)
	where
		clipRect1					= intersectRects clipRect (posSizeToRect itemH.wItemPos` itemH.wItemSize`)
		ableContext1				= ableContext  && itemH.wItemSelect`
		shownContext1				= shownContext && itemH.wItemShow`
	
	setWItemLook _ _ _ _ _ _ _ itemH=:{wItemId`} (looks,tb)
		# (_,_,looks)				= removeOnIdOfTriple wItemId` looks
		= (itemH,(looks,tb))


//	Draw in a customised control.

drawincontrol :: !Id !.(St *Picture .x) !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> *(!Maybe .x,!WindowHandle`,!*OSToolbox)
drawincontrol controlId drawfun wMetrics wPtr wH=:{whItems`,whDefaultId`,whShow`,whSize`,whWindowInfo`} tb
	# (_,itemHs,(x,_,tb))	= setWElement (drawInWItem wMetrics wPtr resizeable whDefaultId` whShow` clipRect) controlId whItems` (Nothing,drawfun,tb)
	= (x,{wH & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	resizeable				= True
	
	drawInWItem :: !OSWindowMetrics !OSWindowPtr Bool (Maybe Id) !Bool !Rect !Id !WItemHandle` !*(!v:(Maybe u:x),w:(St *Picture u:x),!*OSToolbox)
																	   -> (!Bool,!WItemHandle`,!*(!v:(Maybe u:x),z:(St *Picture u:x),!*OSToolbox)), [v<=u,w<=z]
	
	drawInWItem wMetrics wPtr resizeable defId contextShow clipRect id itemH=:{wItemId`,wItemPtr`,wItemKind`=IsCompoundControl} x_tb
		| not (identifyMaybeId id wItemId`)
			# (found,itemHs,x_tb)	= setWElement (drawInWItem wMetrics wPtr resizeable defId itemShow clipRect1) id itemH.wItems` x_tb
			= (found,{itemH & wItems`=itemHs},x_tb)
		| otherwise
			# (_,drawfun,tb)		= x_tb
			# (itemH,tb)			= validateCompoundClipState` wMetrics False wPtr defId contextShow itemH tb
			# (x,itemH,tb)			= drawInCompound` wPtr drawfun clipRect1 itemH tb
			= (True,itemH,(Just x,undef,tb))
	where
		itemShow					= contextShow && itemH.wItemShow`
		info						= getWItemCompoundInfo` itemH.wItemInfo`
		domainRect					= info.compoundDomain
		hasScrolls					= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		itemPos						= itemH.wItemPos`
		itemSize					= itemH.wItemSize`
		itemRect					= posSizeToRect itemPos itemSize
		visScrolls					= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
		contentRect					= getCompoundContentRect wMetrics visScrolls itemRect
		clipRect1					= intersectRects clipRect contentRect
	
	drawInWItem _ wPtr _ _ _ clipRect id itemH=:{wItemId`,wItemKind`=IsCustomButtonControl} x_tb
		| not (identifyMaybeId id wItemId`)
			= (False,itemH,x_tb)
		| otherwise
			# (_,drawfun,tb)		= x_tb
			# (x,itemH,tb)			= drawInCustomButton` wPtr drawfun clipRect itemH tb
			= (True,itemH,(Just x,undef,tb))
	
	drawInWItem _ wPtr _ _ _ clipRect id itemH=:{wItemId`,wItemKind`=IsCustomControl} x_tb
		| not (identifyMaybeId id wItemId`)
			= (False,itemH,x_tb)
		| otherwise
			# (_,drawfun,tb)		= x_tb
			# (x,itemH,tb)			= drawInCustom` wPtr drawfun clipRect itemH tb
			= (True,itemH,(Just x,undef,tb))
	
	drawInWItem wMetrics wPtr resizeable defId contextShow clipRect id itemH=:{wItemId`,wItemKind`=IsLayoutControl} x_tb
		| identifyMaybeId id wItemId`
			= (True,itemH,x_tb)
		| otherwise
			# (found,itemHs,x_tb)	= setWElement (drawInWItem wMetrics wPtr resizeable defId itemShow clipRect1) id itemH.wItems` x_tb
			= (found,{itemH & wItems`=itemHs},x_tb)
	where
		itemShow					= contextShow && itemH.wItemShow`
		clipRect1					= intersectRects clipRect (posSizeToRect itemH.wItemPos` itemH.wItemSize`)
	
	drawInWItem _ _ _ _ _ _ id itemH=:{wItemId`} x_tb
		= (identifyMaybeId id wItemId`,itemH,x_tb)


//	Change the state of the slider and handle proper feedback.

setsliderstates :: ![(Id,IdFun SliderState)] !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setsliderstates id_fs wMetrics wPtr wH`=:{whItems`,whSize`,whWindowInfo`} tb
	# (itemHs,(_,tb))		= setWElements (setSliderState wMetrics wPtr clipRect) whItems` (id_fs,tb)
	= ({wH` & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	setSliderState :: !OSWindowMetrics !OSWindowPtr !Rect !WItemHandle` !(![(Id,IdFun SliderState)],!*OSToolbox)
													  -> (!WItemHandle`,!(![(Id,IdFun SliderState)],!*OSToolbox))
	
	setSliderState wMetrics wPtr clipRect itemH=:{wItemId`,wItemKind`=IsSliderControl,wItemPtr`} (id_fs,tb)
		# (found,id_f,id_fs)= removeOnIdOfPair wItemId` id_fs
		| not found
			= (itemH,(id_fs,tb))
		| otherwise
			# info			= getWItemSliderInfo` itemH.wItemInfo`
			  oldState		= info.sliderInfoState`
			  (_,f)			= id_f
			  newState		= validateSliderState (f oldState)
			  itemH			= {itemH & wItemInfo`=SliderInfo` {info & sliderInfoState`=newState}}
			  (tbMin,tbThumb,tbMax,_)
			  				= toOSscrollbarRange (newState.sliderMin,newState.sliderThumb,newState.sliderMax) 0
			# tb			= osSetSliderThumb wPtr wItemPtr` clipRect (not (isEmptyRect clipRect)) (tbMin,tbThumb,tbMax) tb
			= (itemH,(id_fs,tb))
	
	setSliderState wMetrics wPtr clipRect itemH=:{wItemKind`=IsCompoundControl} (id_fs,tb)
		# (_,_,id_fs)		= removeOnIdOfPair itemH.wItemId` id_fs
		# (itemHs,id_fs_tb)	= setWElements (setSliderState wMetrics wPtr clipRect1) itemH.wItems` (id_fs,tb)
		= ({itemH & wItems`=itemHs},id_fs_tb)
	where
		info				= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1			= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemH.wItemSize`
	
	setSliderState wMetrics wPtr clipRect itemH=:{wItemKind`=IsLayoutControl} (id_fs,tb)
		# (_,_,id_fs)		= removeOnIdOfPair itemH.wItemId` id_fs
		# (itemHs,id_fs_tb)	= setWElements (setSliderState wMetrics wPtr clipRect1) itemH.wItems` (id_fs,tb)
		= ({itemH & wItems`=itemHs},id_fs_tb)
	where
		clipRect1			= intersectRects clipRect (posSizeToRect itemH.wItemPos` itemH.wItemSize`)
	
	setSliderState _ _ _ itemH (id_fs,tb)
		# (_,_,id_fs)		= removeOnIdOfPair itemH.wItemId` id_fs
		= (itemH,(id_fs,tb))


//	Selecting a RadioControl item.

selectradiocontrol :: !Id !Index !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
selectradiocontrol id index wMetrics wPtr wH=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)			= setWElement (selectWItemRadioControl wMetrics wPtr clipRect index) id whItems` tb
	= ({wH & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	selectWItemRadioControl :: !OSWindowMetrics !OSWindowPtr !Rect !Index !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	
	selectWItemRadioControl wMetrics wPtr clipRect index id itemH=:{wItemId`,wItemKind`=IsRadioControl} tb
		| not (identifyMaybeId id wItemId`)
			= (False,itemH,tb)
		# info				= getWItemRadioInfo` itemH.wItemInfo`
		  cur				= info.radioIndex`
		  items				= info.radioItems`
		  nrItems			= length items
		  index				= setBetween index 1 nrItems
		  info				= {info & radioIndex`=index}
		  itemH				= {itemH & wItemInfo`=RadioInfo` info}
		| index==cur
			= (True,itemH,tb)
		| otherwise
			# tb			= osSetRadioControl wPtr (items!!(cur-1)).radioItemPtr` (items!!(index-1)).radioItemPtr` clipRect tb
			= (True,itemH,tb)
	
	selectWItemRadioControl wMetrics wPtr clipRect index id itemH=:{wItemKind`=IsCompoundControl} tb
		| identifyMaybeId id itemH.wItemId`
			= (True,itemH,tb)
		| otherwise
			# (done,itemHs,tb)	= setWElement (selectWItemRadioControl wMetrics wPtr clipRect1 index) id itemH.wItems` tb
			= (done,{itemH & wItems`=itemHs},tb)
	where
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1				= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemH.wItemSize`
	
	selectWItemRadioControl wMetrics wPtr clipRect index id itemH=:{wItemKind`=IsLayoutControl} tb
		| identifyMaybeId id itemH.wItemId`
			= (True,itemH,tb)
		| otherwise
			# (done,itemHs,tb)	= setWElement (selectWItemRadioControl wMetrics wPtr clipRect1 index) id itemH.wItems` tb
			= (done,{itemH & wItems`=itemHs},tb)
	where
		clipRect1				= intersectRects clipRect (posSizeToRect itemH.wItemPos` itemH.wItemSize`)
	
	selectWItemRadioControl _ _ _ _ id itemH tb
		= (identifyMaybeId id itemH.wItemId`,itemH,tb)


//	Select a PopUpControl item.
	
selectpopupitem :: !Id !Index !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
selectpopupitem id index wMetrics wPtr wH=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)			= setWElement (selectWItemPopUp wMetrics wPtr True index clipRect) id whItems` tb
	= ({wH & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	selectWItemPopUp :: !OSWindowMetrics !OSWindowPtr !Bool !Index !Rect !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	
	selectWItemPopUp wMetrics wPtr shownContext index clipRect id itemH=:{wItemId`,wItemKind`=IsPopUpControl} tb
		| not (identifyMaybeId id wItemId`)
			= (False,itemH,tb)
		# info				= getWItemPopUpInfo` itemH.wItemInfo`
		  popUps			= info.popUpInfoItems`
		  curindex			= info.popUpInfoIndex`
		  nrPopUps			= length popUps
		  index				= setBetween index 1 nrPopUps
		| curindex==index
			= (True,itemH,tb)
		| otherwise
			# shownContext1	= shownContext && itemH.wItemShow`
			  info			= {info & popUpInfoIndex`=index}
			  itemH			= {itemH & wItemInfo`=PopUpInfo` info}
			  itemRect		= posSizeToRect itemH.wItemPos` itemH.wItemSize`
			# tb			= osSetPopUpControl wPtr itemH.wItemPtr` clipRect itemRect curindex index (popUps!!(index-1)) shownContext1 tb
			= (True,itemH,tb)
	
	selectWItemPopUp wMetrics wPtr shownContext index clipRect id itemH=:{wItemKind`=IsCompoundControl} tb
		| identifyMaybeId id itemH.wItemId`
			= (True,itemH,tb)
		| otherwise
			# (found,itemHs,tb)	= setWElement (selectWItemPopUp wMetrics wPtr shownContext1 index clipRect1) id itemH.wItems` tb
			= (found,{itemH & wItems`=itemHs},tb)
	where
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1				= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemH.wItemSize`
		shownContext1			= shownContext && itemH.wItemShow`
	
	selectWItemPopUp wMetrics wPtr shownContext index clipRect id itemH=:{wItemKind`=IsLayoutControl} tb
		| identifyMaybeId id itemH.wItemId`
			= (True,itemH,tb)
		| otherwise
			# (found,itemHs,tb)	= setWElement (selectWItemPopUp wMetrics wPtr shownContext1 index clipRect1) id itemH.wItems` tb
			= (found,{itemH & wItems`=itemHs},tb)
	where
		clipRect1				= intersectRects clipRect (posSizeToRect itemH.wItemPos` itemH.wItemSize`)
		shownContext1			= shownContext && itemH.wItemShow`
	
	selectWItemPopUp _ _ _ _ _ id itemH tb
		= (identifyMaybeId id itemH.wItemId`,itemH,tb)


//	Add new items to a PopUpControl. 

openpopupitems :: !Id !Index ![PopUpControlItem .pst] !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox
																-> (!WindowHandle .ls .pst, !*OSToolbox)
openpopupitems id index newItems wPtr wH=:{whItems} tb
	# (_,itemHs,tb)	= openWElementsPopUpItems wPtr index newItems id whItems tb
	= ({wH & whItems=itemHs},tb)
where
	openWElementsPopUpItems :: !OSWindowPtr !Index ![PopUpControlItem .pst] !Id ![WElementHandle .ls .pst] !*OSToolbox
																	  -> (!Bool,![WElementHandle .ls .pst],!*OSToolbox)
	openWElementsPopUpItems _ _ _ _ [] tb
		= (False,[],tb)
	openWElementsPopUpItems wPtr index newItems id [itemH:itemHs] tb
		# (done,itemH,tb)		= openWElementPopUpItems wPtr index newItems id itemH tb
		| done
			= (done,[itemH:itemHs],tb)
		| otherwise
			# (done,itemHs,tb)	= openWElementsPopUpItems wPtr index newItems id itemHs tb
			= (done,[itemH:itemHs],tb)
	where
		openWElementPopUpItems :: !OSWindowPtr !Index ![PopUpControlItem .pst] !Id !(WElementHandle .ls .pst) !*OSToolbox
																		  -> (!Bool,!WElementHandle .ls .pst, !*OSToolbox)
		openWElementPopUpItems wPtr id index newItems (WItemHandle itemH) tb
			# (done,itemH,tb)	= openWItemPopUpItems wPtr id index newItems itemH tb
			= (done,WItemHandle itemH,tb)
		where
			openWItemPopUpItems :: !OSWindowPtr !Index ![PopUpControlItem .pst] !Id !(WItemHandle .ls .pst) !*OSToolbox
																		   -> (!Bool,!WItemHandle .ls .pst, !*OSToolbox)
			openWItemPopUpItems wPtr index items id itemH=:{wItemKind=IsPopUpControl} tb
				| not (identifyMaybeId id itemH.wItemId)
					= (False,itemH,tb)
				# (newPopUpPtr,editPtr,tb)
									= osCreateEmptyPopUpControl wPtr (0,0) itemH.wItemShow ableContext 
																(toTuple popUpPos) (toTuple popUpSize) (length newItems) isEditable tb
				# (_,tb)			= stateMap2 (appendPopUp newPopUpPtr newIndex) newItems (1,tb)
			//	# tb				= osStackWindow newPopUpPtr popUpPtr tb
				# (_,_,tb)			= osStackWindow newPopUpPtr popUpPtr k` 0 tb	// PA: for control delayinfo can be ignored
				# tb				= osDestroyPopUpControl popUpPtr tb
				  newPopUpInfo		= {	popUpInfoItems = newItems
									  ,	popUpInfoIndex = newIndex
									  ,	popUpInfoEdit  = if isEditable (Just {curEditInfo & popUpEditPtr=editPtr}) Nothing
									  }
				  itemH				= {itemH & wItemInfo=PopUpInfo newPopUpInfo,wItemPtr=newPopUpPtr}
				| not hasTip
					= (True,itemH,tb)
				| otherwise
					= (True,itemH,osAddControlToolTip wPtr newPopUpPtr (getControlTipAtt tipAtt) tb)
			where
				(hasTip,tipAtt)	= cselect isControlTip undef itemH.wItemAtts
				isEditable		= contains isControlKeyboard itemH.wItemAtts
				ableContext		= itemH.wItemSelect
				popUpPtr		= itemH.wItemPtr
				popUpSize		= itemH.wItemSize
				popUpPos		= itemH.wItemPos
				popUpInfo		= getWItemPopUpInfo itemH.wItemInfo
				curEditInfo		= fromJust popUpInfo.popUpInfoEdit
				curIndex		= popUpInfo.popUpInfoIndex
				curItems		= popUpInfo.popUpInfoItems
				newItems		= before++[(title,noLS f)\\(title,f)<-items]++after
				newIndex		= if (curIndex<=index) curIndex (curIndex+index)
				(before,after)	= split index curItems
				
				appendPopUp :: !OSWindowPtr !Index !(PopUpControlItem .pst) !(!Int,!*OSToolbox) -> (!Int,!*OSToolbox)
				appendPopUp popUpPtr index (title,_) (itemNr,tb)
					# (_,tb)			= osCreatePopUpControlItem popUpPtr (-1) ableContext title (index==itemNr) tb
					= (itemNr+1,tb)
			
			openWItemPopUpItems wPtr index newItems id itemH=:{wItemId,wItems} tb
				| identifyMaybeId id wItemId
					= (True,itemH,tb)
				| otherwise
					# (done,itemHs,tb)	= openWElementsPopUpItems wPtr index newItems id wItems tb
					= (done,{itemH & wItems=itemHs},tb)
		
		openWElementPopUpItems wPtr index newItems id (WListLSHandle itemHs) tb
			# (done,itemHs,tb)	= openWElementsPopUpItems wPtr index newItems id itemHs tb
			= (done,WListLSHandle itemHs,tb)
		
		openWElementPopUpItems wPtr index newItems id (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
			# (done,itemHs,tb)	= openWElementsPopUpItems wPtr index newItems id itemHs tb
			= (done,WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
		
		openWElementPopUpItems wPtr index newItems id (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
			# (done,itemHs,tb)	= openWElementsPopUpItems wPtr index newItems id itemHs tb
			= (done,WChangeLSHandle {wChH & wChangeItems=itemHs},tb)


//	Remove items from a PopUpControl. 

closepopupitems :: !Id ![Index] !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,!*OSToolbox)
closepopupitems id indexs wPtr wH=:{whItems} tb
	# (_,itemHs,tb)	= closeWElementsPopUpItems wPtr indexs id whItems tb
	= ({wH & whItems=itemHs},tb)
where
	closeWElementsPopUpItems :: !OSWindowPtr ![Index] !Id ![WElementHandle .ls .pst] !*OSToolbox
												-> (!Bool,![WElementHandle .ls .pst],!*OSToolbox)
	closeWElementsPopUpItems _ _ _ [] tb
		= (False,[],tb)
	closeWElementsPopUpItems wPtr indexs id [itemH:itemHs] tb
		# (done,itemH,tb)		= closeWElementPopUpItems wPtr indexs id itemH tb
		| done
			= (done,[itemH:itemHs],tb)
		| otherwise
			# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr indexs id itemHs tb
			= (done,[itemH:itemHs],tb)
	where
		closeWElementPopUpItems :: !OSWindowPtr ![Index] !Id !(WElementHandle .ls .pst) !*OSToolbox
													-> (!Bool,!WElementHandle .ls .pst, !*OSToolbox)
		closeWElementPopUpItems wPtr indexs id (WItemHandle itemH) tb
			# (done,itemH,tb)	= closeWItemPopUpItems wPtr indexs id itemH tb
			= (done,WItemHandle itemH,tb)
		where
			closeWItemPopUpItems :: !OSWindowPtr ![Index] !Id !(WItemHandle .ls .pst) !*OSToolbox
													 -> (!Bool,!WItemHandle .ls .pst, !*OSToolbox)
			
			closeWItemPopUpItems wPtr indexs id itemH=:{wItemKind=IsPopUpControl} tb
				| not (identifyMaybeId id itemH.wItemId)
					= (False,itemH,tb)
				# (newPopUpPtr,editPtr,tb)
									= osCreateEmptyPopUpControl wPtr (0,0) itemH.wItemShow ableContext 
																(toTuple popUpPos) (toTuple popUpSize) (length newItems) isEditable tb
				# (_,tb)			= stateMap2 (appendPopUp newPopUpPtr newIndex) newItems (1,tb)
			//	# tb				= osStackWindow newPopUpPtr popUpPtr tb
				# (_,_,tb)			= osStackWindow newPopUpPtr popUpPtr k` 0 tb	// PA: for control delayinfo can be ignored
				# tb				= osDestroyPopUpControl popUpPtr tb
				  newPopUpInfo		= {	popUpInfoItems = newItems
									  ,	popUpInfoIndex = newIndex
									  ,	popUpInfoEdit  = if isEditable (Just {curEditInfo & popUpEditPtr=editPtr}) Nothing
									  }
				  itemH				= {itemH & wItemInfo=PopUpInfo newPopUpInfo,wItemPtr=newPopUpPtr}
				| not hasTip
					= (True,itemH,tb)
				| otherwise
					= (True,itemH,osAddControlToolTip wPtr newPopUpPtr (getControlTipAtt tipAtt) tb)
			where
				(hasTip,tipAtt)		= cselect isControlTip undef itemH.wItemAtts
				isEditable			= contains isControlKeyboard itemH.wItemAtts
				ableContext			= itemH.wItemSelect
				popUpPtr			= itemH.wItemPtr
				popUpSize			= itemH.wItemSize
				popUpPos			= itemH.wItemPos
				popUpInfo			= getWItemPopUpInfo itemH.wItemInfo
				curEditInfo			= fromJust popUpInfo.popUpInfoEdit
				curIndex			= popUpInfo.popUpInfoIndex
				curItems			= popUpInfo.popUpInfoItems
				newItems			= map snd (filter (\(i,_)->not (isMember i indexs)) (zip2 [1..] curItems))
				nrNewItems			= length newItems
				newIndex			= if (isMember curIndex indexs) 1 (min nrNewItems curIndex)
				
				appendPopUp :: !OSWindowPtr !Index !(PopUpControlItem .ps) !(!Int,!*OSToolbox) -> (!Int,!*OSToolbox)
				appendPopUp popUpPtr index (title,_) (itemNr,tb)
					# (_,tb)		= osCreatePopUpControlItem popUpPtr (-1) ableContext title (index==itemNr) tb
					= (itemNr+1,tb)
			
			closeWItemPopUpItems wPtr indexs id itemH=:{wItemId,wItems} tb
				| identifyMaybeId id wItemId
					= (True,itemH,tb)
				| otherwise
					# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr indexs id wItems tb
					= (done,{itemH & wItems=itemHs},tb)
		
		closeWElementPopUpItems wPtr indexs id (WListLSHandle itemHs) tb
			# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr indexs id itemHs tb
			= (done,WListLSHandle itemHs,tb)
		
		closeWElementPopUpItems wPtr indexs id (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
			# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr indexs id itemHs tb
			= (done,WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
		
		closeWElementPopUpItems wPtr indexs id (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
			# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr indexs id itemHs tb
			= (done,WChangeLSHandle {wChH & wChangeItems=itemHs},tb)


/*	The record MetricsInfo and the functions shiftControls` and setsliderthumb are used by
	movecontrolviewframe and setcontrolviewdomain.
*/
::	MetricsInfo
	=	{	miOSMetrics		:: !OSWindowMetrics
		,	miHMargins		:: !(!Int,!Int)
		,	miVMargins		:: !(!Int,!Int)
		,	miItemSpaces	:: !(!Int,!Int)
		,	miOrientation	:: ![(ViewDomain,Origin)]
		}

shiftControls` :: !Vector2 ![WElementHandle`] -> [WElementHandle`]
shiftControls` v itemHs
	| isEmpty itemHs
		= itemHs
	| otherwise
		# (itemH,itemHs)= hdtl itemHs
		= [shiftControl` v itemH:shiftControls` v itemHs]
where
	shiftControl` :: !Vector2 !WElementHandle` -> WElementHandle`
	shiftControl` v (WItemHandle` itemH=:{wItemPos`,wItems`})
		= WItemHandle` {itemH & wItemPos`=movePoint v wItemPos`, wItems`=shiftControls` v wItems`}
	shiftControl` v (WRecursiveHandle` itemHs kind)
		= WRecursiveHandle` (shiftControls` v itemHs) kind

setsliderthumb :: !Bool OSWindowMetrics OSWindowPtr Bool (Int,Int,Int) Int (Int,Int) !*OSToolbox -> *OSToolbox
setsliderthumb hasScroll wMetrics itemPtr isHScroll scrollValues viewSize maxcoords tb
	| not hasScroll	= tb
	| otherwise		= osSetCompoundSlider wMetrics itemPtr isHScroll (toOSscrollbarRange scrollValues viewSize) maxcoords tb


/*	Move the ViewFrame of a CompoundControl. (In future version also customised controls.)
*/
movecontrolviewframe :: !Id !Vector2 !OSWindowMetrics !WIDS !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
movecontrolviewframe id v wMetrics wids=:{wPtr} wH=:{whKind`,whItems`,whSize`,whAtts`,whSelect`,whDefaultId`,whWindowInfo`} tb
	| whKind`==IsGameWindow
		= (wH,tb)
	# metricsInfo			= {miOSMetrics=wMetrics,miHMargins=hMargins,miVMargins=vMargins,miItemSpaces=spaces,miOrientation=orientation}
	# (_,itemHs,(updRgn,tb))= setWElement (moveWItemFrame metricsInfo wPtr whDefaultId` True whSelect` clipRect v)
								id whItems` (Nothing,tb)
	  wH					= {wH & whItems`=itemHs}
	| isNothing updRgn
		= (wH,tb)
	# updRgn				= fromJust updRgn
	# (empty,tb)			= osisemptyrgn updRgn tb
	| empty
		= (wH,osdisposergn updRgn tb)
	| otherwise
		= updatewindowbackgrounds` wMetrics updRgn wids wH tb
where
	windowInfo				= getWindowInfoWindowData whWindowInfo`
	(domainRect,origin,defHMargin,defVMargin)
							= if (whKind`==IsDialog)
								(sizeToRect whSize`,zero,wMetrics.osmHorMargin,wMetrics.osmVerMargin)
								(windowInfo.windowDomain,windowInfo.windowOrigin,0,0)
	(defHSpace, defVSpace)	= (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)
	hMargins				= getwindowhmargin`   (snd (cselect iswindowhmargin`   (WindowHMargin` defHMargin defHMargin) whAtts`))
	vMargins				= getwindowvmargin`   (snd (cselect iswindowvmargin`   (WindowVMargin` defVMargin defVMargin) whAtts`))
	spaces					= getwindowitemspace` (snd (cselect iswindowitemspace` (WindowItemSpace` defHSpace defVSpace) whAtts`))
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	orientation				= [(rectToRectangle domainRect,origin)]
	
	moveWItemFrame :: !MetricsInfo !OSWindowPtr !(Maybe Id) !Bool !Bool !Rect !Vector2 !Id
							!WItemHandle` !(!Maybe OSRgnHandle,!*OSToolbox)
				  -> (!Bool,!WItemHandle`,!(!Maybe OSRgnHandle,!*OSToolbox))
	
	moveWItemFrame metricsInfo=:{miOSMetrics,miHMargins,miVMargins,miItemSpaces,miOrientation} wPtr defaultId shownContext ableContext clipRect v id
				   itemH=:{wItemId`,wItemKind`} updRgn_tb
		| not (isRecursiveControl wItemKind`)
			= (identifyMaybeId id wItemId`,itemH,updRgn_tb)
		| wItemKind`==IsLayoutControl
			| identifyMaybeId id wItemId`
				= (True,itemH,updRgn_tb)
			// otherwise
				# metricsInfo`	= {metricsInfo & miHMargins=hMargins`,miVMargins=vMargins`,miItemSpaces=spaces`}
				  clipRect1		= intersectRects clipRect (posSizeToRect itemPos itemSize)
				# (done,itemHs,updRgn_tb)
								= setWElement (moveWItemFrame metricsInfo` wPtr defaultId shownContext1 ableContext1 clipRect1 v) id itemH.wItems` updRgn_tb
				= (done,{itemH & wItems`=itemHs},updRgn_tb)
		| not (identifyMaybeId id itemH.wItemId`)
			# orientation`		= [(domain,oldOrigin):miOrientation]
			  clipRect1			= intersectRects contentRect clipRect
			  metricsInfo`		= {metricsInfo & miHMargins=hMargins`,miVMargins=vMargins`,miItemSpaces=spaces`,miOrientation=orientation`}
			# (done,itemHs,updRgn_tb)
								= setWElement (moveWItemFrame metricsInfo` wPtr defaultId shownContext1 ableContext1 clipRect1 v) id itemH.wItems` updRgn_tb
			= (done,{itemH & wItems`=itemHs},updRgn_tb)
		| newOrigin==oldOrigin
			= (True,itemH,updRgn_tb)
		# (updRgn,tb)			= updRgn_tb
		# tb					= setsliderthumb (hasHScroll && newOrigin.x<>oldOrigin.x) miOSMetrics itemPtr True
												 (minx,newOrigin.x,maxx) viewx (toTuple itemSize) tb
		# tb					= setsliderthumb (hasVScroll && newOrigin.y<>oldOrigin.y) miOSMetrics itemPtr False
												 (miny,newOrigin.y,maxy) viewy (toTuple itemSize) tb
		  info					= {info & compoundOrigin=newOrigin}
		  clipRect1				= intersectRects contentRect clipRect
		| isEmpty itemH.wItems`
			# itemH				= {itemH & wItemInfo`=CompoundInfo` info}
			# (itemH,tb)		= drawCompoundLook` miOSMetrics ableContext1 wPtr clipRect1 itemH tb
			= (True,itemH,(updRgn,tb))
		| otherwise
			# oldItems`			= itemH.wItems`
			  orientation`		= [(domain,newOrigin):miOrientation]
			# (_,newItems`,tb)	= layoutControls` miOSMetrics hMargins` vMargins` spaces` itemSize itemSize orientation` oldItems` tb
			  newItems`			= shiftControls` (toVector itemPos) newItems`
			  itemH				= {itemH & wItems`=newItems`,wItemInfo`=CompoundInfo` info}
			# tb				= case updRgn of
									Just rgn -> osdisposergn rgn tb
									nothing  -> tb
			# (itemH, tb)		= forceValidCompoundClipState` miOSMetrics True wPtr defaultId shownContext itemH tb
			# (updRgn,tb)		= relayoutControls` miOSMetrics ableContext1 shownContext1 contentRect contentRect itemPos itemPos itemPtr defaultId oldItems` itemH.wItems` tb
			# (itemH, tb)		= drawCompoundLook` miOSMetrics ableContext1 wPtr clipRect1 itemH tb
			= (True,itemH,(Just updRgn,tb))
	where
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		oldOrigin				= info.compoundOrigin
		domainRect				= info.compoundDomain
		domain					= rectToRectangle domainRect
		itemPtr					= itemH.wItemPtr`
		itemPos					= itemH.wItemPos`
		itemSize				= itemH.wItemSize`
		itemAtts				= itemH.wItemAtts`
		(hasHScroll,hasVScroll)	= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		visScrolls				= osScrollbarsAreVisible miOSMetrics domainRect (toTuple itemSize) (hasHScroll,hasVScroll)
		contentRect				= getCompoundContentRect miOSMetrics visScrolls (posSizeToRect itemPos itemSize)
		contentSize				= rectSize contentRect
		shownContext1			= if shownContext itemH.wItemShow` shownContext
		ableContext1			= ableContext && itemH.wItemSelect`
		hMargins`				= getcontrolhmargin`   (snd (cselect iscontrolhmargin`   (ControlHMargin` (fst miHMargins) (snd miHMargins)) itemAtts))
		vMargins`				= getcontrolvmargin`   (snd (cselect iscontrolvmargin`   (ControlVMargin` (fst miVMargins) (snd miVMargins)) itemAtts))
		spaces`					= getcontrolitemspace` (snd (cselect iscontrolitemspace` (ControlItemSpace` (fst miItemSpaces) (snd miItemSpaces)) itemAtts))
		(minx,maxx,viewx)		= (domainRect.rleft,domainRect.rright, contentSize.w)
		(miny,maxy,viewy)		= (domainRect.rtop, domainRect.rbottom,contentSize.h)
		newOrigin				= {x=setBetween (oldOrigin.x+v.vx) minx (maxx-viewx),y=setBetween (oldOrigin.y+v.vy) miny (maxy-viewy)}


/*	Set the ViewDomain of a CompoundControl. (In future versions also customised controls.)
*/
setcontrolviewdomain :: !Id !ViewDomain !OSWindowMetrics !WIDS !WindowHandle`!*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontrolviewdomain id newDomain wMetrics wids=:{wPtr} wH=:{whKind`,whItems`,whSize`,whAtts`,whSelect`,whDefaultId`,whWindowInfo`} tb
	| whKind`==IsGameWindow
		= (wH,tb)
	# metricsInfo			= {miOSMetrics=wMetrics,miHMargins=hMargins,miVMargins=vMargins,miItemSpaces=spaces,miOrientation=orientation}
	# (_,itemHs,(updRgn,tb))= setWElement (setWItemDomain metricsInfo wPtr whDefaultId` True whSelect` clipRect (validateViewDomain newDomain))
								id whItems` (Nothing,tb)
	  wH					= {wH & whItems`=itemHs}
	| isNothing updRgn
		= (wH,tb)
	# updRgn				= fromJust updRgn
	# (empty,tb)			= osisemptyrgn updRgn tb
	| empty
		= (wH,osdisposergn updRgn tb)
	| otherwise
		= updatewindowbackgrounds` wMetrics updRgn wids wH tb
where
	windowInfo				= getWindowInfoWindowData whWindowInfo`
	(domainRect,origin,defHMargin,defVMargin)
							= if (whKind`==IsDialog)
								(sizeToRect whSize`,zero,wMetrics.osmHorMargin,wMetrics.osmVerMargin)
								(windowInfo.windowDomain,windowInfo.windowOrigin,0,0)
	(defHSpace, defVSpace)	= (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)
	hMargins				= getwindowhmargin`   (snd (cselect iswindowhmargin`   (WindowHMargin` defHMargin defHMargin) whAtts`))
	vMargins				= getwindowvmargin`   (snd (cselect iswindowvmargin`   (WindowVMargin` defVMargin defVMargin) whAtts`))
	spaces					= getwindowitemspace` (snd (cselect iswindowitemspace` (WindowItemSpace` defHSpace defVSpace) whAtts`))
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	orientation				= [(rectToRectangle domainRect,origin)]
	
	setWItemDomain :: !MetricsInfo !OSWindowPtr !(Maybe Id) !Bool !Bool !Rect !ViewDomain !Id !WItemHandle` !(!Maybe OSRgnHandle,!*OSToolbox)
																					-> (!Bool,!WItemHandle`,!(!Maybe OSRgnHandle,!*OSToolbox))
	
	setWItemDomain metricsInfo=:{miOSMetrics,miHMargins,miVMargins,miItemSpaces,miOrientation} wPtr defaultId shownContext ableContext clipRect newDomain id
				   itemH=:{wItemId`,wItemKind`} updRgn_tb=:(updRgn,tb)
		| not (isRecursiveControl wItemKind`)
			= (identifyMaybeId id wItemId`,itemH,updRgn_tb)
		| wItemKind`==IsLayoutControl
			| identifyMaybeId id wItemId`
				= (True,itemH,updRgn_tb)
			// otherwise
				# metricsInfo`	= {metricsInfo & miHMargins=hMargins`,miVMargins=vMargins`,miItemSpaces=spaces`}
				  clipRect1		= intersectRects clipRect (posSizeToRect itemPos itemSize)
				# (done,itemHs,updRgn_tb)
								= setWElement (setWItemDomain metricsInfo` wPtr defaultId shownContext1 ableContext1 clipRect1 newDomain)
										id itemH.wItems` updRgn_tb
				= (done,{itemH & wItems`=itemHs},updRgn_tb)
		| not (identifyMaybeId id itemH.wItemId`)
			# orientation`		= [(oldDomain,oldOrigin):miOrientation]
			  clipRect1			= intersectRects oldContentRect clipRect
			  metricsInfo`		= {metricsInfo & miHMargins=hMargins`,miVMargins=vMargins`,miItemSpaces=spaces`,miOrientation=orientation`}
			# (done,itemHs,updRgn_tb)
								= setWElement (setWItemDomain metricsInfo` wPtr defaultId shownContext1 ableContext1 clipRect1 newDomain)
									id itemH.wItems` updRgn_tb
			= (done,{itemH & wItems`=itemHs},updRgn_tb)
		| newDomain==oldDomain
			= (True,itemH,updRgn_tb)
		# (updRgn,tb)			= updRgn_tb
		# (minx,maxx,viewx)		= (newDomainRect.rleft,newDomainRect.rright, newContentSize.w)
		  (miny,maxy,viewy)		= (newDomainRect.rtop, newDomainRect.rbottom,newContentSize.h)
		  newOrigin				= {x=setBetween oldOrigin.x minx (max minx (maxx-viewx)),y=setBetween oldOrigin.y miny (max miny (maxy-viewy))}
		  info					= {info & compoundOrigin=newOrigin,compoundDomain=newDomainRect}
		# tb					= setsliderthumb hasHScroll miOSMetrics itemPtr True  (minx,newOrigin.x,maxx) viewx itemSize` tb
		# tb					= setsliderthumb hasVScroll miOSMetrics itemPtr False (miny,newOrigin.y,maxy) viewy itemSize` tb
		  oldItems`				= itemH.wItems`
		| isEmpty oldItems`		// CompoundControl has no controls
			# itemH				= {itemH & wItemInfo`=CompoundInfo` info}
			| shownContext1
				# (itemH,tb)	= drawCompoundLook` miOSMetrics ableContext1 wPtr (intersectRects newContentRect clipRect) itemH tb
				= (True,itemH,(updRgn,tb))
			// otherwise
				= (True,itemH,(updRgn,tb))
		// CompoundControl has controls
		# orientation`			= [(newDomain,newOrigin):miOrientation]
		# (_,newItems`,tb)		= layoutControls` miOSMetrics hMargins` vMargins` spaces` itemSize itemSize orientation` oldItems` tb
		  newItems`				= shiftControls` (toVector itemPos) newItems`
		  itemH					= {itemH & wItems`=newItems`,wItemInfo`=CompoundInfo` info}
		# tb					= case updRgn of
									Just rgn -> osdisposergn rgn tb
									nothing  -> tb
		# (itemH, tb)			= forceValidCompoundClipState` miOSMetrics True wPtr defaultId shownContext itemH tb
		# (updRgn,tb)			= relayoutControls` miOSMetrics ableContext1 shownContext1 newContentRect newContentRect itemPos itemPos itemPtr defaultId
									oldItems` itemH.wItems` tb
		| shownContext1
			# (itemH,tb)		= drawCompoundLook` miOSMetrics ableContext1 wPtr (intersectRects newContentRect clipRect) itemH tb
			= (True,itemH,(Just updRgn,tb))
		| otherwise
			= (True,itemH,(Just updRgn,tb))
	where
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		oldOrigin				= info.compoundOrigin
		oldDomainRect			= info.compoundDomain
		oldDomain				= rectToRectangle oldDomainRect
		newDomainRect			= rectangleToRect newDomain
		itemPtr					= itemH.wItemPtr`
		itemPos					= itemH.wItemPos`
		itemSize				= itemH.wItemSize`
		itemSize`				= toTuple itemSize
		itemAtts				= itemH.wItemAtts`
		itemRect				= posSizeToRect itemPos itemSize
		(hasHScroll,hasVScroll)	= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		oldVisScrolls			= osScrollbarsAreVisible miOSMetrics oldDomainRect itemSize` (hasHScroll,hasVScroll)
		newVisScrolls			= osScrollbarsAreVisible miOSMetrics newDomainRect itemSize` (hasHScroll,hasVScroll)
		oldContentRect			= getCompoundContentRect miOSMetrics oldVisScrolls itemRect
		newContentRect			= getCompoundContentRect miOSMetrics newVisScrolls itemRect
		newContentSize			= rectSize newContentRect
		shownContext1			= if shownContext itemH.wItemShow` shownContext
		ableContext1			= ableContext && itemH.wItemSelect`
		hMargins`				= getcontrolhmargin`   (snd (cselect iscontrolhmargin`   (ControlHMargin`   (fst miHMargins)   (snd miHMargins))   itemAtts))
		vMargins`				= getcontrolvmargin`   (snd (cselect iscontrolvmargin`   (ControlVMargin`   (fst miVMargins)   (snd miVMargins))   itemAtts))
		spaces`					= getcontrolitemspace` (snd (cselect iscontrolitemspace` (ControlItemSpace` (fst miItemSpaces) (snd miItemSpaces)) itemAtts))


setcontrolscrollfun	:: !Id !Direction ScrollFunction !WindowHandle` -> WindowHandle`
setcontrolscrollfun id direction scrollFun wH`=:{whItems`}
	# (_,itemHs,_)			= setWElement (setCompoundScrollFun direction scrollFun) id whItems` 0
	= {wH` & whItems`=itemHs}
where
	setCompoundScrollFun :: !Direction ScrollFunction !Id !WItemHandle` .s -> (!Bool,!WItemHandle`,.s)
	
	setCompoundScrollFun direction scrollFun id itemH=:{wItemId`,wItemKind`=IsCompoundControl} s
		| not (identifyMaybeId id wItemId`)
			# (found,itemHs,s)	= setWElement (setCompoundScrollFun direction scrollFun) id itemH.wItems` s
			= (found,{itemH & wItems`=itemHs},s)
		| direction==Horizontal && isJust hScroll
			# info				= {info & compoundHScroll=mapMaybe (setScrollFun scrollFun) hScroll}
			= (True,{itemH & wItemInfo`=CompoundInfo` info},s)
		| direction==Vertical && isJust vScroll
			# info				= {info & compoundVScroll=mapMaybe (setScrollFun scrollFun) vScroll}
			= (True,{itemH & wItemInfo`=CompoundInfo` info},s)
		| otherwise
			= (True,itemH,s)
	where
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		hScroll					= info.compoundHScroll
		vScroll					= info.compoundVScroll
		
		setScrollFun :: !ScrollFunction !ScrollInfo -> ScrollInfo
		setScrollFun f scrollInfo
			= {scrollInfo & scrollFunction=f}
	
	setCompoundScrollFun direction scrollFun id itemH=:{wItemId`,wItems`} s
		| identifyMaybeId id wItemId`
			= (True,itemH,s)
		| otherwise
			# (found,itemHs,s)	= setWElement (setCompoundScrollFun direction scrollFun) id wItems` s
			= (found,{itemH & wItems`=itemHs},s)
