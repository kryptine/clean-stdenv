implementation module controlinternal


//	Clean Object I/O library, version 1.2


import	StdBool, StdEnum, StdList, StdMisc, StdTuple
import	osrgn, oswindow
from	ospicture			import defaultPen
from	ostooltip			import OSaddControlToolTip
//from	osrgn				import osisemptyrgn, osdisposergn
import	commondef, wstateaccess
from	StdControlAttribute	import isControlTip, getControlTipAtt, isControlKeyboard
from	controldraw			import drawCustomButtonLook`, drawCustomLook`, drawCompoundLook`,
									drawInCustomButton`,  drawInCustom`,   drawInCompound`
from	controllayout		import layoutControls`
from	controlrelayout		import relayoutControls`
from	controlvalidate		import validateControlTitle, validateSliderState
from	windowaccess		import identifyMaybeId, getWItemPopUpInfo, getWindowInfoWindowData, getCompoundContentRect, getWindowContentRect
from	windowclipstate		import validateCompoundClipState`, forceValidCompoundClipState`
from	windowupdate		import updatewindowbackgrounds`
from	windowvalidate		import validateViewDomain


controlinternalFatalError :: String String -> .x
controlinternalFatalError function error
	= FatalError function "controlinternal" error


//	General occurrence tests on Id.

maybeRemoveCheck :: !(Maybe x) [x] -> (!Bool,![x])	| Eq x
maybeRemoveCheck maybe ids
	| isJust maybe	= RemoveCheck (fromJust maybe) ids
	| otherwise		= (False,ids)

removeOnIdOfPair :: Id ![(Id,x)] -> (!Bool,(Id,x),![(Id,x)])
removeOnIdOfPair id id_args
	= Remove (\(id`,_)->id`==id) undef id_args

removeOnIdOfTriple :: Id ![(Id,x,y)] -> (!Bool,(Id,x,y),![(Id,x,y)])
removeOnIdOfTriple id id_args
	= Remove (\(id`,_,_)->id`==id) undef id_args


/*	getContentRect returns the content rect of the window. 
*/
getContentRect :: !OSWindowMetrics !WindowInfo !Size -> Rect
getContentRect wMetrics (WindowInfo wInfo) size
	= getWindowContentRect wMetrics visScrolls (SizeToRect size)
where
	domainRect	= wInfo.windowDomain
	hasScrolls	= (isJust wInfo.windowHScroll,isJust wInfo.windowVScroll)
	visScrolls	= OSscrollbarsAreVisible wMetrics domainRect (toTuple size) hasScrolls
getContentRect _ (GameWindowInfo gwInfo) _
	= SizeToRect gwInfo.gamewindowSize
getContentRect _ NoWindowInfo size
	= SizeToRect size

/*	Calculate the intersection of the given Rect with the content of a CompoundControl.
*/
intersectRectContent :: !OSWindowMetrics !Rect !CompoundInfo !Point2 !Size -> Rect
intersectRectContent wMetrics clipRect info itemPos itemSize
	= IntersectRects clipRect contentRect
where
	hasScrolls	= (isJust info.compoundHScroll,isJust info.compoundVScroll)
	domainRect	= info.compoundDomain
	itemRect	= PosSizeToRect itemPos itemSize
	visScrolls	= OSscrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
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
						IsRadioControl	-> (True,\able->StateMap2 (\{radioItemPtr`}->OSsetRadioControlSelect wPtr radioItemPtr` clipRect able) radioItems)
						IsCheckControl	-> (True,\able->StateMap2 (\{checkItemPtr`}->OSsetCheckControlSelect wPtr checkItemPtr` clipRect able) checkItems)
  						IsPopUpControl	-> (True,OSsetPopUpControlSelect  wPtr itemPtr clipRect)
  						IsSliderControl	-> (True,OSsetSliderControlSelect wPtr itemPtr clipRect)
  						IsTextControl	-> (True,OSsetTextControlSelect   wPtr itemPtr clipRect)
  						IsEditControl	-> (True,OSsetEditControlSelect   wPtr itemPtr clipRect)
  						IsButtonControl	-> (True,OSsetButtonControlSelect wPtr itemPtr clipRect)
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
		  						IsCustomButtonControl	-> (True,drawCustomButtonLook`,OSsetCustomButtonControlSelect wPtr itemPtr clipRect)
		  						IsCustomControl			-> (True,drawCustomLook`,      OSsetCustomControlSelect       wPtr itemPtr clipRect)
		  						_						-> (False,undef,undef)
	
	enableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH=:{wItemKind`=IsCompoundControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| found
			# (itemH,tb)		= validateCompoundClipState` wMetrics False wPtr defId itemH tb
			# (itemH,tb)		= drawCompoundLook` wMetrics contextSelect wPtr clipRect1 itemH tb
			# (itemHs,(ids,tb))	= setAllWElements (enableWItemHandle` wMetrics wPtr defId True contextSelect contextShow1 clipRect1) itemH.wItems` (ids,tb)
			# tb				= OSsetCompoundSelect wPtr itemPtr clipRect scrollInfo contextSelect tb
			  itemH				= {itemH & wItemSelect`=True,wItems`=itemHs}
			= (itemH,(ids,tb))
		| overrule
			# (itemH,tb)		= validateCompoundClipState` wMetrics False wPtr defId itemH tb
			# (itemH,tb)		= drawCompoundLook` wMetrics contextSelect1 wPtr clipRect1 itemH tb
			# (itemHs,(ids,tb))	= setAllWElements (enableWItemHandle` wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1) itemH.wItems` (ids,tb)
			# tb				= OSsetCompoundSelect wPtr itemPtr clipRect scrollInfo contextSelect1 tb
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
						IsRadioControl	-> (True,\able->StateMap2 (\{radioItemPtr`}->OSsetRadioControlSelect wPtr radioItemPtr` clipRect able) radioItems)
						IsCheckControl	-> (True,\able->StateMap2 (\{checkItemPtr`}->OSsetCheckControlSelect wPtr checkItemPtr` clipRect able) checkItems)
						IsPopUpControl	-> (True,OSsetPopUpControlSelect  wPtr itemPtr clipRect)
						IsSliderControl	-> (True,OSsetSliderControlSelect wPtr itemPtr clipRect)
						IsTextControl	-> (True,OSsetTextControlSelect   wPtr itemPtr clipRect)
						IsEditControl	-> (True,OSsetEditControlSelect   wPtr itemPtr clipRect)
						IsButtonControl	-> (True,OSsetButtonControlSelect wPtr itemPtr clipRect)
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
								IsCustomButtonControl	-> (True,drawCustomButtonLook`,OSsetCustomButtonControlSelect wPtr itemPtr clipRect)
								IsCustomControl			-> (True,drawCustomLook`,      OSsetCustomControlSelect       wPtr itemPtr clipRect)
								_						-> (False,undef,undef)
	
	disableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH=:{wItemKind`=IsCompoundControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| found
			# itemH				= {itemH & wItemSelect`=False}
			# (itemH,tb)		= validateCompoundClipState` wMetrics False wPtr defId itemH tb
			# (itemH,tb)		= drawCompoundLook` wMetrics False wPtr clipRect1 itemH tb
			# (itemHs,(ids,tb))	= setAllWElements (disableWItemHandle` wMetrics wPtr defId True False contextShow1 clipRect1) itemH.wItems` (ids,tb)
			# tb				= OSsetCompoundSelect wPtr itemPtr clipRect scrollInfo False tb
			  itemH				= {itemH & wItems`=itemHs}
			= (itemH,(ids,tb))
		| overrule
			# (itemH,tb)		= validateCompoundClipState` wMetrics False wPtr defId itemH tb
			# (itemH,tb)		= drawCompoundLook` wMetrics contextSelect1 wPtr clipRect1 itemH tb
			# (itemHs,(ids,tb))	= setAllWElements (disableWItemHandle` wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1) itemH.wItems` (ids,tb)
			# tb				= OSsetCompoundSelect wPtr itemPtr clipRect scrollInfo contextSelect1 tb
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
	
	disableWItemHandle` _ _ _ _ _ _ _ itemH=:{wItemKind`=IsOtherControl _} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| found					= ({itemH & wItemSelect`=False},(ids,tb))
		| otherwise				= (itemH,(ids,tb))


//	Set the show state of the controls and provide proper feedback.

setcontrolsshowstate :: ![Id] !Bool !OSWindowMetrics !WIDS !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontrolsshowstate ids itemsShow wMetrics wids=:{wPtr} wH=:{whItems`,whSelect`,whSize`,whWindowInfo`} tb
	# (updRgn,tb)			= osnewrgn tb											// PA+++
	# (_,itemHs,(updRgn,tb))= setWElements (setWItemShowStates wMetrics wPtr itemsShow contextShow contextSelect clipRect) ids whItems` (updRgn,tb)
	  wH					= {wH & whItems`=itemHs}
//	# (wH,tb)				= updatewindowbackgrounds` wMetrics updRgn wids wH tb	// PA+++
	# (_,updRgnRect,tb)		= osgetrgnbox updRgn tb
	# tb					= OSinvalidateWindowRect wPtr updRgnRect tb
//	# tb					= OSinvalidateWindow wPtr tb
	# tb					= osdisposergn updRgn tb
	= (wH,tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	contextShow				= True
	contextSelect			= if whSelect` Able Unable
	
	setWItemShowStates :: !OSWindowMetrics !OSWindowPtr !Bool !Bool !SelectState !Rect ![Id] !WItemHandle` !(!OSRgnHandle,!*OSToolbox)
																				   -> (![Id],!WItemHandle`,!(!OSRgnHandle,!*OSToolbox))
	
	setWItemShowStates wMetrics wPtr itemsShow contextShow contextSelect clipRect ids itemH=:{wItemKind`=IsRadioControl} (updRgn,tb)
		# (found,ids)		= maybeRemoveCheck itemH.wItemId` ids
		| not found
			= (ids,itemH,(updRgn,tb))
		| otherwise
			# osShow		= contextShow && itemsShow
			  itemH			= {itemH & wItemShow`=itemsShow}
			  info			= getWItemRadioInfo` itemH.wItemInfo`
		//	# (updRgn,tb)	= StateMap2 (\{radioItemPtr`} tb->OSsetRadioControlShow wPtr radioItemPtr` clipRect osShow tb) info.radioItems` (updRgn,tb)
			# (updRgn,tb)	= StateMap2 (setradio osShow clipRect) info.radioItems` (updRgn,tb)
			= (ids,itemH,(updRgn,tb))
	where
		setradio osShow clipRect {radioItemPtr`,radioItemPos`,radioItemSize`} (updRgn,tb)
			# (itemRgn,tb)	= osnewrectrgn (PosSizeToRect radioItemPos` radioItemSize`) tb
			# (unionRgn,tb)	= osunionrgn updRgn itemRgn tb
			# tb			= osdisposergn updRgn (osdisposergn itemRgn tb)
			# tb			= OSsetRadioControlShow wPtr radioItemPtr` clipRect osShow tb
			= (unionRgn,tb)
	
	setWItemShowStates wMetrics wPtr itemsShow contextShow contextSelect clipRect ids itemH=:{wItemKind`=IsCheckControl} (updRgn,tb)
		# (found,ids)		= maybeRemoveCheck itemH.wItemId` ids
		| not found
			= (ids,itemH,(updRgn,tb))
		| otherwise
			# osShow		= contextShow && itemsShow
			  itemH			= {itemH & wItemShow`=itemsShow}
			  info			= getWItemCheckInfo` itemH.wItemInfo`
		//	# (updRgn,tb)	= StateMap2 (\{checkItemPtr`} tb->OSsetCheckControlShow wPtr checkItemPtr` clipRect osShow tb) info.checkItems` (updRgn,tb)
			# (updRgn,tb)	= StateMap2 (setcheck osShow clipRect) info.checkItems` (updRgn,tb)
			= (ids,itemH,(updRgn,tb))
	where
		setcheck osShow clipRect {checkItemPtr`,checkItemPos`,checkItemSize`} (updRgn,tb)
			# (itemRgn,tb)	= osnewrectrgn (PosSizeToRect checkItemPos` checkItemSize`) tb
			# (unionRgn,tb)	= osunionrgn updRgn itemRgn tb
			# tb			= osdisposergn updRgn (osdisposergn itemRgn tb)
			# tb			= OSsetRadioControlShow wPtr checkItemPtr` clipRect osShow tb
			= (unionRgn,tb)
	
	setWItemShowStates wMetrics wPtr itemsShow contextShow contextSelect clipRect ids itemH=:{wItemKind`} (updRgn,tb)
		| isMember wItemKind` [IsPopUpControl,IsSliderControl,IsTextControl,IsEditControl,IsButtonControl,IsCustomControl,IsCustomButtonControl]
			# (found,ids)	= maybeRemoveCheck itemH.wItemId` ids
			| not found
				= (ids,itemH,(updRgn,tb))
			// otherwise
				# osShow	= contextShow && itemsShow
				  itemH		= {itemH & wItemShow`=itemsShow}
				  osAction	= case wItemKind` of
				  				IsPopUpControl			-> OSsetPopUpControlShow
				  				IsSliderControl			-> OSsetSliderControlShow
				  				IsTextControl			-> OSsetTextControlShow
				  				IsEditControl			-> OSsetEditControlShow
				  				IsButtonControl			-> OSsetButtonControlShow
				  				IsCustomButtonControl	-> OSsetCustomButtonControlShow
				  				IsCustomControl			-> OSsetCustomControlShow
				  				_						-> controlinternalFatalError "setWItemShowStates" "unexpected ControlKind alternative"
				# tb		= osAction wPtr itemH.wItemPtr` clipRect osShow tb
				# (updRgn,tb)	= set osShow clipRect itemH.wItemPtr` (PosSizeToRect itemH.wItemPos` itemH.wItemSize`) (updRgn,tb)
				= (ids,itemH,(updRgn,tb))
		where
			set osShow clipRect itemPtr` itemRect (updRgn,tb)
				# (itemRgn,tb)	= osnewrectrgn itemRect tb
				# (unionRgn,tb)	= osunionrgn updRgn itemRgn tb
				# tb			= osdisposergn updRgn (osdisposergn itemRgn tb)
				# tb			= OSsetRadioControlShow wPtr itemPtr` clipRect osShow tb
				= (unionRgn,tb)
	
	setWItemShowStates wMetrics wPtr itemsShow contextShow contextSelect clipRect ids itemH=:{wItemKind`=IsCompoundControl} (updRgn,tb)
		# (found,ids)		= maybeRemoveCheck itemH.wItemId` ids
		# (ids,itemHs,(updRgn,tb))
							= setWElements (setWItemShowStates wMetrics wPtr itemsShow contextShow1 contextSelect1 clipRect1) ids itemH.wItems` (updRgn,tb)
		  itemH				= {itemH & wItems`=itemHs}
		| not found
			= (ids,itemH,(updRgn,tb))
		| otherwise
			# itemH			= {itemH & wItemShow`=itemsShow}
			# tb			= OSsetCompoundShow wPtr itemH.wItemPtr` clipRect itemsShow tb
			= (ids,itemH,(updRgn,tb))
	where
		contextSelect1		= if (enabled contextSelect) (if itemH.wItemSelect` Able Unable) contextSelect
		contextShow1		= contextShow && itemH.wItemShow`
		info				= getWItemCompoundInfo` itemH.wItemInfo`
		itemSize			= itemH.wItemSize`
		clipRect1			= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemSize
	
	setWItemShowStates wMetrics wPtr itemsShow contextShow contextSelect clipRect ids itemH=:{wItemKind`=IsOtherControl _} (updRgn,tb)
		# (found,ids)		= maybeRemoveCheck itemH.wItemId` ids
		| not found
			= (ids,itemH,(updRgn,tb))
		| otherwise
			# itemSelect	= if (enabled contextSelect) (if itemH.wItemSelect` Able Unable) contextSelect
			  itemSelected	= enabled itemSelect
			  itemH			= {itemH & wItemSelect`=itemSelected}
			= (ids,itemH,(updRgn,tb))


//	Set the MarkState of the controls and provide proper feedback.

setcontrolsmarkstate :: !Id !MarkState ![Index] !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontrolsmarkstate id mark indexs wMetrics wPtr wH=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)				= setWElement (setWItemMarks wMetrics wPtr mark clipRect indexs) id whItems` tb
	= ({wH & whItems`=itemHs},tb)
where
	clipRect					= getContentRect wMetrics whWindowInfo` whSize`
	
	setWItemMarks :: !OSWindowMetrics !OSWindowPtr !MarkState !Rect ![Index] !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	setWItemMarks wMetrics wPtr mark clipRect indexs id itemH=:{wItemKind`=IsCompoundControl} tb
		# (found,itemHs,tb)	= setWElement (setWItemMarks wMetrics wPtr mark clipRect1 indexs) id itemH.wItems` tb
		= (found,{itemH & wItems`=itemHs},tb)
	where
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1				= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemH.wItemSize`
	
	setWItemMarks wMetrics wPtr mark clipRect indexs id itemH=:{wItemKind`} tb
		| wItemKind`<>IsCheckControl || not (identifyMaybeId id itemH.wItemId`)
			= (False,itemH,tb)
		# info					= getWItemCheckInfo` itemH.wItemInfo`
		  checkItems			= info.checkItems`
		  nrCheckItems			= length checkItems
		  indexs				= filter (\index->IsBetween index 1 nrCheckItems) indexs
		| isEmpty indexs
			= (True,itemH,tb)
		| otherwise
			# (checkItems,tb)	= StateMap2 (setCheckMark wPtr clipRect mark) indexs (checkItems,tb)
			  itemH				= {itemH & wItemInfo`=CheckInfo` {info & checkItems`=checkItems}}
			= (True,itemH,tb)
	where
		setCheckMark :: !OSWindowPtr !Rect !MarkState !Index !(![CheckItemInfo`],!*OSToolbox) -> (![CheckItemInfo`],!*OSToolbox)
		setCheckMark wPtr clipRect mark index (checkItems,tb)
			# (before,[item:after])	= Split (index-1) checkItems
			  (title,width,_)		= item.checkItem`
			  checkItems			= before++[{item & checkItem`=(title,width,mark)}:after]
			= (checkItems,OSsetCheckControl wPtr item.checkItemPtr` clipRect (marked mark) tb)
/* Mac oswindow version.
OSsetCheckControl :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetCheckControl parentWindow cPtr clipRect check tb
	# (visRgn, tb)			= WindowGetVisRgn parentWindow tb
	# (clipRgn,tb)			= IntersectRgnRect visRgn clipRect tb
	# (port,rgn,font,tb)	= openDrawing parentWindow tb
	# tb					= QSetClip clipRgn tb
	# tb					= SetCtlValue cPtr (if check 1 0) tb
	# tb					= closeDrawing port font rgn [rgn,clipRgn] tb
	= tb
*/

//	Set the text of the controls and provide proper feedback.

setcontroltexts :: ![(Id,String)] !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontroltexts id_texts wMetrics wPtr wH`=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)	= setWElements (setControlText wMetrics wPtr True clipRect) id_texts whItems` tb
	= ({wH` & whItems`=itemHs},tb)
where
	clipRect		= getContentRect wMetrics whWindowInfo` whSize`
	
	setControlText :: !OSWindowMetrics !OSWindowPtr !Bool !Rect ![(Id,String)] !WItemHandle` !*OSToolbox
															-> (![(Id,String)],!WItemHandle`,!*OSToolbox)
	
	setControlText wMetrics wPtr shownContext clipRect id_texts itemH=:{wItemKind`=IsCompoundControl} tb
		# (id_texts,itemHs,tb)	= setWElements (setControlText wMetrics wPtr shownContext1 clipRect1) id_texts itemH.wItems` tb
		= (id_texts,{itemH & wItems`=itemHs},tb)
	where
		info			= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1		= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemH.wItemSize`
		shownContext1	= shownContext && itemH.wItemShow`
	
	setControlText wMetrics wPtr shownContext clipRect id_texts itemH=:{wItemKind`=IsEditControl} tb
		| isNothing itemH.wItemId`
			= (id_texts,itemH,tb)
		# id						= fromJust itemH.wItemId`
		  (found,id_text,id_texts)	= removeOnIdOfPair id id_texts
		| not found
			= (id_texts,itemH,tb)
		| otherwise
			# shownContext1			= shownContext && itemH.wItemShow`
			  (_,text)				= id_text
			  editInfo				= getWItemEditInfo` itemH.wItemInfo`
			  itemH					= {itemH & wItemInfo`=EditInfo` {editInfo & editInfoText=text}}
			  itemRect				= PosSizeToRect itemH.wItemPos` itemH.wItemSize`
			# tb					= OSsetEditControlText wPtr itemH.wItemPtr` clipRect itemRect shownContext1 text tb
			= (id_texts,itemH,tb)
	/* Mac oswindow version.
		OSsetEditControlText :: !OSWindowPtr !OSWindowPtr !Rect !Rect !Bool !String !*OSToolbox -> *OSToolbox
		OSsetEditControlText parentWindow hTE clipRect itemRect text shown tb
			# (visRgn, tb)			= WindowGetVisRgn parentWindow tb
			# (clipRgn,tb)			= IntersectRgnRect visRgn clipRect tb
			# (clipRgn1,tb)			= setEditRgnRect clipRgn shown itemRect tb
			# (port,rgn,font,tb)	= openDrawing parentWindow tb
			# tb					= QSetClip clipRgn1 tb
			# tb					= setEditText text hTE itemRect tb
			# tb					= closeDrawing port font rgn [rgn,clipRgn,clipRgn1] tb
			= tb
		where
			setEditRgnRect :: !RgnHandle !Bool !Rect !*OSToolbox -> (!RgnHandle,!*OSToolbox)
			setEditRgnRect clipRgn shown itemRect tb
				# addEditRect		= if shown zero itemRect
				# (rgn,tb)			= QNewRgn tb
				# tb				= QRectRgn rgn addEditRect tb
				= QDiffRgn clipRgn rgn rgn tb
			
			setEditText :: !String !TEHandle !Rect !*OSToolbox -> *OSToolbox
			setEditText text hTE itemRect tb
				# tb				= TESetSelect 0 32767 hTE tb
				# tb				= TEDelete hTE tb
				# tb				= TESetText text hTE tb
				# tb				= TESetSelect 0 0 hTE tb
				= TEUpdate itemRect hTE tb
	*/
	
	setControlText wMetrics wPtr shownContext clipRect id_texts itemH=:{wItemKind`=IsTextControl} tb
		| isNothing itemH.wItemId`
			= (id_texts,itemH,tb)
		# id						= fromJust itemH.wItemId`
		  (found,id_text,id_texts)	= removeOnIdOfPair id id_texts
		| not found
			= (id_texts,itemH,tb)
		| otherwise
			# (_,text)				= id_text
			  shownContext1			= shownContext && itemH.wItemShow`
			  textInfo				= getWItemTextInfo` itemH.wItemInfo`
			  itemH					= {itemH & wItemInfo`=TextInfo` {textInfo & textInfoText=text}}
			  itemRect				= PosSizeToRect itemH.wItemPos` itemH.wItemSize`
			# tb					= OSsetTextControlText wPtr itemH.wItemPtr` clipRect itemRect shownContext1 text tb
			= (id_texts,itemH,tb)
	/* Mac oswindow version.
	OSsetTextControlText :: !OSWindowPtr !OSWindowPtr !Rect !Rect !Bool !String !*OSToolbox -> *OSToolbox
	OSsetTextControlText parentWindow tPtr clipRect itemRect show text tb
		| not show
		= tb
		# (visRgn, tb)			= WindowGetVisRgn parentWindow parentWindow tb
		# (clipRgn,tb)			= IntersectRgnRect visRgn clipRect tb
		# (port,rgn,font,tb)	= openDrawing parentWindow tb
		# tb					= QSetClip clipRgn tb
		# tb					= TETextBox text itemRect TEFlushDefault tb
		# tb					= closeDrawing port font rgn [rgn,clipRgn] tb
		= tb
	*/
	
	setControlText wMetrics wPtr _ clipRect id_texts itemH=:{wItemKind`=IsButtonControl} tb
		| isNothing itemH.wItemId`
			= (id_texts,itemH,tb)
		# id						= fromJust itemH.wItemId`
		  (found,id_text,id_texts)	= removeOnIdOfPair id id_texts
		| not found
			= (id_texts,itemH,tb)
		| otherwise
			# (_,text)				= id_text
			  buttonInfo			= getWItemButtonInfo` itemH.wItemInfo`
			  itemH					= {itemH & wItemInfo`=ButtonInfo` {buttonInfo & buttonInfoText=text}}
			# tb					= OSsetButtonControlText wPtr itemH.wItemPtr` clipRect (validateControlTitle text) tb
			= (id_texts,itemH,tb)
	/* Mac oswindow version.
	OSsetButtonControlText :: !OSWindowPtr !OSWindowPtr !Rect !String !*OSToolbox -> *OSToolbox
	OSsetButtonControlText parentWindow buttonPtr clipRect text tb
		# (visRgn, tb)			= WindowGetVisRgn parentWindow parentWindow tb
		# (clipRgn,tb)			= IntersectRgnRect visRgn clipRect tb
		# (port,rgn,font,tb)	= openDrawing parentWindow tb
		# tb					= QSetClip clipRgn tb
		# tb					= SetCTitle buttonPtr text tb
		# tb					= closeDrawing port font rgn [rgn,clipRgn] tb
		= tb
	*/
	
	setControlText _ _ _ _ id_texts itemH tb
		= (id_texts,itemH,tb)


//	Set the cursor position of an EditControl, and handle proper feedback.

seteditcontrolcursor :: !Id !Int !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
seteditcontrolcursor id pos wMetrics wPtr wH`=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)			= setWElement (setEditCursor wMetrics wPtr True clipRect pos) id whItems` tb
	= ({wH` & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	setEditCursor :: !OSWindowMetrics !OSWindowPtr !Bool !Rect !Int !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	
	setEditCursor wMetrics wPtr shownContext clipRect pos id itemH=:{wItemKind`=IsCompoundControl} tb
		# (found,itemHs,tb)	= setWElement (setEditCursor wMetrics wPtr shownContext1 clipRect1 pos) id itemH.wItems` tb
		= (found,{itemH & wItems`=itemHs},tb)
	where
		info				= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1			= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemH.wItemSize`
		shownContext1		= shownContext && itemH.wItemShow`
	
	setEditCursor wMetrics wPtr shownContext clipRect pos id itemH=:{wItemKind`=IsEditControl} tb
		| isNothing itemH.wItemId`
			= (False,itemH,tb)
		# itemId			= fromJust itemH.wItemId`
		| itemId<>id
			= (False,itemH,tb)
		| otherwise
			# itemRect		= PosSizeToRect itemH.wItemPos` itemH.wItemSize`
			# tb			= OSsetEditControlCursor wPtr itemH.wItemPtr` clipRect itemRect pos tb
			= (True,itemH,tb)
	/* Mac oswindow version.
	OSsetEditControlCursor :: !OSWindowPtr !OSWindowPtr !Rect !Rect !Int !*OSToolbox -> *OSToolbox
	OSsetEditControlCursor parentWindow hTE clipRect itemRect pos tb
		# (visRgn, tb)		= WindowGetVisRgn parentWindow tb
		# (clipRgn,tb)		= IntersectRgnRect visRgn clipRect tb
		# (port,rgn,font,tb)= openDrawing parentWindow tb
		# tb				= QSetClip clipRgn tb
		# tb				= TESetSelect pos pos hTE tb
		# tb				= closeDrawing port font rgn [rgn,clipRgn] tb
		= tb
	*/
	
	setEditCursor _ _ _ _ _ _ itemH tb
		= (False,itemH,tb)


//	Set the look of a control, and handle proper feedback.

setcontrolslook :: ![(Id,Bool,(Bool,Look))] !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontrolslook looks wMetrics wPtr wH=:{whItems`,whSelect`,whDefaultId`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)	= setWElements (setWItemLook wMetrics wPtr whSelect` True resizeable whDefaultId` clipRect) looks whItems` tb
	= ({wH & whItems`=itemHs},tb)
where
	clipRect		= getContentRect wMetrics whWindowInfo` whSize`
	resizeable		= True
	
	setWItemLook :: !OSWindowMetrics !OSWindowPtr !Bool !Bool !Bool (Maybe Id) !Rect ![(Id,Bool,(Bool,Look))] !WItemHandle` !*OSToolbox
																				 -> (![(Id,Bool,(Bool,Look))],!WItemHandle`,!*OSToolbox)
	
	setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect looks itemH=:{wItemId`,wItemKind`=IsCompoundControl,wItemSize`} tb
		# (found,look,looks)		= if (isJust wItemId`) (removeOnIdOfTriple (fromJust wItemId`) looks) (False,undef,looks)
		# (looks,itemHs,tb)			= setWElements (setWItemLook wMetrics wPtr ableContext1 shownContext1 resizeable defId clipRect1) looks itemH.wItems` tb
		  itemH						= {itemH & wItems`=itemHs}
		| not found
			= (looks,itemH,tb)
		# (_,redraw,(sysLook,cLook))= look
		  info						= {info & compoundLookInfo=Just {lookInfo & compoundLook={	lookFun			= cLook
		  																					 ,	lookPen			= pen
		  																					 ,	lookSysUpdate	= sysLook
		  																					 }}}
		  itemH						= {itemH & wItemInfo`=CompoundInfo` info}
		| not redraw || not shownContext1
			= (looks,itemH,tb)
		| otherwise
			# (itemH,tb)			= validateCompoundClipState` wMetrics False wPtr defId itemH tb
			# (itemH,tb)			= drawCompoundLook` wMetrics ableContext1 wPtr clipRect1 itemH tb
			= (looks,itemH,tb)
	where
		info						= getWItemCompoundInfo` itemH.wItemInfo`
		hasScrolls					= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		hasLook						= isJust info.compoundLookInfo
		compoundLookInfo			= fromJust info.compoundLookInfo
		lookInfo					= if hasLook compoundLookInfo {compoundLook=undef,compoundClip={clipRgn=0,clipOk=False}}
		pen							= if hasLook compoundLookInfo.compoundLook.lookPen defaultPen
		visScrolls					= OSscrollbarsAreVisible wMetrics info.compoundDomain (toTuple wItemSize`) hasScrolls
		itemRect					= PosSizeToRect itemH.wItemPos` wItemSize`
		contentRect					= getCompoundContentRect wMetrics visScrolls itemRect
		clipRect1					= IntersectRects clipRect contentRect
		ableContext1				= ableContext  && itemH.wItemSelect`
		shownContext1				= shownContext && itemH.wItemShow`
	
	setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect looks itemH=:{wItemId`,wItemKind`=IsCustomButtonControl} tb
		# (found,look,looks)		= if (isJust wItemId`) (removeOnIdOfTriple (fromJust wItemId`) looks) (False,undef,looks)
		| not found
			= (looks,itemH,tb)
		# (_,redraw,(sysLook,cLook))= look
		  info						= {info & cButtonInfoLook={itemLook & lookFun=cLook,lookSysUpdate=sysLook}}
		  itemH						= {itemH & wItemInfo`=CustomButtonInfo` info}
		| not redraw || not shownContext1
			= (looks,itemH,tb)
		| otherwise
			# (itemH,tb)			= drawCustomButtonLook` ableContext1 wPtr clipRect itemH tb
			= (looks,itemH,tb)
	where
		info						= getWItemCustomButtonInfo` itemH.wItemInfo`
		itemLook					= info.cButtonInfoLook
		ableContext1				= ableContext  && itemH.wItemSelect`
		shownContext1				= shownContext && itemH.wItemShow`
	
	setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect looks itemH=:{wItemId`,wItemKind`=IsCustomControl} tb
		# (found,look,looks)		= if (isJust wItemId`) (removeOnIdOfTriple (fromJust wItemId`) looks) (False,undef,looks)
		| not found
			= (looks,itemH,tb)
		# (_,redraw,(sysLook,cLook))= look
		# info						= {info & customInfoLook={itemLook & lookFun=cLook,lookSysUpdate=sysLook}}
		# itemH						= {itemH & wItemInfo`=CustomInfo` info}
		| not redraw || not shownContext1
			= (looks,itemH,tb)
		| otherwise
			# (itemH,tb)			= drawCustomLook` ableContext1 wPtr clipRect itemH tb
			= (looks,itemH,tb)
	where
		info						= getWItemCustomInfo` itemH.wItemInfo`
		itemLook					= info.customInfoLook
		ableContext1				= ableContext  && itemH.wItemSelect`
		shownContext1				= shownContext && itemH.wItemShow`
	
	setWItemLook _ _ _ _ _ _ _ looks itemH=:{wItemId`} tb
		# (_,_,looks)				= if (isJust wItemId`) (removeOnIdOfTriple (fromJust wItemId`) looks) (False,undef,looks)
		= (looks,itemH,tb)


//	Draw in a customised control.

drawincontrol :: !Id !.(St *Picture .x) !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!Maybe .x,!WindowHandle`,!*OSToolbox)
drawincontrol controlId drawfun wMetrics wPtr wH=:{whItems`,whDefaultId`,whSize`,whWindowInfo`} tb
	# (_,itemHs,(x,_,tb))	= setWElement (drawInWItem wMetrics wPtr resizeable whDefaultId` clipRect) controlId whItems` (Nothing,drawfun,tb)
	= (x,{wH & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	resizeable				= True
	
	drawInWItem :: !OSWindowMetrics !OSWindowPtr Bool (Maybe Id) !Rect !Id !WItemHandle` !(!Maybe .x,.(St *Picture .x),!*OSToolbox)
																 -> (!Bool,!WItemHandle`,!(!Maybe .x,.(St *Picture .x),!*OSToolbox))
	
	drawInWItem wMetrics wPtr resizeable defId clipRect id itemH=:{wItemId`,wItemPtr`,wItemKind`=IsCompoundControl} x_tb
		| not (identifyMaybeId id wItemId`)
			# (found,itemHs,x_tb)	= setWElement (drawInWItem wMetrics wPtr resizeable defId clipRect1) id itemH.wItems` x_tb
			= (found,{itemH & wItems`=itemHs},x_tb)
		| not hasCompoundLookInfo
			= (True,itemH,x_tb)
		| otherwise
			# (_,drawfun,tb)		= x_tb
			# (itemH,tb)			= validateCompoundClipState` wMetrics False wPtr defId itemH tb
			# (x,itemH,tb)			= drawInCompound` wPtr drawfun clipRect1 itemH tb
			= (True,itemH,(Just x,undef,tb))
	where
		info						= getWItemCompoundInfo` itemH.wItemInfo`
		domainRect					= info.compoundDomain
		hasScrolls					= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		hasCompoundLookInfo			= isJust info.compoundLookInfo
		itemPos						= itemH.wItemPos`
		itemSize					= itemH.wItemSize`
		itemRect					= PosSizeToRect itemPos itemSize
		visScrolls					= OSscrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
		contentRect					= getCompoundContentRect wMetrics visScrolls itemRect
		clipRect1					= IntersectRects clipRect contentRect
	
	drawInWItem _ wPtr _ _ clipRect id itemH=:{wItemId`,wItemKind`=IsCustomButtonControl} x_tb
		| not (identifyMaybeId id wItemId`)
			= (False,itemH,x_tb)
		| otherwise
			# (_,drawfun,tb)		= x_tb
			# (x,itemH,tb)			= drawInCustomButton` wPtr drawfun clipRect itemH tb
			= (True,itemH,(Just x,undef,tb))
	
	drawInWItem _ wPtr _ _ clipRect id itemH=:{wItemId`,wItemKind`=IsCustomControl} x_tb
		| not (identifyMaybeId id wItemId`)
			= (False,itemH,x_tb)
		| otherwise
			# (_,drawfun,tb)		= x_tb
			# (x,itemH,tb)			= drawInCustom` wPtr drawfun clipRect itemH tb
			= (True,itemH,(Just x,undef,tb))
	
	drawInWItem _ _ _ _ _ id itemH=:{wItemId`} x_tb
		= (identifyMaybeId id wItemId`,itemH,x_tb)


//	Change the state of the slider and handle proper feedback.

setsliderstates :: ![(Id,IdFun SliderState)] !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setsliderstates id_fs wMetrics wPtr wH`=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)			= setWElements (setSliderState wMetrics wPtr clipRect) id_fs whItems` tb
	= ({wH` & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	setSliderState :: !OSWindowMetrics !OSWindowPtr !Rect ![(Id,IdFun SliderState)] !WItemHandle` !*OSToolbox
													  -> (![(Id,IdFun SliderState)],!WItemHandle`,!*OSToolbox)
	setSliderState wMetrics wPtr clipRect id_fs itemH=:{wItemKind`=IsCompoundControl} tb
		# (id_fs,itemHs,tb)	= setWElements (setSliderState wMetrics wPtr clipRect1) id_fs itemH.wItems` tb
		= (id_fs,{itemH & wItems`=itemHs},tb)
	where
		info				= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1			= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemH.wItemSize`
	
	setSliderState wMetrics wPtr clipRect id_fs itemH=:{wItemId`,wItemKind`=IsSliderControl,wItemPtr`} tb
		| isNothing wItemId`
			= (id_fs,itemH,tb)
		# id				= fromJust wItemId`
		  (found,id_f,id_fs)= removeOnIdOfPair id id_fs
		| not found
			= (id_fs,itemH,tb)
		| otherwise
			# info			= getWItemSliderInfo` itemH.wItemInfo`
			  oldState		= info.sliderInfoState`
			  (_,f)			= id_f
			  newState		= validateSliderState (f oldState)
			  itemH			= {itemH & wItemInfo`=SliderInfo` {info & sliderInfoState`=newState}}
			  (tbMin,tbThumb,tbMax,_)
			  				= toOSscrollbarRange (newState.sliderMin,newState.sliderThumb,newState.sliderMax) 0
			# tb			= OSsetSliderThumb wPtr wItemPtr` clipRect (not (IsEmptyRect clipRect)) (tbMin,tbThumb,tbMax) tb
			= (id_fs,itemH,tb)
	setSliderState _ _ _ id_fs itemH tb
		= (id_fs,itemH,tb)
/* Mac oswindow version.
OSsetSliderThumb :: !OSWindowPtr !OSWindowPtr !Rect !Bool !(!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
OSsetSliderThumb parentWindow cPtr clipRect redraw (min,thumb,max) tb
	# (visRgn, tb)			= WindowGetVisRgn parentWindow tb
	# (clipRgn,tb)			= IntersectRgnRect visRgn clipRect tb
	# (port,rgn,font,tb)	= openDrawing parentWindow tb
	# tb					= QSetClip clipRgn tb
	# tb					= SetCtlValue cPtr thumb tb
	# tb					= HiliteControl cPtr (if (min<>max) 0 255) tb
	# tb					= closeDrawing port font rgn [rgn,clipRgn] tb
	= tb
*/

//	Selecting a RadioControl item.

selectradiocontrol :: !Id !Index !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
selectradiocontrol id index wMetrics wPtr wH=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)			= setWElement (selectWItemRadioControl wMetrics wPtr clipRect index) id whItems` tb
	= ({wH & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	selectWItemRadioControl :: !OSWindowMetrics !OSWindowPtr !Rect !Index !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	
	selectWItemRadioControl wMetrics wPtr clipRect index id itemH=:{wItemKind`=IsCompoundControl} tb
		# (done,itemHs,tb)	= setWElement (selectWItemRadioControl wMetrics wPtr clipRect1 index) id itemH.wItems` tb
		= (done,{itemH & wItems`=itemHs},tb)
	where
		info				= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1			= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemH.wItemSize`
	
	selectWItemRadioControl wMetrics wPtr clipRect index id itemH=:{wItemId`,wItemKind`} tb
		| wItemKind`<>IsRadioControl || not (identifyMaybeId id wItemId`)
			= (False,itemH,tb)
		# info				= getWItemRadioInfo` itemH.wItemInfo`
		  cur				= info.radioIndex`
		  items				= info.radioItems`
		  nrItems			= length items
		  index				= SetBetween index 1 nrItems
		  info				= {info & radioIndex`=index}
		  itemH				= {itemH & wItemInfo`=RadioInfo` info}
		| index==cur
			= (True,itemH,tb)
		| otherwise
			# tb			= OSsetRadioControl wPtr (items!!(cur-1)).radioItemPtr` (items!!(index-1)).radioItemPtr` clipRect tb
			= (True,itemH,tb)
	/* Mac oswindow version.
	OSsetRadioControl :: !OSWindowPtr !OSWindowPtr !OSWindowPtr !Rect !*OSToolbox -> *OSToolbox
	OSsetRadioControl parentWindow curPtr newPtr clipRect tb
		# (visRgn, tb)			= WindowGetVisRgn parentWindow tb
		# (clipRgn,tb)			= IntersectRgnRect visRgn clipRect tb
		# (port,rgn,font,tb)	= openDrawing parentWindow tb
		# tb					= QSetClip clipRgn tb
		# tb					= SetCtlValue curPtr 0 tb
		# tb					= SetCtlValue newPtr 1 tb
		# tb					= closeDrawing port font rgn [rgn,clipRgn] tb
		= tb
	*/


//	Select a PopUpControl item.
	
selectpopupitem :: !Id !Index !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
selectpopupitem id index wMetrics wPtr wH=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)			= setWElement (selectWItemPopUp wMetrics wPtr True index clipRect) id whItems` tb
	= ({wH & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	selectWItemPopUp :: !OSWindowMetrics !OSWindowPtr !Bool !Index !Rect !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	
	selectWItemPopUp wMetrics wPtr shownContext index clipRect id itemH=:{wItemKind`=IsCompoundControl} tb
		# (found,itemHs,tb)	= setWElement (selectWItemPopUp wMetrics wPtr shownContext1 index clipRect1) id itemH.wItems` tb
		= (found,{itemH & wItems`=itemHs},tb)
	where
		info				= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1			= intersectRectContent wMetrics clipRect info itemH.wItemPos` itemH.wItemSize`
		shownContext1		= shownContext && itemH.wItemShow`
	
	selectWItemPopUp wMetrics wPtr shownContext index clipRect id itemH=:{wItemId`,wItemKind`} tb
		| wItemKind`<>IsPopUpControl || not (identifyMaybeId id wItemId`)
			= (False,itemH,tb)
		# info				= getWItemPopUpInfo` itemH.wItemInfo`
		  popUps			= info.popUpInfoItems`
		  curindex			= info.popUpInfoIndex`
		  nrPopUps			= length popUps
		  index				= SetBetween index 1 nrPopUps
		| curindex==index
			= (True,itemH,tb)
		| otherwise
			# shownContext1	= shownContext && itemH.wItemShow`
			  info			= {info & popUpInfoIndex`=index}
			  itemH			= {itemH & wItemInfo`=PopUpInfo` info}
			  itemRect		= PosSizeToRect itemH.wItemPos` itemH.wItemSize`
			# tb			= OSsetPopUpControl wPtr itemH.wItemPtr` clipRect itemRect curindex index (popUps!!(index-1)) shownContext1 tb
			= (True,itemH,tb)
	/* Mac oswindow version.
	OSsetPopUpControl :: !OSWindowPtr !OSWindowPtr !Rect !Rect !Int !Int !String !Bool !*OSToolbox -> *OSToolbox
	OSsetPopUpControl parentWindow popupPtr clipRect itemRect current new text shown tb
		# tb				= CheckItem popupPtr current False tb
		# tb				= CheckItem popupPtr new     True  tb
		| not shown
		= tb
		# (visRgn, tb)		= WindowGetVisRgn parentWindow tb
		# (clipRgn,tb)		= IntersectRgnRect visRgn clipRect tb
		# (port,rgn,font,tb)= openDrawing parentWindow tb
		# tb				= QSetClip clipRgn tb
		# tb				= redrawPopUpItemText itemRect text tb
		# tb				= closeDrawing port font rgn [rgn,clipRgn] tb
		= tb
	*/


//	Add new items to a PopUpControl. 

openpopupitems :: !Id !Index ![PopUpControlItem .pst] !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox
																-> (!WindowHandle .ls .pst, !*OSToolbox)
openpopupitems id index newItems wPtr wH=:{whItems} tb
	# (_,itemHs,tb)	= openWElementsPopUpItems wPtr id index newItems whItems tb
	= ({wH & whItems=itemHs},tb)
where
	openWElementsPopUpItems :: !OSWindowPtr !Id !Index ![PopUpControlItem .pst] ![WElementHandle .ls .pst] !*OSToolbox
																	  -> (!Bool,![WElementHandle .ls .pst],!*OSToolbox)
	openWElementsPopUpItems wPtr id index newItems itemHs tb
		| isEmpty itemHs
			= (False,itemHs,tb)
		# (itemH,itemHs)		= HdTl itemHs
		# (done,itemH,tb)		= openWElementPopUpItems wPtr id index newItems itemH tb
		| done
			= (done,[itemH:itemHs],tb)
		| otherwise
			# (done,itemHs,tb)	= openWElementsPopUpItems wPtr id index newItems itemHs tb
			= (done,[itemH:itemHs],tb)
	where
		openWElementPopUpItems :: !OSWindowPtr !Id !Index ![PopUpControlItem .pst] !(WElementHandle .ls .pst) !*OSToolbox
																		  -> (!Bool,!WElementHandle .ls .pst, !*OSToolbox)
		openWElementPopUpItems wPtr id index newItems (WItemHandle itemH) tb
			# (done,itemH,tb)	= openWItemPopUpItems wPtr id index newItems itemH tb
			= (done,WItemHandle itemH,tb)
		where
			openWItemPopUpItems :: !OSWindowPtr !Id !Index ![PopUpControlItem .pst] !(WItemHandle .ls .pst) !*OSToolbox
																		   -> (!Bool,!WItemHandle .ls .pst, !*OSToolbox)
			openWItemPopUpItems wPtr id index items itemH=:{wItemKind=IsPopUpControl} tb
				| not (identifyMaybeId id itemH.wItemId)
					= (False,itemH,tb)
				# (newPopUpPtr,editPtr,tb)
									= OScreateEmptyPopUpControl wPtr (0,0) itemH.wItemShow ableContext 
																(toTuple popUpPos) (toTuple popUpSize) (length newItems) isEditable tb
				# (_,tb)			= StateMap2 (appendPopUp newPopUpPtr newIndex) newItems (1,tb)
				# tb				= OSstackWindow newPopUpPtr popUpPtr tb
				# tb				= OSdestroyPopUpControl popUpPtr tb
				  newPopUpInfo		= {	popUpInfoItems = newItems
									  ,	popUpInfoIndex = newIndex
									  ,	popUpInfoEdit  = if isEditable (Just {curEditInfo & popUpEditPtr=editPtr}) Nothing
									  }
				  itemH				= {itemH & wItemInfo=PopUpInfo newPopUpInfo,wItemPtr=newPopUpPtr}
				| not hasTip
					= (True,itemH,tb)
				| otherwise
					= (True,itemH,OSaddControlToolTip wPtr newPopUpPtr (getControlTipAtt tipAtt) tb)
			where
				(hasTip,tipAtt)	= Select isControlTip undef itemH.wItemAtts
				isEditable		= Contains isControlKeyboard itemH.wItemAtts
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
				(before,after)	= Split index curItems
				
				appendPopUp :: !OSWindowPtr !Index !(PopUpControlItem .pst) !(!Int,!*OSToolbox) -> (!Int,!*OSToolbox)
				appendPopUp popUpPtr index (title,_) (itemNr,tb)
					# (_,tb)			= OScreatePopUpControlItem popUpPtr (-1) ableContext title (index==itemNr) tb
					= (itemNr+1,tb)
			
			openWItemPopUpItems wPtr id index newItems itemH=:{wItemKind=IsCompoundControl} tb
				# (done,itemHs,tb)	= openWElementsPopUpItems wPtr id index newItems itemH.wItems tb
				= (done,{itemH & wItems=itemHs},tb)
			
			openWItemPopUpItems _ _ _ _ itemH tb
				= (False,itemH,tb)
		
		openWElementPopUpItems wPtr id index newItems (WListLSHandle itemHs) tb
			# (done,itemHs,tb)	= openWElementsPopUpItems wPtr id index newItems itemHs tb
			= (done,WListLSHandle itemHs,tb)
		
		openWElementPopUpItems wPtr id index newItems (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
			# (done,itemHs,tb)	= openWElementsPopUpItems wPtr id index newItems itemHs tb
			= (done,WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
		
		openWElementPopUpItems wPtr id index newItems (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
			# (done,itemHs,tb)	= openWElementsPopUpItems wPtr id index newItems itemHs tb
			= (done,WChangeLSHandle {wChH & wChangeItems=itemHs},tb)


//	Remove items from a PopUpControl. 

closepopupitems :: !Id ![Index] !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,!*OSToolbox)
closepopupitems id indexs wPtr wH=:{whItems} tb
	# (_,itemHs,tb)	= closeWElementsPopUpItems wPtr id indexs whItems tb
	= ({wH & whItems=itemHs},tb)
where
	closeWElementsPopUpItems :: !OSWindowPtr !Id ![Index] ![WElementHandle .ls .pst] !*OSToolbox
												-> (!Bool,![WElementHandle .ls .pst],!*OSToolbox)
	closeWElementsPopUpItems wPtr id indexs itemHs tb
		| isEmpty itemHs
			= (False,itemHs,tb)
		# (itemH,itemHs)		= HdTl itemHs
		# (done,itemH,tb)		= closeWElementPopUpItems wPtr id indexs itemH tb
		| done
			= (done,[itemH:itemHs],tb)
		| otherwise
			# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr id indexs itemHs tb
			= (done,[itemH:itemHs],tb)
	where
		closeWElementPopUpItems :: !OSWindowPtr !Id ![Index] !(WElementHandle .ls .pst) !*OSToolbox
													-> (!Bool,!WElementHandle .ls .pst, !*OSToolbox)
		closeWElementPopUpItems wPtr id indexs (WItemHandle itemH) tb
			# (done,itemH,tb)	= closeWItemPopUpItems wPtr id indexs itemH tb
			= (done,WItemHandle itemH,tb)
		where
			closeWItemPopUpItems :: !OSWindowPtr !Id ![Index] !(WItemHandle .ls .pst) !*OSToolbox
													 -> (!Bool,!WItemHandle .ls .pst, !*OSToolbox)
			closeWItemPopUpItems wPtr id indexs itemH=:{wItemKind=IsPopUpControl} tb
				| not (identifyMaybeId id itemH.wItemId)
					= (False,itemH,tb)
				# (newPopUpPtr,editPtr,tb)
									= OScreateEmptyPopUpControl wPtr (0,0) itemH.wItemShow ableContext 
																(toTuple popUpPos) (toTuple popUpSize) (length newItems) isEditable tb
				# (_,tb)			= StateMap2 (appendPopUp newPopUpPtr newIndex) newItems (1,tb)
				# tb				= OSstackWindow newPopUpPtr popUpPtr tb
				# tb				= OSdestroyPopUpControl popUpPtr tb
				  newPopUpInfo		= {	popUpInfoItems = newItems
									  ,	popUpInfoIndex = newIndex
									  ,	popUpInfoEdit  = if isEditable (Just {curEditInfo & popUpEditPtr=editPtr}) Nothing
									  }
				  itemH				= {itemH & wItemInfo=PopUpInfo newPopUpInfo,wItemPtr=newPopUpPtr}
				| not hasTip
					= (True,itemH,tb)
				| otherwise
					= (True,itemH,OSaddControlToolTip wPtr newPopUpPtr (getControlTipAtt tipAtt) tb)
			where
				(hasTip,tipAtt)	= Select isControlTip undef itemH.wItemAtts
				isEditable		= Contains isControlKeyboard itemH.wItemAtts
				ableContext		= itemH.wItemSelect
				popUpPtr		= itemH.wItemPtr
				popUpSize		= itemH.wItemSize
				popUpPos		= itemH.wItemPos
				popUpInfo		= getWItemPopUpInfo itemH.wItemInfo
				curEditInfo		= fromJust popUpInfo.popUpInfoEdit
				curIndex		= popUpInfo.popUpInfoIndex
				curItems		= popUpInfo.popUpInfoItems
				newItems		= map snd (filter (\(i,_)->not (isMember i indexs)) (zip2 [1..] curItems))
				nrNewItems		= length newItems
				newIndex		= if (isMember curIndex indexs) 1 (min nrNewItems curIndex)
				
				appendPopUp :: !OSWindowPtr !Index !(PopUpControlItem .ps) !(!Int,!*OSToolbox) -> (!Int,!*OSToolbox)
				appendPopUp popUpPtr index (title,_) (itemNr,tb)
					# (_,tb)			= OScreatePopUpControlItem popUpPtr (-1) ableContext title (index==itemNr) tb
					= (itemNr+1,tb)
			
			closeWItemPopUpItems wPtr id indexs itemH=:{wItemKind=IsCompoundControl} tb
				# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr id indexs itemH.wItems tb
				= (done,{itemH & wItems=itemHs},tb)
			
			closeWItemPopUpItems _ _ _ itemH tb
				= (False,itemH,tb)
		
		closeWElementPopUpItems wPtr id indexs (WListLSHandle itemHs) tb
			# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr id indexs itemHs tb
			= (done,WListLSHandle itemHs,tb)
		
		closeWElementPopUpItems wPtr id indexs (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
			# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr id indexs itemHs tb
			= (done,WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
		
		closeWElementPopUpItems wPtr id indexs (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
			# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr id indexs itemHs tb
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
		# (itemH,itemHs)= HdTl itemHs
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
	| otherwise		= OSsetCompoundSlider wMetrics itemPtr isHScroll (toOSscrollbarRange scrollValues viewSize) maxcoords tb


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
								(SizeToRect whSize`,zero,wMetrics.osmHorMargin,wMetrics.osmVerMargin)
								(windowInfo.windowDomain,windowInfo.windowOrigin,0,0)
	(defHSpace, defVSpace)	= (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)
	hMargins				= getwindowhmargin`   (snd (Select iswindowhmargin`   (WindowHMargin` defHMargin defHMargin) whAtts`))
	vMargins				= getwindowvmargin`   (snd (Select iswindowvmargin`   (WindowVMargin` defVMargin defVMargin) whAtts`))
	spaces					= getwindowitemspace` (snd (Select iswindowitemspace` (WindowItemSpace` defHSpace defVSpace) whAtts`))
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	orientation				= [(RectToRectangle domainRect,origin)]
	
	moveWItemFrame :: !MetricsInfo !OSWindowPtr !(Maybe Id) !Bool !Bool !Rect !Vector2 !Id
							!WItemHandle` !(!Maybe OSRgnHandle,!*OSToolbox)
				  -> (!Bool,!WItemHandle`,!(!Maybe OSRgnHandle,!*OSToolbox))
	
	moveWItemFrame metricsInfo=:{miOSMetrics,miHMargins,miVMargins,miItemSpaces,miOrientation} wPtr defaultId shownContext ableContext clipRect v id
				   itemH=:{wItemKind`} (updRgn,tb)
		| wItemKind`<>IsCompoundControl
			= (False,itemH,(updRgn,tb))
		| not (identifyMaybeId id itemH.wItemId`)
			# orientation`		= [(domain,oldOrigin):miOrientation]
			  metricsInfo`		= {metricsInfo & miHMargins=hMargins`,miVMargins=vMargins`,miItemSpaces=spaces`,miOrientation=orientation`}
			# (done,itemHs,(updRgn,tb))
								= setWElement (moveWItemFrame metricsInfo` wPtr defaultId shownContext1 ableContext1 clipRect1 v) id itemH.wItems` (updRgn,tb)
			= (done,{itemH & wItems`=itemHs},(updRgn,tb))
		| newOrigin==oldOrigin
			= (True,itemH,(updRgn,tb))
		| otherwise
			# tb				= setsliderthumb hasHScroll miOSMetrics itemPtr True  (minx,newOrigin.x,maxx) viewx (toTuple itemSize) tb
			# tb				= setsliderthumb hasVScroll miOSMetrics itemPtr False (miny,newOrigin.y,maxy) viewy (toTuple itemSize) tb
			  oldItems`			= itemH.wItems`
			  orientation`		= [(domain,newOrigin):miOrientation]
			# (_,newItems`,tb)	= layoutControls` miOSMetrics hMargins` vMargins` spaces` itemSize itemSize orientation` oldItems` tb
			  newItems`			= shiftControls` (toVector itemPos) newItems`
			  info				= {info & compoundOrigin=newOrigin}
			  itemH				= {itemH & wItems`=newItems`,wItemInfo`=CompoundInfo` info}
			# (itemH, tb)		= forceValidCompoundClipState` miOSMetrics True wPtr defaultId itemH tb
			# (updRgn,tb)		= relayoutControls` miOSMetrics ableContext1 contentRect contentRect itemPos itemPos itemPtr defaultId oldItems` itemH.wItems` tb
			= (True,itemH,(Just updRgn,tb))
	where
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		oldOrigin				= info.compoundOrigin
		domainRect				= info.compoundDomain
		domain					= RectToRectangle domainRect
		itemPtr					= itemH.wItemPtr`
		itemPos					= itemH.wItemPos`
		itemSize				= itemH.wItemSize`
		itemAtts				= itemH.wItemAtts`
		(hasHScroll,hasVScroll)	= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		visScrolls				= OSscrollbarsAreVisible miOSMetrics domainRect (toTuple itemSize) (hasHScroll,hasVScroll)
		contentRect				= getCompoundContentRect miOSMetrics visScrolls (PosSizeToRect itemPos itemSize)
		contentSize				= RectSize contentRect
		clipRect1				= IntersectRects contentRect clipRect
		shownContext1			= if shownContext itemH.wItemShow` shownContext
		ableContext1			= ableContext && itemH.wItemSelect`
		hMargins`				= getcontrolhmargin`   (snd (Select iscontrolhmargin`   (ControlHMargin` (fst miHMargins) (snd miHMargins)) itemAtts))
		vMargins`				= getcontrolvmargin`   (snd (Select iscontrolvmargin`   (ControlVMargin` (fst miVMargins) (snd miVMargins)) itemAtts))
		spaces`					= getcontrolitemspace` (snd (Select iscontrolitemspace` (ControlItemSpace` (fst miItemSpaces) (snd miItemSpaces)) itemAtts))
		(minx,maxx,viewx)		= (domainRect.rleft,domainRect.rright, contentSize.w)
		(miny,maxy,viewy)		= (domainRect.rtop, domainRect.rbottom,contentSize.h)
		newOrigin				= {x=SetBetween (oldOrigin.x+v.vx) minx (maxx-viewx),y=SetBetween (oldOrigin.y+v.vy) miny (maxy-viewy)}


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
								(SizeToRect whSize`,zero,wMetrics.osmHorMargin,wMetrics.osmVerMargin)
								(windowInfo.windowDomain,windowInfo.windowOrigin,0,0)
	(defHSpace, defVSpace)	= (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)
	hMargins				= getwindowhmargin`   (snd (Select iswindowhmargin`   (WindowHMargin` defHMargin defHMargin) whAtts`))
	vMargins				= getwindowvmargin`   (snd (Select iswindowvmargin`   (WindowVMargin` defVMargin defVMargin) whAtts`))
	spaces					= getwindowitemspace` (snd (Select iswindowitemspace` (WindowItemSpace` defHSpace defVSpace) whAtts`))
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	orientation				= [(RectToRectangle domainRect,origin)]
	
	setWItemDomain :: !MetricsInfo !OSWindowPtr !(Maybe Id) !Bool !Bool !Rect !ViewDomain !Id !WItemHandle` !(!Maybe OSRgnHandle,!*OSToolbox)
																					-> (!Bool,!WItemHandle`,!(!Maybe OSRgnHandle,!*OSToolbox))
	
	setWItemDomain metricsInfo=:{miOSMetrics,miHMargins,miVMargins,miItemSpaces,miOrientation} wPtr defaultId shownContext ableContext clipRect newDomain id
				   itemH=:{wItemKind`} (updRgn,tb)
		| wItemKind`<>IsCompoundControl
			= (False,itemH,(updRgn,tb))
		| not (identifyMaybeId id itemH.wItemId`)
			# orientation`		= [(oldDomain,oldOrigin):miOrientation]
			  clipRect1			= IntersectRects oldContentRect clipRect
			  metricsInfo`		= {metricsInfo & miHMargins=hMargins`,miVMargins=vMargins`,miItemSpaces=spaces`,miOrientation=orientation`}
			# (done,itemHs,(updRgn,tb))
								= setWElement (setWItemDomain metricsInfo` wPtr defaultId shownContext1 ableContext1 clipRect1 newDomain)
									id itemH.wItems` (updRgn,tb)
			= (done,{itemH & wItems`=itemHs},(updRgn,tb))
		| newDomain==oldDomain
			= (True,itemH,(updRgn,tb))
		| otherwise
			# (minx,maxx,viewx)	= (newDomainRect.rleft,newDomainRect.rright, newContentSize.w)
			  (miny,maxy,viewy)	= (newDomainRect.rtop, newDomainRect.rbottom,newContentSize.h)
			  newOrigin			= {x=SetBetween oldOrigin.x minx (max minx (maxx-viewx)),y=SetBetween oldOrigin.y miny (max miny (maxy-viewy))}
			# tb				= setsliderthumb hasHScroll miOSMetrics itemPtr True  (minx,newOrigin.x,maxx) viewx itemSize` tb
			# tb				= setsliderthumb hasVScroll miOSMetrics itemPtr False (miny,newOrigin.y,maxy) viewy itemSize` tb
			  oldItems`			= itemH.wItems`
			  orientation`		= [(newDomain,newOrigin):miOrientation]
			# (_,newItems`,tb)	= layoutControls` miOSMetrics hMargins` vMargins` spaces` itemSize itemSize orientation` oldItems` tb
			  newItems`			= shiftControls` (toVector itemPos) newItems`
			  info				= {info & compoundOrigin=newOrigin,compoundDomain=newDomainRect}
			  itemH				= {itemH & wItems`=newItems`,wItemInfo`=CompoundInfo` info}
			# (itemH, tb)		= forceValidCompoundClipState` miOSMetrics True wPtr defaultId itemH tb
			# (updRgn,tb)		= relayoutControls` miOSMetrics ableContext1 newContentRect newContentRect itemPos itemPos itemPtr defaultId
									oldItems` itemH.wItems` tb
			# (itemH,tb)		= drawCompoundLook` miOSMetrics ableContext1 wPtr (IntersectRects newContentRect clipRect) itemH tb
			= (True,itemH,(Just updRgn,tb))
	where
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		oldOrigin				= info.compoundOrigin
		oldDomainRect			= info.compoundDomain
		oldDomain				= RectToRectangle oldDomainRect
		newDomainRect			= RectangleToRect newDomain
		itemPtr					= itemH.wItemPtr`
		itemPos					= itemH.wItemPos`
		itemSize				= itemH.wItemSize`
		itemSize`				= toTuple itemSize
		itemAtts				= itemH.wItemAtts`
		itemRect				= PosSizeToRect itemPos itemSize
		(hasHScroll,hasVScroll)	= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		oldVisScrolls			= OSscrollbarsAreVisible miOSMetrics oldDomainRect itemSize` (hasHScroll,hasVScroll)
		newVisScrolls			= OSscrollbarsAreVisible miOSMetrics newDomainRect itemSize` (hasHScroll,hasVScroll)
		oldContentRect			= getCompoundContentRect miOSMetrics oldVisScrolls itemRect
		newContentRect			= getCompoundContentRect miOSMetrics newVisScrolls itemRect
		newContentSize			= RectSize newContentRect
		shownContext1			= if shownContext itemH.wItemShow` shownContext
		ableContext1			= ableContext && itemH.wItemSelect`
		hMargins`				= getcontrolhmargin`   (snd (Select iscontrolhmargin`   (ControlHMargin`   (fst miHMargins)   (snd miHMargins))   itemAtts))
		vMargins`				= getcontrolvmargin`   (snd (Select iscontrolvmargin`   (ControlVMargin`   (fst miVMargins)   (snd miVMargins))   itemAtts))
		spaces`					= getcontrolitemspace` (snd (Select iscontrolitemspace` (ControlItemSpace` (fst miItemSpaces) (snd miItemSpaces)) itemAtts))


setcontrolscrollfun	:: !Id !Direction ScrollFunction !WindowHandle` -> WindowHandle`
setcontrolscrollfun id direction scrollFun wH`=:{whItems`}
	# (_,itemHs,_)			= setWElement (setCompoundScrollFun direction scrollFun) id whItems` 0
	= {wH` & whItems`=itemHs}
where
	setCompoundScrollFun :: !Direction ScrollFunction !Id !WItemHandle` .s -> (!Bool,!WItemHandle`,.s)
	
	setCompoundScrollFun direction scrollFun id itemH=:{wItemId`,wItemKind`=IsCompoundControl} s
		| isNothing wItemId` || fromJust wItemId`<>id
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
	
	setCompoundScrollFun _ _ _ itemH s
		= (False,itemH,s)
