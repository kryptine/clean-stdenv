implementation module windowupdate


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	ospicture, osrgn, oswindow
import	commondef, deviceevents, windowhandle, wstate
from	windowaccess	import getWItemCompoundInfo, getWItemCustomButtonInfo, getWItemCustomInfo, getWindowInfoWindowData,
								getWItemPopUpInfo, getWItemRadioInfo, getWItemCheckInfo, getWItemTextInfo
from	windowclipstate	import validateWindowClipState
from	wstateaccess	import getWItemCompoundInfo`

//import windows


windowupdateFatalError :: String String -> .x
windowupdateFatalError rule error
	= fatalError rule "windowupdate" error


/*	updatewindow updates the window, using the UpdateInfo argument.
*/
updatewindow :: !OSWindowMetrics !UpdateInfo !(WindowHandle .ls .pst) !*OSToolbox
										  -> (!WindowHandle .ls .pst, !*OSToolbox)
updatewindow wMetrics info=:{updWIDS={wPtr},updGContext} wH tb
	# (wH,tb)			= validateWindowClipState wMetrics False wPtr wH tb
	# (osPict,tb)		= getUpdateContext wPtr updGContext tb
	# (wH,osPict,tb)	= updatewindowbackground wMetrics False Nothing info wH osPict tb
	# (wH,osPict,tb)	= updatecontrols         wMetrics               info wH osPict tb
	# tb				= setUpdateContext wPtr updGContext osPict tb
	= (wH,tb)
where
	getUpdateContext :: OSWindowPtr !(Maybe OSPictContext) !*OSToolbox -> (!OSPictContext,!*OSToolbox)
	getUpdateContext _ (Just osPict) tb
		= (osPict,tb)
	getUpdateContext wPtr _ tb
		# (osPict,tb)	= osGrabWindowPictContext wPtr tb
		# tb			= osBeginUpdate wPtr tb
		= (osPict,tb)
	
	setUpdateContext :: !OSWindowPtr !(Maybe OSPictContext) !OSPictContext !*OSToolbox -> *OSToolbox
	setUpdateContext wPtr updContext osPict tb
		| isJust updContext
			= tb
		| otherwise
			# tb		= osEndUpdate wPtr tb
			= osReleaseWindowPictContext wPtr osPict tb


/*	updatewindowbackgrounds(`) redraws the (window/compound) backgrounds that are inside the OSRgnHandle argument.
	After redrawing the OSRgnHandle argument is disposed!
*/
updatewindowbackgrounds :: !OSWindowMetrics !OSRgnHandle !WIDS !(WindowHandle .ls .pst) !*OSToolbox
															-> (!WindowHandle .ls .pst, !*OSToolbox)
updatewindowbackgrounds wMetrics backgrRgn wids=:{wPtr} wH=:{whItems,whSelect,whSize,whWindowInfo} tb
	# (_,backgrRect,tb)						= osgetrgnbox backgrRgn tb
	| isEmptyRect backgrRect
		= (wH,osdisposergn backgrRgn tb)
	| otherwise
		# updInfo							= {	updWIDS			= wids
											  ,	updWindowArea	= backgrRect
											  ,	updControls		= []
											  ,	updGContext		= Nothing
											  }
		# (osPict,tb)						= osGrabWindowPictContext wPtr tb
		# picture							= packPicture zero (copyPen pen) True osPict tb		// Make sure picture is initialised with the proper pen, but origin zero
		# (origin,pen,toScreen,osPict,tb)	= peekPicture picture
		# (wH,osPict,tb)					= updatewindowbackground wMetrics True (Just backgrRgn) updInfo wH osPict tb
		# (backgrRgn,wH,osPict,tb)			= updatecontrolbackgrounds wMetrics backgrRgn wH osPict tb
		# picture							= unpeekPicture origin pen toScreen osPict tb
		# (_,_,_,osPict,tb)					= unpackPicture picture								// Make sure picture is cleared
		# tb								= osReleaseWindowPictContext wPtr osPict tb
		= (wH,osdisposergn backgrRgn tb)
where
	pen	= case whWindowInfo of
			WindowInfo info	-> info.windowLook.lookPen
			other			-> defaultPen
	
	updatecontrolbackgrounds :: !OSWindowMetrics !OSRgnHandle !(WindowHandle .ls .pst) !OSPictContext !*OSToolbox
											 -> (!OSRgnHandle, !WindowHandle .ls .pst, !OSPictContext,!*OSToolbox)
	updatecontrolbackgrounds wMetrics backgrRgn wH=:{whItems,whSelect,whSize} osPict tb
		# (backgrRgn,itemHs,osPict,tb)	= updatebackgrounds wMetrics (sizeToRect whSize) whSelect backgrRgn whItems osPict tb
		= (backgrRgn,{wH & whItems=itemHs},osPict,tb)
	where
		updatebackgrounds :: !OSWindowMetrics !OSRect !Bool !OSRgnHandle ![WElementHandle .ls .pst] !OSPictContext !*OSToolbox
													    -> (!OSRgnHandle,![WElementHandle .ls .pst],!OSPictContext,!*OSToolbox)
		updatebackgrounds _ _ _ backgrRgn [] osPict tb
			= (backgrRgn,[],osPict,tb)
		updatebackgrounds wMetrics wFrame ableContext backgrRgn [itemH:itemHs] osPict tb
			# (empty,tb)						= osisemptyrgn backgrRgn tb
			| empty
				= (backgrRgn,[itemH:itemHs],osPict,tb)
			| otherwise
				# (backgrRgn,itemH, osPict,tb)	= updatebackground  wMetrics wFrame ableContext backgrRgn itemH  osPict tb
				# (backgrRgn,itemHs,osPict,tb)	= updatebackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict tb
				= (backgrRgn,[itemH:itemHs],osPict,tb)
		where
			updatebackground :: !OSWindowMetrics !OSRect !Bool !OSRgnHandle !(WElementHandle .ls .pst) !OSPictContext !*OSToolbox
														   -> (!OSRgnHandle, !WElementHandle .ls .pst, !OSPictContext,!*OSToolbox)
			updatebackground wMetrics wFrame ableContext backgrRgn (WItemHandle itemH=:{wItemShow,wItemVirtual,wItemKind,wItemPos,wItemSize}) osPict tb
			//	| not wItemShow || wItemVirtual || wItemKind<>IsCompoundControl
				| not wItemShow || wItemVirtual || not (isRecursiveControl wItemKind)	// PA: isRecursive includes also LayoutControls
					= (backgrRgn,WItemHandle itemH,osPict,tb)
				# (_,backgrRect,tb)					= osgetrgnbox backgrRgn tb
				# compoundRect						= posSizeToRect wItemPos wItemSize
				| disjointRects backgrRect compoundRect
					= (backgrRgn,WItemHandle itemH,osPict,tb)
				| otherwise
					# (backgrRgn,itemH,osPict,tb)	= updatecompoundbackground wMetrics wFrame compoundRect ableContext backgrRgn itemH osPict tb
					= (backgrRgn,WItemHandle itemH,osPict,tb)
			where
				updatecompoundbackground :: !OSWindowMetrics !OSRect !OSRect !Bool !OSRgnHandle !(WItemHandle .ls .pst) !OSPictContext !*OSToolbox
														 				       -> (!OSRgnHandle, !WItemHandle .ls .pst, !OSPictContext,!*OSToolbox)
				updatecompoundbackground wMetrics wFrame compoundRect ableContext backgrRgn itemH=:{wItemKind=IsLayoutControl,wItems} osPict tb
					# (backgrRgn,itemHs,osPict,tb)
											= updatebackgrounds wMetrics cFrame cAble backgrRgn wItems osPict tb
					# (rectRgn,tb)			= osnewrectrgn cFrame tb
					# (diffRgn,tb)			= osdiffrgn backgrRgn rectRgn tb
					# tb					= stateMap2 osdisposergn [rectRgn,backgrRgn] tb
					= (diffRgn,{itemH & wItems=itemHs},osPict,tb)
				where
					cFrame					= intersectRects wFrame compoundRect
					cAble					= ableContext && itemH.wItemSelect
				updatecompoundbackground wMetrics wFrame compoundRect ableContext backgrRgn itemH=:{wItemKind=IsCompoundControl,wItems} osPict tb
					# (updRgn,tb)			= ossectrgn backgrRgn clipInfo.clipRgn tb
					# (empty,tb)			= osisemptyrgn updRgn tb
					| empty
						# (backgrRgn,itemHs,osPict,tb)
											= updatebackgrounds wMetrics cFrame cAble backgrRgn wItems osPict tb
						# (rectRgn,tb)		= osnewrectrgn cFrame tb
						# (diffRgn,tb)		= osdiffrgn backgrRgn rectRgn tb
						# tb				= stateMap2 osdisposergn [rectRgn,backgrRgn,updRgn] tb
						= (diffRgn,{itemH & wItems=itemHs},osPict,tb)
					| otherwise
						# picture			= unpeekPicture zero defaultPen True osPict tb
						# picture			= setpictorigin (origin-itemPos) picture
						# picture			= setpictpen lookPen picture
						# picture			= clipospicture updRgn contentRect (lookFun (if cAble Able Unable) updState) picture
						# picture			= setpictpen defaultPen picture
						# picture			= setpictorigin zero picture
						# (_,_,_,osPict,tb)	= peekPicture picture
						# (backgrRgn,itemHs,osPict,tb)
											= updatebackgrounds wMetrics cFrame cAble backgrRgn wItems osPict tb
						# (rectRgn,tb)		= osnewrectrgn cFrame tb
						# (diffRgn,tb)		= osdiffrgn backgrRgn rectRgn tb
						# tb				= stateMap2 osdisposergn [rectRgn,backgrRgn,updRgn] tb
						= (diffRgn,{itemH & wItems=itemHs},osPict,tb)
				where
					itemPos					= itemH.wItemPos
					itemSize				= itemH.wItemSize
					info					= getWItemCompoundInfo itemH.wItemInfo
					compLookInfo			= info.compoundLookInfo
					{lookFun,lookPen}		= compLookInfo.compoundLook
					clipInfo				= compLookInfo.compoundClip
					origin					= info.compoundOrigin
					domainRect				= info.compoundDomain
					hasScrolls				= (isJust info.compoundHScroll,isJust info.compoundVScroll)
					visScrolls				= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
					contentRect				= osGetCompoundContentRect wMetrics visScrolls compoundRect
					cFrame					= intersectRects wFrame contentRect
					cAble					= ableContext && itemH.wItemSelect
					updFrame				= rectToRectangle cFrame
					updState				= {oldFrame=updFrame,newFrame=updFrame,updArea=[updFrame]}
					
					clipospicture :: !OSRgnHandle !OSRect !(IdFun *Picture) !*Picture -> *Picture
					clipospicture newClipRgn rect drawf picture
						#! (rectRgn,picture)	= accpicttoolbox (osnewrectrgn rect) picture
						#! (curClipRgn,picture)	= pictgetcliprgn picture
						   isEmptyClipRgn		= if (curClipRgn==0) (\_ tb->(True,tb)) osisemptyrgn	// PA: don't test for emptyness on 0 clipping regions
						#! (emptyCur,picture)	= accpicttoolbox (isEmptyClipRgn curClipRgn) picture
						#! picture				= (if emptyCur (pictsetcliprgn rectRgn) (pictandcliprgn rectRgn)) picture
						#! picture				= pictandcliprgn newClipRgn picture
						#! picture				= drawf picture
						#! picture				= pictsetcliprgn curClipRgn picture
						#  picture				= apppicttoolbox (osdisposergn rectRgn o (if (curClipRgn==0) id (osdisposergn curClipRgn))) picture
						= picture
				
				updatecompoundbackground _ _ _ _ _ {wItemKind} _ _
					= windowupdateFatalError "updatecompoundbackground (updatewindowbackgrounds)" ("unexpected control kind: "+++toString wItemKind)
			
			updatebackground wMetrics wFrame ableContext backgrRgn (WListLSHandle itemHs) osPict tb
				# (backgrRgn,itemHs,osPict,tb)	= updatebackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict tb
				= (backgrRgn,WListLSHandle itemHs,osPict,tb)
			
			updatebackground wMetrics wFrame ableContext backgrRgn (WExtendLSHandle wExH=:{wExtendItems=itemHs}) osPict tb
				# (backgrRgn,itemHs,osPict,tb)	= updatebackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict tb
				= (backgrRgn,WExtendLSHandle {wExH & wExtendItems=itemHs},osPict,tb)
			
			updatebackground wMetrics wFrame ableContext backgrRgn (WChangeLSHandle wChH=:{wChangeItems=itemHs}) osPict tb
				# (backgrRgn,itemHs,osPict,tb)	= updatebackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict tb
				= (backgrRgn,WChangeLSHandle {wChH & wChangeItems=itemHs},osPict,tb)

updatewindowbackgrounds` :: !OSWindowMetrics !OSRgnHandle !WIDS !WindowHandle` !*OSToolbox
															-> (!WindowHandle`,!*OSToolbox)
updatewindowbackgrounds` wMetrics backgrRgn wids=:{wPtr} wH=:{whItems`,whSelect`,whSize`,whWindowInfo`} tb
	# (_,backgrRect,tb)						= osgetrgnbox backgrRgn tb
	| isEmptyRect backgrRect
		= (wH,osdisposergn backgrRgn tb)
	| otherwise
		# updInfo							= {	updWIDS			= wids
											  ,	updWindowArea	= backgrRect
											  ,	updControls		= []
											  ,	updGContext		= Nothing
											  }
		# (osPict,tb)						= osGrabWindowPictContext wPtr tb
		# picture							= packPicture zero (copyPen pen) True osPict tb	// Make sure picture is initialised with the proper pen, but origin zero
		# (origin,pen,toScreen,osPict,tb)	= peekPicture picture
		# (wH,osPict,tb)					= updatewindowbackground` wMetrics True (Just backgrRgn) updInfo wH osPict tb
		# (backgrRgn,wH,osPict,tb)			= updatecontrolbackgrounds wMetrics backgrRgn wH osPict tb
		# picture							= unpeekPicture origin pen toScreen osPict tb
		# (_,_,_,osPict,tb)					= unpackPicture picture						// Make sure picture is cleared
		# tb								= osReleaseWindowPictContext wPtr osPict tb
		= (wH,osdisposergn backgrRgn tb)
where
	pen	= case whWindowInfo` of
			WindowInfo info	-> info.windowLook.lookPen
			other			-> defaultPen
	
	updatecontrolbackgrounds :: !OSWindowMetrics !OSRgnHandle !WindowHandle` !OSPictContext !*OSToolbox
											 -> (!OSRgnHandle,!WindowHandle`,!OSPictContext,!*OSToolbox)
	updatecontrolbackgrounds wMetrics backgrRgn wH=:{whItems`,whSelect`,whSize`} osPict tb
		# (backgrRgn,itemHs,osPict,tb)	= updatebackgrounds wMetrics (sizeToRect whSize`) whSelect` backgrRgn whItems` osPict tb
		= (backgrRgn,{wH & whItems`=itemHs},osPict,tb)
	where
		updatebackgrounds :: !OSWindowMetrics !OSRect !Bool !OSRgnHandle ![WElementHandle`] !OSPictContext !*OSToolbox
													    -> (!OSRgnHandle,![WElementHandle`],!OSPictContext,!*OSToolbox)
		updatebackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict tb
			# (empty,tb)						= osisemptyrgn backgrRgn tb
			| empty || isEmpty itemHs
				= (backgrRgn,itemHs,osPict,tb)
			| otherwise
				# (itemH,itemHs)				= hdtl itemHs
				# (backgrRgn,itemH, osPict,tb)	= updatebackground  wMetrics wFrame ableContext backgrRgn itemH  osPict tb
				# (backgrRgn,itemHs,osPict,tb)	= updatebackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict tb
				= (backgrRgn,[itemH:itemHs],osPict,tb)
		where
			updatebackground :: !OSWindowMetrics !OSRect !Bool !OSRgnHandle !WElementHandle` !OSPictContext !*OSToolbox
														   -> (!OSRgnHandle,!WElementHandle`,!OSPictContext,!*OSToolbox)
			updatebackground wMetrics wFrame ableContext backgrRgn (WItemHandle` itemH=:{wItemShow`,wItemVirtual`,wItemKind`,wItemPos`,wItemSize`}) osPict tb
			//	| not wItemShow` || wItemVirtual` || wItemKind`<>IsCompoundControl
				| not wItemShow` || wItemVirtual` || not (isRecursiveControl wItemKind`)	// PA: isRecursive includes also LayoutControls
					= (backgrRgn,WItemHandle` itemH,osPict,tb)
				# (_,backgrRect,tb)					= osgetrgnbox backgrRgn tb
				  compoundRect						= posSizeToRect wItemPos` wItemSize`
				| disjointRects backgrRect compoundRect
					= (backgrRgn,WItemHandle` itemH,osPict,tb)
				| otherwise
					# (backgrRgn,itemH,osPict,tb)	= updatecompoundbackground wMetrics wFrame compoundRect ableContext backgrRgn itemH osPict tb
					= (backgrRgn,WItemHandle` itemH,osPict,tb)
			where
				updatecompoundbackground :: !OSWindowMetrics !OSRect !OSRect !Bool !OSRgnHandle !WItemHandle` !OSPictContext !*OSToolbox
														 					   -> (!OSRgnHandle,!WItemHandle`,!OSPictContext,!*OSToolbox)
				updatecompoundbackground wMetrics wFrame compoundRect ableContext backgrRgn itemH=:{wItemKind`=IsLayoutControl,wItems`} osPict tb
					# (backgrRgn,itemHs,osPict,tb)
											= updatebackgrounds wMetrics cFrame cAble backgrRgn wItems` osPict tb
					# (rectRgn,tb)			= osnewrectrgn cFrame tb
					# (diffRgn,tb)			= osdiffrgn backgrRgn rectRgn tb
					# tb					= stateMap2 osdisposergn [rectRgn,backgrRgn] tb
					= (diffRgn,{itemH & wItems`=itemHs},osPict,tb)
				where
					cFrame					= intersectRects wFrame compoundRect
					cAble					= ableContext && itemH.wItemSelect`
				updatecompoundbackground wMetrics wFrame compoundRect ableContext backgrRgn itemH=:{wItemKind`=IsCompoundControl,wItems`} osPict tb
					# (updRgn,tb)			= ossectrgn backgrRgn clipInfo.clipRgn tb
					# (empty,tb)			= osisemptyrgn updRgn tb
					| empty
						# (backgrRgn,itemHs,osPict,tb)
											= updatebackgrounds wMetrics cFrame cAble backgrRgn wItems` osPict tb
						# (rectRgn,tb)		= osnewrectrgn cFrame tb
						# (diffRgn,tb)		= osdiffrgn backgrRgn rectRgn tb
						# tb				= stateMap2 osdisposergn [rectRgn,backgrRgn,updRgn] tb
						= (diffRgn,{itemH & wItems`=itemHs},osPict,tb)
					| otherwise
						# picture			= unpeekPicture zero defaultPen True osPict tb
						# picture			= setpictorigin (origin-itemPos) picture
						# picture			= setpictpen lookPen picture
						# picture			= clipospicture updRgn contentRect (lookFun (if cAble Able Unable) updState) picture
						# picture			= setpictpen defaultPen picture
						# picture			= setpictorigin zero picture
						# (_,_,_,osPict,tb)	= peekPicture picture
						# (backgrRgn,itemHs,osPict,tb)
											= updatebackgrounds wMetrics cFrame cAble backgrRgn wItems` osPict tb
						# (rectRgn,tb)		= osnewrectrgn cFrame tb
						# (diffRgn,tb)		= osdiffrgn backgrRgn rectRgn tb
						# tb				= stateMap2 osdisposergn [rectRgn,backgrRgn,updRgn] tb
						= (diffRgn,{itemH & wItems`=itemHs},osPict,tb)
				where
					itemPos					= itemH.wItemPos`
					itemSize				= itemH.wItemSize`
					info					= getWItemCompoundInfo` itemH.wItemInfo`
					compLookInfo			= info.compoundLookInfo
					{lookFun,lookPen}		= compLookInfo.compoundLook
					clipInfo				= compLookInfo.compoundClip
					origin					= info.compoundOrigin
					domainRect				= info.compoundDomain
					hasScrolls				= (isJust info.compoundHScroll,isJust info.compoundVScroll)
					visScrolls				= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
					contentRect				= osGetCompoundContentRect wMetrics visScrolls compoundRect
					cFrame					= intersectRects wFrame contentRect
					cAble					= ableContext && itemH.wItemSelect`
					updFrame				= rectToRectangle cFrame
					updState				= {oldFrame=updFrame,newFrame=updFrame,updArea=[updFrame]}
					
					clipospicture :: !OSRgnHandle !OSRect !(IdFun *Picture) !*Picture -> *Picture
					clipospicture newClipRgn rect drawf picture
						#! (rectRgn,picture)	= accpicttoolbox (osnewrectrgn rect) picture
						#! (curClipRgn,picture)	= pictgetcliprgn picture
						   isEmptyClipRgn		= if (curClipRgn==0) (\_ tb->(True,tb)) osisemptyrgn	// PA: don't test for emptyness on 0 clipping regions
						#! (emptyCur,picture)	= accpicttoolbox (isEmptyClipRgn curClipRgn) picture
						#! picture				= (if emptyCur (pictsetcliprgn rectRgn) (pictandcliprgn rectRgn)) picture
						#! picture				= pictandcliprgn newClipRgn picture
						#! picture				= drawf picture
						#! picture				= pictsetcliprgn curClipRgn picture
						#  picture				= apppicttoolbox (osdisposergn rectRgn o (if (curClipRgn==0) id (osdisposergn curClipRgn))) picture
						= picture
				
				updatecompoundbackground _ _ _ _ _ {wItemKind`} _ _
					= windowupdateFatalError "updatecompoundbackground (updatewindowbackgrounds`)" ("unexpected control kind: "+++toString wItemKind`)
			
			updatebackground wMetrics wFrame ableContext backgrRgn (WRecursiveHandle` itemHs wKind) osPict tb
				# (backgrRgn,itemHs,osPict,tb)	= updatebackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict tb
				= (backgrRgn,WRecursiveHandle` itemHs wKind,osPict,tb)

/*	updaterectcontrols updates the controls that fit in the OSRect argument of the indicated window or compound control. 
	The OSRect is in window/compound coordinates. 
*/
updaterectcontrols :: !OSWindowMetrics !OSRect !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox
													     -> (!WindowHandle .ls .pst, !*OSToolbox)
updaterectcontrols wMetrics area wPtr wH=:{whItems,whSelect} tb
	| isEmptyRect area
		= (wH,tb)
	| otherwise
		# (osPict,tb)		= osGrabWindowPictContext wPtr tb
		# (itemHs,osPict,tb)= updatecontrolsinrect wMetrics wPtr whSelect area whItems osPict tb
		# tb				= osReleaseWindowPictContext wPtr osPict tb
		= ({wH & whItems=itemHs},tb)
where
	updatecontrolsinrect :: !OSWindowMetrics !OSWindowPtr !Bool !OSRect ![WElementHandle .ls .pst] !OSPictContext !*OSToolbox
																    -> (![WElementHandle .ls .pst],!OSPictContext,!*OSToolbox)
	updatecontrolsinrect _ _ _ _ [] osPict tb
		= ([],osPict,tb)
	updatecontrolsinrect wMetrics parentPtr ableContext area [itemH:itemHs] osPict tb
		| isEmptyRect area
			= ([itemH:itemHs],osPict,tb)
		| otherwise
			# (itemH, osPict,tb)	= updatecontrolinrect  wMetrics parentPtr ableContext area itemH  osPict tb
			# (itemHs,osPict,tb)	= updatecontrolsinrect wMetrics parentPtr ableContext area itemHs osPict tb
			= ([itemH:itemHs],osPict,tb)
	where
		updatecontrolinrect :: !OSWindowMetrics !OSWindowPtr !Bool !OSRect !(WElementHandle .ls .pst) !OSPictContext !*OSToolbox
																	    -> (!WElementHandle .ls .pst, !OSPictContext,!*OSToolbox)
		updatecontrolinrect wMetrics parentPtr ableContext area 
							wItemH=:(WItemHandle itemH=:{wItemInfo,wItemKind,wItemPtr,wItemPos,wItemSize,wItemShow,wItemSelect,wItemVirtual,wItems})
							osPict tb
			| not wItemShow || wItemVirtual || isEmptyRect intersectRect || ignorecontrol
				= (wItemH,osPict,tb)
			| iscustomcontrol
				# (itemH,osPict,tb)	= updatecustomcontrol wMetrics parentPtr ableContext intersectRect itemH osPict tb
				= (WItemHandle itemH,osPict,tb)
			| wItemKind == IsTextControl
				# info			= getWItemTextInfo wItemInfo
				# text			= info.textInfoText
				# tb			= osUpdateTextControl /*(subVector (toVector wItemPos) intersectRect)*/intersectRect controlRect text (toTuple wItemPos) parentPtr wItemPtr tb
				= (wItemH,osPict,tb)
			| wItemKind == IsButtonControl
				# tb			= osUpdateButtonControl /*(subVector (toVector wItemPos) intersectRect)*/intersectRect controlRect (toTuple wItemPos) parentPtr wItemPtr tb
				= (wItemH,osPict,tb)
			| wItemKind == IsEditControl
				# tb			= osUpdateEditControl /*(subVector (toVector wItemPos) intersectRect)*/intersectRect controlRect (toTuple wItemPos) parentPtr wItemPtr tb
				= (wItemH,osPict,tb)
			| wItemKind == IsPopUpControl
				# info			= getWItemPopUpInfo wItemInfo
				# text			= case info.popUpInfoEdit of
									Nothing		-> fst (info.popUpInfoItems!!(info.popUpInfoIndex - 1))
									(Just {popUpEditText})	-> popUpEditText
				# select		= if (wItemSelect && ableContext) Able Unable
				# editPtr		= mapMaybe (\{popUpEditPtr}->popUpEditPtr) info.popUpInfoEdit
				# tb			= osUpdatePopUpControl (subVector (toVector wItemPos) intersectRect) parentPtr wItemPtr editPtr (toTuple wItemPos) (toTuple wItemSize) (enabled select) text tb
				= (wItemH,osPict,tb)
			| wItemKind == IsRadioControl
				# info			= getWItemRadioInfo wItemInfo
				# (_,tb)		= stateMap (updateradiocontrolitem area) info.radioItems tb
				= (wItemH,osPict,tb)
			| wItemKind == IsCheckControl
				# info			= getWItemCheckInfo wItemInfo
				# (_,tb)		= stateMap (updatecheckcontrolitem area) info.checkItems tb
				= (wItemH,osPict,tb)
			| isoscontrol
				# tb				= updateoscontrol /*(subVector (toVector wItemPos) intersectRect)*/intersectRect (toTuple wItemPos) parentPtr wItemPtr tb
				= (wItemH,osPict,tb)
			| isRecursiveControl wItemKind	// This includes LayoutControl and excludes CompoundControl which is already guarded as iscustomcontrol
				# ableContext1		= ableContext && wItemSelect
				# (itemHs,osPict,tb)= updatecontrolsinrect wMetrics parentPtr ableContext1 area wItems osPict tb	// PA: shouldn't area be clipped (also below at updatecustomcontrol?)
				= (WItemHandle {itemH & wItems=itemHs},osPict,tb)
			| otherwise				// This alternative should never be reached
				= windowupdateFatalError "updatecontrolinrect" "unexpected ControlKind"
		where
			updateradiocontrolitem area item=:{radioItemPos,radioItemSize,radioItemPtr} tb
				# controlRect		= posSizeToRect radioItemPos radioItemSize
				# intersectRect		= intersectRects area controlRect
				# tb				= osUpdateRadioControl intersectRect (toTuple radioItemPos) parentPtr radioItemPtr tb
				= (item,tb)

			updatecheckcontrolitem area item=:{checkItemPos,checkItemSize,checkItemPtr} tb
				# controlRect		= posSizeToRect checkItemPos checkItemSize
				# intersectRect		= intersectRects area controlRect
				# tb				= osUpdateCheckControl intersectRect (toTuple checkItemPos) parentPtr checkItemPtr tb
				= (item,tb)

			controlRect				= posSizeToRect wItemPos wItemSize
			intersectRect			= intersectRects area controlRect
			iscustomcontrol			= case wItemKind of
										IsCustomButtonControl	-> True
										IsCustomControl			-> True
										IsCompoundControl		-> True
										_						-> False
			ignorecontrol			= case wItemKind of
										IsOtherControl _		-> True
										_						-> False
			(isoscontrol,updateoscontrol)
									= case wItemKind of
//										IsRadioControl			-> (True,osUpdateRadioControl)
//										IsCheckControl			-> (True,osUpdateCheckControl)
//										IsPopUpControl			-> (True,osUpdatePopUpControl)
										IsSliderControl			-> (True,osUpdateSliderControl)
//										IsTextControl			-> (True,osUpdateTextControl)
//										IsEditControl			-> (True,osUpdateEditControl)
//										IsButtonControl			-> (True,osUpdateButtonControl)
										_						-> (False,undef)
			
	//		updatecustomcontrol updates a ((Custom)Button/Compound)Control.
			updatecustomcontrol :: !OSWindowMetrics !OSWindowPtr !Bool !OSRect !(WItemHandle .ls .pst) !OSPictContext !*OSToolbox
																		    -> (!WItemHandle .ls .pst, !OSPictContext,!*OSToolbox)
			updatecustomcontrol _ parentPtr contextAble area itemH=:{wItemKind=IsCustomButtonControl,wItemPtr,wItemSelect,wItemInfo,wItemPos,wItemSize} osPict tb
				#! picture				= packPicture (~wItemPos) (copyPen lookInfo.lookPen) True osPict tb
				#! picture				= appClipPicture (toRegion updArea) (lookInfo.lookFun selectState updState) picture
				#! (_,pen,_,osPict,tb)	= unpackPicture picture
				   info					= {info & cButtonInfoLook={lookInfo & lookPen=pen}}
				#! tb					= osValidateWindowRect wItemPtr areaLocal tb
				= ({itemH & wItemInfo=CustomButtonInfo info},osPict,tb)
			where
				selectState				= if (contextAble && wItemSelect) Able Unable
				info					= getWItemCustomButtonInfo wItemInfo
				lookInfo				= info.cButtonInfoLook
				areaLocal				= subVector (toVector wItemPos) area
				cFrame					= sizeToRectangle wItemSize
				updArea					= rectToRectangle areaLocal
				updState				= {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
			
			updatecustomcontrol _ parentPtr contextAble area itemH=:{wItemKind=IsCustomControl,wItemPtr,wItemSelect,wItemInfo,wItemPos,wItemSize} osPict tb
				#! picture				= packPicture (~wItemPos) (copyPen lookInfo.lookPen) True osPict tb
				#! picture				= appClipPicture (toRegion updArea) (lookInfo.lookFun selectState updState) picture
				#! (_,pen,_,osPict,tb)	= unpackPicture picture
				   info					= {info & customInfoLook={lookInfo & lookPen=pen}}
				#! tb					= osValidateWindowRect wItemPtr areaLocal tb
				= ({itemH & wItemInfo=CustomInfo info},osPict,tb)
			where
				selectState				= if (contextAble && wItemSelect) Able Unable
				info					= getWItemCustomInfo wItemInfo
				lookInfo				= info.customInfoLook
				areaLocal				= subVector (toVector wItemPos) area
				cFrame					= sizeToRectangle wItemSize
				updArea					= rectToRectangle areaLocal
				updState				= {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
			
			updatecustomcontrol wMetrics parentPtr contextAble area itemH=:{wItemKind=IsCompoundControl,wItemPtr,wItemSelect,wItemInfo,wItemPos,wItemSize,wItems} osPict tb
				#! picture				= packPicture (~wItemPos) (copyPen lookInfo.lookPen) True osPict tb
				#! picture				= appClipPicture (toRegion updArea) (lookInfo.lookFun selectState updState) picture
				#! (_,pen,_,osPict,tb)	= unpackPicture picture
				   info					= {info & compoundLookInfo={compLookInfo & compoundLook={lookInfo & lookPen=pen}}}
				#! tb					= updatescrollareas tb
				#! (itemHs,osPict,tb)	= updatecontrolsinrect wMetrics parentPtr compoundAble area wItems osPict tb
				#! tb					= osValidateWindowRect wItemPtr areaLocal tb
				= ({itemH & wItemInfo=CompoundInfo info,wItems=itemHs},osPict,tb)
			where
				compoundAble			= contextAble && wItemSelect
				selectState				= if compoundAble Able Unable
				info					= getWItemCompoundInfo wItemInfo
				compLookInfo			= info.compoundLookInfo
				lookInfo				= compLookInfo.compoundLook
				(origin,domainRect,hasScrolls)
										= (info.compoundOrigin,info.compoundDomain,(isJust info.compoundHScroll,isJust info.compoundVScroll))
				visScrolls				= osScrollbarsAreVisible wMetrics domainRect (toTuple wItemSize) hasScrolls
				contentRect				= osGetCompoundContentRect wMetrics visScrolls (posSizeToRect origin wItemSize)
				areaLocal				= subVector (toVector wItemPos) area
				cFrame					= rectToRectangle contentRect
				updArea					= rectToRectangle (intersectRects contentRect (subVector (toVector (wItemPos-origin)) area))
				updState				= {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
				
				updatescrollareas		= if (emptyH && emptyV) id
										 (if emptyH				(osUpdateCompoundControl updVRect (toTuple wItemPos) parentPtr wItemPtr)
										 (if emptyV				(osUpdateCompoundControl updHRect (toTuple wItemPos) parentPtr wItemPtr)
										 						(osUpdateCompoundControl updHRect (toTuple wItemPos) parentPtr wItemPtr o
										 						 osUpdateCompoundControl {updVRect & rbottom=updHRect.rbottom} (toTuple wItemPos) parentPtr wItemPtr)))
				itemRect				= posSizeToRect wItemPos wItemSize
				hRect					= osGetCompoundHScrollRect wMetrics hasScrolls itemRect
				vRect					= osGetCompoundVScrollRect wMetrics hasScrolls itemRect
				updHRect				= intersectRects hRect area
				updVRect				= intersectRects vRect area
				emptyH					= isEmptyRect updHRect
				emptyV					= isEmptyRect updVRect
			
			updatecustomcontrol _ _ _ _ {wItemKind} _ _
				= windowupdateFatalError "updatecustomcontrol" ("unexpected ControlKind: "+++toString wItemKind)
		
		updatecontrolinrect wMetrics parentPtr ableContext area (WListLSHandle itemHs) osPict tb
			# (itemHs,osPict,tb)	= updatecontrolsinrect wMetrics parentPtr ableContext area itemHs osPict tb
			= (WListLSHandle itemHs,osPict,tb)
		
		updatecontrolinrect wMetrics parentPtr ableContext area (WExtendLSHandle wExH=:{wExtendItems=itemHs}) osPict tb
			# (itemHs,osPict,tb)	= updatecontrolsinrect wMetrics parentPtr ableContext area itemHs osPict tb
			= (WExtendLSHandle {wExH & wExtendItems=itemHs},osPict,tb)
		
		updatecontrolinrect wMetrics parentPtr ableContext area (WChangeLSHandle wChH=:{wChangeItems=itemHs}) osPict tb
			# (itemHs,osPict,tb)	= updatecontrolsinrect wMetrics parentPtr ableContext area itemHs osPict tb
			= (WChangeLSHandle {wChH & wChangeItems=itemHs},osPict,tb)


/*	updatebackground(`) updates the background of the window.
*/
updatewindowbackground :: !OSWindowMetrics !Bool !(Maybe OSRgnHandle) !UpdateInfo !(WindowHandle .ls .pst) !OSPictContext !*OSToolbox
																			   -> (!WindowHandle .ls .pst, !OSPictContext,!*OSToolbox)
updatewindowbackground wMetrics pictureInitialised alsoClipRgn info=:{updWIDS={wPtr}} wH=:{whKind,whWindowInfo,whSize} osPict tb
	| isEmptyRect updRect || whKind<>IsWindow			// Nothing to update at all
		= (wH,osPict,tb)
	| isEmptyRect updAreaRect							// Nothing to update inside viewframe
		= (wH,osPict,tb)
	| otherwise
		#! picture					= toPicture zero (copyPen initPen) True osPict tb
		#! (curClipRgn,picture)		= pictgetcliprgn picture
		   (clipAction,dispose)		= if (curClipRgn==0) (pictsetcliprgn,\_ p->p) (pictandcliprgn,osdisposergn)
		#! picture					= clipAction clipState.clipRgn picture
		   clipAlsoAction			= case alsoClipRgn of
		   								Just clip	-> pictandcliprgn clip
		   								_			-> id
		#! picture					= clipAlsoAction picture
		#! picture					= setpictorigin origin picture
		#! picture					= appClipPicture (toRegion updArea) (lookInfo.lookFun selectState updState) picture
		#! picture					= pictsetcliprgn curClipRgn picture
		#! (_,pen,_,osPict,tb)		= fromPicture picture
		#! tb						= dispose curClipRgn tb
		   wH						= {wH & whWindowInfo=WindowInfo {windowInfo & windowLook={lookInfo & lookPen=pen}}}
		= (wH,osPict,tb)
where
	(toPicture,fromPicture)			= if pictureInitialised (unpeekPicture,peekPicture) (packPicture,unpackPicture)
	updRect							= info.updWindowArea
	windowInfo						= getWindowInfoWindowData whWindowInfo
	(origin,domainRect,hasScrolls)	= (windowInfo.windowOrigin,windowInfo.windowDomain,(isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll))
	visScrolls						= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
//	{right=w,bottom=h}				= osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize)
//	wFrameRect						= posSizeToRect origin {w=w,h=h}
	wFrameRect						= posSizeToRect origin (rectSize (osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize)))
	wFrame							= rectToRectangle wFrameRect
	updAreaRect						= intersectRects wFrameRect (addVector (toVector origin) updRect)
	updArea							= rectToRectangle updAreaRect
	updState						= {oldFrame=wFrame,newFrame=wFrame,updArea=[updArea]}
	lookInfo						= windowInfo.windowLook
	clipState						= windowInfo.windowClip
	selectState						= if wH.whSelect Able Unable
	initPen							= lookInfo.lookPen

updatewindowbackground` :: !OSWindowMetrics !Bool !(Maybe OSRgnHandle) !UpdateInfo !WindowHandle` !OSPictContext !*OSToolbox
																			   -> (!WindowHandle`,!OSPictContext,!*OSToolbox)
updatewindowbackground` wMetrics pictureInitialised alsoClipRgn info=:{updWIDS={wPtr}} wH=:{whKind`,whWindowInfo`,whSize`} osPict tb
	| isEmptyRect updRect || whKind`<>IsWindow			// Nothing to update at all
		= (wH,osPict,tb)
	| isEmptyRect updAreaRect							// Nothing to update inside viewframe
		= (wH,osPict,tb)
	| otherwise
		#! picture					= toPicture zero (copyPen initPen) True osPict tb
		#! (curClipRgn,picture)		= pictgetcliprgn picture
		   (clipAction,dispose)		= if (curClipRgn==0) (pictsetcliprgn,\_ p->p) (pictandcliprgn,osdisposergn)
		#! picture					= clipAction clipState.clipRgn picture
		   clipAlsoAction			= case alsoClipRgn of
		   								Just clip	-> pictandcliprgn clip
		   								_			-> id
		#! picture					= clipAlsoAction picture
		#! picture					= setpictorigin origin picture
		#! picture					= appClipPicture (toRegion updArea) (lookInfo.lookFun selectState updState) picture
		#! picture					= pictsetcliprgn curClipRgn picture
		#! (_,pen,_,osPict,tb)		= fromPicture picture
		#! tb						= dispose curClipRgn tb
		   wH						= {wH & whWindowInfo`=WindowInfo {windowInfo & windowLook={lookInfo & lookPen=pen}}}
		= (wH,osPict,tb)
where
	(toPicture,fromPicture)			= if pictureInitialised (unpeekPicture,peekPicture) (packPicture,unpackPicture)
	updRect							= info.updWindowArea
	windowInfo						= getWindowInfoWindowData whWindowInfo`
	(origin,domainRect,hasScrolls)	= (windowInfo.windowOrigin,windowInfo.windowDomain,(isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll))
	visScrolls						= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize`) hasScrolls
	wFrameRect						= posSizeToRect origin (rectSize (osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize`)))
	wFrame							= rectToRectangle wFrameRect
	updAreaRect						= intersectRects wFrameRect (addVector (toVector origin) updRect)
	updArea							= rectToRectangle updAreaRect
	updState						= {oldFrame=wFrame,newFrame=wFrame,updArea=[updArea]}
	lookInfo						= windowInfo.windowLook
	clipState						= windowInfo.windowClip
	selectState						= if wH.whSelect` Able Unable
	initPen							= lookInfo.lookPen


/*	updatecontrols updates the controls that are registered for update in UpdateInfo.
*/
updatecontrols :: !OSWindowMetrics !UpdateInfo !(WindowHandle .ls .pst) !OSPictContext !*OSToolbox
											-> (!WindowHandle .ls .pst, !OSPictContext,!*OSToolbox)
updatecontrols wMetrics info=:{updWIDS,updControls} wH=:{whSelect,whItems} osPict tb
	# (_,itemHs,osPict,tb)	= updateControls wMetrics updWIDS whSelect updControls whItems osPict tb
	= ({wH & whItems=itemHs},osPict,tb)
where
	updateControls :: !OSWindowMetrics !WIDS !Bool ![ControlUpdateInfo] ![WElementHandle .ls .pst] !OSPictContext !*OSToolbox
											   -> (![ControlUpdateInfo],![WElementHandle .ls .pst],!OSPictContext,!*OSToolbox)
	updateControls _ _ _ updControls [] osPict tb
		= (updControls,[],osPict,tb)
	updateControls wMetrics wids contextAble updControls [itemH:itemHs] osPict tb
		| isEmpty updControls
			= (updControls,[itemH:itemHs],osPict,tb)
		| otherwise
			# (updControls,itemH, osPict,tb)= updateControl  wMetrics wids contextAble updControls itemH  osPict tb
			# (updControls,itemHs,osPict,tb)= updateControls wMetrics wids contextAble updControls itemHs osPict tb
			= (updControls,[itemH:itemHs],osPict,tb)
	where
		updateControl :: !OSWindowMetrics !WIDS !Bool ![ControlUpdateInfo] !(WElementHandle .ls .pst) !OSPictContext !*OSToolbox
												  -> (![ControlUpdateInfo], !WElementHandle .ls .pst, !OSPictContext,!*OSToolbox)
		updateControl wMetrics wids contextAble updControls (WItemHandle itemH) osPict tb
			# (updControls,itemH,osPict,tb)		= updateControl` wMetrics wids contextAble updControls itemH osPict tb
			= (updControls,WItemHandle itemH,osPict,tb)
		where
			updateControl` :: !OSWindowMetrics !WIDS !Bool ![ControlUpdateInfo] !(WItemHandle .ls .pst) !OSPictContext !*OSToolbox
													   -> (![ControlUpdateInfo], !WItemHandle .ls .pst, !OSPictContext,!*OSToolbox)
			updateControl` wMetrics wids contextAble updControls itemH=:{wItemNr} osPict tb
				# (found,updInfo,updControls)	= remove (\{cuItemNr}->cuItemNr==wItemNr) undef updControls
				# (itemH,osPict,tb)				= case found of
						True	-> updateControl`` wMetrics wids contextAble updInfo.cuArea itemH osPict tb
						_		-> (itemH,osPict,tb)
				// moet bij recursie rekening houden met gewijzigde omgeving...control in groene compound ziet er anders uit
				// dan control in wit window etc...
				# (updControls,itemHs,osPict,tb)= updateControls wMetrics wids (contextAble && itemH.wItemSelect) updControls itemH.wItems osPict tb
				  itemH							= {itemH & wItems=itemHs}
				= (updControls,itemH,osPict,tb)
/*
				# (found,updInfo,updControls)	= Remove (\{cuItemNr}->cuItemNr==wItemNr) undef updControls
				# (updControls,itemHs,osPict,tb)= updateControls wMetrics wids (contextAble && itemH.wItemSelect) updControls itemH.wItems osPict tb
				  itemH							= {itemH & wItems=itemHs}
				| not found
					= (updControls,itemH,osPict,tb)
				| otherwise
					# (itemH,osPict,tb)			= updateControl`` wMetrics wids contextAble updInfo.cuArea itemH osPict tb
					= (updControls,itemH,osPict,tb)
*/
			where
				updateControl`` :: !OSWindowMetrics !WIDS !Bool !OSRect !(WItemHandle .ls .pst) !OSPictContext !*OSToolbox
																     -> (!WItemHandle .ls .pst, !OSPictContext,!*OSToolbox)
				
				updateControl`` wMetrics wids contextAble area itemH=:{wItemPos,wItemKind=IsCustomButtonControl} osPict tb
// PA				#! picture				= packPicture (~wItemPos) (copyPen lookInfo.lookPen) True osPict tb
					#! picture				= packPicture (if osCustomButtonControlHasOrigin zero (~wItemPos)) (copyPen lookInfo.lookPen) True osPict tb
					#! picture				= appClipPicture (toRegion cFrame) (lookInfo.lookFun selectState updState) picture
					#! (_,pen,_,osPict,tb)	= unpackPicture picture
					   info					= {info & cButtonInfoLook={lookInfo & lookPen=pen}}	// hier  het vreemde effect van de memoiserende pen...
					= ({itemH & wItemInfo=CustomButtonInfo info},osPict,tb)
				where
					selectState				= if (contextAble && itemH.wItemSelect) Able Unable
					info					= getWItemCustomButtonInfo itemH.wItemInfo
					lookInfo				= info.cButtonInfoLook
					cFrame					= sizeToRectangle itemH.wItemSize
					updArea					= rectToRectangle (subVector (toVector itemH.wItemPos) area)
					updState				= {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
				
				updateControl`` wMetrics wids contextAble area itemH=:{wItemPos,wItemKind=IsCustomControl} osPict tb
// PA				#! picture				= packPicture (~wItemPos) (copyPen lookInfo.lookPen) True osPict tb
					#! picture				= packPicture (if osCustomControlHasOrigin zero (~wItemPos)) (copyPen lookInfo.lookPen) True osPict tb
					#! picture				= appClipPicture (toRegion cFrame) (lookInfo.lookFun selectState updState) picture
					#! (_,pen,_,osPict,tb)	= unpackPicture picture
					   info					= {info & customInfoLook={lookInfo & lookPen=pen}}
					= ({itemH & wItemInfo=CustomInfo info},osPict,tb)
				where
					selectState				= if (contextAble && itemH.wItemSelect) Able Unable
					info					= getWItemCustomInfo itemH.wItemInfo
					lookInfo				= info.customInfoLook
					cFrame					= sizeToRectangle itemH.wItemSize
					updArea					= rectToRectangle (subVector (toVector itemH.wItemPos) area)
					updState				= {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
				
				updateControl`` wMetrics wids contextAble area itemH=:{wItemPos,wItemKind=IsCompoundControl} osPict tb
// PA				#! picture				= packPicture (origin-wItemPos) (copyPen lookInfo.lookPen) True osPict tb
					#! picture				= packPicture (if osCompoundControlHasOrigin origin (origin-wItemPos)) (copyPen lookInfo.lookPen) True osPict tb
					#! picture				= appClipPicture (toRegion cFrame) (lookInfo.lookFun selectState updState) picture
					#! (_,pen,_,osPict,tb)	= unpackPicture picture
					   info					= {info & compoundLookInfo={compLookInfo & compoundLook={lookInfo & lookPen=pen}}}
					#! tb = case visHScroll of
								True	-> osUpdateSliderControl /*(addVector (toVector (~origin)) area)*/area (toTuple origin) wids.wPtr hPtr tb
								_		-> tb
					#! tb = case visVScroll of
								True	-> osUpdateSliderControl /*(addVector (toVector (~origin)) area)*/area (toTuple origin) wids.wPtr vPtr tb
								_		-> tb
					= ({itemH & wItemInfo=CompoundInfo info},osPict,tb)
				where
					selectState				= if (contextAble && itemH.wItemSelect) Able Unable
					itemSize				= itemH.wItemSize
					itemPos					= itemH.wItemPos
					info					= getWItemCompoundInfo itemH.wItemInfo
					domainRect				= info.compoundDomain
					hasScrolls				= (isJust info.compoundHScroll,isJust info.compoundVScroll)
					visScrolls				= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
					(visHScroll,visVScroll)	= visScrolls
					hPtr					= (fromJust info.compoundHScroll).scrollItemPtr
					vPtr					= (fromJust info.compoundVScroll).scrollItemPtr
					compLookInfo			= info.compoundLookInfo
					lookInfo				= compLookInfo.compoundLook
					origin					= info.compoundOrigin
					cFrame					= posSizeToRectangle origin (rectSize (osGetCompoundContentRect wMetrics visScrolls (sizeToRect itemSize)))
//					cFrame					= sizeToRectangle (rectSize (osGetCompoundContentRect wMetrics visScrolls (sizeToRect itemSize)))
					updArea					= rectToRectangle (intersectRects (rectangleToRect cFrame) (addVector (toVector (origin-itemPos)) area))
					updState				= {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsRadioControl,wItemPtr,wItemInfo} osPict tb
					# info			= getWItemRadioInfo wItemInfo
					# (_,tb)		= stateMap (updateradiocontrolitem area wids.wPtr) info.radioItems tb
					= (itemH,osPict,tb)
				where
					updateradiocontrolitem area parentPtr item=:{radioItemPos,radioItemSize,radioItemPtr} tb
						# controlRect		= posSizeToRect radioItemPos radioItemSize
						# intersectRect		= intersectRects area controlRect
						# tb				= osUpdateRadioControl intersectRect (toTuple radioItemPos) parentPtr radioItemPtr tb
						= (item,tb)
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsCheckControl,wItemPtr,wItemInfo} osPict tb
					# info			= getWItemCheckInfo wItemInfo
					# (_,tb)		= stateMap (updatecheckcontrolitem area wids.wPtr) info.checkItems tb
					= (itemH,osPict,tb)
				where
					updatecheckcontrolitem area parentPtr item=:{checkItemPos,checkItemSize,checkItemPtr} tb
						# controlRect		= posSizeToRect checkItemPos checkItemSize
						# intersectRect		= intersectRects area controlRect
						# tb				= osUpdateCheckControl intersectRect (toTuple checkItemPos) parentPtr checkItemPtr tb
						= (item,tb)
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsPopUpControl,wItemPtr,wItemPos,wItemSize,wItemSelect,wItemInfo} osPict tb
					= (itemH,osPict,osUpdatePopUpControl area wids.wPtr wItemPtr editPtr (toTuple wItemPos) (toTuple wItemSize) (enabled select) text tb)
				where
					info	= getWItemPopUpInfo wItemInfo
					editPtr	= mapMaybe (\{popUpEditPtr}->popUpEditPtr) info.popUpInfoEdit
					text	= case info.popUpInfoEdit of
									Nothing		-> text`
									(Just {popUpEditText})	-> popUpEditText
					text`	# j = (info.popUpInfoIndex)
							# l = info.popUpInfoItems
							= case j > length l of
								True	-> ""
								_		-> fst (l!!(j - 1))
					select	= if (wItemSelect && contextAble) Able Unable
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsSliderControl,wItemPtr,wItemPos} osPict tb
					= (itemH,osPict,osUpdateSliderControl area (toTuple wItemPos) wids.wPtr wItemPtr tb)
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsTextControl,wItemPtr,wItemPos,wItemSize,wItemInfo} osPict tb
					= (itemH,osPict,osUpdateTextControl area controlRect text (toTuple wItemPos) wids.wPtr wItemPtr tb)
				where
					controlRect			= posSizeToRect wItemPos wItemSize
					info				= getWItemTextInfo wItemInfo
					text				= info.textInfoText
				 
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsEditControl,wItemPtr,wItemPos,wItemSize} osPict tb
					= (itemH,osPict,osUpdateEditControl area controlRect (toTuple wItemPos) wids.wPtr wItemPtr tb)
				where
					controlRect			= posSizeToRect wItemPos wItemSize
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsButtonControl,wItemPtr,wItemPos,wItemSize} osPict tb
					= (itemH,osPict,osUpdateButtonControl area controlRect (toTuple wItemPos) wids.wPtr wItemPtr tb)
				where
					controlRect			= posSizeToRect wItemPos wItemSize
				
				updateControl`` _ _ _ _ itemH=:{wItemKind=IsOtherControl _} osPict tb
					= (itemH,osPict,tb)
				
				updateControl`` _ _ _ _ itemH=:{wItemKind=IsLayoutControl} _ _
					= windowupdateFatalError "updatecontrols (updateControl``)" "LayoutControl should never be updated"
		
		updateControl wMetrics wids contextAble updControls (WListLSHandle itemHs) osPict tb
			# (updControls,itemHs,osPict,tb)	= updateControls wMetrics wids contextAble updControls itemHs osPict tb
			= (updControls,WListLSHandle itemHs,osPict,tb)
		
		updateControl wMetrics wids contextAble updControls (WExtendLSHandle wExH=:{wExtendItems=itemHs}) osPict tb
			# (updControls,itemHs,osPict,tb)	= updateControls wMetrics wids contextAble updControls itemHs osPict tb
			= (updControls,WExtendLSHandle {wExH & wExtendItems=itemHs},osPict,tb)
		
		updateControl wMetrics wids contextAble updControls (WChangeLSHandle wChH=:{wChangeItems=itemHs}) osPict tb
			# (updControls,itemHs,osPict,tb)	= updateControls wMetrics wids contextAble updControls itemHs osPict tb
			= (updControls,WChangeLSHandle {wChH & wChangeItems=itemHs},osPict,tb)
