implementation module windowupdate


//	Clean Object I/O library, version 1.2


import	StdBool, StdFunc, StdList, StdMisc
import	ospicture, osrgn, oswindow
import	commondef, deviceevents, windowhandle, wstate
from	windowaccess	import getWItemCompoundInfo, getWItemCustomButtonInfo, getWItemCustomInfo, getWindowInfoWindowData,
								getCompoundContentRect, getWindowContentRect, getCompoundHScrollRect, getCompoundVScrollRect
from	windowclipstate	import validateWindowClipState
from	wstateaccess	import getWItemCompoundInfo`


windowupdateFatalError :: String String -> .x
windowupdateFatalError rule error = FatalError rule "windowupdate" error


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
	getUpdateContext _ (Just osPict) tb	= (osPict,tb)
	getUpdateContext wPtr _ tb			= OSgrabWindowPictContext wPtr tb
	
	setUpdateContext :: !OSWindowPtr !(Maybe OSPictContext) !OSPictContext !*OSToolbox -> *OSToolbox
	setUpdateContext wPtr updContext osPict tb
		| isJust updContext	= tb
		| otherwise			= OSreleaseWindowPictContext wPtr osPict tb


/*	updatewindowbackgrounds(`) redraws the (window/compound) backgrounds that are inside the OSRgnHandle argument.
	After redrawing the OSRgnHandle argument is disposed!
*/
updatewindowbackgrounds :: !OSWindowMetrics !OSRgnHandle !WIDS !(WindowHandle .ls .pst) !*OSToolbox
															-> (!WindowHandle .ls .pst, !*OSToolbox)
updatewindowbackgrounds wMetrics backgrRgn wids=:{wPtr} wH=:{whItems,whSelect,whSize,whWindowInfo} tb
	# (_,backgrRect,tb)						= osgetrgnbox backgrRgn tb
	| IsEmptyRect backgrRect
		= (wH,osdisposergn backgrRgn tb)
	| otherwise
		# updInfo							= {	updWIDS			= wids
											  ,	updWindowArea	= backgrRect
											  ,	updControls		= []
											  ,	updGContext		= Nothing
											  }
		# (osPict,tb)						= OSgrabWindowPictContext wPtr tb
		# picture							= packPicture zero (copyPen pen) True osPict tb		// Make sure picture is initialised with the proper pen, but origin zero
		# (origin,pen,toScreen,osPict,tb)	= peekPicture picture
		# (wH,osPict,tb)					= updatewindowbackground wMetrics True (Just backgrRgn) updInfo wH osPict tb
		# (backgrRgn,wH,osPict,tb)			= updatecontrolbackgrounds wMetrics backgrRgn wH osPict tb
		# picture							= unpeekPicture origin pen toScreen osPict tb
		# (_,_,_,osPict,tb)					= unpackPicture picture								// Make sure picture is cleared
		# tb								= OSreleaseWindowPictContext wPtr osPict tb
		= (wH,osdisposergn backgrRgn tb)
where
	pen	= case whWindowInfo of
			WindowInfo info	-> info.windowLook.lookPen
			other			-> defaultPen
	
	updatecontrolbackgrounds :: !OSWindowMetrics !OSRgnHandle !(WindowHandle .ls .pst) !OSPictContext !*OSToolbox
											 -> (!OSRgnHandle, !WindowHandle .ls .pst, !OSPictContext,!*OSToolbox)
	updatecontrolbackgrounds wMetrics backgrRgn wH=:{whItems,whSelect,whSize} osPict tb
		# (backgrRgn,itemHs,osPict,tb)	= updatebackgrounds wMetrics (SizeToRect whSize) whSelect backgrRgn whItems osPict tb
		= (backgrRgn,{wH & whItems=itemHs},osPict,tb)
	where
		updatebackgrounds :: !OSWindowMetrics !Rect !Bool !OSRgnHandle ![WElementHandle .ls .pst] !OSPictContext !*OSToolbox
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
			updatebackground :: !OSWindowMetrics !Rect !Bool !OSRgnHandle !(WElementHandle .ls .pst) !OSPictContext !*OSToolbox
														 -> (!OSRgnHandle, !WElementHandle .ls .pst, !OSPictContext,!*OSToolbox)
			updatebackground wMetrics wFrame ableContext backgrRgn (WItemHandle itemH=:{wItemShow,wItemVirtual,wItemKind,wItemPos,wItemSize}) osPict tb
			//	| not wItemShow || wItemVirtual || wItemKind<>IsCompoundControl
				| not wItemShow || wItemVirtual || not (isRecursiveControl wItemKind)	// PA: isRecursive includes also LayoutControls
					= (backgrRgn,WItemHandle itemH,osPict,tb)
				# (_,backgrRect,tb)					= osgetrgnbox backgrRgn tb
				# compoundRect						= PosSizeToRect wItemPos wItemSize
				| DisjointRects backgrRect compoundRect
					= (backgrRgn,WItemHandle itemH,osPict,tb)
				| otherwise
					# (backgrRgn,itemH,osPict,tb)	= updatecompoundbackground wMetrics wFrame compoundRect ableContext backgrRgn itemH osPict tb
					= (backgrRgn,WItemHandle itemH,osPict,tb)
			where
				updatecompoundbackground :: !OSWindowMetrics !Rect !Rect !Bool !OSRgnHandle !(WItemHandle .ls .pst) !OSPictContext !*OSToolbox
														 				   -> (!OSRgnHandle, !WItemHandle .ls .pst, !OSPictContext,!*OSToolbox)
				updatecompoundbackground wMetrics wFrame compoundRect ableContext backgrRgn itemH=:{wItemKind=IsLayoutControl,wItems} osPict tb
					# (backgrRgn,itemHs,osPict,tb)
											= updatebackgrounds wMetrics cFrame cAble backgrRgn wItems osPict tb
					# (rectRgn,tb)			= osnewrectrgn cFrame tb
					# (diffRgn,tb)			= osdiffrgn backgrRgn rectRgn tb
					# tb					= StateMap2 osdisposergn [rectRgn,backgrRgn] tb
					= (diffRgn,{itemH & wItems=itemHs},osPict,tb)
				where
					cFrame					= IntersectRects wFrame compoundRect
					cAble					= ableContext && itemH.wItemSelect
				updatecompoundbackground wMetrics wFrame compoundRect ableContext backgrRgn itemH=:{wItemKind=IsCompoundControl,wItems} osPict tb
					# (updRgn,tb)			= ossectrgn backgrRgn clipInfo.clipRgn tb
					# (empty,tb)			= osisemptyrgn updRgn tb
					| empty
						# (backgrRgn,itemHs,osPict,tb)
											= updatebackgrounds wMetrics cFrame cAble backgrRgn wItems osPict tb
						# (rectRgn,tb)		= osnewrectrgn cFrame tb
						# (diffRgn,tb)		= osdiffrgn backgrRgn rectRgn tb
						# tb				= StateMap2 osdisposergn [rectRgn,backgrRgn,updRgn] tb
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
						# tb				= StateMap2 osdisposergn [rectRgn,backgrRgn,updRgn] tb
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
					visScrolls				= OSscrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
					contentRect				= getCompoundContentRect wMetrics visScrolls compoundRect
					cFrame					= IntersectRects wFrame contentRect
					cAble					= ableContext && itemH.wItemSelect
					updFrame				= RectToRectangle cFrame
					updState				= {oldFrame=updFrame,newFrame=updFrame,updArea=[updFrame]}
					
					clipospicture :: !OSRgnHandle !Rect !(IdFun *Picture) !*Picture -> *Picture
					clipospicture newClipRgn rect drawf picture
						#! (rectRgn,picture)	= accpicttoolbox (osnewrectrgn rect) picture
						#! (curClipRgn,picture)	= pictgetcliprgn picture
						#! picture				= (if (curClipRgn==0) (pictsetcliprgn rectRgn) (pictandcliprgn rectRgn)) picture
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
	| IsEmptyRect backgrRect
		= (wH,osdisposergn backgrRgn tb)
	| otherwise
		# updInfo							= {	updWIDS			= wids
											  ,	updWindowArea	= backgrRect
											  ,	updControls		= []
											  ,	updGContext		= Nothing
											  }
		# (osPict,tb)						= OSgrabWindowPictContext wPtr tb
		# picture							= packPicture zero (copyPen pen) True osPict tb	// Make sure picture is initialised with the proper pen, but origin zero
		# (origin,pen,toScreen,osPict,tb)	= peekPicture picture
		# (wH,osPict,tb)					= updatewindowbackground` wMetrics True (Just backgrRgn) updInfo wH osPict tb
		# (backgrRgn,wH,osPict,tb)			= updatecontrolbackgrounds wMetrics backgrRgn wH osPict tb
		# picture							= unpeekPicture origin pen toScreen osPict tb
		# (_,_,_,osPict,tb)					= unpackPicture picture						// Make sure picture is cleared
		# tb								= OSreleaseWindowPictContext wPtr osPict tb
		= (wH,osdisposergn backgrRgn tb)
where
	pen	= case whWindowInfo` of
			WindowInfo info	-> info.windowLook.lookPen
			other			-> defaultPen
	
	updatecontrolbackgrounds :: !OSWindowMetrics !OSRgnHandle !WindowHandle` !OSPictContext !*OSToolbox
											 -> (!OSRgnHandle,!WindowHandle`,!OSPictContext,!*OSToolbox)
	updatecontrolbackgrounds wMetrics backgrRgn wH=:{whItems`,whSelect`,whSize`} osPict tb
		# (backgrRgn,itemHs,osPict,tb)	= updatebackgrounds wMetrics (SizeToRect whSize`) whSelect` backgrRgn whItems` osPict tb
		= (backgrRgn,{wH & whItems`=itemHs},osPict,tb)
	where
		updatebackgrounds :: !OSWindowMetrics !Rect !Bool !OSRgnHandle ![WElementHandle`] !OSPictContext !*OSToolbox
													  -> (!OSRgnHandle,![WElementHandle`],!OSPictContext,!*OSToolbox)
		updatebackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict tb
			# (empty,tb)						= osisemptyrgn backgrRgn tb
			| empty || isEmpty itemHs
				= (backgrRgn,itemHs,osPict,tb)
			| otherwise
				# (itemH,itemHs)				= HdTl itemHs
				# (backgrRgn,itemH, osPict,tb)	= updatebackground  wMetrics wFrame ableContext backgrRgn itemH  osPict tb
				# (backgrRgn,itemHs,osPict,tb)	= updatebackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict tb
				= (backgrRgn,[itemH:itemHs],osPict,tb)
		where
			updatebackground :: !OSWindowMetrics !Rect !Bool !OSRgnHandle !WElementHandle` !OSPictContext !*OSToolbox
														 -> (!OSRgnHandle,!WElementHandle`,!OSPictContext,!*OSToolbox)
			updatebackground wMetrics wFrame ableContext backgrRgn (WItemHandle` itemH=:{wItemShow`,wItemVirtual`,wItemKind`,wItemPos`,wItemSize`}) osPict tb
			//	| not wItemShow` || wItemVirtual` || wItemKind`<>IsCompoundControl
				| not wItemShow` || wItemVirtual` || not (isRecursiveControl wItemKind`)	// PA: isRecursive includes also LayoutControls
					= (backgrRgn,WItemHandle` itemH,osPict,tb)
				# (_,backgrRect,tb)					= osgetrgnbox backgrRgn tb
				  compoundRect						= PosSizeToRect wItemPos` wItemSize`
				| DisjointRects backgrRect compoundRect
					= (backgrRgn,WItemHandle` itemH,osPict,tb)
				| otherwise
					# (backgrRgn,itemH,osPict,tb)	= updatecompoundbackground wMetrics wFrame compoundRect ableContext backgrRgn itemH osPict tb
					= (backgrRgn,WItemHandle` itemH,osPict,tb)
			where
				updatecompoundbackground :: !OSWindowMetrics !Rect !Rect !Bool !OSRgnHandle !WItemHandle` !OSPictContext !*OSToolbox
														 				   -> (!OSRgnHandle,!WItemHandle`,!OSPictContext,!*OSToolbox)
				updatecompoundbackground wMetrics wFrame compoundRect ableContext backgrRgn itemH=:{wItemKind`=IsLayoutControl,wItems`} osPict tb
					# (backgrRgn,itemHs,osPict,tb)
											= updatebackgrounds wMetrics cFrame cAble backgrRgn wItems` osPict tb
					# (rectRgn,tb)			= osnewrectrgn cFrame tb
					# (diffRgn,tb)			= osdiffrgn backgrRgn rectRgn tb
					# tb					= StateMap2 osdisposergn [rectRgn,backgrRgn] tb
					= (diffRgn,{itemH & wItems`=itemHs},osPict,tb)
				where
					cFrame					= IntersectRects wFrame compoundRect
					cAble					= ableContext && itemH.wItemSelect`
				updatecompoundbackground wMetrics wFrame compoundRect ableContext backgrRgn itemH=:{wItemKind`=IsCompoundControl,wItems`} osPict tb
					# (updRgn,tb)			= ossectrgn backgrRgn clipInfo.clipRgn tb
					# (empty,tb)			= osisemptyrgn updRgn tb
					| empty
						# (backgrRgn,itemHs,osPict,tb)
											= updatebackgrounds wMetrics cFrame cAble backgrRgn wItems` osPict tb
						# (rectRgn,tb)		= osnewrectrgn cFrame tb
						# (diffRgn,tb)		= osdiffrgn backgrRgn rectRgn tb
						# tb				= StateMap2 osdisposergn [rectRgn,backgrRgn,updRgn] tb
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
						# tb				= StateMap2 osdisposergn [rectRgn,backgrRgn,updRgn] tb
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
					visScrolls				= OSscrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
					contentRect				= getCompoundContentRect wMetrics visScrolls compoundRect
					cFrame					= IntersectRects wFrame contentRect
					cAble					= ableContext && itemH.wItemSelect`
					updFrame				= RectToRectangle cFrame
					updState				= {oldFrame=updFrame,newFrame=updFrame,updArea=[updFrame]}
					
					clipospicture :: !OSRgnHandle !Rect !(IdFun *Picture) !*Picture -> *Picture
					clipospicture newClipRgn rect drawf picture
						#! (rectRgn,picture)	= accpicttoolbox (osnewrectrgn rect) picture
						#! (curClipRgn,picture)	= pictgetcliprgn picture
						#! picture				= (if (curClipRgn==0) (pictsetcliprgn rectRgn) (pictandcliprgn rectRgn)) picture
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

/*	updaterectcontrols updates the controls that fit in the Rect argument of the indicated window or compound control. 
	The Rect is in window/compound coordinates. 
*/
updaterectcontrols :: !OSWindowMetrics !Rect !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox
													   -> (!WindowHandle .ls .pst, !*OSToolbox)
updaterectcontrols wMetrics area wPtr wH=:{whItems,whSelect} tb
	| IsEmptyRect area
		= (wH,tb)
	| otherwise
		# (osPict,tb)		= OSgrabWindowPictContext wPtr tb
		# (itemHs,osPict,tb)= updatecontrolsinrect wMetrics wPtr whSelect area whItems osPict tb
		# tb				= OSreleaseWindowPictContext wPtr osPict tb
		= ({wH & whItems=itemHs},tb)
where
	updatecontrolsinrect :: !OSWindowMetrics !OSWindowPtr !Bool !Rect ![WElementHandle .ls .pst] !OSPictContext !*OSToolbox
																  -> (![WElementHandle .ls .pst],!OSPictContext,!*OSToolbox)
	updatecontrolsinrect _ _ _ _ [] osPict tb
		= ([],osPict,tb)
	updatecontrolsinrect wMetrics parentPtr ableContext area [itemH:itemHs] osPict tb
		| IsEmptyRect area
			= ([itemH:itemHs],osPict,tb)
		| otherwise
			# (itemH, osPict,tb)	= updatecontrolinrect  wMetrics parentPtr ableContext area itemH  osPict tb
			# (itemHs,osPict,tb)	= updatecontrolsinrect wMetrics parentPtr ableContext area itemHs osPict tb
			= ([itemH:itemHs],osPict,tb)
	where
		updatecontrolinrect :: !OSWindowMetrics !OSWindowPtr !Bool !Rect !(WElementHandle .ls .pst) !OSPictContext !*OSToolbox
																	  -> (!WElementHandle .ls .pst, !OSPictContext,!*OSToolbox)
		updatecontrolinrect wMetrics parentPtr ableContext area 
							wItemH=:(WItemHandle itemH=:{wItemKind,wItemPtr,wItemPos,wItemSize,wItemShow,wItemSelect,wItemVirtual,wItems})
							osPict tb
			| not wItemShow || wItemVirtual || IsEmptyRect intersectRect || ignorecontrol
				= (wItemH,osPict,tb)
			| iscustomcontrol
				# (itemH,osPict,tb)	= updatecustomcontrol wMetrics parentPtr ableContext intersectRect itemH osPict tb
				= (WItemHandle itemH,osPict,tb)
			| isoscontrol
				# tb				= updateoscontrol (subVector (toVector wItemPos) intersectRect) parentPtr wItemPtr tb
				= (wItemH,osPict,tb)
			| isRecursiveControl wItemKind	// This includes LayoutControl and excludes CompoundControl which is already guarded as iscustomcontrol
				# ableContext1		= ableContext && wItemSelect
				# (itemHs,osPict,tb)= updatecontrolsinrect wMetrics parentPtr ableContext1 area wItems osPict tb	// PA: shouldn't area be clipped (also below at updatecustomcontrol?)
				= (WItemHandle {itemH & wItems=itemHs},osPict,tb)
			| otherwise				// This alternative should never be reached
				= windowupdateFatalError "updatecontrolinrect" "unexpected ControlKind"
		where
			controlRect				= PosSizeToRect wItemPos wItemSize
			intersectRect			= IntersectRects area controlRect
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
										IsRadioControl			-> (True,OSupdateRadioControl)
										IsCheckControl			-> (True,OSupdateCheckControl)
										IsPopUpControl			-> (True,OSupdatePopUpControl)
										IsSliderControl			-> (True,OSupdateSliderControl)
										IsTextControl			-> (True,OSupdateTextControl)
										IsEditControl			-> (True,OSupdateEditControl)
										IsButtonControl			-> (True,OSupdateButtonControl)
										_						-> (False,undef)
			
	//		updatecustomcontrol updates a ((Custom)Button/Compound)Control.
			updatecustomcontrol :: !OSWindowMetrics !OSWindowPtr !Bool !Rect !(WItemHandle .ls .pst) !OSPictContext !*OSToolbox
																		  -> (!WItemHandle .ls .pst, !OSPictContext,!*OSToolbox)
			updatecustomcontrol _ parentPtr contextAble area itemH=:{wItemKind=IsCustomButtonControl,wItemPtr,wItemSelect,wItemInfo,wItemPos,wItemSize} osPict tb
				#! picture				= packPicture (~wItemPos) (copyPen lookInfo.lookPen) True osPict tb
				#! picture				= appClipPicture (toRegion updArea) (lookInfo.lookFun selectState updState) picture
				#! (_,pen,_,osPict,tb)	= unpackPicture picture
				   info					= {info & cButtonInfoLook={lookInfo & lookPen=pen}}
				#! tb					= OSvalidateWindowRect wItemPtr areaLocal tb
				= ({itemH & wItemInfo=CustomButtonInfo info},osPict,tb)
			where
				selectState				= if (contextAble && wItemSelect) Able Unable
				info					= getWItemCustomButtonInfo wItemInfo
				lookInfo				= info.cButtonInfoLook
				areaLocal				= subVector (toVector wItemPos) area
				cFrame					= SizeToRectangle wItemSize
				updArea					= RectToRectangle areaLocal
				updState				= {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
			
			updatecustomcontrol _ parentPtr contextAble area itemH=:{wItemKind=IsCustomControl,wItemPtr,wItemSelect,wItemInfo,wItemPos,wItemSize} osPict tb
				#! picture				= packPicture (~wItemPos) (copyPen lookInfo.lookPen) True osPict tb
				#! picture				= appClipPicture (toRegion updArea) (lookInfo.lookFun selectState updState) picture
				#! (_,pen,_,osPict,tb)	= unpackPicture picture
				   info					= {info & customInfoLook={lookInfo & lookPen=pen}}
				#! tb					= OSvalidateWindowRect wItemPtr areaLocal tb
				= ({itemH & wItemInfo=CustomInfo info},osPict,tb)
			where
				selectState				= if (contextAble && wItemSelect) Able Unable
				info					= getWItemCustomInfo wItemInfo
				lookInfo				= info.customInfoLook
				areaLocal				= subVector (toVector wItemPos) area
				cFrame					= SizeToRectangle wItemSize
				updArea					= RectToRectangle areaLocal
				updState				= {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
			
			updatecustomcontrol wMetrics parentPtr contextAble area itemH=:{wItemKind=IsCompoundControl,wItemPtr,wItemSelect,wItemInfo,wItemPos,wItemSize,wItems} osPict tb
				#! picture				= packPicture (~wItemPos) (copyPen lookInfo.lookPen) True osPict tb
				#! picture				= appClipPicture (toRegion updArea) (lookInfo.lookFun selectState updState) picture
				#! (_,pen,_,osPict,tb)	= unpackPicture picture
				   info					= {info & compoundLookInfo={compLookInfo & compoundLook={lookInfo & lookPen=pen}}}
				#! tb					= updatescrollareas tb
				#! (itemHs,osPict,tb)	= updatecontrolsinrect wMetrics parentPtr compoundAble area wItems osPict tb
				#! tb					= OSvalidateWindowRect wItemPtr areaLocal tb
				= ({itemH & wItemInfo=CompoundInfo info,wItems=itemHs},osPict,tb)
			where
				compoundAble			= contextAble && wItemSelect
				selectState				= if compoundAble Able Unable
				info					= getWItemCompoundInfo wItemInfo
				compLookInfo			= info.compoundLookInfo
				lookInfo				= compLookInfo.compoundLook
				(origin,domainRect,hasScrolls)
										= (info.compoundOrigin,info.compoundDomain,(isJust info.compoundHScroll,isJust info.compoundVScroll))
				visScrolls				= OSscrollbarsAreVisible wMetrics domainRect (toTuple wItemSize) hasScrolls
				contentRect				= getCompoundContentRect wMetrics visScrolls (PosSizeToRect origin wItemSize)
				areaLocal				= subVector (toVector wItemPos) area
				cFrame					= RectToRectangle contentRect
				updArea					= RectToRectangle (IntersectRects contentRect (subVector (toVector (wItemPos-origin)) area))
				updState				= {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
				
				updatescrollareas		= if (emptyH && emptyV) id
										 (if emptyH				(OSupdateCompoundControl updVRect parentPtr wItemPtr)
										 (if emptyV				(OSupdateCompoundControl updHRect parentPtr wItemPtr)
										 						(OSupdateCompoundControl updHRect parentPtr wItemPtr o
										 						 OSupdateCompoundControl {updVRect & rbottom=updHRect.rbottom} parentPtr wItemPtr)))
				itemRect				= PosSizeToRect wItemPos wItemSize
				hRect					= getCompoundHScrollRect wMetrics hasScrolls itemRect
				vRect					= getCompoundVScrollRect wMetrics hasScrolls itemRect
				updHRect				= IntersectRects hRect area
				updVRect				= IntersectRects vRect area
				emptyH					= IsEmptyRect updHRect
				emptyV					= IsEmptyRect updVRect
			
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
	| IsEmptyRect updRect || whKind<>IsWindow			// Nothing to update at all
		= (wH,osPict,tb)
	| IsEmptyRect updAreaRect							// Nothing to update inside viewframe
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
	visScrolls						= OSscrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
//	{right=w,bottom=h}				= getWindowContentRect wMetrics visScrolls (SizeToRect whSize)
//	wFrameRect						= PosSizeToRect origin {w=w,h=h}
	wFrameRect						= PosSizeToRect origin (RectSize (getWindowContentRect wMetrics visScrolls (SizeToRect whSize)))
	wFrame							= RectToRectangle wFrameRect
	updAreaRect						= IntersectRects wFrameRect (addVector (toVector origin) updRect)
	updArea							= RectToRectangle updAreaRect
	updState						= {oldFrame=wFrame,newFrame=wFrame,updArea=[updArea]}
	lookInfo						= windowInfo.windowLook
	clipState						= windowInfo.windowClip
	selectState						= if wH.whSelect Able Unable
	initPen							= lookInfo.lookPen

updatewindowbackground` :: !OSWindowMetrics !Bool !(Maybe OSRgnHandle) !UpdateInfo !WindowHandle` !OSPictContext !*OSToolbox
																			   -> (!WindowHandle`,!OSPictContext,!*OSToolbox)
updatewindowbackground` wMetrics pictureInitialised alsoClipRgn info=:{updWIDS={wPtr}} wH=:{whKind`,whWindowInfo`,whSize`} osPict tb
	| IsEmptyRect updRect || whKind`<>IsWindow			// Nothing to update at all
		= (wH,osPict,tb)
	| IsEmptyRect updAreaRect							// Nothing to update inside viewframe
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
	visScrolls						= OSscrollbarsAreVisible wMetrics domainRect (toTuple whSize`) hasScrolls
	wFrameRect						= PosSizeToRect origin (RectSize (getWindowContentRect wMetrics visScrolls (SizeToRect whSize`)))
	wFrame							= RectToRectangle wFrameRect
	updAreaRect						= IntersectRects wFrameRect (addVector (toVector origin) updRect)
	updArea							= RectToRectangle updAreaRect
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
				# (found,updInfo,updControls)	= Remove (\{cuItemNr}->cuItemNr==wItemNr) undef updControls
				# (updControls,itemHs,osPict,tb)= updateControls wMetrics wids (contextAble && itemH.wItemSelect) updControls itemH.wItems osPict tb
				  itemH							= {itemH & wItems=itemHs}
				| not found
					= (updControls,itemH,osPict,tb)
				| otherwise
					# (itemH,osPict,tb)			= updateControl`` wMetrics wids contextAble updInfo.cuArea itemH osPict tb
					= (updControls,itemH,osPict,tb)
			where
				updateControl`` :: !OSWindowMetrics !WIDS !Bool !Rect !(WItemHandle .ls .pst) !OSPictContext !*OSToolbox
																   -> (!WItemHandle .ls .pst, !OSPictContext,!*OSToolbox)
				
				updateControl`` wMetrics wids contextAble area itemH=:{wItemKind=IsCustomButtonControl} osPict tb
					#! picture				= packPicture zero (copyPen lookInfo.lookPen) True osPict tb
					#! picture				= appClipPicture (toRegion cFrame) (lookInfo.lookFun selectState updState) picture
					#! (_,pen,_,osPict,tb)	= unpackPicture picture
					   info					= {info & cButtonInfoLook={lookInfo & lookPen=pen}}
					= ({itemH & wItemInfo=CustomButtonInfo info},osPict,tb)
				where
					selectState				= if (contextAble && itemH.wItemSelect) Able Unable
					info					= getWItemCustomButtonInfo itemH.wItemInfo
					lookInfo				= info.cButtonInfoLook
					cFrame					= SizeToRectangle itemH.wItemSize
					updArea					= RectToRectangle (subVector (toVector itemH.wItemPos) area)
					updState				= {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
				
				updateControl`` wMetrics wids contextAble area itemH=:{wItemKind=IsCustomControl} osPict tb
					#! picture				= packPicture zero (copyPen lookInfo.lookPen) True osPict tb
					#! picture				= appClipPicture (toRegion cFrame) (lookInfo.lookFun selectState updState) picture
					#! (_,pen,_,osPict,tb)	= unpackPicture picture
					   info					= {info & customInfoLook={lookInfo & lookPen=pen}}
					= ({itemH & wItemInfo=CustomInfo info},osPict,tb)
				where
					selectState				= if (contextAble && itemH.wItemSelect) Able Unable
					info					= getWItemCustomInfo itemH.wItemInfo
					lookInfo				= info.customInfoLook
					cFrame					= SizeToRectangle itemH.wItemSize
					updArea					= RectToRectangle (subVector (toVector itemH.wItemPos) area)
					updState				= {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
				
				updateControl`` wMetrics wids contextAble area itemH=:{wItemKind=IsCompoundControl} osPict tb
					#! picture				= packPicture origin (copyPen lookInfo.lookPen) True osPict tb
					#! picture				= appClipPicture (toRegion cFrame) (lookInfo.lookFun selectState updState) picture
					#! (_,pen,_,osPict,tb)	= unpackPicture picture
					   info					= {info & compoundLookInfo={compLookInfo & compoundLook={lookInfo & lookPen=pen}}}
					= ({itemH & wItemInfo=CompoundInfo info},osPict,tb)
				where
					selectState				= if (contextAble && itemH.wItemSelect) Able Unable
					itemSize				= itemH.wItemSize
					itemPos					= itemH.wItemPos
					info					= getWItemCompoundInfo itemH.wItemInfo
					domainRect				= info.compoundDomain
					hasScrolls				= (isJust info.compoundHScroll,isJust info.compoundVScroll)
					visScrolls				= OSscrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
					compLookInfo			= info.compoundLookInfo
					lookInfo				= compLookInfo.compoundLook
					origin					= info.compoundOrigin
					cFrame					= PosSizeToRectangle origin (RectSize (getCompoundContentRect wMetrics visScrolls (SizeToRect itemSize)))
					updArea					= RectToRectangle (IntersectRects (RectangleToRect cFrame) (addVector (toVector (origin-itemPos)) area))
					updState				= {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsRadioControl,wItemPtr} osPict tb
					= (itemH,osPict,OSupdateRadioControl area wids.wPtr wItemPtr tb)
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsCheckControl,wItemPtr} osPict tb
					= (itemH,osPict,OSupdateCheckControl area wids.wPtr wItemPtr tb)
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsPopUpControl,wItemPtr} osPict tb
					= (itemH,osPict,OSupdatePopUpControl area wids.wPtr wItemPtr tb)
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsSliderControl,wItemPtr} osPict tb
					= (itemH,osPict,OSupdateSliderControl area wids.wPtr wItemPtr tb)
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsTextControl,wItemPtr} osPict tb
					= (itemH,osPict,OSupdateTextControl area wids.wPtr wItemPtr tb)
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsEditControl,wItemPtr} osPict tb
					= (itemH,osPict,OSupdateEditControl area wids.wPtr wItemPtr tb)
				
				updateControl`` _ wids contextAble area itemH=:{wItemKind=IsButtonControl,wItemPtr} osPict tb
					= (itemH,osPict,OSupdateButtonControl area wids.wPtr wItemPtr tb)
				
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
