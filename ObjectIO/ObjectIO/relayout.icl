implementation module relayout


//	Clean object I/O library, version 1.2


import	StdBool, StdFunc, StdList, StdTuple
import	osrgn, oswindow
from	ospicture		import packPicture, unpackPicture, defaultPen,apppicttoolbox, accpicttoolbox,
								getpictorigin, setpictorigin, getpictpen, setpictpen,
								pictgetcliprgn, pictsetcliprgn, pictandcliprgn
import	commondef, windowhandle
from	windowaccess	import getCompoundContentRect, getCompoundHScrollRect, getCompoundVScrollRect


::	RelayoutItem
	=	{	rliItemKind		:: !ControlKind			// The control kind
		,	rliItemPtr		:: !OSWindowPtr			// The ptr to the item
		,	rliItemPos		:: !Point2				// The exact position of the item
		,	rliItemSize		:: !Size				// The exact size of the item
		,	rliItemSelect	:: !Bool				// The item is Able (True) or Unable (False)
		,	rliItemShow		:: !Bool				// The item is visible (True) or invisible (False)
		,	rliItemInfo		:: CompoundInfo			// If the control kind is IsCompoundControl: its CompoundInfo; otherwise undefined
		,	rliItemLook		:: LookInfo				// If the control kind is IsCustom(Button)Control: its LookInfo; otherwise undefined
		,	rliItems		:: ![RelayoutItem]		// If the control kind is Is(Compound/Layout)Control: its elements; otherwise: []
		}

relayoutFatalError :: String String -> .x
relayoutFatalError function error
	= FatalError function "relayout" error


/*	relayoutItems resizes and moves changed items.
		The two Rect   arguments are the window frames in which the elements reside.
		The two Point2 arguments are the positions of the parent window/compound.
		The OSWindowPtr is the parent window/dialog.
		The first  RelayoutItem list contains the elements at their original location and size.
		The second RelayoutItem list contains the elements at their new location and size.
	Assumptions: 
		* The two lists contain elements that are identical except for size and position
		* (Radio/Check)Controls are flattened and have rliItemKind Is(Radio/Check)Control
		* The ClipStates of CompoundControls are valid.
	This version uses the HDC of the parent window in order to suppress calls to initpicture.
		Regions are used to clip sibling controls.
		In addition, two regions (validRegion,invalidRegion) are maintained that administrate whether part of the window requires
		update after relayout. This is done as follows:
			* initially validRegion and invalidRegion are empty.
			* for each relayoutitem: if its oldFrame<>newFrame then it adds newFrame to validRegion, and oldFrame to invalidRegion
			* the area to be updated equals validRegion - invalidRegion (so if its empty, then no update is required)
	relayoutItems returns the update region. 
*/
relayoutItems :: !OSWindowMetrics !Bool !Rect !Rect !Point2 !Point2 !OSWindowPtr ![RelayoutItem] ![RelayoutItem] !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
relayoutItems wMetrics isAble oldFrame newFrame oldParentPos newParentPos wPtr oldHs newHs tb
	#! (clipRgn,tb)		= osnewrectrgn newFrame tb
	#! (validRgn,tb)	= osnewrectrgn zero tb
	#! (invalidRgn,tb)	= osnewrectrgn zero tb
	#! (osPict,tb)		= OSgrabWindowPictContext wPtr tb
	#! picture			= packPicture zero defaultPen True osPict tb
	#! ((clipRgn,validRgn,invalidRgn),picture)
						= accClipPicture (toRegion (RectToRectangle newFrame)) 
							(relayoutItems` wPtr wMetrics isAble newArea (oldFrame,oldParentPos,oldHs)
																		 (newFrame,newParentPos,newHs)
																		 (clipRgn,validRgn,invalidRgn)
							) picture
	#! (_,_,_,osPict,tb)= unpackPicture picture
	#! tb				= OSreleaseWindowPictContext wPtr osPict tb
	#! (updRgn,tb)		= osdiffrgn	invalidRgn validRgn tb
	#! tb				= StateMap2 osdisposergn [clipRgn,validRgn,invalidRgn] tb
	= (updRgn,tb)
where
	newArea				= SubtractRects newFrame oldFrame
	
	relayoutItems` :: !OSWindowPtr !OSWindowMetrics !Bool ![Rect] !(!Rect,!Point2,![RelayoutItem]) !(!Rect,!Point2,![RelayoutItem])
					  !(!OSRgnHandle,!OSRgnHandle,!OSRgnHandle) !*Picture
				  -> (!(!OSRgnHandle,!OSRgnHandle,!OSRgnHandle),!*Picture)
	relayoutItems` wPtr wMetrics isAble newArea (oldFrame,oldParentPos,[oldH:oldHs]) (newFrame,newParentPos,[newH:newHs]) rgnHs picture
		# (rgnHs,picture)	= relayoutItem   wPtr wMetrics isAble newArea (oldFrame,oldParentPos,oldH)  (newFrame,newParentPos,newH)  rgnHs picture
		# (rgnHs,picture)	= relayoutItems` wPtr wMetrics isAble newArea (oldFrame,oldParentPos,oldHs) (newFrame,newParentPos,newHs) rgnHs picture
		= (rgnHs,picture)
	where
		relayoutItem :: !OSWindowPtr !OSWindowMetrics !Bool ![Rect] !(!Rect,!Point2,!RelayoutItem) !(!Rect,!Point2,!RelayoutItem)
						!(!OSRgnHandle,!OSRgnHandle,!OSRgnHandle) !*Picture
					-> (!(!OSRgnHandle,!OSRgnHandle,!OSRgnHandle),!*Picture)
		relayoutItem wPtr wMetrics isAble newArea old=:(_,_,{rliItemKind=k1}) new=:(_,_,{rliItemKind=k2}) rgnHs picture
			| k1==k2		= relayout wPtr wMetrics isAble newArea k1 old new rgnHs picture
			| otherwise		= relayoutFatalError "relayoutItem" "mismatching RelayoutItems"
		where
			/*	relayout assumes that the two RelayoutItem arguments 
				have the same ControlKind (fifth argument) and differ only in size or position or both.
			*/
			relayout :: !OSWindowPtr !OSWindowMetrics !Bool ![Rect] !ControlKind !(!Rect,!Point2,!RelayoutItem) !(!Rect,!Point2,!RelayoutItem)
						!(!OSRgnHandle,!OSRgnHandle,!OSRgnHandle) !*Picture
					-> (!(!OSRgnHandle,!OSRgnHandle,!OSRgnHandle),!*Picture)
			
			relayout wPtr wMetrics isAble newArea IsCompoundControl (oldFrame,oldParentPos,old) (newFrame,newParentPos,new)
																	(clipRgn,validRgn,invalidRgn) picture
				#! picture				= apppicttoolbox (moveF o sizeF) picture
				#! picture				= updF picture
				#! picture				= apppicttoolbox updScrollbars picture		// update scrollbars AFTER moving/sizing/updating
				#! ((clipRgn,validRgn,invalidRgn),picture)
										= relayoutItems` wPtr wMetrics (isAble && new.rliItemSelect) newArea1 (oldFrame1,oldPos,old.rliItems)
																											  (newFrame1,newPos,new.rliItems)
																											  (clipRgn,validRgn,invalidRgn) picture
				#! ((validRgn,invalidRgn),picture)
										= accpicttoolbox (checkUpdateRegions oldFrame1 newFrame1 (validRgn,invalidRgn)) picture
				#! (clipRgn,picture)	= accpicttoolbox (subtractRectFromRgn (IntersectRects newFrame newCompoundRect) clipRgn) picture
				=  ((clipRgn,validRgn,invalidRgn),picture)
			where
				sameSize		= oldSize==newSize
				samePos			= OSCompoundMovesControls && oldPos-oldParentPos==newPos-newParentPos || oldPos==newPos
				sizeF			= if sameSize
									id (OSsetCompoundSize wPtr newParentPos` itemPtr newPos` newSize` True)
				moveF			= if (samePos && all IsEmptyRect (map (IntersectRects newFrame1) newArea))
									id (OSsetCompoundPos  wPtr newParentPos` itemPtr newPos` newSize` True)
				updScrollbars	= if (sameSize && samePos && all IsEmptyRect (flatten (map (\area->[IntersectRects hRect` area,IntersectRects vRect` area]) newArea)))
									id ( (setCompoundScroll (snd hasScrolls) wMetrics itemPtr False newVThumbSize oldOrigin.y newOrigin.y vRect)
									   o (setCompoundScroll (fst hasScrolls) wMetrics itemPtr True  newHThumbSize oldOrigin.x newOrigin.x hRect)
									   )
				updF			= if (sameSize && oldPos==newPos && oldFrame1==newFrame1 || IsEmptyRect newFrame1)
									id (updatecustomcontrol wPtr clipRgn newFrame1 isAble new)
				newParentPos`	= toTuple newParentPos;
				itemPtr			= new.rliItemPtr
				newSize			= new.rliItemSize;		newSize`	= toTuple newSize;	oldSize			= old.rliItemSize;
				newPos			= new.rliItemPos;		newPos`		= toTuple newPos;	oldPos			= old.rliItemPos;
				newInfo			= new.rliItemInfo;										oldInfo			= old.rliItemInfo
				newOrigin		= newInfo.compoundOrigin;								oldOrigin		= oldInfo.compoundOrigin
				newDomainRect	= newInfo.compoundDomain;								oldDomainRect	= oldInfo.compoundDomain
				newCompoundRect	= PosSizeToRect newPos newSize;							oldCompoundRect	= PosSizeToRect oldPos oldSize
				newFrame1		= IntersectRects newFrame newContentRect;				oldFrame1		= IntersectRects oldFrame oldContentRect
				newArea1		= SubtractRects newFrame1 oldFrame1
				hasScrolls		= (isJust newInfo.compoundHScroll,isJust newInfo.compoundVScroll)
				newVisScrolls	= OSscrollbarsAreVisible wMetrics newDomainRect newSize` hasScrolls
				oldVisScrolls	= OSscrollbarsAreVisible wMetrics oldDomainRect (toTuple oldSize) hasScrolls
				newHThumbSize	= if (snd newVisScrolls) (newSize.w-wMetrics.osmVSliderWidth +1) (newSize.w+1)
				newVThumbSize	= if (fst newVisScrolls) (newSize.h-wMetrics.osmHSliderHeight+1) (newSize.h+1)
				oldContentRect	= getCompoundContentRect wMetrics oldVisScrolls oldCompoundRect
				newContentRect	= getCompoundContentRect wMetrics newVisScrolls newCompoundRect
				hRect			= getCompoundHScrollRect wMetrics newVisScrolls (SizeToRect newSize);	hRect`	= addVector (toVector newPos) hRect
				vRect			= getCompoundVScrollRect wMetrics newVisScrolls (SizeToRect newSize);	vRect`	= addVector (toVector newPos) vRect
				
				setCompoundScroll :: !Bool OSWindowMetrics OSWindowPtr Bool Int Int Int !Rect !*OSToolbox -> *OSToolbox
				setCompoundScroll hasScroll wMetrics compoundPtr isHorizontal size old new {rright,rbottom} tb
					| not hasScroll	= tb
					# tb			= OSsetCompoundSliderThumbSize wMetrics compoundPtr isHorizontal size (rright,rbottom) (old==new) tb
					| old==new		= tb
					| otherwise		= OSsetCompoundSliderThumb wMetrics compoundPtr isHorizontal new (rright,rbottom) True tb
			
			relayout wPtr wMetrics isAble newArea IsLayoutControl (oldFrame,oldParentPos,old) (newFrame,newParentPos,new)
																  (clipRgn, validRgn,invalidRgn) picture
				#! ((clipRgn,validRgn,invalidRgn),picture)
										= relayoutItems` wPtr wMetrics (isAble && new.rliItemSelect) newArea1 (oldFrame1,oldPos,old.rliItems)
																											  (newFrame1,newPos,new.rliItems)
																											  (clipRgn,validRgn,invalidRgn) picture
				#! ((validRgn,invalidRgn),picture)
										= accpicttoolbox (checkUpdateRegions oldFrame1 newFrame1 (validRgn,invalidRgn)) picture
				#! (clipRgn,picture)	= accpicttoolbox (subtractRectFromRgn (IntersectRects newFrame newLayoutRect) clipRgn) picture
				=  ((clipRgn,validRgn,invalidRgn),picture)
			where
				newSize					= new.rliItemSize;								oldSize			= old.rliItemSize;
				newPos					= new.rliItemPos;								oldPos			= old.rliItemPos;
				newLayoutRect			= PosSizeToRect newPos newSize;					oldLayoutRect	= PosSizeToRect oldPos oldSize
				newFrame1				= IntersectRects newFrame newLayoutRect;		oldFrame1		= IntersectRects oldFrame oldLayoutRect
				newArea1				= SubtractRects newLayoutRect oldLayoutRect
				
			relayout wPtr wMetrics isAble newArea controlKind (oldFrame,oldParentPos,old)
															  (newFrame,newParentPos,new)
															  (clipRgn,validRgn,invalidRgn) picture
				#! picture				= apppicttoolbox (moveF o sizeF) picture
				#! picture				= updF picture
				#! ((validRgn,invalidRgn),picture)
										= accpicttoolbox (checkUpdateRegions oldFrame1 newFrame1 (validRgn,invalidRgn)) picture
				#! (clipRgn,picture)	= accpicttoolbox (subtractRectFromRgn newFrame1 clipRgn) picture
				= ((clipRgn,validRgn,invalidRgn),picture)
			where
				sameSize				= oldSize==newSize
				samePos					= OSCompoundMovesControls && oldPos-oldParentPos==newPos-newParentPos || oldPos==newPos
				sizeF					= if sameSize id (setSize wPtr newParentPos` itemPtr newPos` newSize` (not redraw))
				moveF					= if (samePos && all IsEmptyRect (map (IntersectRects newFrame1) newArea))
											          id (setPos  wPtr newParentPos` itemPtr newPos` (toTuple oldSize) (not redraw))
				updF					= if (not redraw || sameSize && oldPos==newPos && newFrame1==oldFrame1 || IsEmptyRect newFrame1)
													  id (updatecustomcontrol wPtr clipRgn newFrame1 isAble new)
				newParentPos`			= toTuple newParentPos
				itemPtr					= new.rliItemPtr
				newPos					= new.rliItemPos;		newPos`		= toTuple newPos;		oldPos	= old.rliItemPos;
				newSize					= new.rliItemSize;		newSize`	= toTuple newSize;		oldSize	= old.rliItemSize;
				oldFrame1				= IntersectRects oldFrame (PosSizeToRect oldPos oldSize)
				newFrame1				= IntersectRects newFrame (PosSizeToRect newPos newSize)
				(setPos,setSize,redraw)	= case controlKind of
											IsRadioControl			-> (OSsetRadioControlPos,		\_ _ _ _ _ _ tb->tb,		 False)
											IsCheckControl			-> (OSsetCheckControlPos,		\_ _ _ _ _ _ tb->tb,		 False)
											IsPopUpControl			-> (OSsetPopUpControlPos,		OSsetPopUpControlSize,		 False)
											IsSliderControl			-> (OSsetSliderControlPos,		OSsetSliderControlSize,		 False)
											IsTextControl			-> (OSsetTextControlPos,		OSsetTextControlSize,		 False)
											IsEditControl			-> (OSsetEditControlPos,		OSsetEditControlSize,		 False)
											IsButtonControl			-> (OSsetButtonControlPos,		OSsetButtonControlSize,		 False)
											IsCustomButtonControl	-> (OSsetCustomButtonControlPos,OSsetCustomButtonControlSize,True)
											IsCustomControl			-> (OSsetCustomControlPos,		OSsetCustomControlSize,		 True)
											(IsOtherControl _)		-> (\_ _ _ _ _ _ tb->tb,		\_ _ _ _ _ _ tb->tb,		 False)
											_						-> relayoutFatalError "relayout" "unexpected ControlKind alternative"
			
	relayoutItems` _ _ _ _ (_,_,[]) (_,_,[]) rgnHs picture
		= (rgnHs,picture)
	relayoutItems` _ _ _ _ _ _ _ _
		= relayoutFatalError "relayoutItems`" "mismatching RelayoutItems"
	
	checkUpdateRegions :: !Rect !Rect !(!OSRgnHandle,!OSRgnHandle) !*OSToolbox -> (!(!OSRgnHandle,!OSRgnHandle),!*OSToolbox)
	checkUpdateRegions oldFrame newFrame rgnHs=:(validRgn,invalidRgn) tb
		| oldFrame==newFrame
			= (rgnHs,tb)
		| otherwise
			# (newFrameRgn,  tb)= osnewrectrgn newFrame tb
			# (oldFrameRgn,  tb)= osnewrectrgn oldFrame tb
			# (okNewRgn,     tb)= osdiffrgn  newFrameRgn invalidRgn tb	// PA+++
			# (newValidRgn,  tb)= osunionrgn okNewRgn validRgn tb		// PA: okNewRgn <-- newFrameRgn
			# (newInvalidRgn,tb)= osunionrgn oldFrameRgn invalidRgn tb
			# tb				= StateMap2 osdisposergn [validRgn,invalidRgn,newFrameRgn,oldFrameRgn,okNewRgn] tb	// PA: okNewRgn added
			= ((newValidRgn,newInvalidRgn),tb)
	
	subtractRectFromRgn :: !Rect !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
	subtractRectFromRgn rect rgn tb
		| IsEmptyRect rect
			= (rgn,tb)
		| otherwise
			# (rectRgn,tb)	= osnewrectrgn rect tb
			# (diffRgn,tb)	= osdiffrgn rgn rectRgn tb
			# tb			= osdisposergn rectRgn tb
			# tb			= osdisposergn rgn tb
			= (diffRgn,tb)
	
	updatecustomcontrol :: !OSWindowPtr !OSRgnHandle !Rect !Bool !RelayoutItem !*Picture -> *Picture
	updatecustomcontrol parentPtr clipRgn contentRect isAble itemH=:{rliItemKind=IsCustomButtonControl} picture
		| not itemH.rliItemShow
			= picture
		| otherwise
			#! (curOrigin,picture)	= getpictorigin picture
			#! (curPen,   picture)	= getpictpen picture
			#! picture				= setpictorigin (zero-itemPos) picture
			#! picture				= setpictpen lookPen picture
			#! picture				= clipospicture clipRgn contentRect (lookFun selectState updState) picture
			#! picture				= setpictpen curPen picture
			#  picture				= setpictorigin curOrigin picture
			= picture
	where
		selectState					= if (isAble && itemH.rliItemSelect) Able Unable
		itemPos						= itemH.rliItemPos
		{lookFun,lookPen}			= itemH.rliItemLook
		cFrame						= SizeToRectangle itemH.rliItemSize
		updState					= {oldFrame=cFrame,newFrame=cFrame,updArea=[cFrame]}
	
	updatecustomcontrol parentPtr clipRgn contentRect isAble itemH=:{rliItemKind=IsCustomControl} picture
		| not itemH.rliItemShow
			= picture
		| otherwise
			#! (curOrigin,picture)	= getpictorigin picture
			#! (curPen,   picture)	= getpictpen picture
			#! picture				= setpictorigin (zero-itemPos) picture
			#! picture				= setpictpen lookPen picture
			#! picture				= clipospicture clipRgn contentRect (lookFun selectState updState) picture
			#! picture				= setpictpen curPen picture
			# picture				= setpictorigin curOrigin picture
			= picture
	where
		selectState					= if (isAble && itemH.rliItemSelect) Able Unable
		itemPos						= itemH.rliItemPos
		{lookFun,lookPen}			= itemH.rliItemLook
		cFrame						= SizeToRectangle itemH.rliItemSize
		updState					= {oldFrame=cFrame,newFrame=cFrame,updArea=[cFrame]}
	
	updatecustomcontrol parentPtr clipRgn contentRect isAble itemH=:{rliItemKind=IsCompoundControl} picture
		| not itemH.rliItemShow
			= picture
		| otherwise
			#! (curOrigin,picture)		= getpictorigin picture
			#! (curPen,   picture)		= getpictpen picture
			#! picture					= setpictorigin (origin-itemPos) picture
			#! picture					= setpictpen lookPen picture
			#! (clip,picture)			= accpicttoolbox (ossectrgn clipRgn clipInfo.clipRgn) picture	// PA+++
			#! picture					= clipospicture /*clipInfo.clipRgn*/clip clipRect (lookFun selectState updState) picture
		//	#! picture					= clipospicture clip (PosSizeToRect itemPos itemSize) drawbackground picture
			#! picture					= apppicttoolbox (osdisposergn clip) picture					// PA+++
			#! picture					= setpictpen curPen picture
			# picture					= setpictorigin curOrigin picture
			= picture
	where
		selectState						= if (isAble && itemH.rliItemSelect) Able Unable
		itemSize						= itemH.rliItemSize
		itemPos							= itemH.rliItemPos
		info							= itemH.rliItemInfo
		(origin,domainRect,hasScrolls)	= (info.compoundOrigin,info.compoundDomain,(isJust info.compoundHScroll,isJust info.compoundVScroll))
		visScrolls						= OSscrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
		cFrameRect						= getCompoundContentRect wMetrics visScrolls (PosSizeToRect origin itemSize)
		cFrame							= RectToRectangle cFrameRect
		compLookInfo					= info.compoundLookInfo
		{lookFun,lookPen}				= compLookInfo.compoundLook
		clipInfo						= compLookInfo.compoundClip
		updState						= {oldFrame=cFrame,newFrame=cFrame,updArea=[cFrame]}
		cRect							= addVector (toVector (itemPos-origin)) cFrameRect
		clipRect						= IntersectRects contentRect cRect
	
	clipospicture :: !OSRgnHandle !Rect !(IdFun *Picture) !*Picture -> *Picture
	clipospicture newClipRgn rect drawf picture
		#! (rectRgn,picture)	= accpicttoolbox (osnewrectrgn rect) picture
		#! (curClipRgn,picture)	= pictgetcliprgn picture
		#! picture				= (if (curClipRgn==0) (pictsetcliprgn rectRgn) (pictandcliprgn rectRgn)) picture
		#! picture				= (if (newClipRgn==0) id (pictandcliprgn newClipRgn)) picture
		#! picture				= drawf picture
		#! picture				= pictsetcliprgn curClipRgn picture
		#  picture				= apppicttoolbox (osdisposergn rectRgn o (if (curClipRgn==0) id (osdisposergn curClipRgn))) picture
		= picture
