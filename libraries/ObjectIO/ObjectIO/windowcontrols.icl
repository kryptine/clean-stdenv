implementation module windowcontrols


//	Clean Object I/O library, version 1.2


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	commondef, controlcreate, windowclipstate
from	StdControlAttribute	import isControlPos
from	StdWindowAttribute	import isWindowHMargin,    getWindowHMarginAtt, 
									isWindowVMargin,   getWindowVMarginAtt, 
									isWindowItemSpace, getWindowItemSpaceAtt
from	controllayout		import layoutControls, getWindowContentRect
from	controlrelayout		import relayoutControls
from	windowaccess		import identifyMaybeId, genWElementItemNrs
from	windowdispose		import disposeWItemHandle
from	windowdraw			import drawwindowlook
from	windowupdate		import updatewindowbackgrounds
import	osdocumentinterface, oswindow


windowcontrolsFatalError :: String String -> .x
windowcontrolsFatalError function error
	= FatalError function "windowcontrols" error

//	Auxiliary functions:

getHMarginValue :: !(Int,Int) ![WindowAttribute .pst] -> (Int,Int)
getHMarginValue (left,right) atts = getWindowHMarginAtt (snd (Select isWindowHMargin (WindowHMargin left right) atts))

getVMarginValue :: !(Int,Int) ![WindowAttribute .pst] -> (Int,Int)
getVMarginValue (top,bottom) atts = getWindowVMarginAtt (snd (Select isWindowVMargin (WindowVMargin top bottom) atts))

getItemSpaceValue :: !(Int,Int) ![WindowAttribute .pst] -> (Int,Int)
getItemSpaceValue (hor,vert) atts = getWindowItemSpaceAtt (snd (Select isWindowItemSpace (WindowItemSpace hor vert) atts))

checkNewWindowSize :: !Size !Size !OSWindowPtr !OSDInfo !*OSToolbox -> *OSToolbox
checkNewWindowSize curSize newSize=:{w,h} wPtr (OSSDInfo {ossdFrame,ossdToolbar}) tb
	| curSize==newSize
		= tb
	| otherwise
		# tb	= OSsetWindowSize wPtr (w,h) True tb
		# tb	= OSsetWindowSize ossdFrame (w,h+tbHeight) True tb
		= tb
where
	tbHeight	= case ossdToolbar of
					Just {toolbarHeight}	-> toolbarHeight
					_						-> 0
checkNewWindowSize curSize newSize wPtr _ tb
	| curSize==newSize	= tb
	| otherwise			= OSsetWindowSize wPtr (toTuple newSize) True tb


/*	opencontrols adds the given controls to the window. 
	It is assumed that the new controls do not conflict with the current controls.
*/
opencontrols :: !OSWindowMetrics .ls ![WElementHandle .ls .pst] !(WindowStateHandle .pst) !*OSToolbox -> (!WindowStateHandle .pst,!*OSToolbox)
opencontrols wMetrics ls newItems wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems=curItems,whSize}}} tb
	# (nrCurItems,curItems)		= Ulength curItems
	  (itemNrs,newItems)		= genWElementItemNrs whItemNrs newItems
	  newItems					= [WChangeLSHandle {wChangeLS=ls,wChangeItems=newItems}]
	  allItems					= curItems++newItems
	  visScrolls				= OSscrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
	  {rright=curw,rbottom=curh}= getWindowContentRect wMetrics visScrolls (SizeToRect whSize)
	  hMargins					= getHMarginValue   (if isWindow (0,0) (wMetrics.osmHorMargin,   wMetrics.osmHorMargin))    whAtts
	  vMargins					= getVMarginValue   (if isWindow (0,0) (wMetrics.osmVerMargin,   wMetrics.osmVerMargin))    whAtts
	  spaces					= getItemSpaceValue (                  (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)) whAtts
	  reqSize					= {w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
	# (_,allItems,tb)			= layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] allItems tb
	  (curItems,newItems)		= Split nrCurItems allItems
	# (newItems,tb)				= createControls wMetrics whDefaultId whCancelId whSelect wPtr newItems tb
	  allItems					= curItems++newItems
	  wH						= {wH & whItemNrs=itemNrs,whItems=allItems}
	  wH						= invalidateWindowClipState wH
	  wsH						= {wsH & wshHandle=Just {wlsH & wlsHandle=wH}}
	= (wsH,tb)
where
	wPtr						= wshIds.wPtr
	whAtts						= wH.whAtts
	whDefaultId					= wH.whDefaultId
	whCancelId					= wH.whCancelId
	whSelect					= wH.whSelect
	whItemNrs					= wH.whItemNrs
	whKind						= wH.whKind
	isWindow					= whKind==IsWindow
	info						= wH.whWindowInfo
	(origin,hasHScroll,hasVScroll,domainRect)
								= case info of
									WindowInfo info	-> (info.windowOrigin,isJust info.windowHScroll,isJust info.windowVScroll,info.windowDomain)
									other			-> (zero,             False,                    False,                    SizeToRect whSize)
	domain						= RectToRectangle domainRect
opencontrols _ _ _ _ _
	= windowcontrolsFatalError "opencontrols" "unexpected window placeholder argument"


/*	opencompoundcontrols adds the given controls to the compound control of the given window. 
	It is assumed that the new controls do not conflict with the current controls.
*/
opencompoundcontrols :: !OSDInfo !OSWindowMetrics !Id .ls ![WElementHandle .ls .pst] !(WindowStateHandle .pst) !*OSToolbox
																			-> (!Bool,!WindowStateHandle .pst, !*OSToolbox)
opencompoundcontrols osdInfo wMetrics compoundId ls newItems wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems}}} tb
	# (found,nrSkip,_,_,itemNrs,oldItemHs)
								= addControlsToCompound compoundId ls newItems whItemNrs whItems
	| not found
		= (False,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=oldItemHs}}},tb)
	| otherwise
		# (curw,curh)			= (whSize.w-(if visVScroll wMetrics.osmVSliderWidth 0),whSize.h-(if visHScroll wMetrics.osmHSliderHeight 0))
		  curSize				= {w=curw,h=curh}
		  wFrame				= SizeToRect curSize
		  hMargins				= getHMarginValue   (if isWindow (0,0) (wMetrics.osmHorMargin,   wMetrics.osmHorMargin))   whAtts
		  vMargins				= getVMarginValue   (if isWindow (0,0) (wMetrics.osmVerMargin,   wMetrics.osmVerMargin))   whAtts
		  spaces				= getItemSpaceValue                    (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace) whAtts
		  reqSize				= {w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
		# (derSize,newItemHs,tb)= layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] oldItemHs tb
		# tb					= checkNewWindowSize curSize derSize wPtr osdInfo tb	// PA: curSize might be bigger than domain, then you shouldn't resize!
		# (newItemHs,tb)		= createCompoundControls wMetrics compoundId nrSkip whDefaultId whCancelId whSelect wPtr newItemHs tb
		  wH					= {wH & whItemNrs=itemNrs,whItems=newItemHs}
		# (wH,tb)				= forceValidWindowClipState wMetrics True wPtr wH tb
		# (updRgn,tb)			= relayoutControls wMetrics whSelect wFrame wFrame zero zero wPtr whDefaultId oldItemHs wH.whItems tb
		# (wH,tb)				= updatewindowbackgrounds wMetrics updRgn wshIds wH tb
		= (True,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
where
	wPtr						= wshIds.wPtr
	whAtts						= wH.whAtts
	whDefaultId					= wH.whDefaultId
	whCancelId					= wH.whCancelId
	whSelect					= wH.whSelect
	whItemNrs					= wH.whItemNrs
	whKind						= wH.whKind
	whSize						= wH.whSize
	whWindowInfo				= wH.whWindowInfo
	isWindow					= whKind==IsWindow
	domain						= RectToRectangle domainRect
	(origin,domainRect,hasHScroll,hasVScroll)
								= case whWindowInfo of
									WindowInfo info	-> (info.windowOrigin,info.windowDomain,isJust info.windowHScroll,isJust info.windowVScroll)
									other			-> (zero,             SizeToRect whSize,False,False)
	(visHScroll,visVScroll)		= OSscrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
	
	addControlsToCompound :: !Id .ls` ![WElementHandle .ls` .pst] [Int] ![WElementHandle .ls .pst]
				  -> (!Bool,!Int,.ls`,![WElementHandle .ls` .pst],[Int],![WElementHandle .ls .pst])
	addControlsToCompound compoundId ls newItems itemNrs itemHs
		| isEmpty itemHs
			= (False,0,ls,newItems,itemNrs,itemHs)
		# (itemH,itemHs)								= HdTl itemHs
		# (found,nrSkip,ls,newItems,itemNrs,itemH)		= addControlsToCompound` compoundId ls newItems itemNrs itemH
		| found
			= (found,nrSkip,ls,newItems,itemNrs,[itemH:itemHs])
		| otherwise
			# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToCompound compoundId ls newItems itemNrs itemHs
			= (found,nrSkip,ls,newItems,itemNrs,[itemH:itemHs])
	where
		addControlsToCompound` :: !Id .ls` ![WElementHandle .ls` .pst] [Int] !(WElementHandle .ls .pst)
					   -> (!Bool,!Int,.ls`,![WElementHandle .ls` .pst],[Int], !WElementHandle .ls .pst)
		addControlsToCompound` compoundId ls newItems itemNrs (WItemHandle itemH)
			# (found,nrSkip,ls,newItems,itemNrs,itemH) = addControlsToCompound`` compoundId ls newItems itemNrs itemH
			= (found,nrSkip,ls,newItems,itemNrs,WItemHandle itemH)
		where
			addControlsToCompound`` :: !Id .ls` ![WElementHandle .ls` .pst] [Int] !(WItemHandle .ls .pst)
							-> (!Bool,!Int,.ls`,![WElementHandle .ls` .pst],[Int], !WItemHandle .ls .pst)
			addControlsToCompound`` compoundId ls newItems itemNrs itemH=:{wItemKind,wItemId}
				| wItemKind<>IsCompoundControl
					= (False,0,ls,newItems,itemNrs,itemH)
				| not (identifyMaybeId compoundId wItemId)
					# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToCompound compoundId ls newItems itemNrs itemH.wItems
					  itemH										= {itemH & wItems=itemHs}
					  itemH										= if found (invalidateCompoundClipState itemH) itemH
					= (found,nrSkip,ls,newItems,itemNrs,itemH)
				| otherwise
					# (nrSkip,curItems)	= Ulength itemH.wItems
					  (itemNrs,newItems)= genWElementItemNrs itemNrs newItems
					  newItems			= [WChangeLSHandle {wChangeLS=ls,wChangeItems=newItems}]
					  itemH				= {itemH & wItems=curItems++newItems}
					  itemH				= invalidateCompoundClipState itemH
					= (True,nrSkip,undef,[],itemNrs,itemH)
		
		addControlsToCompound` compoundId ls newItems itemNrs (WListLSHandle itemHs)
			# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToCompound compoundId ls newItems itemNrs itemHs
			= (found,nrSkip,ls,newItems,itemNrs,WListLSHandle itemHs)
		
		addControlsToCompound` compoundId ls newItems itemNrs (WExtendLSHandle wExH=:{wExtendItems=itemHs})
			# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToCompound compoundId ls newItems itemNrs itemHs
			= (found,nrSkip,ls,newItems,itemNrs,WExtendLSHandle {wExH & wExtendItems=itemHs})
		
		addControlsToCompound` compoundId ls newItems itemNrs (WChangeLSHandle wChH=:{wChangeItems=itemHs})
			# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToCompound compoundId ls newItems itemNrs itemHs
			= (found,nrSkip,ls,newItems,itemNrs,WChangeLSHandle {wChH & wChangeItems=itemHs})
opencompoundcontrols _ _ _ _ _ _ _
	= windowcontrolsFatalError "opencompoundcontrols" "unexpected window placeholder argument"


/*	closecontrols closes the indicated controls and returns their R(2)Ids (first result [Id]) and
	Ids (second result [Id]) if appropriate.
	When closecontrols returns, the indicated controls will have been hidden. To actually dispose of them,
	the return (IdFun *OSToolbox) function should be applied.
*/
closecontrols :: !OSWindowMetrics ![Id] !Bool !(WindowStateHandle .pst) !*OSToolbox -> (![Id],![Id],!IdFun *OSToolbox,!WindowStateHandle .pst,!*OSToolbox)
closecontrols wMetrics closeIds relayout wsH=:{wshIds=wshIds=:{wPtr},wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems=curItems}}} tb
	# (freeRIds,freeIds,disposeFun,_,itemNrs,oldItemHs,tb)
							= closeWElementHandles wPtr closeIds whItemNrs curItems tb
	| not relayout
		# wH				= {wH & whItemNrs=itemNrs,whItems=oldItemHs}
		  wH				= invalidateWindowClipState wH
		= (freeRIds,freeIds,disposeFun,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	| otherwise
		# (curw,curh)		= (whSize.w-(if visVScroll wMetrics.osmVSliderWidth 0),whSize.h-(if visHScroll wMetrics.osmHSliderHeight 0))
		  wFrame			= SizeToRect {w=curw,h=curh}
		  hMargins			= getHMarginValue   (if isWindow (0,0) (wMetrics.osmHorMargin,   wMetrics.osmHorMargin))   whAtts
		  vMargins			= getVMarginValue   (if isWindow (0,0) (wMetrics.osmVerMargin,   wMetrics.osmVerMargin))   whAtts
		  spaces			= getItemSpaceValue                    (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace) whAtts
		  reqSize			= {w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
		# (_,newItemHs,tb)	= layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] oldItemHs tb
		  wH				= {wH & whItemNrs=itemNrs, whItems=newItemHs}
		# (wH,tb)			= forceValidWindowClipState wMetrics True wPtr wH tb
		# (updRgn,tb)		= relayoutControls wMetrics whSelect wFrame wFrame zero zero wPtr whDefaultId oldItemHs wH.whItems tb
		# (wH,tb)			= updatewindowbackgrounds wMetrics updRgn wshIds wH tb
		= (freeRIds,freeIds,disposeFun,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
where
	whItemNrs				= wH.whItemNrs
	whAtts					= wH.whAtts
	whKind					= wH.whKind
	whSize					= wH.whSize
	whSelect				= wH.whSelect
	whDefaultId				= wH.whDefaultId
	isWindow				= whKind==IsWindow
	domain					= RectToRectangle domainRect
	(origin,domainRect,hasHScroll,hasVScroll)
							= case wH.whWindowInfo of
								WindowInfo info	-> (info.windowOrigin,info.windowDomain,isJust info.windowHScroll,isJust info.windowVScroll)
								other			-> (zero,             SizeToRect whSize,False,False)
	(visHScroll,visVScroll)	= OSscrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
	
	closeWElementHandles :: !OSWindowPtr ![Id]  [Int] ![WElementHandle .ls .pst] !*OSToolbox
	   -> (![Id],![Id],!IdFun *OSToolbox,![Id],![Int],![WElementHandle .ls .pst],!*OSToolbox)
	closeWElementHandles parentPtr ids itemNrs itemHs tb
		| isEmpty ids || isEmpty itemHs
			= ([],[],id,ids,itemNrs,itemHs,tb)
		# (itemH,itemHs)										= HdTl itemHs
		# (close,freeRIds1,freeIds1,f1,ids,itemNrs,itemH, tb)	= closeWElementHandle  parentPtr ids itemNrs itemH  tb
		# (      freeRIds2,freeIds2,f2,ids,itemNrs,itemHs,tb)	= closeWElementHandles parentPtr ids itemNrs itemHs tb
		  freeRIds												= freeRIds1++freeRIds2
		  freeIds												= freeIds1 ++freeIds2
		  f														= f2 o f1
		| close
			= (freeRIds,freeIds,f,ids,itemNrs,       itemHs, tb)
		| otherwise
			= (freeRIds,freeIds,f,ids,itemNrs,[itemH:itemHs],tb)
	where
		closeWElementHandle :: !OSWindowPtr ![Id] [Int] !(WElementHandle .ls .pst) !*OSToolbox
		  -> (!Bool,![Id],![Id],!IdFun *OSToolbox,![Id],![Int],!WElementHandle .ls .pst,!*OSToolbox)
		closeWElementHandle parentPtr ids itemNrs (WItemHandle itemH) tb
			# (keep,freeRIds,freeIds,f,ids,itemNrs,itemH,tb)= closeWItemHandle parentPtr ids itemNrs itemH tb
			= (keep,freeRIds,freeIds,f,ids,itemNrs,WItemHandle itemH,tb)
		where
			closeWItemHandle :: !OSWindowPtr ![Id] [Int] !(WItemHandle .ls .pst) !*OSToolbox
				-> (!Bool,![Id],![Id],!IdFun *OSToolbox,![Id],![Int], !WItemHandle .ls .pst, !*OSToolbox)
			closeWItemHandle parentPtr ids itemNrs itemH=:{wItemKind=IsCompoundControl} tb
				# (close,ids)									= case itemH.wItemId of
																	(Just id)	-> RemoveCheck id ids
																	_			-> (False,ids)
				# (freeRIds,freeIds,f1,ids,itemNrs,itemHs,tb)	= closeWElementHandles parentPtr ids itemNrs itemH.wItems tb
				  itemH											= {itemH & wItems=itemHs}
				| not close
					= (close,freeRIds,freeIds,f1,ids,itemNrs,invalidateCompoundClipState itemH,tb)
				| otherwise
					# ((freeRIds1,freeIds1,f2),tb)				= disposeWItemHandle parentPtr itemH tb
					# tb										= OSinvalidateWindowRect parentPtr (PosSizeToRect itemH.wItemPos itemH.wItemSize) tb
					= (close,freeRIds1++freeRIds,freeIds1++freeIds,f2 o f1,ids,itemNrs,itemH,tb)
			closeWItemHandle parentPtr ids itemNrs itemH tb
				# (close,ids)									= case itemH.wItemId of
																	(Just id)	-> RemoveCheck id ids
																	_			-> (False,ids)
				| not close
					= (close,[],[],id,ids,itemNrs,itemH,tb)
				| otherwise
					# ((freeRIds,freeIds,f),tb)					= disposeWItemHandle parentPtr itemH tb
					# tb										= OSinvalidateWindowRect parentPtr (PosSizeToRect itemH.wItemPos itemH.wItemSize) tb
					= (close,freeRIds,freeIds,f,ids,[itemH.wItemNr:itemNrs],itemH,tb)
		
		closeWElementHandle parentPtr ids itemNrs (WListLSHandle itemHs) tb
			# (freeRIds,freeIds,f,ids,itemNrs,itemHs,tb)	= closeWElementHandles parentPtr ids itemNrs itemHs tb
			= (isEmpty itemHs,freeRIds,freeIds,f,ids,itemNrs,WListLSHandle itemHs,tb)
		
		closeWElementHandle parentPtr ids itemNrs (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
			# (freeRIds,freeIds,f,ids,itemNrs,itemHs,tb)	= closeWElementHandles parentPtr ids itemNrs itemHs tb
			= (isEmpty itemHs,freeRIds,freeIds,f,ids,itemNrs,WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
		
		closeWElementHandle parentPtr ids itemNrs (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
			# (freeRIds,freeIds,f,ids,itemNrs,itemHs,tb)	= closeWElementHandles parentPtr ids itemNrs itemHs tb
			= (isEmpty itemHs,freeRIds,freeIds,f,ids,itemNrs,WChangeLSHandle {wChH & wChangeItems=itemHs},tb)
closecontrols _ _ _ _ _
	= windowcontrolsFatalError "closecontrols" "unexpected window placeholder argument"


/*	closeallcontrols closes all controls and returns their R(2)Ids (first result [Id]) and Ids (second result [Id]).
	When closeallcontrols returns, the indicated controls will have been hidden. To actually dispose of them,
	the return (IdFun *OSToolbox) function should be applied.
*/
closeallcontrols :: !(WindowStateHandle .pst) !*OSToolbox -> (![Id],![Id],!IdFun *OSToolbox,!WindowStateHandle .pst,!*OSToolbox)
closeallcontrols wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems=curItems,whItemNrs}}} tb
	# (freeRIds,freeIds,disposeFun,itemNrs,tb)	= closeWElementHandles wPtr curItems whItemNrs tb
	  wH										= {wH & whItemNrs=itemNrs,whItems=[]}
	  wH										= invalidateWindowClipState wH
	= (freeRIds,freeIds,disposeFun,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
where
	closeWElementHandles :: !OSWindowPtr ![WElementHandle .ls .pst]  [Int] !*OSToolbox -> (![Id],![Id],!IdFun *OSToolbox,![Int],!*OSToolbox)
	closeWElementHandles parentPtr itemHs itemNrs tb
		| isEmpty itemHs
			= ([],[],id,itemNrs,tb)
		| otherwise
			# (itemH,itemHs)					= HdTl itemHs
			# (freeRIds1,freeIds1,f1,itemNrs,tb)= closeWElementHandle  parentPtr itemH  itemNrs tb
			# (freeRIds2,freeIds2,f2,itemNrs,tb)= closeWElementHandles parentPtr itemHs itemNrs tb
			= (freeRIds1++freeRIds2,freeIds1++freeIds2,f2 o f1,itemNrs,tb)
	where
		closeWElementHandle :: !OSWindowPtr !(WElementHandle .ls .pst) [Int] !*OSToolbox -> (![Id],![Id],!IdFun *OSToolbox,![Int],!*OSToolbox)
		closeWElementHandle parentPtr (WItemHandle itemH) itemNrs tb
			= closeWItemHandle parentPtr itemH itemNrs tb
		where
			closeWItemHandle :: !OSWindowPtr !(WItemHandle .ls .pst) [Int] !*OSToolbox -> (![Id],![Id],!IdFun *OSToolbox,![Int],!*OSToolbox)
			closeWItemHandle parentPtr itemH=:{wItemKind=IsCompoundControl} itemNrs tb
				# (freeRIds1,freeIds1,f1,itemNrs,tb)= closeWElementHandles parentPtr itemH.wItems itemNrs tb
				# ((freeRIds2,freeIds2,f2),tb)		= disposeWItemHandle parentPtr {itemH & wItems=[]} tb		// PA: itemH --> {itemH & wItems=[]}
				# tb								= OSinvalidateWindowRect parentPtr (PosSizeToRect itemH.wItemPos itemH.wItemSize) tb
				= (freeRIds2++freeRIds1,freeIds2++freeIds1,f2 o f1,itemNrs,tb)
			closeWItemHandle parentPtr itemH itemNrs tb
				# ((freeRIds,freeIds,f),tb)			= disposeWItemHandle parentPtr itemH tb
				# tb								= OSinvalidateWindowRect parentPtr (PosSizeToRect itemH.wItemPos itemH.wItemSize) tb
				= (freeRIds,freeIds,f,[itemH.wItemNr:itemNrs],tb)
		
		closeWElementHandle parentPtr (WListLSHandle itemHs) itemNrs tb
			= closeWElementHandles parentPtr itemHs itemNrs tb
		
		closeWElementHandle parentPtr (WExtendLSHandle {wExtendItems=itemHs}) itemNrs tb
			= closeWElementHandles parentPtr itemHs itemNrs tb
		
		closeWElementHandle parentPtr (WChangeLSHandle {wChangeItems=itemHs}) itemNrs tb
			= closeWElementHandles parentPtr itemHs itemNrs tb
closeallcontrols _ _
	= windowcontrolsFatalError "closeallcontrols" "unexpected window placeholder argument"


/*	setcontrolpositions changes the position of the indicated controls.
*/
setcontrolpositions :: !OSWindowMetrics ![(Id,ItemPos)] !(WindowStateHandle .pst) !*OSToolbox -> (!Bool,!WindowStateHandle .pst,!*OSToolbox)
setcontrolpositions wMetrics newPoss wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems=oldItems}}} tb
	| not (validateNewItemPoss newPoss oldItems)
		= (False,wsH,tb)
	| otherwise
		# (curw,curh)		= (whSize.w-(if visVScroll wMetrics.osmVSliderWidth 0),whSize.h-(if visHScroll wMetrics.osmHSliderHeight 0))
		  wFrame			= SizeToRect {w=curw,h=curh}
		  hMargins			= getHMarginValue   (if isWindow (0,0) (wMetrics.osmHorMargin,   wMetrics.osmHorMargin))   whAtts
		  vMargins			= getVMarginValue   (if isWindow (0,0) (wMetrics.osmVerMargin,   wMetrics.osmVerMargin))   whAtts
		  spaces			= getItemSpaceValue                    (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace) whAtts
		  reqSize			= {w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
		  (_,newItems)		= setNewItemPoss newPoss oldItems
		# (_,newItems,tb)	= layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] newItems tb
		  wH				= {wH & whItems=newItems}
		# (wH,tb)			= forceValidWindowClipState wMetrics True wPtr wH tb
		  viewFrame			= PosSizeToRectangle origin {w=curw,h=curh}
		  updState			= RectangleToUpdateState viewFrame
		# (wH,tb)			= drawwindowlook wMetrics wPtr id updState wH tb
		# (updRgn,tb)		= relayoutControls wMetrics whSelect wFrame wFrame zero zero wPtr whDefaultId oldItems wH.whItems tb
		# (wH,tb)			= updatewindowbackgrounds wMetrics updRgn wshIds wH tb
		# tb				= OSvalidateWindowRect wPtr (SizeToRect whSize) tb
		  wsH				= {wsH & wshHandle=Just {wlsH & wlsHandle=wH}}
		= (True,wsH,tb)
where
	wPtr					= wshIds.wPtr
	whAtts					= wH.whAtts
	whKind					= wH.whKind
	whSize					= wH.whSize
	whSelect				= wH.whSelect
	whDefaultId				= wH.whDefaultId
	isWindow				= whKind==IsWindow
	domain					= RectToRectangle domainRect
	(origin,domainRect,hasHScroll,hasVScroll)
							= case wH.whWindowInfo of
								WindowInfo info	-> (info.windowOrigin,info.windowDomain,isJust info.windowHScroll,isJust info.windowVScroll)
								other			-> (zero,             SizeToRect whSize,False,False)
	(visHScroll,visVScroll)	= OSscrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)

	validateNewItemPoss :: ![(Id,ItemPos)] ![WElementHandle .ls .pst] -> Bool
	validateNewItemPoss idPoss itemHs
		= isEmpty (controlsExist (getids idPoss) itemHs)
	where
		getids :: ![(Id,ItemPos)] -> [Id]
		getids [(controlId,(itemLoc,_)):idPoss]
			= case itemLoc of
				LeftOf	id	-> [id:ids]
				RightTo	id	-> [id:ids]
				Above	id	-> [id:ids]
				Below	id	-> [id:ids]
				_			-> ids
		where
			ids	= [controlId:getids idPoss]
		getids _
			= []
		
		controlsExist :: ![Id] ![WElementHandle .ls .pst] -> [Id]
		controlsExist ids itemHs
			| isEmpty ids || isEmpty itemHs
				= ids
			| otherwise
				# (itemH,itemHs)	= HdTl itemHs
				= controlsExist (controlsExist` ids itemH) itemHs
		where
			controlsExist` :: ![Id] !(WElementHandle .ls .pst) -> [Id]
			controlsExist` ids (WItemHandle {wItemId,wItems})
				= controlsExist (if (isJust wItemId) (removeMember (fromJust wItemId) ids) ids) wItems
			controlsExist` ids (WListLSHandle itemHs)					= controlsExist ids itemHs
			controlsExist` ids (WExtendLSHandle {wExtendItems=itemHs})	= controlsExist ids itemHs
			controlsExist` ids (WChangeLSHandle {wChangeItems=itemHs})	= controlsExist ids itemHs
	
	setNewItemPoss :: ![(Id,ItemPos)] ![WElementHandle .ls .pst] -> (![(Id,ItemPos)],![WElementHandle .ls .pst])
	setNewItemPoss idPoss itemHs
		| isEmpty idPoss || isEmpty itemHs
			= (idPoss,itemHs)
		| otherwise
			# (itemH,itemHs)	= HdTl itemHs
			  (idPoss,itemH)	= setNewItemPos` idPoss itemH
			  (idPoss,itemHs)	= setNewItemPoss idPoss itemHs
			= (idPoss,[itemH:itemHs])
	where
		setNewItemPos` :: ![(Id,ItemPos)] !(WElementHandle .ls .pst) -> (![(Id,ItemPos)],!WElementHandle .ls .pst)
		setNewItemPos` idPoss (WItemHandle itemH)
			# (idPoss,itemH)	= setNewItemPos`` idPoss itemH
			= (idPoss,WItemHandle itemH)
		where
			setNewItemPos`` :: ![(Id,ItemPos)] !(WItemHandle .ls .pst) -> (![(Id,ItemPos)],!WItemHandle .ls .pst)
			setNewItemPos`` idPoss itemH=:{wItemId,wItemKind,wItemAtts,wItems}
				| isNothing wItemId
					| wItemKind==IsCompoundControl
						# (idPoss,itemHs)	= setNewItemPoss idPoss wItems
						= (idPoss,{itemH & wItems=itemHs})
					// otherwise
						= (idPoss,itemH)
				# (found,idPos,idPoss)	= Remove ((==) itemId o fst) undef idPoss
				  itemH					= {itemH & wItemAtts=if found (snd (Replace isControlPos (ControlPos (snd idPos)) wItemAtts)) wItemAtts}
				| wItemKind==IsCompoundControl
					# (idPoss,itemHs)	= setNewItemPoss idPoss wItems
					  itemH				= {itemH & wItems=itemHs}
					  itemH				= if found (invalidateCompoundClipState itemH) itemH
					= (idPoss,itemH)
				| otherwise
					= (idPoss,itemH)
			where
				itemId					= fromJust wItemId
		
		setNewItemPos` idPoss (WListLSHandle itemHs)
			# (idPoss,itemHs)	= setNewItemPoss idPoss itemHs
			= (idPoss,WListLSHandle itemHs)
		
		setNewItemPos` idPoss (WExtendLSHandle wExH=:{wExtendItems=itemHs})
			# (idPoss,itemHs)	= setNewItemPoss idPoss itemHs
			= (idPoss,WExtendLSHandle {wExH & wExtendItems=itemHs})
		
		setNewItemPos` idPoss (WChangeLSHandle wChH=:{wChangeItems=itemHs})
			# (idPoss,itemHs)	= setNewItemPoss idPoss itemHs
			= (idPoss,WChangeLSHandle {wChH & wChangeItems=itemHs})

setcontrolpositions _ _ _ _
	= windowcontrolsFatalError "setcontrolpositions" "unexpected window placeholder argument"
