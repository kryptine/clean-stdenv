implementation module controlcreate


//	Clean Object I/O library, version 1.2

//	Control creation.


import	StdBool, StdInt, StdList, StdMisc
import	ostooltip, oswindow
import	commondef, controldefaccess, controllayout, controlvalidate, windowaccess
from	StdPSt	import PSt, IOSt


/*	createControls generates the proper system resources for all given WElementHandles of the window.
*/
createControls :: !OSWindowMetrics !(Maybe Id) !(Maybe Id) !Bool !OSWindowPtr ![WElementHandle .ls .pst] !*OSToolbox
																		  -> (![WElementHandle .ls .pst],!*OSToolbox)
createControls wMetrics okId cancelId ableContext wPtr itemHs tb
	= StateMap (createWElementHandle wMetrics okId cancelId True ableContext zero wPtr) itemHs tb

/*	createCompoundControls generates the proper system resources for those controls that are part of the 
	CompoundControl with the given Id, skipping its first nr of controls given by the Int argument.
	The WElementHandles must be the complete list of controls of the window.
*/
createCompoundControls :: !OSWindowMetrics !Id !Int !(Maybe Id) !(Maybe Id) !Bool !OSWindowPtr ![WElementHandle .ls .pst] !*OSToolbox
																						   -> (![WElementHandle .ls .pst],!*OSToolbox)
createCompoundControls wMetrics compoundId nrSkip okId cancelId ableContext wPtr itemHs tb
	= StateMap (createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId True ableContext zero wPtr) itemHs tb
where
	createCompoundWElementHandle :: !OSWindowMetrics !Id !Int !(Maybe Id) !(Maybe Id) !Bool !Bool !Point2 !OSWindowPtr
									!(WElementHandle .ls .pst) !*OSToolbox
								 -> (!WElementHandle .ls .pst, !*OSToolbox)
	createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr (WItemHandle itemH=:{wItemKind,wItemId}) tb
		| wItemKind<>IsCompoundControl
			= (WItemHandle itemH,tb)
		| not (identifyMaybeId compoundId wItemId)
			# (itemHs,tb)			= StateMap (createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext1 ableContext1 itemPos itemPtr) itemH.wItems tb
			= (WItemHandle {itemH & wItems=itemHs},tb)
		| otherwise
			# (oldItems,newItems)	= Split nrSkip itemH.wItems
			# (newItems,tb)			= StateMap (createWElementHandle wMetrics okId cancelId showContext1 ableContext1 itemPos itemPtr) newItems tb
			# tb					= OSinvalidateCompound itemPtr tb		// PA: added
			= (WItemHandle {itemH & wItems=oldItems++newItems},tb)
	where
		showContext1				= showContext && itemH.wItemShow
		ableContext1				= ableContext && itemH.wItemSelect
		itemPos						= itemH.wItemPos
		itemPtr						= itemH.wItemPtr
	
	createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr (WListLSHandle itemHs) tb
		# (itemHs,tb)	= StateMap (createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr) itemHs tb
		= (WListLSHandle itemHs,tb)
	
	createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
		# (itemHs,tb)	= StateMap (createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr) itemHs tb
		= (WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
	
	createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
		# (itemHs,tb)	= StateMap (createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr) itemHs tb
		= (WChangeLSHandle {wChH & wChangeItems=itemHs},tb)


/*	toOKorCANCEL okId cancelId controlId
		checks if the optional Id of a control (controlId) is the OK control (OK), the CANCEL control (CANCEL), or a normal button (NORMAL).
*/
toOKorCANCEL :: (Maybe Id) (Maybe Id) !(Maybe Id) -> OKorCANCEL
toOKorCANCEL okId cancelId maybeControlId
	= case maybeControlId of
		Just id	-> if (isJust okId     && fromJust okId    ==id) OK
		          (if (isJust cancelId && fromJust cancelId==id) CANCEL
		              NORMAL
		          )
		nothing	-> NORMAL

/*	createWElementHandle generates the proper system resources.
*/
createWElementHandle :: !OSWindowMetrics !(Maybe Id) !(Maybe Id) !Bool !Bool !Point2 !OSWindowPtr !(WElementHandle .ls .pst) !*OSToolbox
																							   -> (!WElementHandle .ls .pst, !*OSToolbox)
createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr (WItemHandle itemH) tb
	# (itemH,tb)	= createWItemHandle wMetrics okId cancelId showContext ableContext parentPos wPtr itemH tb
	= (WItemHandle itemH,tb)
where
	createWItemHandle :: !OSWindowMetrics !(Maybe Id) !(Maybe Id) !Bool !Bool !Point2 !OSWindowPtr !(WItemHandle .ls .pst) !*OSToolbox
																							    -> (!WItemHandle .ls .pst, !*OSToolbox)
	
	createWItemHandle wMetrics okId cancelId showContext ableContext parentPos wPtr itemH=:{wItemKind=IsRadioControl} tb
		# radioInfo				= getWItemRadioInfo itemH.wItemInfo
		  show					= showContext && itemH.wItemShow
		  able					= ableContext && itemH.wItemSelect
		# (radioItems,(_,tb))	= StateMap (createRadioItem show able (toTuple parentPos) wPtr radioInfo.radioIndex) radioInfo.radioItems (1,tb)
		  radioInfo				= {radioInfo & radioItems=radioItems}
		= ({itemH & wItemInfo=RadioInfo radioInfo},tb)
	where
		(hasTip,tipAtt)			= Select isControlTip undef itemH.wItemAtts
		tip						= getControlTipAtt tipAtt
		
		createRadioItem :: !Bool !Bool !(!Int,!Int) !OSWindowPtr !Index !(RadioItemInfo .pst) !(!Index,!*OSToolbox)
																 	 -> (!RadioItemInfo .pst, !(!Index,!*OSToolbox))
		createRadioItem show able parentPos wPtr index item=:{radioItem=(title,_,_),radioItemPos,radioItemSize} (itemNr,tb)
			# (radioPtr,tb)		= OScreateRadioControl wPtr parentPos title show able (toTuple radioItemPos) (toTuple radioItemSize) (index==itemNr) (itemNr==1) tb
			  itemH				= {item & radioItemPtr=radioPtr}
			| not hasTip
				= (itemH,(itemNr+1,tb))
			| otherwise
				= (itemH,(itemNr+1,OSaddControlToolTip wPtr radioPtr tip tb))
	
	createWItemHandle wMetrics okId cancelId showContext ableContext parentPos wPtr itemH=:{wItemKind=IsCheckControl} tb
		# checkInfo				= getWItemCheckInfo itemH.wItemInfo
		  show					= showContext && itemH.wItemShow
		  able					= ableContext && itemH.wItemSelect
		# (checkItems,(_,tb))	= StateMap (createCheckItem show able (toTuple parentPos) wPtr) checkInfo.checkItems (1,tb)
		  checkInfo				= {checkInfo & checkItems=checkItems}
		= ({itemH & wItemInfo=CheckInfo checkInfo},tb)
	where
		(hasTip,tipAtt)			= Select isControlTip undef itemH.wItemAtts
		tip						= getControlTipAtt tipAtt
		
		createCheckItem :: !Bool !Bool !(!Int,!Int) !OSWindowPtr !(CheckItemInfo .pst) !(!Index,!*OSToolbox)
															  -> (!CheckItemInfo .pst, !(!Index,!*OSToolbox))
		createCheckItem show able parentPos wPtr item=:{checkItem=(title,_,mark,_),checkItemPos,checkItemSize} (itemNr,tb)
			# (checkPtr,tb)		= OScreateCheckControl wPtr parentPos title show able (toTuple checkItemPos) (toTuple checkItemSize) (marked mark) (itemNr==1) tb
			  itemH				= {item & checkItemPtr=checkPtr}
			| not hasTip
				= (itemH,(itemNr+1,tb))
			| otherwise
				= (itemH,(itemNr+1,OSaddControlToolTip wPtr checkPtr tip tb))
	
	createWItemHandle wMetrics okId cancelId showContext ableContext parentPos wPtr itemH=:{wItemKind=IsPopUpControl} tb
		# (popUpPtr,editPtr,tb)	= OScreateEmptyPopUpControl wPtr (toTuple parentPos) show able (toTuple pos) (toTuple size) (length items) isEditable tb
		# (_,tb)				= StateMap2 (appendPopUp popUpPtr info.popUpInfoIndex) items (1,tb)
		  info					= if isEditable {info & popUpInfoEdit=Just {popUpEditText="",popUpEditPtr=editPtr}} info
		  itemH					= {itemH & wItemPtr=popUpPtr, wItemInfo=PopUpInfo info}
		| not hasTip
			= (itemH,tb)
		| otherwise
			= (itemH,OSaddControlToolTip wPtr popUpPtr (getControlTipAtt tipAtt) tb)
	where
		pos						= itemH.wItemPos
		size					= itemH.wItemSize
		show					= showContext && itemH.wItemShow
		able					= ableContext && itemH.wItemSelect
		info					= getWItemPopUpInfo itemH.wItemInfo
		items					= info.popUpInfoItems
		(hasTip,tipAtt)			= Select isControlTip undef itemH.wItemAtts
		isEditable				= Contains isControlKeyboard itemH.wItemAtts
		
		appendPopUp :: !OSWindowPtr !Index !(PopUpControlItem .pst) !(!Int,!*OSToolbox) -> (!Int,!*OSToolbox)
		appendPopUp popUpPtr index (title,_) (itemNr,tb)
			# (_,tb)			= OScreatePopUpControlItem popUpPtr (-1) ableContext title (index==itemNr) tb
			= (itemNr+1,tb)
	
	createWItemHandle wMetrics okId cancelId showContext ableContext parentPos wPtr itemH=:{wItemKind=IsSliderControl} tb
		# (sliderPtr,tb)		= OScreateSliderControl wPtr (toTuple parentPos) show able (direction==Horizontal) (toTuple pos) (toTuple size) (osMin,osThumb,osMax,osThumbSize) tb
		  itemH					= {itemH & wItemPtr=sliderPtr}
		| not hasTip
			= (itemH,tb)
		| otherwise
			= (itemH,OSaddControlToolTip wPtr sliderPtr (getControlTipAtt tipAtt) tb)
	where
		show					= showContext && itemH.wItemShow
		able					= ableContext && itemH.wItemSelect
		info					= getWItemSliderInfo itemH.wItemInfo
		direction				= info.sliderInfoDir
		sliderState				= info.sliderInfoState
		min						= sliderState.sliderMin
		max						= sliderState.sliderMax
		(osMin,osThumb,osMax,osThumbSize)
								= toOSscrollbarRange (min,sliderState.sliderThumb,max) 0
		pos						= itemH.wItemPos
		size					= itemH.wItemSize
		(hasTip,tipAtt)			= Select isControlTip undef itemH.wItemAtts

	createWItemHandle wMetrics okId cancelId showContext ableContext parentPos wPtr itemH=:{wItemKind=IsTextControl} tb
		# (textPtr,tb)			= OScreateTextControl wPtr (toTuple parentPos) title show (toTuple pos) (toTuple size) tb
		  itemH					= {itemH & wItemPtr=textPtr}
		| not hasTip
			= (itemH,tb)
		| otherwise
			= (itemH,OSaddControlToolTip wPtr textPtr (getControlTipAtt tipAtt) tb)
	where
		show					= showContext && itemH.wItemShow
		pos						= itemH.wItemPos
		size					= itemH.wItemSize
		title					= (getWItemTextInfo itemH.wItemInfo).textInfoText
		(hasTip,tipAtt)			= Select isControlTip undef itemH.wItemAtts
	
	createWItemHandle wMetrics okId cancelId showContext ableContext parentPos wPtr itemH=:{wItemKind=IsEditControl} tb
		# (editPtr,tb)			= OScreateEditControl wPtr (toTuple parentPos) text show able keySensitive (toTuple pos) (toTuple size) tb
		  itemH					= {itemH & wItemPtr=editPtr}
		| not hasTip
			= (itemH,tb)
		| otherwise
			= (itemH,OSaddControlToolTip wPtr editPtr (getControlTipAtt tipAtt) tb)
	where
		show					= showContext && itemH.wItemShow
		able					= ableContext && itemH.wItemSelect
		atts					= itemH.wItemAtts
		keySensitive			= Contains isControlKeyboard atts
		pos						= itemH.wItemPos
		size					= itemH.wItemSize
		text					= (getWItemEditInfo itemH.wItemInfo).editInfoText
		(hasTip,tipAtt)			= Select isControlTip undef atts
	
	createWItemHandle wMetrics okId cancelId showContext ableContext parentPos wPtr itemH=:{wItemKind=IsButtonControl} tb
		# (buttonPtr,tb)		= OScreateButtonControl wPtr (toTuple parentPos) title show able (toTuple pos) (toTuple size) okOrCancel tb
		  itemH					= {itemH & wItemPtr=buttonPtr}
		| not hasTip
			= (itemH,tb)
		| otherwise
			= (itemH,OSaddControlToolTip wPtr buttonPtr (getControlTipAtt tipAtt) tb)
	where
		show					= showContext && itemH.wItemShow
		able					= ableContext && itemH.wItemSelect
		pos						= itemH.wItemPos
		size					= itemH.wItemSize
		itemId					= itemH.wItemId
		okOrCancel				= toOKorCANCEL okId cancelId itemId
		title					= (getWItemButtonInfo itemH.wItemInfo).buttonInfoText
		(hasTip,tipAtt)			= Select isControlTip undef itemH.wItemAtts
	
	createWItemHandle wMetrics okId cancelId showContext ableContext parentPos wPtr itemH=:{wItemKind=IsCustomButtonControl} tb
		# (buttonPtr,tb)		= OScreateCustomButtonControl wPtr (toTuple parentPos) show able (toTuple pos) (toTuple size) okOrCancel tb
		  itemH					= {itemH & wItemPtr=buttonPtr}
		| not hasTip
			= (itemH,tb)
		| otherwise
			= (itemH,OSaddControlToolTip wPtr buttonPtr (getControlTipAtt tipAtt) tb)
	where
		show					= showContext && itemH.wItemShow
		able					= ableContext && itemH.wItemSelect
		pos						= itemH.wItemPos
		size					= itemH.wItemSize
		itemId					= itemH.wItemId
		okOrCancel				= toOKorCANCEL okId cancelId itemId
		(hasTip,tipAtt)			= Select isControlTip undef itemH.wItemAtts
	
	createWItemHandle wMetrics okId cancelId showContext ableContext parentPos wPtr itemH=:{wItemKind=IsCustomControl} tb
		# (customPtr,tb)		= OScreateCustomControl wPtr (toTuple parentPos) show able (toTuple pos) (toTuple size) tb
		  itemH					= {itemH & wItemPtr=customPtr}
		| not hasTip
			= (itemH,tb)
		| otherwise
			= (itemH,OSaddControlToolTip wPtr customPtr (getControlTipAtt tipAtt) tb)
	where
		show					= showContext && itemH.wItemShow
		able					= ableContext && itemH.wItemSelect
		pos						= itemH.wItemPos
		size					= itemH.wItemSize
		(hasTip,tipAtt)			= Select isControlTip undef itemH.wItemAtts
	
	createWItemHandle wMetrics okId cancelId showContext ableContext parentPos wPtr itemH=:{wItemKind=IsCompoundControl} tb
		# (compoundPtr,hPtr,vPtr,tb)
								= OScreateCompoundControl wMetrics wPtr (toTuple parentPos) show able (toTuple pos) (toTuple size) hScroll vScroll tb
		  compoundInfo			= {info & compoundHScroll=setScrollbarPtr hPtr info.compoundHScroll
		  								, compoundVScroll=setScrollbarPtr vPtr info.compoundVScroll
		  						  }
		# (itemHs,tb)			= StateMap (createWElementHandle wMetrics okId cancelId show able itemH.wItemPos compoundPtr) itemH.wItems tb
		  itemH					= {itemH & wItemInfo=CompoundInfo compoundInfo,wItemPtr=compoundPtr,wItems=itemHs}
		| not hasTip
			= (itemH,tb)
		| otherwise
			= (itemH,OSaddControlToolTip wPtr compoundPtr (getControlTipAtt tipAtt) tb)
	where
		show					= showContext && itemH.wItemShow
		able					= ableContext && itemH.wItemSelect
		pos						= itemH.wItemPos
		size					= itemH.wItemSize
		info					= getWItemCompoundInfo itemH.wItemInfo
		domainRect				= info.compoundDomain
		origin					= info.compoundOrigin
		(hasHScroll,hasVScroll)	= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		visScrolls				= OSscrollbarsAreVisible wMetrics domainRect (toTuple size) (hasHScroll,hasVScroll)
		{w,h}					= RectSize (getCompoundContentRect wMetrics visScrolls (SizeToRect size))
		(hasTip,tipAtt)			= Select isControlTip undef itemH.wItemAtts
		
		hScroll :: ScrollbarInfo
		hScroll
			| hasHScroll		= {cbiHasScroll=True, cbiPos=toTuple hInfo.scrollItemPos,cbiSize=toTuple hSize,cbiState=hState}
			| otherwise			= {cbiHasScroll=False,cbiPos=undef,cbiSize=undef,cbiState=undef}
		where
			hInfo				= fromJust info.compoundHScroll
			hSize				= hInfo.scrollItemSize
			hState				= toOSscrollbarRange (domainRect.rleft,origin.x,domainRect.rright) w
		vScroll :: ScrollbarInfo
		vScroll
			| hasVScroll		= {cbiHasScroll=True, cbiPos=toTuple vInfo.scrollItemPos,cbiSize=toTuple vSize,cbiState=vState}
			| otherwise			= {cbiHasScroll=False,cbiPos=undef,cbiSize=undef,cbiState=undef}
		where
			vInfo				= fromJust info.compoundVScroll
			vSize				= vInfo.scrollItemSize
			vState				= toOSscrollbarRange (domainRect.rtop,origin.y,domainRect.rbottom) h

		setScrollbarPtr :: OSWindowPtr !(Maybe ScrollInfo) -> Maybe ScrollInfo
		setScrollbarPtr scrollPtr (Just info)	= Just {info & scrollItemPtr=scrollPtr}
		setScrollbarPtr _ nothing				= nothing

	createWItemHandle _ _ _ _ _ _ _ itemH tb
		= (itemH,tb)

createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr (WListLSHandle itemHs) tb
	# (itemHs,tb)	= StateMap (createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr) itemHs tb
	= (WListLSHandle itemHs,tb)

createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
	# (itemHs,tb)	= StateMap (createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr) itemHs tb
	= (WExtendLSHandle {wExH & wExtendItems=itemHs},tb)

createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
	# (itemHs,tb)	= StateMap (createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr) itemHs tb
	= (WChangeLSHandle {wChH & wChangeItems=itemHs},tb)
