implementation module menudevice


//	Clean object I/O library, version 1.2


import	StdBool, StdEnum, StdList, StdMisc
import	menuevent, osmenu
from	ostypes				import OSNoWindowPtr
import	commondef, devicefunctions, iostate, menucreate, menudefaccess, StdId
from	menuaccess			import menuStateHandleGetMenuId
from	StdProcessAttribute	import getProcessToolbarAtt, isProcessToolbar
from	StdPSt				import accPIO


menudeviceFatalError :: String String -> .x
menudeviceFatalError rule error
	= FatalError rule "menudevice" error


MenuFunctions :: DeviceFunctions (PSt .l)
MenuFunctions
	= {	dDevice	= MenuDevice
	  ,	dShow	= menuShow
	  ,	dHide	= menuHide
	  ,	dEvent	= menuEvent
	  ,	dDoIO	= menuIO
	  ,	dOpen	= menuOpen
	  ,	dClose	= menuClose
	  }

menuShow :: !(PSt .l) -> PSt .l
/* PA: mOSMenuBar information is stored in IOSt:OSDInfo.
menuShow pState=:{io=ioState}
	# (activeIO,ioState)	= IOStIsActive ioState
	| not activeIO
		= {pState & io=ioState}
	| otherwise
		# (tb,ioState)		= getIOToolbox ioState
		# (menus,ioState)	= IOStGetDevice MenuDevice ioState
		  mHs				= MenuSystemStateGetMenuHandles menus
		# (menuBar, tb)		= OSMenuBarSet mHs.mOSMenuBar tb
		  mHs				= {mHs & mOSMenuBar = menuBar}
		# ioState			= setIOToolbox tb ioState
		# ioState			= IOStSetDevice (MenuSystemState mHs) ioState
		= {pState & io=ioState}
*/
menuShow pState=:{io=ioState}
	# (activeIO,ioState)		= IOStIsActive ioState
	| not activeIO
		= {pState & io=ioState}
	# (osdinfo,ioState)			= IOStGetOSDInfo ioState
	  maybeMenuBar				= getOSDInfoOSMenuBar osdinfo
	| isNothing maybeMenuBar
		= menudeviceFatalError "MenuFunctions.dShow" "OSMenuBar could not be retrieved from OSDInfo"
	| otherwise
		# osMenuBar				= fromJust maybeMenuBar
		# (osMenuBar,ioState)	= accIOToolbox (OSMenuBarSet osMenuBar) ioState
		  osdinfo				= setOSDInfoOSMenuBar osMenuBar osdinfo
		# ioState				= IOStSetOSDInfo osdinfo ioState
		= {pState & io=ioState}

menuClose :: !(PSt .l) -> PSt .l
menuClose pState=:{io=ioState}
	# (osdinfo,ioState)			= IOStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdinfo
	| isNothing maybeOSMenuBar
		= menudeviceFatalError "MenuFunctions.dClose" "OSMenuBar could not be retrieved from OSDInfo"
	# (found,menus,ioState)		= IOStGetDevice MenuDevice ioState
	| not found
		= menudeviceFatalError "MenuFunctions.dClose" "could not retrieve MenuSystemState from IOSt"
	| otherwise
		# osMenuBar				= fromJust maybeOSMenuBar
		# (opt_guishare,ioState)= IOStGetGUIShare ioState
		  mHs					= MenuSystemStateGetMenuHandles menus
		# (osMenuBar,ioState)	= accIOToolbox (disposeMenuHandles (isJust opt_guishare) mHs osMenuBar) ioState
		  osdinfo				= setOSDInfoOSMenuBar osMenuBar osdinfo
		# ioState				= IOStSetOSDInfo osdinfo ioState
		# (ioid,ioState)		= IOStGetIOId ioState
		# (rt,ioState)			= IOStGetReceiverTable ioState
		# (it,ioState)			= IOStGetIdTable ioState
		  (rt,it)				= StateMap2 (disposeIds ioid) mHs.mMenus (rt,it)
		# ioState				= IOStSetIdTable it ioState
		# ioState				= IOStSetReceiverTable rt ioState
		# ioState				= IOStRemoveDevice MenuDevice ioState
		# ioState				= IOStRemoveDeviceFunctions MenuDevice ioState
		= {pState & io=ioState}
where
	disposeIds :: !SystemId !(MenuStateHandle .pst) !(!ReceiverTable,!IdTable) -> (!ReceiverTable,!IdTable)
	disposeIds ioid (MenuLSHandle {mlsHandle={mItems}}) ts
		= StateMap2 (disposeMenuIds ioid) mItems ts

menuHide :: !(PSt .l) -> PSt .l
menuHide pState=:{io=ioState}
	# (activeIO,ioState)	= IOStIsActive ioState
	| not activeIO
		= {pState & io=ioState}
	# (found,menus,ioState)	= IOStGetDevice MenuDevice ioState
	| not found
		= {pState & io=ioState}
	| otherwise
		# mHs				= MenuSystemStateGetMenuHandles menus
		# (tb,ioState)		= getIOToolbox ioState
		# tb				= OSMenuBarClear tb
		# ioState			= setIOToolbox tb ioState
		# ioState			= IOStSetDevice (MenuSystemState mHs) ioState
		= {pState & io=ioState}


/*	Opening menus:
	Note:	all interactive processes have atleast the AppleMenu to identify the 
			process, and is used for checking whether the process is active
			(see IOStIsActive further down this module).
			If the process is a subprocess, then the ioguishare of its IOSt
			contains the list to the Mac toolbox menu system.
*/
menuOpen :: !(PSt .l) -> PSt .l
menuOpen pState=:{io=ioState}
	# (hasMenu,ioState)		= IOStHasDevice MenuDevice ioState
	| hasMenu
		= {pState & io=ioState}
	| otherwise
		# (di,     ioState)	= IOStGetDocumentInterface ioState
		# (popUpId,ioState)	= getPopUpId di ioState
		  bound				= case di of
								NDI -> Finite 0
								SDI -> Infinite
								MDI -> Infinite
		  mHs				= {	mMenus		= []
							  ,	mKeys		= []
			//PA---			  ,	mOSMenuBar	= OSMenuBarNew OSNoWindowPtr OSNoWindowPtr (-1) // PA: menubar will be initialised by the (M/S)DI window creation
							  ,	mEnabled	= SystemAble
							  ,	mNrMenuBound= bound
							  ,	mPopUpId	= popUpId
							  }
		# ioState			= IOStSetDevice (MenuSystemState mHs) ioState
		# ioState			= IOStSetDeviceFunctions MenuFunctions ioState
		= {pState & io=ioState}
where
	getPopUpId :: !DocumentInterface !(IOSt .l) -> (!Maybe Id,!IOSt .l)
	getPopUpId NDI ioState
		= (Nothing,ioState)
	getPopUpId _ ioState
		# (id,ioState)	= openId ioState
		= (Just id,ioState)


menuIO :: !DeviceEvent !(PSt .l) -> (!DeviceEvent,!PSt .l)
menuIO deviceEvent pState
	# (ok,pState)	= accPIO (IOStHasDevice MenuDevice) pState
	| not ok		// This condition should never occur
		= menudeviceFatalError "MenuFunctions.dDoIO" "could not retrieve MenuSystemState from IOSt"
	| otherwise
		= menuIO deviceEvent pState
where
	menuIO :: !DeviceEvent !(PSt .l) -> (!DeviceEvent,!PSt .l)
	
	menuIO receiverEvent=:(ReceiverEvent msgEvent) pState
		= (ReceiverEvent msgEvent1,pState2)
	where
		(_,mDevice,ioState)			= IOStGetDevice MenuDevice pState.io
		menus						= MenuSystemStateGetMenuHandles mDevice
		ioState1					= IOStSetDevice (MenuSystemState menus1) ioState
		pState1						= {pState & io=ioState1}
		(msgEvent1,menus1,pState2)	= menuMsgIO msgEvent menus pState1
		
		menuMsgIO :: !MsgEvent !(MenuHandles (PSt .l)) (PSt .l) -> (!MsgEvent,!MenuHandles (PSt .l),PSt .l)
		menuMsgIO msgEvent menus=:{mMenus=mHs} pState
			# (msgEvent,mHs,pState)	= menusMsgIO (getMsgEventRecLoc msgEvent).rlParentId msgEvent mHs pState
			= (msgEvent,{menus & mMenus=mHs},pState)
		where
			menusMsgIO :: !Id !MsgEvent ![MenuStateHandle (PSt .l)] (PSt .l)
						  -> (!MsgEvent,![MenuStateHandle (PSt .l)], PSt .l)
			menusMsgIO menuId msgEvent msHs pState
				| isEmpty msHs
					= menudeviceFatalError "menuIO (ReceiverEvent _) _" "menu could not be found"
				# (msH,msHs)				= HdTl msHs
				  (id,msH)					= menuStateHandleGetMenuId msH
				| id==menuId
					# (msgEvent,msH,pState)	= menuStateMsgIO msgEvent msH pState
					= (msgEvent,[msH:msHs],pState)
				| otherwise
					# (msgEvent,msHs,pState)= menusMsgIO menuId msgEvent msHs pState
					= (msgEvent,[msH:msHs],pState)
	
	menuIO deviceEvent=:(MenuTraceEvent info) pState
		= (deviceEvent,pState2)
	where
		(_,mDevice,ioState)	= IOStGetDevice MenuDevice pState.io
		menus				= MenuSystemStateGetMenuHandles mDevice
		ioState1			= IOStSetDevice (MenuSystemState menus1) ioState
		pState1				= {pState & io=ioState1}
		(menus1,pState2)	= menuTraceIO info menus pState1
		
		menuTraceIO :: !MenuTraceInfo !(MenuHandles (PSt .l)) (PSt .l) -> (!MenuHandles (PSt .l),PSt .l)
		menuTraceIO info=:{mtId} menus=:{mMenus=mHs} pState
			# (mHs,pState)	= menusTraceIO mtId info mHs pState
			= ({menus & mMenus=mHs},pState)
		where
			menusTraceIO :: !Id !MenuTraceInfo ![MenuStateHandle (PSt .l)] (PSt .l) -> (![MenuStateHandle (PSt .l)],PSt .l)
			menusTraceIO menuId info msHs pState
				| isEmpty msHs
					= menudeviceFatalError "menuIO (MenuTraceEvent _) _" "menu could not be found"
				# (msH,msHs)		= HdTl msHs
				  (id, msH)			= menuStateHandleGetMenuId msH
				| id==menuId
					# (msH,pState)	= menuStateTraceIO info msH pState
					= ([msH:msHs],pState)
				| otherwise
					# (msHs,pState)	= menusTraceIO menuId info msHs pState
					= ([msH:msHs],pState)
	
	menuIO deviceEvent=:(ToolbarSelection {tbsItemNr}) pState
		# (atts,pState)			= accPIO IOStGetProcessAttributes pState
		  (hasToolbarAtt,att)	= Select isProcessToolbar undef atts
		| not hasToolbarAtt
			= (deviceEvent,pState)
		| otherwise
			# toolbarItems		= getProcessToolbarAtt att
			  f					= gettoolbarfunction tbsItemNr toolbarItems
			= (deviceEvent,f pState)
	where
		gettoolbarfunction :: !Int ![ToolbarItem .pst] -> IdFun .pst
		gettoolbarfunction i [item:items]
			| i==1 && isItem	= f
			| otherwise			= gettoolbarfunction i` items
		where
			(isItem,i`,f)		= case item of
					  				ToolbarItem _ _ f	-> (True,i-1,f)
					  				ToolbarSeparator	-> (False,i,undef)
		gettoolbarfunction _ []
			= menudeviceFatalError "menuIO (ToolbarSelection)" "toolbar index out of range"
	
	menuIO _ _
		= menudeviceFatalError "menuIO" "unexpected DeviceEvent"


/*	Apply the Menu(Mods)Function of a selected menu item.
*/
menuStateTraceIO :: !MenuTraceInfo !(MenuStateHandle (PSt .l)) (PSt .l) -> (!MenuStateHandle (PSt .l),PSt .l)
menuStateTraceIO info=:{mtParents} (MenuLSHandle {mlsState=ls,mlsHandle=mH=:{mItems}}) pState
	# (mItems,(ls,pState)) = subMenusTraceIO info mtParents mItems (ls,pState)
	= (MenuLSHandle {mlsState=ls,mlsHandle={mH & mItems=mItems}},pState)
where
//	subMenusTraceIO finds the final submenu that contains the selected menu item and then applies its Menu(Mods)Function.
	subMenusTraceIO :: !MenuTraceInfo ![Int] ![MenuElementHandle .ls .pst] !(.ls,.pst)
										 -> (![MenuElementHandle .ls .pst], (.ls,.pst))
	subMenusTraceIO info [] itemHs ls_ps
		# (_,itemHs,ls_ps)	= menuElementsTraceIO info.mtItemNr info 0 itemHs ls_ps
		= (itemHs,ls_ps)
	subMenusTraceIO info [subIndex:subIndices] itemHs ls_ps
		# (_,itemHs,ls_ps)	= subMenuTraceIO subIndex info subIndices 0 itemHs ls_ps
		= (itemHs,ls_ps)
	where
		subMenuTraceIO :: !Int !MenuTraceInfo ![Int] !Int ![MenuElementHandle .ls .pst] !(.ls,.pst)
												 -> (!Int,![MenuElementHandle .ls .pst], (.ls,.pst))
		subMenuTraceIO parentIndex info parentsIndex zIndex [itemH:itemHs] ls_ps
			# (zIndex,itemH,ls_ps)	= subMenuTraceIO` parentIndex info parentsIndex zIndex itemH ls_ps
			| parentIndex<zIndex
				= (zIndex,[itemH:itemHs],ls_ps)
			| otherwise
				# (zIndex,itemHs,ls_ps)	= subMenuTraceIO parentIndex info parentsIndex zIndex itemHs ls_ps
				= (zIndex,[itemH:itemHs],ls_ps)
		where
			subMenuTraceIO` :: !Int !MenuTraceInfo ![Int] !Int !(MenuElementHandle .ls .pst) !(.ls,.pst)
													  -> (!Int, !MenuElementHandle .ls .pst,  (.ls,.pst))
			subMenuTraceIO` parentIndex info parentsIndex zIndex itemH=:(SubMenuHandle subH=:{mSubItems}) ls_ps
				| parentIndex<>zIndex
					= (zIndex+1,itemH,ls_ps)
				| otherwise
					# (itemHs,ls_ps)= subMenusTraceIO info parentsIndex mSubItems ls_ps
					= (zIndex+1,SubMenuHandle {subH & mSubItems=itemHs},ls_ps)
			subMenuTraceIO` parentIndex info parentsIndex zIndex (RadioMenuHandle itemH=:{mRadioItems}) ls_ps
				# (nrRadios,itemHs)	= Ulength mRadioItems
				= (zIndex+nrRadios,RadioMenuHandle {itemH & mRadioItems=itemHs},ls_ps)
			subMenuTraceIO` parentIndex info parentsIndex zIndex itemH=:(MenuItemHandle _) ls_ps
				= (zIndex+1,itemH,ls_ps)
			subMenuTraceIO` parentIndex info parentsIndex zIndex (MenuListLSHandle itemHs) ls_ps
				# (zIndex,itemHs,ls_ps)	= subMenuTraceIO parentIndex info parentsIndex zIndex itemHs ls_ps
				= (zIndex,MenuListLSHandle itemHs,ls_ps)
			subMenuTraceIO` parentIndex info parentsIndex zIndex (MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs}) (ls,ps)
				# (zIndex,itemHs,((ls1,ls),ps))	= subMenuTraceIO parentIndex info parentsIndex zIndex itemHs ((ls1,ls),ps)
				= (zIndex,MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs},(ls,ps))
			subMenuTraceIO` parentIndex info parentsIndex zIndex (MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs}) (ls,ps)
				# (zIndex,itemHs,(ls1,ps))	= subMenuTraceIO parentIndex info parentsIndex zIndex itemHs (ls1,ps)
				= (zIndex,MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs},(ls,ps))
			subMenuTraceIO` _ _ _ zIndex itemH ls_ps
				= (zIndex,itemH,ls_ps)
		subMenuTraceIO _ _ _ zIndex itemHs ls_ps
			= (zIndex,itemHs,ls_ps)
	
//	menuElementsTraceIO applies the Menu(Mods)Function of the menu item at index itemIndex to the context state.
	menuElementsTraceIO :: !Int !MenuTraceInfo !Int ![MenuElementHandle .ls .pst] !(.ls,.pst)
										   -> (!Int,![MenuElementHandle .ls .pst], (.ls,.pst))
	menuElementsTraceIO itemIndex info zIndex [itemH:itemHs] ls_ps
		# (zIndex,itemH,ls_ps)		= menuElementTraceIO itemIndex info zIndex itemH ls_ps
		| itemIndex<zIndex
			= (zIndex,[itemH:itemHs],ls_ps)
		| otherwise
			# (zIndex,itemHs,ls_ps)	= menuElementsTraceIO itemIndex info zIndex itemHs ls_ps
			= (zIndex,[itemH:itemHs],ls_ps)
	where
		menuElementTraceIO :: !Int !MenuTraceInfo !Int !(MenuElementHandle .ls .pst) !(.ls,.pst)
											  -> (!Int, !MenuElementHandle .ls .pst,  (.ls,.pst))
		menuElementTraceIO itemIndex info zIndex itemH=:(MenuItemHandle {mItemAtts}) (ls,ps)
			| itemIndex<>zIndex || not hasFun	= (zIndex+1,itemH,  (ls,ps))
			| otherwise							= (zIndex+1,itemH,f (ls,ps))
		where
			(hasFun,fAtt)	= Select isEitherFun undef mItemAtts
			f				= if (isMenuFunction fAtt)	(getMenuFun fAtt)
														(getMenuModsFun fAtt info.mtModifiers)
			isEitherFun f	= isMenuFunction f || isMenuModsFunction f
		menuElementTraceIO itemIndex info zIndex (RadioMenuHandle radioH=:{mRadioIndex,mRadioItems}) ls_ps
			# (nrRadios,itemHs)		= Ulength mRadioItems
			| itemIndex>zIndex+nrRadios
				// Selected item is not one of these radio items
				= (zIndex+nrRadios,RadioMenuHandle radioH,ls_ps)
			| otherwise
				# (_,itemHs,ls_ps)	= menuElementsTraceIO itemIndex info zIndex itemHs ls_ps
				= (zIndex+nrRadios,RadioMenuHandle {radioH & mRadioItems=itemHs},ls_ps)		// It is assumed that the mRadioIndex is correctly set by the menu EventFunction
		menuElementTraceIO itemIndex info zIndex itemH=:(SubMenuHandle _) ls_ps
			= (zIndex+1,itemH,ls_ps)
		menuElementTraceIO itemIndex info zIndex (MenuListLSHandle itemHs) ls_ps
			# (zIndex,itemHs,ls_ps) = menuElementsTraceIO itemIndex info zIndex itemHs ls_ps
			= (zIndex,MenuListLSHandle itemHs,ls_ps)
		menuElementTraceIO itemIndex info zIndex (MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs}) (ls,ps)
			# (zIndex,itemHs,((ls1,ls),ps)) = menuElementsTraceIO itemIndex info zIndex itemHs ((ls1,ls),ps)
			= (zIndex,MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs},(ls,ps))
		menuElementTraceIO itemIndex info zIndex (MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs}) (ls,ps)
			# (zIndex,itemHs,(ls1,ps)) = menuElementsTraceIO itemIndex info zIndex itemHs (ls1,ps)
			= (zIndex,MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs},(ls,ps))
		menuElementTraceIO _ _ zIndex itemH ls_ps
			= (zIndex,itemH,ls_ps)
	menuElementsTraceIO _ _ zIndex itemHs ls_ps
		= (zIndex,itemHs,ls_ps)


/*	menuStateMsgIO handles all message events.
*/
menuStateMsgIO :: !MsgEvent !(MenuStateHandle (PSt .l)) (PSt .l) -> (!MsgEvent,!MenuStateHandle (PSt .l),PSt .l)
menuStateMsgIO msgEvent msH=:(MenuLSHandle mlsH=:{mlsState=ls,mlsHandle=mH}) pState
	= (msgEvent1,MenuLSHandle {mlsH & mlsState=ls1,mlsHandle=mH1},pState1)
where
	recLoc							= getMsgEventRecLoc msgEvent
	rId								= recLoc.rlReceiverId
	action							= case msgEvent of
										(QASyncMessage msg)	-> menuQASyncIO rId msg
										( ASyncMessage msg) -> menuASyncIO  rId msg
										(  SyncMessage msg) -> menuSyncIO   rId msg
	(msgEvent1,mH1,(ls1,pState1))	= action mH (ls,pState)

	//	menuQASyncIO queues an asynchronous message in the message queue of the indicated receiver menu element.
	menuQASyncIO :: !Id !QASyncMessage !(MenuHandle .ls .pst) (.ls,.pst) -> (!MsgEvent,!MenuHandle .ls .pst,(.ls,.pst))
	menuQASyncIO rId msg mH=:{mItems} ls_ps
		= (QASyncMessage msg,{mH & mItems=itemHs},ls_ps)
	where
		(_,itemHs)	= elementsQASyncIO rId msg.qasmMsg mItems
		
		elementsQASyncIO :: !Id !SemiDynamic ![MenuElementHandle .ls .pst] -> (!Bool,![MenuElementHandle .ls .pst])
		elementsQASyncIO rId msg [itemH:itemHs]
			# (done,itemH)		= elementQASyncIO rId msg itemH
			| done
				= (done,[itemH:itemHs])
			| otherwise
				# (done,itemHs)	= elementsQASyncIO rId msg itemHs
				= (done,[itemH:itemHs])
		where
			elementQASyncIO :: !Id !SemiDynamic !(MenuElementHandle .ls .pst) -> (!Bool,!MenuElementHandle .ls .pst)
			elementQASyncIO rId msg mrH=:(MenuReceiverHandle itemH=:{mReceiverHandle=rH})
				| rId<>rH.rId	= (False,mrH)
				| otherwise		= (True,MenuReceiverHandle {itemH & mReceiverHandle=receiverAddASyncMessage rId msg rH})
			elementQASyncIO rId msg (SubMenuHandle itemH=:{mSubItems=itemHs})
				# (done,itemHs)	= elementsQASyncIO rId msg itemHs
				= (done,SubMenuHandle {itemH & mSubItems=itemHs})
			elementQASyncIO rId msg (MenuListLSHandle itemHs)
				# (done,itemHs)	= elementsQASyncIO rId msg itemHs
				= (done,MenuListLSHandle itemHs)
			elementQASyncIO rId msg (MenuExtendLSHandle	mExH=:{mExtendItems=itemHs})
				# (done,itemHs)	= elementsQASyncIO rId msg itemHs
				= (done,MenuExtendLSHandle {mExH & mExtendItems=itemHs})
			elementQASyncIO rId msg (MenuChangeLSHandle	mChH=:{mChangeItems=itemHs})
				# (done,itemHs)	= elementsQASyncIO rId msg itemHs
				= (done,MenuChangeLSHandle {mChH & mChangeItems=itemHs})
			elementQASyncIO rId msg itemH
				= (False,itemH)
		elementsQASyncIO _ _ _
			= (False,[])

	//	menuASyncIO handles the first asynchronous message in the message queue of the indicated receiver menu element.
	menuASyncIO :: !Id !ASyncMessage !(MenuHandle .ls .pst) (.ls,.pst) -> (!MsgEvent,!MenuHandle .ls .pst,(.ls,.pst))
	menuASyncIO rId msg mH=:{mItems} ls_ps
		= (ASyncMessage msg,{mH & mItems=itemHs},ls_ps1)
	where
		(_,itemHs,ls_ps1)	= elementsASyncIO rId mItems ls_ps
		
		elementsASyncIO :: !Id ![MenuElementHandle .ls .pst] (.ls,.pst) -> (!Bool,![MenuElementHandle .ls .pst],(.ls,.pst))
		elementsASyncIO rId [itemH:itemHs] ls_ps
			# (done,itemH,ls_ps)		= elementASyncIO rId itemH ls_ps
			| done
				= (done,[itemH:itemHs],ls_ps)
			| otherwise
				# (done,itemHs,ls_ps)	= elementsASyncIO rId itemHs ls_ps
				= (done,[itemH:itemHs],ls_ps)
		where
			elementASyncIO :: !Id !(MenuElementHandle .ls .pst) (.ls,.pst) -> (!Bool,!MenuElementHandle .ls .pst,(.ls,.pst))
			elementASyncIO rId mrH=:(MenuReceiverHandle itemH=:{mReceiverHandle=rH}) ls_ps
				| rId<>rH.rId
					= (False,mrH,ls_ps)
				| otherwise
					# (rH,ls_ps)	= receiverASyncIO rH ls_ps
					= (True,MenuReceiverHandle {itemH & mReceiverHandle=rH},ls_ps)
			where
				receiverASyncIO :: !(ReceiverHandle .ls .pst) (.ls,.pst) -> (!ReceiverHandle .ls .pst,(.ls,.pst))
				receiverASyncIO rH=:{rASMQ=[msg:msgs],rFun} ls_ps
					# (ls,_,ps)	= rFun msg ls_ps
					= ({rH & rASMQ=msgs},(ls,ps))
				receiverASyncIO _ _
					= menudeviceFatalError "receiverASyncIO" "unexpected empty asynchronous message queue"
			elementASyncIO rId (SubMenuHandle itemH=:{mSubItems=itemHs}) ls_ps
				# (done,itemHs,ls_ps)	= elementsASyncIO rId itemHs ls_ps
				= (done,SubMenuHandle {itemH & mSubItems=itemHs},ls_ps)
			elementASyncIO rId (MenuListLSHandle itemHs) ls_ps
				# (done,itemHs,ls_ps)	= elementsASyncIO rId itemHs ls_ps
				= (done,MenuListLSHandle itemHs,ls_ps)
			elementASyncIO rId (MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs}) (ls,ps)
				# (done,itemHs,((ls1,ls),ps))	= elementsASyncIO rId itemHs ((ls1,ls),ps)
				= (done,MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs},(ls,ps))
			elementASyncIO rId (MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs}) (ls,ps)
				# (done,itemHs,(ls1,ps))	= elementsASyncIO rId itemHs (ls1,ps)
				= (done,MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs},(ls,ps))
			elementASyncIO _ itemH ls_ps
				= (False,itemH,ls_ps)
		elementsASyncIO _ _ ls_ps
			= (False,[],ls_ps)

	//	menuSyncIO lets the indicated receiver control handle the synchronous message.
	menuSyncIO :: !Id !SyncMessage !(MenuHandle .ls .pst) (.ls,.pst) -> (!MsgEvent,MenuHandle .ls .pst,(.ls,.pst))
	menuSyncIO r2Id msg mH=:{mItems} ls_ps
		= (SyncMessage msg1,{mH & mItems=itemHs},ls_ps1)
	where
		(_,msg1,itemHs,ls_ps1)	= elementsSyncIO r2Id msg mItems ls_ps
		
		elementsSyncIO :: !Id !SyncMessage ![MenuElementHandle .ls .pst] (.ls,.pst)
					-> (!Bool,!SyncMessage, [MenuElementHandle .ls .pst],(.ls,.pst))
		elementsSyncIO r2Id msg [itemH:itemHs] ls_ps
			# (done,msg,itemH,ls_ps)		= elementSyncIO r2Id msg itemH ls_ps
			| done
				= (done,msg,[itemH:itemHs],ls_ps)
			| otherwise
				# (done,msg,itemHs,ls_ps)	= elementsSyncIO r2Id msg itemHs ls_ps
				= (done,msg,[itemH:itemHs],ls_ps)
		where
			elementSyncIO :: !Id !SyncMessage !(MenuElementHandle .ls .pst) (.ls,.pst)
					   -> (!Bool,!SyncMessage,  MenuElementHandle .ls .pst, (.ls,.pst))
			elementSyncIO rId msg mrH=:(MenuReceiverHandle itemH=:{mReceiverHandle=rH}) ls_ps
				| rId<>rH.rId
					= (False,msg,mrH,ls_ps)
				| otherwise
					# (msg,rH,ls_ps)= receiverSyncIO msg rH ls_ps
					= (True,msg,MenuReceiverHandle {itemH & mReceiverHandle=rH},ls_ps)
			where
				receiverSyncIO :: !SyncMessage !(ReceiverHandle .ls .pst) (.ls,.pst)
							  -> (!SyncMessage,  ReceiverHandle .ls .pst, (.ls,.pst))
				receiverSyncIO msg rH ls_ps
					# (response,rH,ls_ps)	= receiverHandleSyncMessage msg rH ls_ps
					= ({msg & smResp=response},rH,ls_ps)
			elementSyncIO rId msg (SubMenuHandle itemH=:{mSubItems=itemHs}) ls_ps
				# (done,msg,itemHs,ls_ps)	= elementsSyncIO rId msg itemHs ls_ps
				= (done,msg,SubMenuHandle {itemH & mSubItems=itemHs},ls_ps)
			elementSyncIO rId msg (MenuListLSHandle itemHs) ls_ps
				# (done,msg,itemHs,ls_ps)	= elementsSyncIO rId msg itemHs ls_ps
				= (done,msg,MenuListLSHandle itemHs,ls_ps)
			elementSyncIO rId msg (MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs}) (ls,ps)
				# (done,msg,itemHs,((ls1,ls),ps))	= elementsSyncIO rId msg itemHs ((ls1,ls),ps)
				= (done,msg,MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs},(ls,ps))
			elementSyncIO rId msg (MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs}) (ls,ps)
				# (done,msg,itemHs,(ls1,ps))	= elementsSyncIO rId msg itemHs (ls1,ps)
				= (done,msg,MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs},(ls,ps))
			elementSyncIO rId msg itemH ls_ps
				= (False,msg,itemH,ls_ps)
		elementsSyncIO _ msg _ ls_ps
			= (False,msg,[],ls_ps)


/*	Check if the interactive process is active:
	Note:	this test depends on the fact that every interactive process has an AppleMenu,
			with mac menu ID AppleMenuId. This is taken care of by the creation
			of the interactive process (see menuOpen above).
*/
IOStIsActive :: !(IOSt .l) -> (!Bool, !IOSt .l)
IOStIsActive ioState
/* RWS +++
	# (globalHandle,ioState)= accIOToolbox (GetMHandle AppleMenuId) ioState
	  (menus,ioState)		= IOStGetDevice MenuDevice ioState
	  (mHs,menuHs)			= MenuHandlesGetMenuStateHandles (MenuSystemStateGetMenuHandles menus)
	  (appleH,others)		= HdTl mHs
	  (apple,appleH1)		= menuStateHandleGetHandle appleH
	  ioState				= IOStSetDevice (MenuSystemState {menuHs & mMenus=[appleH1:others]}) ioState
	= (apple==globalHandle,ioState)
*/	= (True,ioState)


/*	Activate the interactive process:
*/
ActivateMenuSystem :: !(IOSt .l) -> IOSt .l
ActivateMenuSystem ioState
	# ioState					= SelectIOSt ioState
	# (osdinfo,ioState)			= IOStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdinfo
	| isNothing maybeOSMenuBar
		= ioState
	| otherwise
		# osMenuBar				= fromJust maybeOSMenuBar
		# (osMenuBar,ioState)	= accIOToolbox (OSMenuBarSet osMenuBar) ioState
		  osdinfo				= setOSDInfoOSMenuBar osMenuBar osdinfo
		# ioState				= IOStSetOSDInfo osdinfo ioState
		= ioState
