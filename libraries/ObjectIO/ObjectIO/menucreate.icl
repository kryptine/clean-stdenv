implementation module menucreate


//	Clean Object I/O library, version 1.2


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	StdMenuElementClass, StdPSt
import	commondef, devicesystemstate, iostate, menuaccess, menudefaccess, menuhandle, sdisize
from	menuevent	import MenuSystemStateGetMenuHandles, MenuHandlesGetMenuStateHandles
import	osmenu
from	menuCrossCall_12 import WinRemoveMenuShortKey


menucreateFatalError :: String String -> .x
menucreateFatalError rule error
	= FatalError rule "menucreate" error


/*	Creating menus:
		Because in a SDI process menus might be added to the process window, the ViewFrame of the
		process window can change size.
		In that case, the layout of the controls should be recalculated, and the window updated.
	OpenMenu` assumes that the Id argument has been verified.
*/
OpenMenu` :: !Id .ls !(Menu m .ls (PSt .l .p)) !(PSt .l .p) -> (!ErrorReport,!PSt .l .p) | MenuElements m
OpenMenu` menuId ls mDef pState
	# (idtable,ioState)					= IOStGetIdTable pState.io
	# (mDevice,ioState)					= IOStGetDevice MenuDevice ioState
	  mHs								= MenuSystemStateGetMenuHandles mDevice
	| Contains (isMenuWithThisId menuId) mHs.mMenus	// This condition should never hold
		= menucreateFatalError "openMenu (Menu)" "inconsistency detected between IdTable and ReceiverTable"
	# (menus,mHs)						= MenuHandlesGetMenuStateHandles mHs
	  (nrmenus,menus)					= Ulength menus
	  (optIndex,mDef)					= menuDefGetIndex mDef
	  index								= if (isJust optIndex) (fromJust optIndex) nrmenus
	  hasMenuWindowMenu					= Contains (isMenuWithThisId WindowMenuId) menus
	  index`							= SetBetween index 0 (max 0 (if hasMenuWindowMenu (nrmenus-1) nrmenus))
	# (rt,ioState)						= IOStGetReceiverTable ioState
	# (ioid,ioState)					= IOStGetIOId ioState
	# (sdiSize1,sdiPtr,ioState)			= getSDIWindowSize ioState
	# pState							= {pState & io=ioState}
	# (ok,mH,mHs,rt,idtable,pState)		= createMenu index` ioid menuId mDef mHs rt idtable pState
 	# ioState							= IOStSetReceiverTable rt pState.io
 	# ioState							= IOStSetIdTable idtable ioState
	| not ok
		# ioState						= IOStSetDevice (MenuSystemState mHs) ioState
		= (ErrorIdsInUse,{pState & io=ioState})
	| otherwise
		= (NoError,pState2)
	with
		(before,after)					= splitAt index` menus
		msH								= MenuLSHandle {mlsState=ls1,mlsHandle=mH}
		mHs1							= {mHs & mMenus=before++[msH:after]}
		ioState1						= appIOToolbox (DrawMenuBar mHs.mOSMenuBar) ioState
		ioState2						= IOStSetDevice (MenuSystemState mHs1) ioState1
		ioState3						= checkSDISize sdiPtr sdiSize1 ioState2
		pState1							= {pState & io=ioState3}
		(ls1,pState2)					= menuInit (ls,pState1)
where
	menuInit							= getMenuDefInit mDef
	
	checkSDISize :: !OSWindowPtr !Size !(IOSt .l .p) -> IOSt .l .p
	checkSDISize sdiPtr sdiSize1 ioState
		# (sdiSize2,_,ioState)			= getSDIWindowSize ioState
		| sdiSize1==sdiSize2			= ioState
		| otherwise						= resizeSDIWindow sdiPtr sdiSize1 sdiSize2 ioState
	
	getMenuDefInit :: !(Menu m .ls .pst) -> IdFun *(.ls,.pst)
	getMenuDefInit (Menu _ _ atts)
		= getMenuInitFun (snd (Select isMenuInit (MenuInit id) atts))

	createMenu :: !Int !SystemId !Id !(Menu m .ls (PSt .l .p)) !(MenuHandles (PSt .l .p)) !ReceiverTable !IdTable !(PSt .l .p)
						 -> (!Bool,MenuHandle .ls (PSt .l .p),  !MenuHandles (PSt .l .p), !ReceiverTable,!IdTable, !PSt .l .p)
						 |  MenuElements m
	createMenu index ioId menuId mDef mHs=:{mOSMenuBar=menuBar, mKeys=keys} rt it pState
		# (ms,pState)				= menuElementToHandles (menuDefGetElements mDef) pState
		  itemHs					= map MenuElementStateToMenuElementHandle ms
		  (ok,itemHs,rt,it)			= menuIdsAreConsistent ioId menuId itemHs rt it
		| not ok
			= (False,undef,mHs,rt,it,pState)
		| otherwise
			# (tb,ioState)			= getIOToolbox pState.io
			# (menu,mH,menuBar,tb)	= NewMenuHandle mDef index menuId menuBar tb
			# (_,itemHs,keys,tb)	= createMenuElements menuBar menu 1 itemHs keys tb
			  mH					= {mH & mItems=itemHs}
			  mHs					= {mHs & mOSMenuBar=menuBar, mKeys=keys}
			# ioState				= setIOToolbox tb ioState
			# pState				= {pState & io=ioState}
			  (_,it)				= addIdToIdTable menuId {idpIOId=ioId,idpDevice=MenuDevice,idpId=menuId} it
			= (True,mH,mHs,rt,it,pState)

isMenuWithThisId :: !Id !(MenuStateHandle .pst) -> Bool
isMenuWithThisId id msH
	= id==fst (menuStateHandleGetMenuId msH)

/*	creating pop up menus.
	It is assumed that MenuHandles contains no pop up menu in mMenus and that mPopUpId contains an Id.
*/
createPopUpMenu :: !SystemId .ls !(PopUpMenu m .ls (PSt .l .p)) !(MenuHandles (PSt .l .p)) !ReceiverTable !IdTable !(PSt .l .p)
													   -> (!Bool,!MenuHandles (PSt .l .p), !ReceiverTable,!IdTable, !PSt .l .p)
													   |  PopUpMenuElements m
createPopUpMenu ioId ls (PopUpMenu items) mHs=:{mMenus, mOSMenuBar=menuBar, mKeys=keys, mPopUpId} rt it pState
	# (ms,pState)			= popUpMenuElementToHandles items pState
	  itemHs				= map MenuElementStateToMenuElementHandle ms
	  menuId				= fromJust mPopUpId
	  (ok,itemHs,rt,it)		= menuIdsAreConsistent ioId menuId itemHs rt it
	| not ok
		= (False,mHs,rt,it,pState)
	| otherwise
		# (tb,ioState)		= getIOToolbox pState.io
		# (menu,tb)			= OScreatePopUpMenu tb
		# (_,itemHs,keys,tb)= createMenuElements menuBar menu 1 itemHs keys tb
		  itemHs			= map validatePopUpMenuFunction itemHs
		  mlsH				= {	mHandle		= menu
							  , mMenuId		= menuId
							  ,	mOSMenuNr	= 0
							  ,	mTitle		= ""
							  ,	mSelect		= True
							  ,	mItems		= itemHs
							  }
		  msH				= MenuLSHandle {mlsState=ls,mlsHandle=mlsH}
		  mHs				= {mHs & mMenus=[msH:mMenus], mOSMenuBar=menuBar, mKeys=keys, mPopUpId=Nothing}
		# ioState			= setIOToolbox tb ioState
		# pState			= {pState & io=ioState}
		= (True,mHs,rt,it,pState)
where
/*	validatePopUpMenuFunction takes care that all Menu(Mods)Function arguments of the elements
	apply closePopUpMenu after their own action.
*/
	validatePopUpMenuFunction :: !(MenuElementHandle .ls (PSt .l .p)) -> MenuElementHandle .ls (PSt .l .p)
	validatePopUpMenuFunction (MenuItemHandle itemH=:{mItemAtts})
		= MenuItemHandle {itemH & mItemAtts=map validateMenuFunction mItemAtts}
	where
		validateMenuFunction :: !(MenuAttribute *(.ls,PSt .l .p)) -> MenuAttribute *(.ls,PSt .l .p)
		validateMenuFunction (MenuFunction f)
			= MenuFunction (f` f)
		where
			f` :: (IdFun *(.ls,PSt .l .p)) !*(.ls,PSt .l .p) -> *(.ls,PSt .l .p)
			f` f (ls,pst)
				# (ls,pst)	= f (ls,pst)
				= (ls,closePopUpMenu pst)
		validateMenuFunction (MenuModsFunction f)
			= MenuModsFunction (f` f)
		where
			f` :: (ModifiersFunction *(.ls,PSt .l .p)) !Modifiers !*(.ls,PSt .l .p) -> *(.ls,PSt .l .p)
			f` f modifiers (ls,pst)
				# (ls,pst)	= f modifiers (ls,pst)
				= (ls,closePopUpMenu pst)
		validateMenuFunction att
			= att
	validatePopUpMenuFunction (MenuReceiverHandle _)
		= menucreateFatalError "validatePopUpMenuFunction" "Receiver(2) should not be an element of PopUpMenus"
	validatePopUpMenuFunction (SubMenuHandle _)
		= menucreateFatalError "validatePopUpMenuFunction" "SubMenu should not be an element of PopUpMenus"
	validatePopUpMenuFunction (RadioMenuHandle radioH=:{mRadioItems})
		= RadioMenuHandle {radioH & mRadioItems=map validatePopUpMenuFunction mRadioItems}
	validatePopUpMenuFunction (MenuSeparatorHandle separatorH)
		= MenuSeparatorHandle separatorH
	validatePopUpMenuFunction (MenuListLSHandle itemHs)
		= MenuListLSHandle (map validatePopUpMenuFunction itemHs)
	validatePopUpMenuFunction (MenuExtendLSHandle mExH=:{mExtendItems})
		= MenuExtendLSHandle {mExH & mExtendItems=map validatePopUpMenuFunction mExtendItems}
	validatePopUpMenuFunction (MenuChangeLSHandle mChH=:{mChangeItems})
		= MenuChangeLSHandle {mChH & mChangeItems=map validatePopUpMenuFunction mChangeItems}
	
/*	closePopUpMenu takes care that the internal administration of the menus is restored again to
	no open pop up menu. It is assumed that all resources have been freed.
*/
	closePopUpMenu :: !(PSt .l .p) -> PSt .l .p
	closePopUpMenu pState=:{io}
		# (mDevice,ioState)	= IOStGetDevice MenuDevice io
		  mHs				= MenuSystemStateGetMenuHandles mDevice
		  mHs				= closepopupmenu mHs
		# ioState			= IOStSetDevice (MenuSystemState mHs) ioState
		= {pState & io=ioState}


/*	Creating menu elements: retrieving toolbox handles and ids for elements, and building the menu gui.
*/
createMenuElements :: !OSMenuBar !OSMenu !Int ![MenuElementHandle .ls .pst] ![Char] !*OSToolbox
									 -> (!Int,![MenuElementHandle .ls .pst],![Char],!*OSToolbox)
createMenuElements osmenubar menu iNr itemHs keys tb
	| isEmpty itemHs
		= (iNr,itemHs,keys,tb)
	| otherwise
		# (itemH,itemHs)		= HdTl itemHs
		# (iNr,itemH, keys,tb)	= createMenuElement  osmenubar menu iNr itemH keys tb
		# (iNr,itemHs,keys,tb)	= createMenuElements osmenubar menu iNr itemHs keys tb
		= (iNr,[itemH:itemHs],keys,tb)
where
	createMenuElement :: !OSMenuBar !OSMenu !Int !(MenuElementHandle .ls .pst) ![Char] !*OSToolbox
										-> (!Int, !MenuElementHandle .ls .pst, ![Char],!*OSToolbox)
	createMenuElement osmenubar menu iNr (SubMenuHandle subH) keys tb
		# (subH,tb)				= NewSubMenuHandle subH iNr menu tb
		# (_,itemHs,keys,tb)	= createMenuElements osmenubar subH.mSubHandle 1 subH.mSubItems keys tb
		  subH					= {subH & mSubItems=itemHs}
		= (iNr+1,SubMenuHandle subH,keys,tb)
	createMenuElement osmenubar menu iNr (RadioMenuHandle itemH=:{mRadioItems=itemHs}) keys tb
		# (iNr,itemHs,keys,tb)	= createMenuElements osmenubar menu iNr itemHs keys tb
		= (iNr,RadioMenuHandle {itemH & mRadioItems=itemHs},keys,tb)
	createMenuElement osmenubar menu iNr (MenuItemHandle itemH) keys tb
		# (itemH,keys)			= checkshortcutkey itemH keys
		# (osMenuItem,tb)		= insertMenu osmenubar menu iNr itemH tb
		  itemH					= {itemH & mOSMenuItem=osMenuItem}
		= (iNr+1,MenuItemHandle itemH,keys,tb)
	createMenuElement osmenubar menu iNr (MenuSeparatorHandle itemH) keys tb
		# (osMenuSeparator,_,tb)= OSAppendMenuSeparator iNr menu tb
		  itemH					= {itemH & mOSMenuSeparator=osMenuSeparator}
		= (iNr+1,MenuSeparatorHandle itemH,keys,tb)
	createMenuElement _ _ iNr itemH=:(MenuReceiverHandle _) keys tb
		= (iNr,itemH,keys,tb)
	createMenuElement osmenubar menu iNr (MenuListLSHandle itemHs) keys tb
		# (iNr,itemHs,keys,tb)	= createMenuElements osmenubar menu iNr itemHs keys tb
		= (iNr,MenuListLSHandle itemHs,keys,tb)
	createMenuElement osmenubar menu iNr (MenuExtendLSHandle exH=:{mExtendItems=itemHs}) keys tb
		# (iNr,itemHs,keys,tb)	= createMenuElements osmenubar menu iNr itemHs keys tb
		= (iNr,MenuExtendLSHandle {exH & mExtendItems=itemHs},keys,tb)
	createMenuElement osmenubar menu iNr (MenuChangeLSHandle chH=:{mChangeItems=itemHs}) keys tb
		# (iNr,itemHs,keys,tb)	= createMenuElements osmenubar menu iNr itemHs keys tb
		= (iNr,MenuChangeLSHandle {chH & mChangeItems=itemHs},keys,tb)
	createMenuElement _ _ _ _ _ _
		= menucreateFatalError "createMenuElements" "unmatched MenuElementHandle"


/*	Extend an existing menu with new menu elements.
*/
extendMenu :: !OSMenuBar !OSMenu !Int ![MenuElementHandle .ls .pst] ![MenuElementHandle .ls .pst] ![Char] !*OSToolbox
															    -> (![MenuElementHandle .ls .pst],![Char],!*OSToolbox)
extendMenu osmenubar menu iNr itemHs items keys tb
	| isEmpty itemHs
		= (items,keys,tb)
	| otherwise
		# (itemH,itemHs)	= HdTl itemHs
		# (items,keys,tb)	= extendMenu  osmenubar menu iNr itemHs items keys tb
		# (itemH,keys,tb)	= extendMenu` osmenubar menu iNr itemH        keys tb
		= ([itemH:items],keys,tb)
where
	extendMenu` :: !OSMenuBar !OSMenu !Int !(MenuElementHandle .ls .pst) ![Char] !*OSToolbox
										-> (!MenuElementHandle .ls .pst, ![Char],!*OSToolbox)
	extendMenu` osmenubar menu iNr (SubMenuHandle subH) keys tb
		# (subH,tb)						= NewSubMenuHandle subH iNr menu tb
		# (_,itemHs,keys,tb)			= createMenuElements osmenubar subH.mSubHandle 1 subH.mSubItems keys tb
		  subH							= {subH & mSubItems=itemHs}
		= (SubMenuHandle subH,keys,tb)
	extendMenu` osmenubar menu iNr (RadioMenuHandle itemH=:{mRadioItems=itemHs}) keys tb
		# (_,itemHs,keys,tb)			= createMenuElements osmenubar menu iNr itemHs keys tb
		= (RadioMenuHandle {itemH & mRadioItems=itemHs},keys,tb)
	extendMenu` osmenubar menu iNr (MenuItemHandle itemH) keys tb
		# (itemH,keys)					= checkshortcutkey itemH keys
		# (osMenuItem,tb)				= insertMenu osmenubar menu iNr itemH tb
		= (MenuItemHandle {itemH & mOSMenuItem=osMenuItem},keys,tb)
	extendMenu` osmenubar menu iNr (MenuSeparatorHandle itemH) keys tb
		# (osMenuItem,_,tb)				= OSAppendMenuSeparator iNr menu tb
		= (MenuSeparatorHandle {itemH & mOSMenuSeparator=osMenuItem},keys,tb)
	extendMenu` _ _ _ itemH=:(MenuReceiverHandle _) keys tb
		= (itemH,keys,tb)
	extendMenu` osmenubar menu iNr (MenuListLSHandle itemHs) keys tb
		# (itemHs,keys,tb)				= extendMenu osmenubar menu iNr itemHs [] keys tb
		= (MenuListLSHandle itemHs,keys,tb)
	extendMenu` osmenubar menu iNr (MenuExtendLSHandle mExH=:{mExtendItems=itemHs}) keys tb
		# (itemHs,keys,tb)				= extendMenu osmenubar menu iNr itemHs [] keys tb
		= (MenuExtendLSHandle {mExH & mExtendItems=itemHs},keys,tb)
	extendMenu` osmenubar menu iNr (MenuChangeLSHandle mChH=:{mChangeItems=itemHs}) keys tb
		# (itemHs,keys,tb)	= extendMenu osmenubar menu iNr itemHs [] keys tb
		= (MenuChangeLSHandle {mChH & mChangeItems=itemHs},keys,tb)
	extendMenu` _ _ _ _ _ _
		= menucreateFatalError "extendMenu" "unmatched MenuElementHandle"


insertMenu :: !OSMenuBar !OSMenu !Int !(MenuItemHandle .ls .pst) !*OSToolbox -> (!OSMenuItem,!*OSToolbox)
insertMenu osmenubar menu iNr {mItemKey,mItemTitle,mItemSelect,mItemMark,mItemAtts} tb
	# (osMenuItem,_,tb)	= OSAppendMenuItem osmenubar iNr menu mItemTitle mItemSelect mItemMark shortcut tb
	= (osMenuItem,tb)
where
	shortcut			= case mItemKey of
							(Just key)	-> key
							_			-> '\0'

checkshortcutkey :: !(MenuItemHandle .ls .pst) ![Char] -> (!MenuItemHandle .ls .pst,![Char])
checkshortcutkey mItemH=:{mItemKey} cs
	| isNothing mItemKey	= ( mItemH,cs)
	| isMember c cs			= ({mItemH & mItemKey=Nothing},cs)
	| otherwise				= ( mItemH,[c:cs])
where
	c						= fromJust mItemKey


//	Creation and manipulation of Menu(Element)Handles:


SystemAble			:== True
SystemUnable		:== False

//	Initialisation and Allocation:

NewMenuHandle :: !(Menu m .ls .pst) !Int !Id !OSMenuBar !*OSToolbox -> (!OSMenu,!MenuHandle .ls .pst,!OSMenuBar,!*OSToolbox)
NewMenuHandle mDef index menuId menuBar tb
	# (ok,osMenuNr,tb)	= OSNewMenuNr tb		// PA: generation of internal menu numbers added
	| not ok
		= menucreateFatalError "NewMenuHandle" "To many Menus created for one interactive process"
	# (select,mDef)		= menuDefGetSelectState	mDef
	  (title,_)			= menuDefGetTitle		mDef
	# (menu,menuBar,tb)	= OSMenuInsert index osMenuNr title menuBar tb
	  mH				= {	mHandle		= menu
						  , mMenuId		= menuId
						  ,	mOSMenuNr	= osMenuNr
						  ,	mTitle		= title
						  ,	mSelect		= enabled select
						  ,	mItems		= []
						  }
	| enabled select
		= (menu,mH,menuBar,OSEnableMenu  index menuBar tb)
	| otherwise
		= (menu,mH,menuBar,OSDisableMenu index menuBar tb)

//	PA: New version of creating a SubMenu:
NewSubMenuHandle :: !(SubMenuHandle .ls .pst) !Int !OSMenu !*OSToolbox -> (!SubMenuHandle .ls .pst,!*OSToolbox)
NewSubMenuHandle mH=:{mSubTitle,mSubSelect} index menu tb
	# (ok,osMenuNr,tb)	= OSNewSubMenuNr tb
	| not ok
		= menucreateFatalError "NewSubMenuHandle" "To many SubMenus created for one interactive process"
	# (osH,_,tb)		= OSSubMenuInsert index osMenuNr mSubTitle menu tb
	# mH				= {mH & mSubHandle=osH,mSubOSMenuNr=osMenuNr}
	| mSubSelect
		= (mH,OSEnableMenuItem  menu osH tb)
	| otherwise
		= (mH,OSDisableMenuItem menu osH tb)

closepopupmenu :: !(MenuHandles .pst) -> MenuHandles .pst
closepopupmenu mHs=:{mMenus,mPopUpId}
	| isJust mPopUpId
		= mHs
	| otherwise
		# (msH,msHs)	= HdTl mMenus
		  (id,_)		= menuStateHandleGetMenuId msH
		= {mHs & mMenus=msHs,mPopUpId=Just id}

disposeMenuItemHandle :: !OSMenu !Int !(MenuItemHandle .ls .pst) !(![Char],!IdTable,!*OSToolbox) -> (![Char],!IdTable,!*OSToolbox)
disposeMenuItemHandle menu iNr {mItemKey,mItemId,mOSMenuItem} (keys,it,tb)
	= (keys`,it`,snd (OSMenuRemoveItem mOSMenuItem menu tb))
where
	keys`	= if (isJust mItemKey) [fromJust mItemKey:keys] keys
	it`		= if (isJust mItemId)  (snd (removeIdFromIdTable (fromJust mItemId) it)) it


disposeSubMenuHandles :: !(MenuElementHandle .ls .pst) !*OSToolbox -> *OSToolbox
disposeSubMenuHandles (MenuListLSHandle		 mListItems)	tb = StateMap2 disposeSubMenuHandles mListItems   tb
disposeSubMenuHandles (MenuExtendLSHandle	{mExtendItems})	tb = StateMap2 disposeSubMenuHandles mExtendItems tb
disposeSubMenuHandles (MenuChangeLSHandle	{mChangeItems})	tb = StateMap2 disposeSubMenuHandles mChangeItems tb
disposeSubMenuHandles _										tb = tb

disposeShortcutkeys :: !OSWindowPtr !(MenuElementHandle .ls .pst) !(![Char],!*OSToolbox) -> (![Char],!*OSToolbox)
disposeShortcutkeys framePtr (MenuItemHandle {mItemKey,mOSMenuItem}) (keys,tb)
	| isJust mItemKey
		= (thd3 (Remove ((==) key) key keys),WinRemoveMenuShortKey framePtr mOSMenuItem tb)
	with
		key = fromJust mItemKey
	| otherwise
		= (keys,tb)
disposeShortcutkeys framePtr (SubMenuHandle   {mSubItems})       keys_tb = StateMap2 (disposeShortcutkeys framePtr) mSubItems    keys_tb
disposeShortcutkeys framePtr (RadioMenuHandle {mRadioItems})     keys_tb = StateMap2 (disposeShortcutkeys framePtr) mRadioItems  keys_tb
disposeShortcutkeys framePtr (MenuListLSHandle	 mListItems)     keys_tb = StateMap2 (disposeShortcutkeys framePtr) mListItems   keys_tb
disposeShortcutkeys framePtr (MenuExtendLSHandle {mExtendItems}) keys_tb = StateMap2 (disposeShortcutkeys framePtr) mExtendItems keys_tb
disposeShortcutkeys framePtr (MenuChangeLSHandle {mChangeItems}) keys_tb = StateMap2 (disposeShortcutkeys framePtr) mChangeItems keys_tb
disposeShortcutkeys _ _                                          keys_tb = keys_tb

disposeMenuIds :: !SystemId !(MenuElementHandle .ls .pst) !(!ReceiverTable,!IdTable) -> (!ReceiverTable,!IdTable)
disposeMenuIds pid (MenuItemHandle {mItemId}) (rt,it)
	| isNothing mItemId		= (rt,it)
	| otherwise				= (rt,snd (removeIdFromIdTable (fromJust mItemId) it))
disposeMenuIds pid (MenuReceiverHandle {mReceiverHandle={rId}}) (rt,it)
	= (snd (removeReceiverFromReceiverTable rId rt),snd (removeIdFromIdTable rId it))
disposeMenuIds pid (SubMenuHandle {mSubItems}) ts
	= StateMap2 (disposeMenuIds pid) mSubItems ts
disposeMenuIds pid (RadioMenuHandle {mRadioId,mRadioItems}) ts
	# (rt,it)				= StateMap2 (disposeMenuIds pid) mRadioItems ts
	| isNothing mRadioId	= (rt,it)
	| otherwise				= (rt,snd (removeIdFromIdTable (fromJust mRadioId) it))
disposeMenuIds pid (MenuSeparatorHandle {mSepId}) (rt,it)
	| isNothing mSepId		= (rt,it)
	| otherwise				= (rt,snd (removeIdFromIdTable (fromJust mSepId) it))
disposeMenuIds pid (MenuListLSHandle mListItems) ts
	= StateMap2 (disposeMenuIds pid) mListItems ts
disposeMenuIds pid (MenuExtendLSHandle {mExtendItems}) ts
	= StateMap2 (disposeMenuIds pid) mExtendItems ts
disposeMenuIds pid (MenuChangeLSHandle {mChangeItems}) ts
	= StateMap2 (disposeMenuIds pid) mChangeItems ts

disposeMenuHandles :: !Bool !(MenuHandles .pst) !*OSToolbox -> (!OSMenuBar,!*OSToolbox)
disposeMenuHandles _ menus=:{mOSMenuBar,mMenus} tb
	= StateMap2 dispose mMenus (mOSMenuBar,tb)
where
	dispose :: !(MenuStateHandle .pst) !(!OSMenuBar,!*OSToolbox) -> (!OSMenuBar,!*OSToolbox)
	dispose mH (menuBar,tb)
		= OSMenuRemove (fst (menuStateHandleGetHandle mH)) menuBar tb
