implementation module menuhandle


//	Clean Object I/O library, version 1.2


import	StdBool, StdList, StdTuple
import	StdMenuDef
import	commondef, receiverhandle, receivertable
import	osmenu


::	MenuElementState		ls pst							// The internal implementation of a menu element
	:==	MenuElementHandle	ls pst							// is a MenuElementHandle

::	MenuHandles pst
	=	{	mMenus			:: ![MenuStateHandle pst]		// The menus and their elements of a process
		,	mKeys			:: ![Char]						// All shortcut keys of the menus
//PA---	,	mOSMenuBar		:: !OSMenuBar					// The handle to the toolbox menu bar
		,	mEnabled		:: !Bool						// Flag: the whole menusystem is enabled
		,	mNrMenuBound	:: !Bound						// The maximum number of menus that are allowed to be opened
		,	mPopUpId		:: !Maybe Id					// The Id of the PopUpMenu (Nothing if open; (Just id) if available)
		}
::	MenuStateHandle pst
	=	E..ls: MenuLSHandle	!(MenuLSHandle ls pst)			// A menu with local state
::	MenuLSHandle ls pst
	=	{	mlsState		:: ls							// The local state of this menu
		,	mlsHandle		:: !MenuHandle ls pst			// The menu implementation
		}
::	MenuHandle ls pst
	=	{	mHandle			:: !OSMenu						// The handle to the menu as created by the OS
		,	mMenuId			:: !Id							// The menu id
		,	mOSMenuNr		:: !OSMenuNr					// The OSMenuNr
		,	mTitle			:: !String						// The title of the menu
		,	mSelect			:: !Bool						// The MenuSelect==Able (by default True)
		,	mItems			:: ![MenuElementHandle ls pst]	// The menu elements of this menu
		}
::	MenuElementHandle ls pst
	=	MenuItemHandle		!(MenuItemHandle      ls pst)
	|	MenuReceiverHandle	!(MenuReceiverHandle  ls pst)
	|	SubMenuHandle		!(SubMenuHandle       ls pst)
	|	RadioMenuHandle		!(RadioMenuHandle     ls pst)
	|	MenuSeparatorHandle	!(MenuSeparatorHandle ls pst)
	|	MenuListLSHandle	![MenuElementHandle   ls pst]
	|	MenuExtendLSHandle	!(MenuExtendLSHandle  ls pst)
	|	MenuChangeLSHandle	!(MenuChangeLSHandle  ls pst)
::	MenuItemHandle ls pst
	=	{	mItemId			:: !Maybe Id
		,	mItemKey		:: !Maybe Char
		,	mItemTitle		:: !Title
		,	mItemSelect		:: !Bool
		,	mItemMark		:: !Bool
		,	mItemAtts		:: ![MenuAttribute *(ls,pst)]
		,	mOSMenuItem		:: !OSMenuItem
		}
::	MenuReceiverHandle ls pst
	=	{	mReceiverHandle	:: !ReceiverHandle ls pst
		,	mReceiverAtts	:: ![MenuAttribute *(ls,pst)]
		}
::	SubMenuHandle ls pst
	=	{	mSubHandle		:: !OSMenu
		,	mSubMenuId		:: !Maybe Id
		,	mSubOSMenuNr	:: !OSSubMenuNr
		,	mSubItems		:: ![MenuElementHandle ls pst]
		,	mSubTitle		:: !Title
		,	mSubSelect		:: !Bool
		,	mSubAtts		:: ![MenuAttribute *(ls,pst)]
		}
::	RadioMenuHandle ls pst
	=	{	mRadioId		:: !Maybe Id
		,	mRadioIndex		:: !Int
		,	mRadioItems		:: ![MenuElementHandle ls pst]
		,	mRadioSelect	:: !Bool
		,	mRadioAtts		:: ![MenuAttribute *(ls,pst)]
		}
::	MenuSeparatorHandle	ls pst
	=	{	mSepId			:: !Maybe Id
		,	mOSMenuSeparator:: !OSMenuSeparator
		}
::	MenuExtendLSHandle	ls pst
	=	E..ls1:
		{	mExtendLS		:: ls1
		,	mExtendItems	:: ![MenuElementHandle *(ls1,ls) pst]
		}
::	MenuChangeLSHandle	ls pst
	=	E..ls1:
		{	mChangeLS		:: ls1
		,	mChangeItems	:: ![MenuElementHandle ls1 pst]
		}


menuhandleFatalError :: String String -> .x
menuhandleFatalError function error
	= FatalError function "menuhandle" error


//	Conversion functions from MenuElementState to MenuElementHandle, and vice versa:
MenuElementHandleToMenuElementState	:: !(MenuElementHandle .ls .pst) -> MenuElementState  .ls .pst
MenuElementHandleToMenuElementState mH = mH

MenuElementStateToMenuElementHandle	:: !(MenuElementState  .ls .pst) -> MenuElementHandle .ls .pst
MenuElementStateToMenuElementHandle mH = mH

/*	menuIdsAreConsistent checks whether the MenuElementHandles contain (R(2))Ids that have already been
	associated with open receivers and if there are no duplicate Ids. 
	Neither the ReceiverTable nor the IdTable are changed if there are duplicate (R(2))Ids; 
	otherwise all (R(2))Ids have been bound.
*/
menuIdsAreConsistent :: !SystemId !Id ![MenuElementHandle .ls .pst] !ReceiverTable !IdTable
							-> (!Bool,![MenuElementHandle .ls .pst],!ReceiverTable,!IdTable)
menuIdsAreConsistent ioId menuId itemHs rt it
	# (itemHs,ids)	= StateMap getMenuElementMenuId itemHs []
	| not (okMembersIdTable ids it)
		= (False,itemHs,rt,it)
	# (ok,it)		= addIdsToIdTable (map (\id->(id,{idpIOId=ioId,idpDevice=MenuDevice,idpId=menuId})) ids) it
	# (itemHs,rt)	= bindReceiverMenuIds ioId menuId itemHs rt
	| not ok
		= menuhandleFatalError "menuIdsAreConsistent" "could not add all Ids to IdTable"
	| otherwise
		= (True,itemHs,rt,it)
where
	getMenuElementMenuId :: !(MenuElementHandle .ls .pst) ![Id] -> (!MenuElementHandle .ls .pst,![Id])
	getMenuElementMenuId itemH=:(MenuItemHandle {mItemId}) ids
		| isNothing mItemId		= (itemH,ids)
		| otherwise				= (itemH,[fromJust mItemId:ids])
	getMenuElementMenuId itemH=:(MenuReceiverHandle _) ids
		= (itemH,ids)
	getMenuElementMenuId (SubMenuHandle itemH=:{mSubMenuId,mSubItems=itemHs}) ids
		# (itemHs,ids)			= StateMap getMenuElementMenuId itemHs ids
		  subH					= SubMenuHandle {itemH & mSubItems=itemHs}
		| isNothing mSubMenuId	= (subH,ids)
		| otherwise				= (subH,[fromJust mSubMenuId:ids])
	getMenuElementMenuId (RadioMenuHandle itemH=:{mRadioId,mRadioItems=itemHs}) ids
		# (itemHs,ids)			= StateMap getMenuElementMenuId itemHs ids
		  radioH				= RadioMenuHandle {itemH & mRadioItems=itemHs}
		| isNothing mRadioId	= (radioH,ids)
		| otherwise				= (radioH,[fromJust mRadioId:ids])
	getMenuElementMenuId itemH=:(MenuSeparatorHandle {mSepId}) ids
		| isNothing mSepId		= (itemH,ids)
		| otherwise				= (itemH,[fromJust mSepId:ids])
	getMenuElementMenuId (MenuListLSHandle itemHs) ids
		# (itemHs,ids)			= StateMap getMenuElementMenuId itemHs ids
		= (MenuListLSHandle itemHs,ids)
	getMenuElementMenuId (MenuExtendLSHandle mExH=:{mExtendItems=itemHs}) ids
		# (itemHs,ids)			= StateMap getMenuElementMenuId itemHs ids
		= (MenuExtendLSHandle {mExH & mExtendItems=itemHs},ids)
	getMenuElementMenuId (MenuChangeLSHandle mChH=:{mChangeItems=itemHs}) ids
		# (itemHs,ids)			= StateMap getMenuElementMenuId itemHs ids
		= (MenuChangeLSHandle {mChH & mChangeItems=itemHs},ids)
	
/*	bindReceiverMenuIds binds all R(2)Ids in the MenuElementState list. 
	It assumes that it has already been checked that no R(2)Id is already bound in the ReceiverTable.
*/
	bindReceiverMenuIds :: !SystemId !Id ![MenuElementState .ls .pst] !ReceiverTable
									 -> (![MenuElementState .ls .pst],!ReceiverTable)
	bindReceiverMenuIds ioId menuId [itemH:itemHs] rt
		# (itemH, rt)	= bindReceiverMenuId` ioId menuId itemH rt
		# (itemHs,rt)	= bindReceiverMenuIds ioId menuId itemHs rt
		= ([itemH:itemHs],rt)
	where
		bindReceiverMenuId` :: !SystemId !Id !(MenuElementHandle .ls .pst) !ReceiverTable
										  -> (!MenuElementHandle .ls .pst, !ReceiverTable)
		bindReceiverMenuId` ioId menuId itemH=:(MenuReceiverHandle {mReceiverHandle={rId,rSelect}}) rt
			= (itemH,snd (addReceiverToReceiverTable rte rt))
		where
			rte	= {	rteLoc			= {	rlIOId		= ioId
									  ,	rlDevice	= MenuDevice
									  ,	rlParentId	= menuId
									  ,	rlReceiverId= rId
									  }
				  ,	rteSelectState	= rSelect
				  ,	rteASMCount		= 0
				  }
		bindReceiverMenuId` ioId menuId (SubMenuHandle itemH=:{mSubItems=itemHs}) rt
			# (itemHs,rt)	= bindReceiverMenuIds ioId menuId itemHs rt
			= (SubMenuHandle {itemH & mSubItems=itemHs},rt)
		bindReceiverMenuId` ioId menuId (MenuListLSHandle itemHs) rt
			# (itemHs,rt)	= bindReceiverMenuIds ioId menuId itemHs rt
			= (MenuListLSHandle itemHs,rt)
		bindReceiverMenuId` ioId menuId (MenuExtendLSHandle	mExH=:{mExtendItems=itemHs}) rt
			# (itemHs,rt)	= bindReceiverMenuIds ioId menuId itemHs rt
			= (MenuExtendLSHandle {mExH & mExtendItems=itemHs},rt)
		bindReceiverMenuId` ioId menuId (MenuChangeLSHandle	mChH=:{mChangeItems=itemHs}) rt
			# (itemHs,rt)	= bindReceiverMenuIds ioId menuId itemHs rt
			= (MenuChangeLSHandle {mChH & mChangeItems=itemHs},rt)
		bindReceiverMenuId` _ _ itemH rt
			= (itemH,rt)
	bindReceiverMenuIds _ _ itemHs rt
		= (itemHs,rt)

//	Convert a RadioMenuItem to the MenuItemHandle alternative of MenuElementHandle:
RadioMenuItemToMenuElementHandle :: !(MenuRadioItem *(.ls,.pst)) -> MenuElementHandle .ls .pst
RadioMenuItemToMenuElementHandle (title,optId,optShortKey,f)
	= MenuItemHandle {	mItemId		= optId
					 ,	mItemKey	= optShortKey
					 ,	mItemTitle	= title
					 ,	mItemSelect	= True
					 ,	mItemMark	= False
					 ,	mItemAtts	= [MenuFunction f]
					 ,	mOSMenuItem	= OSNoMenuItem
					 }
