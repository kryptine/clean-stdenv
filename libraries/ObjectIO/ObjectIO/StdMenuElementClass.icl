implementation module StdMenuElementClass


//	Clean Object I/O library, version 1.2

//	Definition of the MenuElements class for menu elements.


import	StdBool, StdList, StdMisc, StdTuple
import	StdMenuDef, StdPSt
import	commondef, menudefaccess, menuhandle
import	osmenu


class MenuElements m where
	menuElementToHandles	:: !(m .ls (PSt .l .p)) !(PSt .l .p)-> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
	getMenuElementType		::  (m .ls .pst)					-> MenuElementType


/*	Translating menu elements into the internal representation.
	Fields which values can not be determined now are filled with dummy values.
	These are the following:
	-	SubMenuHandle
		-	mSubHandle			(the handle to the sub menu)
		-	mSubOSMenuNr		(the internal system id of the sub menu)
	-	MenuItemHandle
		-	mOSMenuItem			(the handle to the item)
	-	MenuSeparatorHandle
		-	mOSMenuSeparator	(the handle to the item)
	The remaining attributes are copied.
*/
instance MenuElements (AddLS m)	| MenuElements m where
	menuElementToHandles :: !(AddLS m .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p) | MenuElements m
	menuElementToHandles {addLS,addDef} pState
		# (ms,pState)	= menuElementToHandles addDef pState
		= (	[MenuElementHandleToMenuElementState 
				(MenuExtendLSHandle {	mExtendLS	= addLS
									,	mExtendItems= map MenuElementStateToMenuElementHandle ms
									}
				)
			]
		  ,	pState
		  )
	getMenuElementType _
		= ""

instance MenuElements (NewLS m)	| MenuElements m where
	menuElementToHandles :: !(NewLS m .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p) | MenuElements m
	menuElementToHandles {newLS,newDef} pState
		# (ms,pState)	= menuElementToHandles newDef pState
		= (	[MenuElementHandleToMenuElementState
				(MenuChangeLSHandle {	mChangeLS	= newLS
									,	mChangeItems= map MenuElementStateToMenuElementHandle ms
									}
				)
			]
		  ,	pState
		  )
	getMenuElementType _
		= ""

instance MenuElements (ListLS m)	| MenuElements m where
	menuElementToHandles :: !(ListLS m .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p) | MenuElements m
	menuElementToHandles (ListLS mDefs) pState
		# (mss,pState)	= StateMap menuElementToHandles mDefs pState
		= (	[MenuElementHandleToMenuElementState
			  (MenuListLSHandle (map MenuElementStateToMenuElementHandle (flatten mss)))
			]
		  ,	pState
		  )
	getMenuElementType _
		= ""

instance MenuElements NilLS where
	menuElementToHandles :: !(NilLS .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
	menuElementToHandles NilLS pState
		= ([MenuElementHandleToMenuElementState (MenuListLSHandle [])],pState)
	getMenuElementType _
		= ""

instance MenuElements ((:+:) m1 m2)	| MenuElements m1 & MenuElements m2 where
	menuElementToHandles :: !((:+:) m1 m2 .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
						 | MenuElements m1 & MenuElements m2
	menuElementToHandles (m1:+:m2) pState
		# (ms1,pState)	= menuElementToHandles m1 pState
		# (ms2,pState)	= menuElementToHandles m2 pState
		= (ms1 ++ ms2,pState)
	getMenuElementType _
		= ""

instance MenuElements (SubMenu m)	| MenuElements m where
	menuElementToHandles :: !(SubMenu m .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p) | MenuElements m
	menuElementToHandles (SubMenu title items atts) pState
		# (ms,pState)		= menuElementToHandles items pState
		  (selectAtt,atts)	= validateSelectState atts
		  (idAtt,    atts)	= validateId          atts
		= (	[MenuElementHandleToMenuElementState
			  (SubMenuHandle {	mSubHandle	= OSNoMenu
							 ,	mSubMenuId	= idAtt
							 ,	mSubOSMenuNr= 0
							 ,	mSubItems	= map MenuElementStateToMenuElementHandle ms
							 ,	mSubTitle	= title
							 ,	mSubSelect	= enabled selectAtt
							 ,	mSubAtts	= atts
							 }
			  )
			]
		  ,	pState
		  )
	getMenuElementType _
		= "SubMenu"

instance MenuElements RadioMenu where
	menuElementToHandles :: !(RadioMenu .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
	menuElementToHandles (RadioMenu items index atts) pState
		# nrRadios			= length items
		  validIndex		= if (nrRadios==0) 0 (SetBetween index 1 nrRadios)
		  itemHs			= validateRadioMenuIndex validIndex (map RadioMenuItemToMenuElementHandle items)
		  (selectAtt,atts)	= validateSelectState atts
		  (idAtt,    atts)	= validateId          atts
		= (	[MenuElementHandleToMenuElementState
			  (RadioMenuHandle {	mRadioId	= idAtt
							   ,	mRadioIndex	= validIndex
							   ,	mRadioItems	= itemHs
							   ,	mRadioSelect= enabled selectAtt
							   ,	mRadioAtts	= atts
							   }
			  )
			]
		  ,	pState
		  )
	getMenuElementType _
		= "RadioMenu"

instance MenuElements MenuItem where
	menuElementToHandles :: !(MenuItem .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
	menuElementToHandles (MenuItem title atts) pState
		# (selectAtt,atts)	= validateSelectState atts
		  (markAtt,  atts)	= validateMarkState   atts
		  (keyAtt,   atts)	= validateShortKey    atts
		  (idAtt,    atts)	= validateId          atts
		= (	[MenuElementHandleToMenuElementState
			  (MenuItemHandle {	mItemId		= idAtt
							  ,	mItemKey	= keyAtt
							  ,	mItemTitle	= title
							  ,	mItemSelect	= enabled selectAtt
							  ,	mItemMark	= marked  markAtt
							  ,	mItemAtts	= atts
							  ,	mOSMenuItem	= OSNoMenuItem
							  }
			  )
			]
		  ,	pState
		  )
	getMenuElementType _
		= "MenuItem"

instance MenuElements MenuSeparator where
	menuElementToHandles :: !(MenuSeparator .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
	menuElementToHandles (MenuSeparator atts) pState
		# (idAtt,_)		= validateId atts
		= (	[MenuElementHandleToMenuElementState 
			  (MenuSeparatorHandle { mSepId			 = idAtt
								   , mOSMenuSeparator= OSNoMenuSeparator
								   }
			  )
			]
		  ,	pState
		  )
	getMenuElementType _
		= "MenuSeparator"


//	Obtain the SelectState attribute from the attribute list:
validateSelectState :: ![MenuAttribute .ps] -> (!SelectState,![MenuAttribute .ps])
validateSelectState atts
	# (found,selectAtt,atts)= Remove isMenuSelectState undef atts
	| found					= (getMenuSelectStateAtt selectAtt,atts)
	| otherwise				= (Able,atts)

//	Obtain the MarkState attribute from the attribute list:
validateMarkState :: ![MenuAttribute .ps] -> (!MarkState,![MenuAttribute .ps])
validateMarkState atts
	# (found,markAtt,atts)	= Remove isMenuMarkState undef atts
	| found					= (getMenuMarkStateAtt markAtt,atts)
	| otherwise				= (NoMark,atts)

//	Obtain the Id attribute from the attribute list:
validateId :: ![MenuAttribute .ps] -> (!Maybe Id,![MenuAttribute .ps])
validateId atts
	# (found,idAtt,atts)	= Remove isMenuId undef atts
	| found					= (Just (getMenuIdAtt idAtt),atts)
	| otherwise				= (Nothing,atts)

//	Obtain the ShortKey attribute from the attribute list:
validateShortKey :: ![MenuAttribute .ps] -> (!Maybe Char,![MenuAttribute .ps])
validateShortKey atts
	# (hasKey,keyAtt,atts)	= Remove isMenuShortKey undef atts
	| hasKey				= (Just (getMenuShortKeyAtt keyAtt),atts)
	| otherwise				= (Nothing,atts)

//	validateRadioMenuIndex ensures that only the element at the valid index position of the RadioMenu
//	has a check mark and all others don't.
validateRadioMenuIndex :: !Int ![MenuElementHandle .ls .ps] -> [MenuElementHandle .ls .ps]
validateRadioMenuIndex index itemHs
	= fst (StateMap (\(MenuItemHandle itemH) i->(MenuItemHandle {itemH & mItemMark=i==index},i+1)) itemHs 1)


/*	Menu elements for PopUpMenus:
*/
class PopUpMenuElements m where
	popUpMenuElementToHandles	:: !(m .ls (PSt .l .p)) !(PSt .l .p)
				-> (![MenuElementState .ls (PSt .l .p)], !PSt .l .p)
	getPopUpMenuElementType		::  (m .ls .pst)
				-> MenuElementType

instance PopUpMenuElements (AddLS m) | PopUpMenuElements m where
	popUpMenuElementToHandles :: !(AddLS m .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p) | PopUpMenuElements m
	popUpMenuElementToHandles {addLS,addDef} pState
		# (ms,pState)	= popUpMenuElementToHandles addDef pState
		= (	[MenuElementHandleToMenuElementState 
				(MenuExtendLSHandle {	mExtendLS	= addLS
									,	mExtendItems= map MenuElementStateToMenuElementHandle ms
									}
				)
			]
		  ,	pState
		  )
	getPopUpMenuElementType _
		= ""

instance PopUpMenuElements (NewLS m) | PopUpMenuElements m where
	popUpMenuElementToHandles :: !(NewLS m .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p) | PopUpMenuElements m
	popUpMenuElementToHandles {newLS,newDef} pState
		# (ms,pState)	= popUpMenuElementToHandles newDef pState
		= (	[MenuElementHandleToMenuElementState
				(MenuChangeLSHandle {	mChangeLS	= newLS
									,	mChangeItems= map MenuElementStateToMenuElementHandle ms
									}
				)
			]
		  ,	pState
		  )
	getPopUpMenuElementType _
		= ""

instance PopUpMenuElements (ListLS m) | PopUpMenuElements m where
	popUpMenuElementToHandles :: !(ListLS m .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p) | PopUpMenuElements m
	popUpMenuElementToHandles (ListLS mDefs) pState
		# (mss,pState)	= StateMap popUpMenuElementToHandles mDefs pState
		= (	[MenuElementHandleToMenuElementState
			  (MenuListLSHandle (map MenuElementStateToMenuElementHandle (flatten mss)))
			]
		  ,	pState
		  )
	getPopUpMenuElementType _
		= ""

instance PopUpMenuElements NilLS where
	popUpMenuElementToHandles :: !(NilLS .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
	popUpMenuElementToHandles NilLS pState
		= ([MenuElementHandleToMenuElementState (MenuListLSHandle [])],pState)
	getPopUpMenuElementType _
		= ""

instance PopUpMenuElements ((:+:) m1 m2) | PopUpMenuElements m1 & PopUpMenuElements m2 where
	popUpMenuElementToHandles :: !((:+:) m1 m2 .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
							  |  PopUpMenuElements m1 & PopUpMenuElements m2
	popUpMenuElementToHandles (m1:+:m2) pState
		# (ms1,pState)	= popUpMenuElementToHandles m1 pState
		# (ms2,pState)	= popUpMenuElementToHandles m2 pState
		= (ms1 ++ ms2,pState)
	getPopUpMenuElementType _
		= ""

instance PopUpMenuElements RadioMenu where
	popUpMenuElementToHandles :: !(RadioMenu .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
	popUpMenuElementToHandles  radioMenu pState
		= menuElementToHandles radioMenu pState
	getPopUpMenuElementType    radioMenu
		= getMenuElementType   radioMenu

instance PopUpMenuElements MenuItem where
	popUpMenuElementToHandles :: !(MenuItem .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
	popUpMenuElementToHandles  menuItem pState
		= menuElementToHandles menuItem pState
	getPopUpMenuElementType    menuItem
		= getMenuElementType   menuItem

instance PopUpMenuElements MenuSeparator where
	popUpMenuElementToHandles :: !(MenuSeparator .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
	popUpMenuElementToHandles  menuSeparator pState
		= menuElementToHandles menuSeparator pState
	getPopUpMenuElementType    menuSeparator
		= getMenuElementType   menuSeparator
