implementation module menuinternal


//	Clean Object I/O library, version 1.2


import	StdBool, StdList, StdMisc
import	osmenu
from	menuevent	import MenuSystemStateGetMenuHandles, MenuHandlesGetMenuStateHandles
from	oswindow	import OSNoWindowPtr
import	iostate, menuaccess, menuitems, sdisize
from	commondef	import StateMap2, RemoveCheck, URemove, UCond, HdTl
from	menucreate	import disposeMenuIds, disposeShortcutkeys, disposeSubMenuHandles


::	DeltaMenuSystem l p
	:==	(MenuHandles (PSt l p)) -> *OSToolbox -> (MenuHandles (PSt l p),*OSToolbox)
::	AccessMenuSystem x ps
	:==	(MenuHandles ps) -> *OSToolbox -> (x,MenuHandles ps,*OSToolbox)
::	DeltaMenuHandles ps
	:==	[MenuStateHandle ps] -> *OSToolbox -> ([MenuStateHandle ps],*OSToolbox)
::	DeltaMenuHandle ps
	:==	(MenuStateHandle ps) -> *OSToolbox -> ( MenuStateHandle ps, *OSToolbox)


//	General rules to access MenuHandles:

changeMenuSystemState :: !Bool !(DeltaMenuSystem .l .p) !(IOSt .l .p) -> IOSt .l .p
changeMenuSystemState redrawMenus f ioState
	# (mDevice,ioState)		= IOStGetDevice MenuDevice ioState
	# (tb,ioState)			= getIOToolbox ioState
	  menus					= MenuSystemStateGetMenuHandles mDevice
	# (menus,tb)			= f menus tb
	| not redrawMenus
		# ioState			= setIOToolbox tb ioState
		= IOStSetDevice (MenuSystemState menus) ioState
	| otherwise
		# ioState			= setIOToolbox (DrawMenuBar menus.mOSMenuBar tb) ioState
		= IOStSetDevice (MenuSystemState menus) ioState

accessMenuSystemState :: !Bool !(AccessMenuSystem x (PSt .l .p)) !(IOSt .l .p) -> (!x, !IOSt .l .p)
accessMenuSystemState redrawMenus f ioState
	# (mDevice,ioState)		= IOStGetDevice MenuDevice ioState
	# (tb,ioState)			= getIOToolbox ioState
	  menus					= MenuSystemStateGetMenuHandles mDevice
	# (x,menus,tb)			= f menus tb
	| not redrawMenus
		# ioState			= setIOToolbox tb ioState
		= (x,IOStSetDevice (MenuSystemState menus) ioState)
	| otherwise
		# ioState			= setIOToolbox (DrawMenuBar menus.mOSMenuBar tb) ioState
		= (x,IOStSetDevice (MenuSystemState menus) ioState)


/*	Closing a menu.
	Because in a SDI process menus might reside in the process window, the ViewFrame of the
	process window can change size.
	In that case, the layout of the controls should be recalculated, and the window updated.
*/
closemenu :: !Id !(IOSt .l .p) -> IOSt .l .p
closemenu id ioState
	# (mDevice,ioState)			= IOStGetDevice MenuDevice ioState
	  mHs						= MenuSystemStateGetMenuHandles mDevice
	  (menus,mHs)				= MenuHandlesGetMenuStateHandles mHs
	  (found,mH,menus)			= URemove (isMenuWithThisId id) undef menus
	| not found
		= IOStSetDevice (MenuSystemState {mHs & mMenus=menus}) ioState
	# (menu,mH)					= menuStateHandleGetHandle mH
	  (keys,osMenuBar,mHs)		= (\mHs=:{mKeys,mOSMenuBar}->(mKeys,mOSMenuBar,mHs)) mHs
	# (osdInfo,ioState)			= IOStGetOSDInfo ioState
	# (sdiSize1,sdiPtr,ioState)	= getSDIWindowSize ioState
	# (tb,ioState)				= getIOToolbox ioState
	# (keys,tb)					= filterShortcutkeys osdInfo mH keys tb
	# (rt,ioState)				= IOStGetReceiverTable ioState
	# (it,ioState)				= IOStGetIdTable ioState
	  (_,it)					= removeIdFromIdTable id it
	# (ioid,ioState)			= IOStGetIOId ioState
	  (rt,it)					= closeMenuIds ioid mH rt it
	# ioState					= IOStSetIdTable it ioState
	# ioState					= IOStSetReceiverTable rt ioState
	# tb						= closeSubMenus mH tb
	# (osMenuBar,tb)			= OSMenuRemove menu osMenuBar tb
	# tb						= DrawMenuBar osMenuBar tb
	  mHs						= {mHs & mMenus=menus,mKeys=keys,mOSMenuBar=osMenuBar}
	# ioState					= setIOToolbox tb ioState
	# ioState					= IOStSetDevice (MenuSystemState mHs) ioState
	# (sdiSize2,_,ioState)		= getSDIWindowSize ioState
	| sdiSize1==sdiSize2
		= ioState
	| otherwise
		= resizeSDIWindow sdiPtr sdiSize1 sdiSize2 ioState
where
	isMenuWithThisId :: !Id !(MenuStateHandle .ps) -> (!Bool,!MenuStateHandle .ps)
	isMenuWithThisId id msH
		# (menuId,msH)	= menuStateHandleGetMenuId msH
		= (id==menuId,msH)


closeSubMenus :: !(MenuStateHandle .ps) !*OSToolbox -> *OSToolbox
closeSubMenus (MenuLSHandle {mlsHandle={mItems}}) tb
	= StateMap2 disposeSubMenuHandles mItems tb

closeMenuIds :: !SystemId !(MenuStateHandle .ps) !ReceiverTable !IdTable -> (!ReceiverTable,!IdTable)
closeMenuIds pid (MenuLSHandle {mlsHandle={mItems}}) rt it
	= StateMap2 (disposeMenuIds pid) mItems (rt,it)

filterShortcutkeys :: !OSDInfo !(MenuStateHandle .ps) ![Char] !*OSToolbox -> (![Char],!*OSToolbox)
filterShortcutkeys osdInfo (MenuLSHandle {mlsHandle={mItems}}) keys tb
	= StateMap2 (disposeShortcutkeys framePtr) mItems (keys,tb)
where
	framePtr	= case osdInfo of
					OSMDInfo info	-> info.osmdFrame
					OSSDInfo info	-> info.ossdFrame
					_				-> OSNoWindowPtr


//	Enabling and Disabling of Menus:

enablemenus :: ![Id] !(IOSt .l .p) -> IOSt .l .p
enablemenus ids ioState
	= changeMenuSystemState True (setSelectMenus ids Able) ioState

disablemenus :: ![Id] !(IOSt .l .p) -> IOSt .l .p
disablemenus ids ioState
	= changeMenuSystemState True (setSelectMenus ids Unable) ioState

setSelectMenus :: ![Id] !SelectState !(MenuHandles .ps) !*OSToolbox -> (!MenuHandles .ps,!*OSToolbox)
setSelectMenus ids select menus=:{mOSMenuBar,mEnabled,mMenus} tb
	# (_,msHs,tb)	= setSelectMenuHandles 0 select mOSMenuBar mEnabled ids mMenus tb
	= ({menus & mMenus=msHs},tb)
where	
	setSelectMenuHandles :: !Int !SelectState !OSMenuBar !Bool ![Id] ![MenuStateHandle .ps] !*OSToolbox
														   -> (![Id],![MenuStateHandle .ps],!*OSToolbox)
	setSelectMenuHandles zIndex select osMenuBar systemAble ids msHs tb
		| isEmpty ids || isEmpty msHs
			= (ids,msHs,tb)
		| otherwise
			# (msH,msHs)	= HdTl msHs
			# (ids,msH, tb)	= setSelectMenuHandle  zIndex     select osMenuBar systemAble ids msH  tb
			# (ids,msHs,tb)	= setSelectMenuHandles (zIndex+1) select osMenuBar systemAble ids msHs tb
			= (ids,[msH:msHs],tb)
	where
		setSelectMenuHandle :: !Int !SelectState !OSMenuBar !Bool ![Id] !(MenuStateHandle .ps) !*OSToolbox
															  -> (![Id], !MenuStateHandle .ps, !*OSToolbox)
		setSelectMenuHandle zIndex select osMenuBar systemAble ids msH=:(MenuLSHandle mlsH=:{mlsHandle=mH=:{mMenuId}}) tb
			# (containsId,ids)	= RemoveCheck mMenuId ids
			| not containsId	= (ids,msH,tb)
			# msH				= MenuLSHandle {mlsH & mlsHandle={mH & mSelect=enabled select}}
			| not systemAble	= (ids,msH,tb)
			| enabled select	= (ids,msH,OSEnableMenu  zIndex osMenuBar tb)
			| otherwise			= (ids,msH,OSDisableMenu zIndex osMenuBar tb)


//	Removing menu elements from (sub/radio)menus:

closemenuelements :: !Id ![Id] !(IOSt .l .p) -> IOSt .l .p
closemenuelements mId ids ioState
	# (pid,ioState)		= IOStGetIOId ioState
	# (rt,ioState)		= IOStGetReceiverTable ioState
	# (it,ioState)		= IOStGetIdTable ioState
	# (osdInfo,ioState)	= IOStGetOSDInfo ioState
	# ((rt,it),ioState)	= accessMenuSystemState True (removeMenusItems osdInfo mId ids pid rt it) ioState
	# ioState			= IOStSetIdTable it ioState
	# ioState			= IOStSetReceiverTable rt ioState
	= ioState


//	Removing menu elements from (sub/radio)menus by index (counting from 1):

RemoveSpecialMenuElements	:==	True		// For closemenuindexelements:        remove elements with special ids
NotRemoveSpecialMenuElements:==	False		// For closemenuindexelements: do not remove elements with special ids

closemenuindexelements :: !Bool !Bool !SystemId !(!Id,!Maybe Id) ![Index] !(IOSt .l .p) -> IOSt .l .p
closemenuindexelements removeSpecialElements fromRadioMenu pid loc indices ioState
	# (rt,ioState)		= IOStGetReceiverTable ioState
	# (it,ioState)		= IOStGetIdTable ioState
	# (osdInfo,ioState)	= IOStGetOSDInfo ioState
	# ((rt,it),ioState)	= accessMenuSystemState True (removeMenusIndexItems osdInfo removeSpecialElements fromRadioMenu loc indices pid rt it) ioState
	# ioState			= IOStSetIdTable it ioState
	# ioState			= IOStSetReceiverTable rt ioState
	= ioState


//	Set & Get the title of a menu.

setmenutitle :: !Id !Title !(IOSt .l .p) -> IOSt .l .p
setmenutitle id title ioState
	= changeMenuSystemState True (setOSMenuTitle id title) ioState
where
	setOSMenuTitle :: !Id !Title !(MenuHandles .ps) !*OSToolbox -> (!MenuHandles .ps,!*OSToolbox)
	setOSMenuTitle id title menus=:{mOSMenuBar,mMenus} tb
		# (msHs,tb)	= setOSMenusTitle id title mOSMenuBar mMenus tb
		= ({menus & mMenus=msHs},tb)
	where
		setOSMenusTitle :: !Id !Title !OSMenuBar ![MenuStateHandle .ps] !*OSToolbox -> (![MenuStateHandle .ps],!*OSToolbox)
		setOSMenusTitle id title osMenuBar [msH:msHs] tb
			# (menuId,msH)	= menuStateHandleGetMenuId msH
			| id==menuId
				# (mH,msH)	= menuStateHandleGetHandle msH
				  msH		= menuStateHandleSetTitle title msH
				# tb		= OSChangeMenuTitle osMenuBar mH title tb
				= ([msH:msHs],tb)
			| otherwise
				# (msHs,tb)	= setOSMenusTitle id title osMenuBar msHs tb
				= ([msH:msHs],tb)
		setOSMenusTitle _ _ _ msHs tb
			= (msHs,tb)
