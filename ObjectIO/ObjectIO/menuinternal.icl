implementation module menuinternal


//	Clean Object I/O library, version 1.2


import	StdBool, StdList, StdMisc
import	osmenu
from	menuevent	import MenuSystemStateGetMenuHandles, MenuHandlesGetMenuStateHandles
from	oswindow	import OSNoWindowPtr
import	iostate, menuaccess, menuitems, sdisize
from	commondef	import FatalError, StateMap2, RemoveCheck, URemove, UCond, HdTl
from	menucreate	import disposeMenuIds, disposeShortcutkeys, disposeSubMenuHandles


::	DeltaMenuSystem l p
	:==	!OSMenuBar -> !(MenuHandles (PSt l p)) -> !*OSToolbox -> (!MenuHandles (PSt l p),!*OSToolbox)
::	AccessMenuSystem x pst
	:==	!OSMenuBar -> !(MenuHandles pst) -> !*OSToolbox -> (!x,!MenuHandles pst,!*OSToolbox)
::	DeltaMenuHandles pst
	:==	[MenuStateHandle pst] -> *OSToolbox -> ([MenuStateHandle pst],*OSToolbox)
::	DeltaMenuHandle pst
	:==	(MenuStateHandle pst) -> *OSToolbox -> ( MenuStateHandle pst, *OSToolbox)


menuinternalFatalError :: String String -> .x
menuinternalFatalError function error
	= FatalError function "menuinternal" error


//	General rules to access MenuHandles:

changeMenuSystemState :: !Bool !(DeltaMenuSystem .l .p) !(IOSt .l .p) -> IOSt .l .p
changeMenuSystemState redrawMenus f ioState
	# (found,mDevice,ioState)	= IOStGetDevice MenuDevice ioState
	| not found
		= ioState
	# (osdinfo,ioState)			= IOStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdinfo
	| isNothing maybeOSMenuBar	// This condition should never hold
		= menuinternalFatalError "changeMenuSystemState" "could not retrieve OSMenuBar from OSDInfo"
	# osMenuBar					= fromJust maybeOSMenuBar
	# (tb,ioState)				= getIOToolbox ioState
	  menus						= MenuSystemStateGetMenuHandles mDevice
	# (menus,tb)				= f osMenuBar menus tb
	| not redrawMenus
		# ioState				= setIOToolbox tb ioState
		= IOStSetDevice (MenuSystemState menus) ioState
	| otherwise
		# ioState				= setIOToolbox (DrawMenuBar osMenuBar tb) ioState
		= IOStSetDevice (MenuSystemState menus) ioState

accessMenuSystemState :: !Bool !(AccessMenuSystem x (PSt .l .p)) !(IOSt .l .p) -> (!Maybe x, !IOSt .l .p)
accessMenuSystemState redrawMenus f ioState
	# (found,mDevice,ioState)	= IOStGetDevice MenuDevice ioState
	| not found
		= (Nothing,ioState)
	# (osdinfo,ioState)			= IOStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdinfo
	| isNothing maybeOSMenuBar	// This condition should never hold
		= menuinternalFatalError "accessMenuSystemState" "could not retrieve OSMenuBar from OSDInfo"
	# osMenuBar					= fromJust maybeOSMenuBar
	# (tb,ioState)				= getIOToolbox ioState
	  menus						= MenuSystemStateGetMenuHandles mDevice
	# (x,menus,tb)				= f osMenuBar menus tb
	| not redrawMenus
		# ioState				= setIOToolbox tb ioState
		= (Just x,IOStSetDevice (MenuSystemState menus) ioState)
	| otherwise
		# ioState				= setIOToolbox (DrawMenuBar osMenuBar tb) ioState
		= (Just x,IOStSetDevice (MenuSystemState menus) ioState)


/*	Closing a menu.
	Because in a SDI process menus might reside in the process window, the ViewFrame of the
	process window can change size.
	In that case, the layout of the controls should be recalculated, and the window updated.
*/
closemenu :: !Id !(IOSt .l .p) -> IOSt .l .p
closemenu id ioState
	# (osdInfo,ioState)			= IOStGetOSDInfo ioState
	| getOSDInfoDocumentInterface osdInfo==NDI
		= ioState
	# maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar
		= menuinternalFatalError "closemenu" "could not retrieve OSMenuBar from OSDInfo"
	# (found,mDevice,ioState)	= IOStGetDevice MenuDevice ioState
	| not found
		= ioState
	# mHs						= MenuSystemStateGetMenuHandles mDevice
	  (menus,mHs)				= MenuHandlesGetMenuStateHandles mHs
	  (found,mH,menus)			= URemove (isMenuWithThisId id) undef menus
	| not found
		= IOStSetDevice (MenuSystemState {mHs & mMenus=menus}) ioState
	# (menu,mH)					= menuStateHandleGetHandle mH
	  (keys,mHs)				= (\mHs=:{mKeys}->(mKeys,mHs)) mHs
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
	  osMenuBar					= fromJust maybeOSMenuBar
	# (osMenuBar,tb)			= OSMenuRemove menu osMenuBar tb
	# tb						= DrawMenuBar osMenuBar tb
	  osdInfo					= setOSDInfoOSMenuBar osMenuBar osdInfo
	# ioState					= IOStSetOSDInfo osdInfo ioState
	  mHs						= {mHs & mMenus=menus,mKeys=keys}
	# ioState					= setIOToolbox tb ioState
	# ioState					= IOStSetDevice (MenuSystemState mHs) ioState
	# (sdiSize2,_,ioState)		= getSDIWindowSize ioState
	| sdiSize1==sdiSize2
		= ioState
	| otherwise
		= resizeSDIWindow sdiPtr sdiSize1 sdiSize2 ioState
where
	isMenuWithThisId :: !Id !(MenuStateHandle .pst) -> (!Bool,!MenuStateHandle .pst)
	isMenuWithThisId id msH
		# (menuId,msH)	= menuStateHandleGetMenuId msH
		= (id==menuId,msH)


closeSubMenus :: !(MenuStateHandle .pst) !*OSToolbox -> *OSToolbox
closeSubMenus (MenuLSHandle {mlsHandle={mItems}}) tb
	= StateMap2 disposeSubMenuHandles mItems tb

closeMenuIds :: !SystemId !(MenuStateHandle .pst) !ReceiverTable !IdTable -> (!ReceiverTable,!IdTable)
closeMenuIds pid (MenuLSHandle {mlsHandle={mItems}}) rt it
	= StateMap2 (disposeMenuIds pid) mItems (rt,it)

filterShortcutkeys :: !OSDInfo !(MenuStateHandle .pst) ![Char] !*OSToolbox -> (![Char],!*OSToolbox)
filterShortcutkeys osdInfo (MenuLSHandle {mlsHandle={mItems}}) keys tb
	= StateMap2 (disposeShortcutkeys framePtr) mItems (keys,tb)
where
	framePtr	= case (getOSDInfoOSInfo osdInfo) of
					Just info -> info.osFrame
					_         -> OSNoWindowPtr


//	Enabling and Disabling of Menus:

enablemenus :: ![Id] !(IOSt .l .p) -> IOSt .l .p
enablemenus ids ioState
	= changeMenuSystemState True (setSelectMenus ids Able) ioState

disablemenus :: ![Id] !(IOSt .l .p) -> IOSt .l .p
disablemenus ids ioState
	= changeMenuSystemState True (setSelectMenus ids Unable) ioState

setSelectMenus :: ![Id] !SelectState !OSMenuBar !(MenuHandles .pst) !*OSToolbox -> (!MenuHandles .pst,!*OSToolbox)
setSelectMenus ids select osMenuBar menus=:{mEnabled,mMenus} tb
	# (_,msHs,tb)	= setSelectMenuHandles 0 select osMenuBar mEnabled ids mMenus tb
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
	# (result, ioState)	= accessMenuSystemState True (removeMenusItems osdInfo mId ids pid rt it) ioState
	| isNothing result
		= ioState
	| otherwise
		# (rt,it)		= fromJust result
		# ioState		= IOStSetIdTable it ioState
		# ioState		= IOStSetReceiverTable rt ioState
		= ioState


//	Removing menu elements from (sub/radio)menus by index (counting from 1):

RemoveSpecialMenuElements	:==	True		// For closemenuindexelements:        remove elements with special ids
NotRemoveSpecialMenuElements:==	False		// For closemenuindexelements: do not remove elements with special ids

closemenuindexelements :: !Bool !Bool !SystemId !(!Id,!Maybe Id) ![Index] !(IOSt .l .p) -> IOSt .l .p
closemenuindexelements removeSpecialElements fromRadioMenu pid loc indices ioState
	# (rt,ioState)		= IOStGetReceiverTable ioState
	# (it,ioState)		= IOStGetIdTable ioState
	# (osdInfo,ioState)	= IOStGetOSDInfo ioState
	# (result, ioState)	= accessMenuSystemState True (removeMenusIndexItems osdInfo removeSpecialElements fromRadioMenu loc indices pid rt it) ioState
	| isNothing result
		= ioState
	| otherwise
		# (rt,it)		= fromJust result
		# ioState		= IOStSetIdTable it ioState
		# ioState		= IOStSetReceiverTable rt ioState
		= ioState


//	Set & Get the title of a menu.

setmenutitle :: !Id !Title !(IOSt .l .p) -> IOSt .l .p
setmenutitle id title ioState
	= changeMenuSystemState True (setOSMenuTitle id title) ioState
where
	setOSMenuTitle :: !Id !Title !OSMenuBar !(MenuHandles .pst) !*OSToolbox -> (!MenuHandles .pst,!*OSToolbox)
	setOSMenuTitle id title osMenuBar menus=:{mMenus} tb
		# (msHs,tb)	= setOSMenusTitle id title osMenuBar mMenus tb
		= ({menus & mMenus=msHs},tb)
	where
		setOSMenusTitle :: !Id !Title !OSMenuBar ![MenuStateHandle .pst] !*OSToolbox -> (![MenuStateHandle .pst],!*OSToolbox)
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
