implementation module menuinternal


//	Clean Object I/O library, version 1.2


import	StdBool, StdList, StdMisc
import	osmenu
from	ostypes		import OSNoWindowPtr
import	iostate, menuaccess, menuitems, sdisize
from	commondef	import FatalError, StateMap, RemoveCheck, URemove, UCond, HdTl
from	menucreate	import disposeMenuIds, disposeShortcutkeys, disposeSubMenuHandles


menuinternalFatalError :: String String -> .x
menuinternalFatalError function error
	= FatalError function "menuinternal" error


//	General rules to access MenuHandles:

changeMenuSystemState :: !Bool 
						 !(OSMenuBar -> (MenuHandles (PSt .l)) -> *(*OSToolbox -> *(MenuHandles (PSt .l),*OSToolbox)))
						 !(IOSt .l)
						-> IOSt .l
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

accessMenuSystemState :: !Bool
						 !(OSMenuBar -> u:x -> u:((MenuHandles (PSt .l)) -> u:(*OSToolbox -> *(u:x,MenuHandles (PSt .l),*OSToolbox))))
						 u:x
						 !(IOSt .l)
				  -> *(u:x,!IOSt .l)
accessMenuSystemState redrawMenus f x ioState
	# (found,mDevice,ioState)	= IOStGetDevice MenuDevice ioState
	| not found
		= (x,ioState)
	# (osdinfo,ioState)			= IOStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdinfo
	| isNothing maybeOSMenuBar	// This condition should never hold
		= menuinternalFatalError "accessMenuSystemState" "could not retrieve OSMenuBar from OSDInfo"
	# osMenuBar					= fromJust maybeOSMenuBar
	# (tb,ioState)				= getIOToolbox ioState
	  menus						= MenuSystemStateGetMenuHandles mDevice
	# (x,menus,tb)				= f osMenuBar x menus tb
	| not redrawMenus
		# ioState				= setIOToolbox tb ioState
		= (x,IOStSetDevice (MenuSystemState menus) ioState)
	| otherwise
		# ioState				= setIOToolbox (DrawMenuBar osMenuBar tb) ioState
		= (x,IOStSetDevice (MenuSystemState menus) ioState)


/*	Closing a menu.
	Because in a SDI process menus might reside in the process window, the ViewFrame of the
	process window can change size.
	In that case, the layout of the controls should be recalculated, and the window updated.
*/
closemenu :: !Id !(IOSt .l) -> IOSt .l
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
	  (menus,mHs)				= menuHandlesGetMenus mHs
	  (found,mH,menus)			= URemove (isMenuWithThisId id) undef menus
	| not found
		= IOStSetDevice (MenuSystemState {mHs & mMenus=menus}) ioState
	# (menu,mH)					= menuStateHandleGetHandle mH
	  (keys,mHs)				= menuHandlesGetKeys mHs
	# (sdiSize1,sdiPtr,ioState)	= getSDIWindowSize ioState
	# (tb,ioState)				= getIOToolbox ioState
	# (mH,(keys,tb))			= filterShortcutkeys osdInfo mH keys tb
	# (rt,ioState)				= IOStGetReceiverTable ioState
	# (it,ioState)				= IOStGetIdTable ioState
	  (_,it)					= removeIdFromIdTable id it
	# (ioid,ioState)			= IOStGetIOId ioState
	  (mH,(rt,it))				= closeMenuIds ioid mH (rt,it)
	# ioState					= IOStSetIdTable it ioState
	# ioState					= IOStSetReceiverTable rt ioState
	# (_,tb)					= closeSubMenus mH tb
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
	isMenuWithThisId :: !Id !(MenuStateHandle .pst) -> *(!Bool,!MenuStateHandle .pst)
	isMenuWithThisId id msH
		# (menuId,msH)	= menuStateHandleGetMenuId msH
		= (id==menuId,msH)


closeSubMenus :: !(MenuStateHandle .pst) !*OSToolbox -> (!MenuStateHandle .pst,!*OSToolbox)
closeSubMenus (MenuLSHandle mH=:{mlsHandle=mlsH=:{mItems=itemHs}}) tb
	# (itemHs,tb)	= StateMap disposeSubMenuHandles itemHs tb
	= (MenuLSHandle {mH & mlsHandle={mlsH & mItems=itemHs}},tb)

closeMenuIds :: !SystemId !(MenuStateHandle .pst) !*(!*ReceiverTable,!*IdTable) -> (!MenuStateHandle .pst,!*(!*ReceiverTable,!*IdTable))
closeMenuIds pid (MenuLSHandle mH=:{mlsHandle=mlsH=:{mItems=itemHs}}) (rt,it)
	# (itemHs,ts)	= StateMap (disposeMenuIds pid) itemHs (rt,it)
	= (MenuLSHandle {mH & mlsHandle={mlsH & mItems=itemHs}},ts)

filterShortcutkeys :: !OSDInfo !(MenuStateHandle .pst) ![Char] !*OSToolbox -> (!MenuStateHandle .pst,!(![Char],!*OSToolbox))
filterShortcutkeys osdInfo (MenuLSHandle mH=:{mlsHandle=mlsH=:{mItems=itemHs}}) keys tb
	# (itemHs,keys_tb)	= StateMap (disposeShortcutkeys framePtr) itemHs (keys,tb)
	= (MenuLSHandle {mH & mlsHandle={mlsH & mItems=itemHs}},keys_tb)
where
	framePtr			= case (getOSDInfoOSInfo osdInfo) of
							Just info -> info.osFrame
							_         -> OSNoWindowPtr


//	Enabling and Disabling of Menus:

enablemenus :: ![Id] !(IOSt .l) -> IOSt .l
enablemenus ids ioState
	= changeMenuSystemState True (setSelectMenus ids Able) ioState

disablemenus :: ![Id] !(IOSt .l) -> IOSt .l
disablemenus ids ioState
	= changeMenuSystemState True (setSelectMenus ids Unable) ioState

setSelectMenus :: ![Id] !SelectState !OSMenuBar !(MenuHandles .pst) !*OSToolbox -> (!MenuHandles .pst,!*OSToolbox)
setSelectMenus ids select osMenuBar menus=:{mEnabled,mMenus} tb
	# (_,msHs,tb)	= setSelectMenuHandles 0 select osMenuBar mEnabled ids mMenus tb
	= ({menus & mMenus=msHs},tb)
where	
	setSelectMenuHandles :: !Int !SelectState !OSMenuBar !Bool ![Id] !*[MenuStateHandle .ps] !*OSToolbox
														   -> (![Id],!*[MenuStateHandle .ps],!*OSToolbox)
	setSelectMenuHandles _ _ _ _ ids [] tb
		= (ids,[],tb)
	setSelectMenuHandles zIndex select osMenuBar systemAble ids [msH:msHs] tb
		| isEmpty ids
			= (ids,[msH:msHs],tb)
		| otherwise
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

closemenuelements :: !Id ![Id] !(IOSt .l) -> IOSt .l
closemenuelements mId ids ioState
	# (pid,ioState)		= IOStGetIOId ioState
	# (rt,ioState)		= IOStGetReceiverTable ioState
	# (it,ioState)		= IOStGetIdTable ioState
	# (osdInfo,ioState)	= IOStGetOSDInfo ioState
	# ((rt,it),ioState)	= accessMenuSystemState True (removeMenusItems osdInfo mId ids pid) (rt,it) ioState
	# ioState			= IOStSetIdTable it ioState
	# ioState			= IOStSetReceiverTable rt ioState
	= ioState


//	Removing menu elements from (sub/radio)menus by index (counting from 1):

RemoveSpecialMenuElements	:==	True		// For closemenuindexelements:        remove elements with special ids
NotRemoveSpecialMenuElements:==	False		// For closemenuindexelements: do not remove elements with special ids

closemenuindexelements :: !Bool !Bool !SystemId !(!Id,!Maybe Id) ![Index] !(IOSt .l) -> IOSt .l
closemenuindexelements removeSpecialElements fromRadioMenu pid loc indices ioState
	# (rt,ioState)		= IOStGetReceiverTable ioState
	# (it,ioState)		= IOStGetIdTable ioState
	# (osdInfo,ioState)	= IOStGetOSDInfo ioState
	# ((rt,it),ioState)	= accessMenuSystemState True (removeMenusIndexItems osdInfo removeSpecialElements fromRadioMenu loc indices pid) (rt,it) ioState
	# ioState			= IOStSetIdTable it ioState
	# ioState			= IOStSetReceiverTable rt ioState
	= ioState


//	Set & Get the title of a menu.

setmenutitle :: !Id !Title !(IOSt .l) -> IOSt .l
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
		setOSMenusTitle _ _ _ [] tb
			= ([],tb)
