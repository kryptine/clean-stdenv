implementation module StdMenu


//	Clean Object I/O library, version 1.2


import	StdBool, StdList, StdTuple
import	osmenu
import	commondef, iostate, menucreate, menudevice, menuinternal, menuitems, StdId
from	devicesystemstate	import WindowSystemStateGetWindowHandles
from	menuaccess			import menuStateHandleGetMenuId, menuStateHandleGetSelect, menuStateHandleGetTitle, menuStateHandleGetHandle
from	menudefaccess		import menuDefGetMenuId
from	menuevent			import MenuSystemStateGetMenuHandles, MenuHandlesGetMenuStateHandles
from	StdPSt				import accPIO
from	windowaccess		import getWindowHandlesActiveModalDialog


StdMenuFatalError :: String String -> .x
StdMenuFatalError function error
	= FatalError function "StdMenu" error


::	DeltaMenuSystem l p
	:==	!OSMenuBar -> !(MenuHandles (PSt l)) -> !*OSToolbox -> (!MenuHandles (PSt l),!*OSToolbox)
::	AccessMenuSystem x pst
	:==	!OSMenuBar -> !(MenuHandles pst) -> !*OSToolbox -> (!x,!MenuHandles pst,!*OSToolbox)
::	DeltaMenuHandle pst
	:==	!(MenuStateHandle pst) -> !*OSToolbox -> (!MenuStateHandle pst,!*OSToolbox)
::	AccessMenuHandle x pst
	:==	!(MenuStateHandle pst) -> (!x,!MenuStateHandle pst)


//	General rules to access MenuHandles:

accessMenuHandles :: !Id !(AccessMenuHandle x (PSt .l)) !(IOSt .l) -> (!Maybe x, !IOSt .l)
accessMenuHandles id f ioState
	# (found,mDevice,ioState)	= IOStGetDevice MenuDevice ioState
	| not found
		= (Nothing,ioState)
	# mHs						= MenuSystemStateGetMenuHandles mDevice
	  (result,msHs)				= accessmenuhandles id f mHs.mMenus
	# ioState					= IOStSetDevice (MenuSystemState {mHs & mMenus=msHs}) ioState
	= (result,ioState)
where
	accessmenuhandles :: !Id !(AccessMenuHandle x .pst) ![MenuStateHandle .pst] -> (!Maybe x,![MenuStateHandle .pst])
	accessmenuhandles id f [mH:mHs]
		# (menu_id,mH)			= menuStateHandleGetMenuId mH
		| id==menu_id
			# (result,mH)		= f mH
			= (Just result,[mH:mHs])
		| otherwise
			# (opt_result,mHs)	= accessmenuhandles id f mHs
			= (opt_result,[mH:mHs])
	accessmenuhandles _ _ _
		= (Nothing,[])

changeMenuSystemState :: !Bool !(DeltaMenuSystem .l .p) !(IOSt .l) -> IOSt .l
changeMenuSystemState redrawMenus f ioState
	# (osdInfo,ioState)			= IOStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar		// This condition should never hold
		= StdMenuFatalError "changeMenuSystemState" "could not retrieve OSMenuBar from IOSt"
	# osMenuBar					= fromJust maybeOSMenuBar
	# (found,mDevice,ioState)	= IOStGetDevice MenuDevice ioState
	| not found
		= ioState
	# (tb,ioState)				= getIOToolbox ioState
	  menus						= MenuSystemStateGetMenuHandles mDevice
	# (menus,tb)				= f osMenuBar menus tb
	| not redrawMenus
		# ioState				= setIOToolbox tb ioState
		= IOStSetDevice (MenuSystemState menus) ioState
	| otherwise
		# tb					= DrawMenuBar osMenuBar tb
		  osdInfo				= setOSDInfoOSMenuBar osMenuBar osdInfo
		# ioState				= IOStSetOSDInfo osdInfo ioState
		# ioState				= setIOToolbox tb ioState
		= IOStSetDevice (MenuSystemState menus) ioState

accessMenuSystemState :: !Bool !(AccessMenuSystem .x (PSt .l)) !(IOSt .l) -> (!Maybe .x,!IOSt .l)
accessMenuSystemState redrawMenus f ioState
	# (osdInfo,ioState)			= IOStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar		// This condition should never hold
		= StdMenuFatalError "accessMenuSystemState" "could not retrieve OSMenuBar from IOSt"
	# osMenuBar					= fromJust maybeOSMenuBar
	# (found,mDevice,ioState)	= IOStGetDevice MenuDevice ioState
	| not found
		= (Nothing,ioState)
	# (tb,ioState)				= getIOToolbox ioState
	  menus						= MenuSystemStateGetMenuHandles mDevice
	# (x,menus,tb)				= f osMenuBar menus tb
	| not redrawMenus
		# ioState				= setIOToolbox tb ioState
		= (Just x,IOStSetDevice (MenuSystemState menus) ioState)
	| otherwise
		# tb					= DrawMenuBar osMenuBar tb
		  osdInfo				= setOSDInfoOSMenuBar osMenuBar osdInfo
		# ioState				= IOStSetOSDInfo osdInfo ioState
		# ioState				= setIOToolbox tb ioState
		= (Just x,IOStSetDevice (MenuSystemState menus) ioState)


//	Opening a menu for an interactive process.

class Menus mdef where
	openMenu	:: .ls !(mdef .ls (PSt .l)) !(PSt .l)	-> (!ErrorReport,!PSt .l)
	getMenuType	::      (mdef .ls .pst)					-> MenuType

instance Menus (Menu m)	| MenuElements m where
	openMenu :: .ls !(Menu m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)	| MenuElements m
	openMenu ls mDef pState
		# pState			= MenuFunctions.dOpen pState
		# (isZero,pState)	= accPIO checkZeroMenuBound pState
		| isZero
			= (ErrorViolateDI,pState)
		# (optMenuId,mDef)	= menuDefGetMenuId mDef
		# (optMenuId,pState)= accPIO (validateMenuId optMenuId) pState
		| isNothing optMenuId
			= (ErrorIdsInUse,pState)
		# menuId			= fromJust optMenuId
		| menuId==WindowMenuId
			= (ErrorIdsInUse,pState)
		| otherwise
			= OpenMenu` menuId ls mDef pState
	where
		checkZeroMenuBound :: !(IOSt .l) -> (!Bool,!IOSt .l)
		checkZeroMenuBound ioState
			# (found,mDevice,ioState)	= IOStGetDevice MenuDevice ioState
			| not found					// This condition should never occur
				= StdMenuFatalError "openMenu (Menu)" "could not retrieve MenuSystemState from IOSt"
			# mHs						= MenuSystemStateGetMenuHandles mDevice
			  (bound,mHs)				= (\msHs=:{mNrMenuBound}->(mNrMenuBound,msHs)) mHs
			# ioState					= IOStSetDevice (MenuSystemState mHs) ioState
			= (zeroBound bound,ioState)
	
	getMenuType :: (Menu m .ls .pst) -> MenuType | MenuElements m
	getMenuType _ = "Menu"

validateMenuId :: !(Maybe Id) !(IOSt .l) -> (!Maybe Id,!IOSt .l)
validateMenuId Nothing ioState
	# (mId,ioState)				= openId ioState
	= (Just mId,ioState)
validateMenuId (Just id) ioState
	# (idtable,ioState)			= IOStGetIdTable ioState
	| memberIdTable id idtable	= (Nothing,ioState)
	| otherwise					= (Just id,ioState)

instance Menus (PopUpMenu m) | PopUpMenuElements m where
	openMenu :: .ls !(PopUpMenu m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | PopUpMenuElements m
	openMenu ls mDef pState
		# (osdInfo,pState)			= accPIO IOStGetOSDInfo pState
		| getOSDInfoDocumentInterface osdInfo==NDI
			= (ErrorViolateDI,pState)
		# maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
		| isNothing maybeOSMenuBar	// This condition should never occur
			= StdMenuFatalError "openMenu (PopUpMen)" "OSMenuBar could not be retrieved from OSDInfo"
		# pState					= MenuFunctions.dOpen pState
		# (found,mDevice,ioState)	= IOStGetDevice MenuDevice pState.io
		| not found
			= (ErrorUnknownObject,{pState & io=ioState})
		# mHs						= MenuSystemStateGetMenuHandles mDevice
		  mHs						= closepopupmenu mHs
		  osMenuBar					= fromJust maybeOSMenuBar
		# (idtable,ioState)			= IOStGetIdTable ioState
		# (rt,ioState)				= IOStGetReceiverTable ioState
		# (ioid,ioState)			= IOStGetIOId ioState
		# (ok,mHs,rt,idtable,osMenuBar,pState)
									= createPopUpMenu ioid ls mDef mHs rt idtable osMenuBar {pState & io=ioState}
	 	  osdInfo					= setOSDInfoOSMenuBar osMenuBar osdInfo
	 	# ioState					= IOStSetOSDInfo osdInfo pState.io
	 	# ioState					= IOStSetReceiverTable rt ioState
	 	# ioState					= IOStSetIdTable idtable ioState
		# ioState					= IOStSetDevice (MenuSystemState mHs) ioState
		# pState					= {pState & io=ioState}
		| ok
			= handlePopUpMenu pState
		| otherwise
			= (ErrorIdsInUse,pState)
	where
	//	handlePopUpMenu opens the pop up menu.
		handlePopUpMenu :: !(PSt .l) -> (!ErrorReport,!PSt .l)
		handlePopUpMenu pState
			# (osdInfo,ioState)			= IOStGetOSDInfo pState.io
			  framePtr					= case (getOSDInfoOSInfo osdInfo) of
			  								Just info -> info.osFrame
			  								nothing   -> StdMenuFatalError "openMenu (PopUpMenu)" "incorrect OSDInfo retrieved"
			# (found,mDevice,ioState)	= IOStGetDevice MenuDevice ioState
			| not found					// This condition should never occur
				= StdMenuFatalError "openMenu (PopUpMenu)" "could not retrieve MenuSystemState from IOSt"
			# mHs						= MenuSystemStateGetMenuHandles mDevice
			  (menus,mHs)				= MenuHandlesGetMenuStateHandles mHs
			  (popUpMenu,menus)			= HdTl menus
			  (popUpId,popUpMenu)		= menuStateHandleGetMenuId popUpMenu
			  (mPtr,popUpMenu)			= menuStateHandleGetHandle popUpMenu
			# (ok,ioState)				= accIOToolbox (OStrackPopUpMenu mPtr framePtr) ioState
			| not ok
				# ioState				= IOStSetDevice (MenuSystemState {mHs & mMenus=menus,mPopUpId=Just popUpId}) ioState
				# pState				= {pState & io=ioState}
				= (OtherError "PopUpMenu tracking error",pState)
			| otherwise
				# ioState				= IOStSetDevice (MenuSystemState {mHs & mMenus=[popUpMenu:menus]}) ioState
				# pState				= {pState & io=ioState}
				= (NoError,pState)
	
	getMenuType :: (PopUpMenu m .ls .pst) -> MenuType | PopUpMenuElements m
	getMenuType _ = "PopUpMenu"


//	Closing a menu.

closeMenu :: !Id !(IOSt .l) -> IOSt .l
closeMenu id ioState
	| id==WindowMenuId	= ioState
	| otherwise			= closemenu id ioState


//	Enabling and Disabling of the MenuSystem:

enableMenuSystem :: !(IOSt .l) -> IOSt .l
enableMenuSystem ioState
/*	# (optModal,ioState)	= IOStGetIOIsModal ioState
	# (ioId,    ioState)	= IOStGetIOId ioState
	  modalId				= fromJust optModal
	| isJust optModal && ioId==modalId
		= ioState */
	# (isModal,ioState)		= hasModalDialog ioState
	| isModal
		= ioState
	# (di,ioState)			= IOStGetDocumentInterface ioState
	| di==NDI
		= ioState
	| otherwise
		= changeMenuSystemState True (enablemenusystem di) ioState
where
	hasModalDialog :: !(IOSt .l) -> (!Bool,!IOSt .l)
	hasModalDialog ioState
		# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
		| not found
			= (False,ioState)
		# windows					= WindowSystemStateGetWindowHandles wDevice
		  (modalWIDS,windows)		= getWindowHandlesActiveModalDialog windows
		# ioState					= IOStSetDevice (WindowSystemState windows) ioState
		= (isJust modalWIDS,ioState)
	
	enablemenusystem :: !DocumentInterface !OSMenuBar !(MenuHandles .pst) !*OSToolbox -> (!MenuHandles .pst,!*OSToolbox)
	enablemenusystem di osMenuBar menus=:{mEnabled,mMenus} tb
		| mEnabled
			= (menus,tb)
		| otherwise
			# (nrMenus,msHs)= Ulength mMenus
			# tb			= enablemenus (if (di==MDI) (nrMenus+1) (nrMenus-1)) osMenuBar tb
			= ({menus & mMenus=msHs,mEnabled=SystemAble},tb)
	where
		enablemenus :: !Int !OSMenuBar !*OSToolbox -> *OSToolbox
		enablemenus i osmenubar tb
			| i<0			= tb
			| otherwise		= enablemenus (i-1) osMenuBar (OSEnableMenu i osMenuBar tb)

disableMenuSystem :: !(IOSt .l) -> IOSt .l
disableMenuSystem ioState
	# (di,ioState)	= IOStGetDocumentInterface ioState
	| di==NDI		= ioState
	| otherwise		= changeMenuSystemState True (disablemenusystem di) ioState
where
	disablemenusystem :: !DocumentInterface !OSMenuBar !(MenuHandles .pst) !*OSToolbox -> (!MenuHandles .pst,!*OSToolbox)
	disablemenusystem di osMenuBar menus=:{mEnabled,mMenus} tb
		| not mEnabled
			= (menus,tb)
		| otherwise
			# (nrMenus,msHs)= Ulength mMenus
			# tb			= disablemenus (if (di==MDI) (nrMenus+1) (nrMenus-1)) osMenuBar tb
			= ({menus & mMenus=msHs,mEnabled=SystemUnable},tb)
	where
		disablemenus :: !Int !OSMenuBar !*OSToolbox -> *OSToolbox
		disablemenus i osMenuBar tb
			| i<0			= tb
			| otherwise		= disablemenus (i-1) osMenuBar (OSDisableMenu i osMenuBar tb)


//	Enabling and Disabling of Menus:

enableMenus :: ![Id] !(IOSt .l) -> IOSt .l
enableMenus ids ioState
	# ids			= filter ((<>) WindowMenuId) ids
	| isEmpty ids	= ioState
	| otherwise		= enablemenus ids ioState

disableMenus :: ![Id] !(IOSt .l) -> IOSt .l
disableMenus ids ioState
	# ids			= filter ((<>) WindowMenuId) ids
	| isEmpty ids	= ioState
	| otherwise		= disablemenus ids ioState


//	Get the SelectState of a menu: 

getMenuSelectState :: !Id !(IOSt .l) -> (!Maybe SelectState,!IOSt .l)
getMenuSelectState id ioState
	# (optSelect,ioState)	= accessMenuHandles id menuStateHandleGetSelect ioState
	| isNothing optSelect	= (Nothing,		ioState)
	| fromJust optSelect	= (Just Able,	ioState)
	| otherwise				= (Just Unable,	ioState)


/*	Adding menu elements to (sub/radio)menus:
		Items in a (sub/radio)menu are positioned starting from 1 and increasing by 1.
		Open with a position less than 1 adds the new elements in front
		Open with a position higher than the number of items adds the new elements to
		the end.
		Open an item on a position adds the item AFTER the item on that position.
*/
openMenuElements :: !Id !Index .ls (m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | MenuElements m
openMenuElements mId pos ls new pState
	# (it,ioState)					= IOStGetIdTable pState.io
	  maybeParent					= getIdParent mId it
	| isNothing maybeParent
		= (ErrorUnknownObject,{pState & io=ioState})
	# parent						= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= (ErrorUnknownObject,{pState & io=ioState})
	# (pid,ioState)					= IOStGetIOId ioState
	| parent.idpIOId<>pid
		= (ErrorUnknownObject,{pState & io=ioState})
	| parent.idpId<>mId
		= (ErrorUnknownObject,{pState & io=ioState})
	# (found,mDevice,ioState)		= IOStGetDevice MenuDevice ioState
	| not found
		= (ErrorUnknownObject,{pState & io=ioState})
	# (osdInfo,ioState)				= IOStGetOSDInfo ioState
	  maybeOSMenuBar				= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar			// This condition should not occur
		= StdMenuFatalError "openMenuElements" "OSMenuBar could not be retrieved from OSDInfo"
	| otherwise
		# osMenuBar					= fromJust maybeOSMenuBar
		# (rt, ioState)				= IOStGetReceiverTable ioState
		# (tb, ioState)				= getIOToolbox ioState
		# pState					= {pState & io=ioState}
		  menus						= MenuSystemStateGetMenuHandles mDevice
		# ((error,rt,it),menus,osMenuBar,pState)
									= addMenusItems (mId,Nothing) (max 0 pos) ls new pid rt it menus osMenuBar pState
		# ioState					= setIOToolbox (DrawMenuBar osMenuBar tb) pState.io
		  mDevice					= MenuSystemState menus
		  osdInfo					= setOSDInfoOSMenuBar osMenuBar osdInfo
		# ioState					= IOStSetOSDInfo osdInfo ioState
		# ioState					= IOStSetDevice mDevice ioState
		# ioState					= IOStSetIdTable it ioState
		# ioState					= IOStSetReceiverTable rt ioState
		# pState					= {pState & io=ioState}
		= (error,pState)

openSubMenuElements :: !Id !Index .ls (m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)	| MenuElements m
openSubMenuElements sId pos ls new pState
	# (it,ioState)				= IOStGetIdTable pState.io
	  maybeParent				= getIdParent sId it
	| isNothing maybeParent
		= (ErrorUnknownObject,{pState & io=ioState})
	# parent					= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= (ErrorUnknownObject,{pState & io=ioState})
	# (pid,ioState)				= IOStGetIOId ioState
	| parent.idpIOId<>pid
		= (ErrorUnknownObject,{pState & io=ioState})
	# (found,mDevice,ioState)	= IOStGetDevice MenuDevice ioState
	| not found
		= (ErrorUnknownObject,{pState & io=ioState})
	# (osdInfo,ioState)			= IOStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar			// This condition should not occur
		= StdMenuFatalError "openSubMenuElements" "OSMenuBar could not be retrieved from OSDInfo"
	| otherwise
		# osMenuBar				= fromJust maybeOSMenuBar
		# (rt,ioState)			= IOStGetReceiverTable ioState
		# (tb,ioState)			= getIOToolbox ioState
		# pState				= {pState & io=ioState}
		  menus					= MenuSystemStateGetMenuHandles mDevice
		# ((error,rt,it),menus,osMenuBar,pState)
								= addMenusItems (parent.idpId,Just sId) (max 0 pos) ls new pid rt it menus osMenuBar pState
		# ioState				= setIOToolbox (DrawMenuBar osMenuBar tb) pState.io
		  mDevice				= MenuSystemState menus
		  osdInfo				= setOSDInfoOSMenuBar osMenuBar osdInfo
		# ioState				= IOStSetOSDInfo osdInfo ioState
		# ioState				= IOStSetDevice mDevice ioState
		# ioState				= IOStSetIdTable it ioState
		# ioState				= IOStSetReceiverTable rt ioState
		# pState				= {pState & io=ioState}
		= (error,pState)

openRadioMenuItems :: !Id !Index ![MenuRadioItem (PSt .l)] !(IOSt .l) -> (!ErrorReport,!IOSt .l)
openRadioMenuItems rId pos radioItems ioState
	# (idtable,ioState)		= IOStGetIdTable ioState
	  maybeParent			= getIdParent rId idtable
	| isNothing maybeParent
		= (ErrorUnknownObject,ioState)
	# parent				= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= (ErrorUnknownObject,ioState)
	# (ioId,ioState)		= IOStGetIOId ioState
	| parent.idpIOId<>ioId
		= (ErrorUnknownObject,ioState)
	| isEmpty radioItems
		= (NoError,ioState)
	# radioIds				= FilterMap (\(_,maybeId,_,_)->(isJust maybeId,fromJust maybeId)) radioItems
	| not (okMembersIdTable radioIds idtable)
		= (ErrorIdsInUse,ioState)
	| otherwise
		# mId				= parent.idpId
		# (error,ioState)	= accessMenuSystemState True (addMenuRadioItems (mId,rId) (max 0 pos) radioItems) ioState
		# ioState			= IOStSetIdTable (snd (addIdsToIdTable (map (\id->(id,{idpIOId=ioId,idpDevice=MenuDevice,idpId=mId})) radioIds) idtable)) ioState
		  error				= case error of
		  						Nothing  -> ErrorUnknownObject
		  						Just err -> err
		= (error,ioState)


//	Removing menu elements from (sub/radio)menus:

closeMenuElements :: !Id ![Id] !(IOSt .l) -> IOSt .l
closeMenuElements mId ids ioState
	# ids			= filter (\id->not (isSpecialId id)) ids
	| isEmpty ids	= ioState
	| otherwise		= closemenuelements mId ids ioState


//	Removing menu elements from (sub/radio)menus by index (counting from 1):

closeMenuIndexElements :: !Id ![Index] !(IOSt .l) -> IOSt .l
closeMenuIndexElements mId indices ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	  maybeParent		= getIdParent mId idtable
	| isNothing maybeParent
		= ioState
	# parent			= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	| parent.idpIOId<>ioId || parent.idpId<>mId
		= ioState
	| otherwise
		= closemenuindexelements NotRemoveSpecialMenuElements False ioId (mId,Nothing) indices ioState

closeSubMenuIndexElements :: !Id ![Index] !(IOSt .l) -> IOSt .l
closeSubMenuIndexElements sId indices ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	  maybeParent		= getIdParent sId idtable
	| isNothing maybeParent
		= ioState
	# parent			= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	| parent.idpIOId<>ioId
		= ioState
	| otherwise
		= closemenuindexelements NotRemoveSpecialMenuElements False ioId (parent.idpId,Just sId) indices ioState

closeRadioMenuIndexElements :: !Id ![Index] !(IOSt .l) -> IOSt .l
closeRadioMenuIndexElements rId indices ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	  maybeParent		= getIdParent rId idtable
	| isNothing maybeParent
		= ioState
	# parent			= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	| parent.idpIOId<>ioId
		= ioState
	| otherwise
		= closemenuindexelements NotRemoveSpecialMenuElements True ioId (parent.idpId,Just rId) indices ioState


//	Determine the Ids and MenuTypes of all menus.

getMenus :: !(IOSt .l) -> (![(Id,MenuType)],!IOSt .l)
getMenus ioState
	# (found,mDevice,ioState)	= IOStGetDevice MenuDevice ioState
	| not found
		= ([],ioState)
	# mHs						= MenuSystemStateGetMenuHandles mDevice
	  (idtypes,msHs)			= AccessList getIdType mHs.mMenus
	# ioState					= IOStSetDevice (MenuSystemState {mHs & mMenus=msHs}) ioState
	= (tl idtypes,ioState)
where
	getIdType :: !(MenuStateHandle .pst) -> ((Id,MenuType),!MenuStateHandle .pst)
	getIdType msH
		# (id,msH)		= menuStateHandleGetMenuId msH
		= ((id,"Menu"),msH)


//	Determine the index position of a menu.

getMenuPos :: !Id !(IOSt .l) -> (!Maybe Index,!IOSt .l)
getMenuPos id ioState
	# (found,mDevice,ioState)	= IOStGetDevice MenuDevice ioState
	| not found
		= (Nothing,ioState)
	# mHs						= MenuSystemStateGetMenuHandles mDevice
	  (optIndex,msHs)			= getmenuindex id 0 mHs.mMenus
	# ioState					= IOStSetDevice (MenuSystemState {mHs & mMenus=msHs}) ioState
	= (optIndex,ioState)
where
	getmenuindex :: !Id !Int ![MenuStateHandle .pst] -> (!Maybe Int,![MenuStateHandle .pst])
	getmenuindex id index [mH:mHs]
		# (menu_id,mH)	= menuStateHandleGetMenuId mH
		| id==menu_id
			= (Just index,[mH:mHs])
		| otherwise
			# (optIndex,mHs)= getmenuindex id (index+1) mHs
			= (optIndex, [mH:mHs])
	getmenuindex _ _ _
		= (Nothing,[])


//	Set & Get the title of a menu.

setMenuTitle :: !Id !Title !(IOSt .l) -> IOSt .l
setMenuTitle id title ioState
	| id==WindowMenuId	= ioState
	| otherwise			= setmenutitle id title ioState

getMenuTitle :: !Id !(IOSt .l) -> (!Maybe Title,!IOSt .l)
getMenuTitle id ioState
	= accessMenuHandles id menuStateHandleGetTitle ioState
