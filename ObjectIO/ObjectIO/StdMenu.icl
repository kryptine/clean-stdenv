implementation module StdMenu


//	Clean Object I/O library, version 1.2.1


import	StdBool, StdList, StdTuple
import	osmenu
import	commondef, iostate, menuaccess, menucreate, menudevice, menuinternal, menuitems, StdId
from	devicesystemstate	import windowSystemStateGetWindowHandles
from	menudefaccess		import menuDefGetMenuId
from	menuevent			import menuSystemStateGetMenuHandles
from	StdPSt				import accPIO
from	windowaccess		import getWindowHandlesActiveModalDialog


stdMenuFatalError :: String String -> .x
stdMenuFatalError function error
	= fatalError function "StdMenu" error


//	General rules to access MenuHandles:

accessMenuHandles :: !Id !((MenuStateHandle (PSt .l)) -> (x,!MenuStateHandle (PSt .l))) !(IOSt .l) -> (!Maybe x,!IOSt .l)
accessMenuHandles id f ioState
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= (Nothing,ioState)
	# mHs						= menuSystemStateGetMenuHandles mDevice
	  (result,msHs)				= accessmenuhandles id f mHs.mMenus
	# ioState					= ioStSetDevice (MenuSystemState {mHs & mMenus=msHs}) ioState
	= (result,ioState)
where
	accessmenuhandles :: !Id !((MenuStateHandle .pst) -> (x,!MenuStateHandle .pst)) ![MenuStateHandle .pst] -> (!Maybe x,![MenuStateHandle .pst])
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

changeMenuSystemState :: !Bool
						 !(OSMenuBar -> (MenuHandles (PSt .l)) -> *(*OSToolbox -> *(MenuHandles (PSt .l),*OSToolbox)))
						 !(IOSt .l)
						-> IOSt .l
changeMenuSystemState redrawMenus f ioState
	# (osdInfo,ioState)			= ioStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar		// This condition should never hold
		= stdMenuFatalError "changeMenuSystemState" "could not retrieve OSMenuBar from IOSt"
	# osMenuBar					= fromJust maybeOSMenuBar
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= ioState
	# (tb,ioState)				= getIOToolbox ioState
	  menus						= menuSystemStateGetMenuHandles mDevice
	# (menus,tb)				= f osMenuBar menus tb
	| not redrawMenus
		# ioState				= setIOToolbox tb ioState
		= ioStSetDevice (MenuSystemState menus) ioState
	| otherwise
		# tb					= osDrawMenuBar osMenuBar tb
		  osdInfo				= setOSDInfoOSMenuBar osMenuBar osdInfo
		# ioState				= ioStSetOSDInfo osdInfo ioState
		# ioState				= setIOToolbox tb ioState
		= ioStSetDevice (MenuSystemState menus) ioState

accessMenuSystemState :: !Bool
						 !(OSMenuBar -> (MenuHandles (PSt .l)) -> *(*OSToolbox -> *(.x,MenuHandles (PSt .l),*OSToolbox)))
						 !(IOSt .l)
					-> (!Maybe .x,!IOSt .l)
accessMenuSystemState redrawMenus f ioState
	# (osdInfo,ioState)			= ioStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar		// This condition should never hold
		= stdMenuFatalError "accessMenuSystemState" "could not retrieve OSMenuBar from IOSt"
	# osMenuBar					= fromJust maybeOSMenuBar
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= (Nothing,ioState)
	# (tb,ioState)				= getIOToolbox ioState
	  menus						= menuSystemStateGetMenuHandles mDevice
	# (x,menus,tb)				= f osMenuBar menus tb
	| not redrawMenus
		# ioState				= setIOToolbox tb ioState
		= (Just x,ioStSetDevice (MenuSystemState menus) ioState)
	| otherwise
		# tb					= osDrawMenuBar osMenuBar tb
		  osdInfo				= setOSDInfoOSMenuBar osMenuBar osdInfo
		# ioState				= ioStSetOSDInfo osdInfo ioState
		# ioState				= setIOToolbox tb ioState
		= (Just x,ioStSetDevice (MenuSystemState menus) ioState)


//	Opening a menu for an interactive process.

class Menus mdef where
	openMenu	:: .ls !(mdef .ls (PSt .l)) !(PSt .l)	-> (!ErrorReport,!PSt .l)
	getMenuType	::      (mdef .ls .pst)					-> MenuType

instance Menus (Menu m)	| MenuElements m where
	openMenu :: .ls !(Menu m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)	| MenuElements m
	openMenu ls mDef pState
		# pState			= menuFunctions.dOpen pState
		# (isZero,pState)	= accPIO checkZeroMenuBound pState
		| isZero
			= (ErrorViolateDI,pState)
		# (optMenuId,mDef)	= menuDefGetMenuId mDef
		# (optMenuId,pState)= accPIO (validateMenuId optMenuId) pState
		| isNothing optMenuId
			= (ErrorIdsInUse,pState)
		# menuId			= fromJust optMenuId
		| menuId==windowMenuId
			= (ErrorIdsInUse,pState)
		| otherwise
			= openMenu` menuId ls mDef pState
	where
		checkZeroMenuBound :: !(IOSt .l) -> (!Bool,!IOSt .l)
		checkZeroMenuBound ioState
			# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
			| not found					// This condition should never occur
				= stdMenuFatalError "openMenu (Menu)" "could not retrieve MenuSystemState from IOSt"
			# mHs						= menuSystemStateGetMenuHandles mDevice
			  (bound,mHs)				= (\msHs=:{mNrMenuBound}->(mNrMenuBound,msHs)) mHs
			# ioState					= ioStSetDevice (MenuSystemState mHs) ioState
			= (zeroBound bound,ioState)
	
	getMenuType :: (Menu m .ls .pst) -> MenuType | MenuElements m
	getMenuType _ = "Menu"

validateMenuId :: !(Maybe Id) !(IOSt .l) -> (!Maybe Id,!IOSt .l)
validateMenuId Nothing ioState
	# (mId,ioState)		= openId ioState
	= (Just mId,ioState)
validateMenuId (Just id) ioState
	# (idtable,ioState)	= ioStGetIdTable ioState
	# (member,idtable)	= memberIdTable id idtable
	| member			= (Nothing,ioStSetIdTable idtable ioState)
	| otherwise			= (Just id,ioStSetIdTable idtable ioState)

instance Menus (PopUpMenu m) | PopUpMenuElements m where
	openMenu :: .ls !(PopUpMenu m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | PopUpMenuElements m
	openMenu ls mDef pState
		# (osdInfo,pState)			= accPIO ioStGetOSDInfo pState
		| getOSDInfoDocumentInterface osdInfo==NDI
			= (ErrorViolateDI,pState)
		# maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
		| isNothing maybeOSMenuBar	// This condition should never occur
			= stdMenuFatalError "openMenu (PopUpMen)" "OSMenuBar could not be retrieved from OSDInfo"
		# pState					= menuFunctions.dOpen pState
		# (found,mDevice,ioState)	= ioStGetDevice MenuDevice pState.io
		| not found
			= (ErrorUnknownObject,{pState & io=ioState})
		# mHs						= menuSystemStateGetMenuHandles mDevice
		  mHs						= closepopupmenu mHs
		  osMenuBar					= fromJust maybeOSMenuBar
		# (idtable,ioState)			= ioStGetIdTable ioState
		# (rt,ioState)				= ioStGetReceiverTable ioState
		# (ioid,ioState)			= ioStGetIOId ioState
		# (ok,mHs,rt,idtable,osMenuBar,pState)
									= createPopUpMenu ioid ls mDef mHs rt idtable osMenuBar {pState & io=ioState}
	 	  osdInfo					= setOSDInfoOSMenuBar osMenuBar osdInfo
	 	# ioState					= ioStSetOSDInfo osdInfo pState.io
	 	# ioState					= ioStSetReceiverTable rt ioState
	 	# ioState					= ioStSetIdTable idtable ioState
		# ioState					= ioStSetDevice (MenuSystemState mHs) ioState
		# pState					= {pState & io=ioState}
		| ok
			= handlePopUpMenu pState
		| otherwise
			= (ErrorIdsInUse,pState)
	where
	//	handlePopUpMenu opens the pop up menu.
		handlePopUpMenu :: !(PSt .l) -> (!ErrorReport,!PSt .l)
		handlePopUpMenu pState
			# (osdInfo,ioState)			= ioStGetOSDInfo pState.io
			  framePtr					= case (getOSDInfoOSInfo osdInfo) of
			  								Just info -> info.osFrame
			  								nothing   -> stdMenuFatalError "openMenu (PopUpMenu)" "incorrect OSDInfo retrieved"
			# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
			| not found					// This condition should never occur
				= stdMenuFatalError "openMenu (PopUpMenu)" "could not retrieve MenuSystemState from IOSt"
			# mHs						= menuSystemStateGetMenuHandles mDevice
			  (menus,mHs)				= menuHandlesGetMenus mHs
			  (popUpMenu,menus)			= hdtl menus
			  (popUpId,popUpMenu)		= menuStateHandleGetMenuId popUpMenu
			  (mPtr,popUpMenu)			= menuStateHandleGetHandle popUpMenu
			# (ok,ioState)				= accIOToolbox (osTrackPopUpMenu mPtr framePtr) ioState
			| not ok
				# ioState				= ioStSetDevice (MenuSystemState {mHs & mMenus=menus,mPopUpId=Just popUpId}) ioState
				# pState				= {pState & io=ioState}
				= (OtherError "PopUpMenu tracking error",pState)
			| otherwise
				# ioState				= ioStSetDevice (MenuSystemState {mHs & mMenus=[popUpMenu:menus]}) ioState
				# pState				= {pState & io=ioState}
				= (NoError,pState)
	
	getMenuType :: (PopUpMenu m .ls .pst) -> MenuType | PopUpMenuElements m
	getMenuType _ = "PopUpMenu"


//	Closing a menu.

closeMenu :: !Id !(IOSt .l) -> IOSt .l
closeMenu id ioState
	| id==windowMenuId	= ioState
	| otherwise			= closemenu id ioState


//	Enabling and Disabling of the MenuSystem:

enableMenuSystem :: !(IOSt .l) -> IOSt .l
enableMenuSystem ioState
/*	# (optModal,ioState)	= ioStGetIOIsModal ioState
	# (ioId,    ioState)	= ioStGetIOId ioState
	  modalId				= fromJust optModal
	| isJust optModal && ioId==modalId
		= ioState */
	# (isModal,ioState)		= hasModalDialog ioState
	| isModal
		= ioState
	# (di,ioState)			= ioStGetDocumentInterface ioState
	| di==NDI
		= ioState
	| otherwise
		= changeMenuSystemState True (enablemenusystem di) ioState
where
	hasModalDialog :: !(IOSt .l) -> (!Bool,!IOSt .l)
	hasModalDialog ioState
		# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
		| not found
			= (False,ioState)
		| otherwise
			# windows				= windowSystemStateGetWindowHandles wDevice
			  (modalWIDS,windows)	= getWindowHandlesActiveModalDialog windows
			# ioState				= ioStSetDevice (WindowSystemState windows) ioState
			= (isJust modalWIDS,ioState)
	
	enablemenusystem :: !DocumentInterface !OSMenuBar !(MenuHandles .pst) !*OSToolbox -> (!MenuHandles .pst,!*OSToolbox)
	enablemenusystem di osMenuBar menus=:{mEnabled,mMenus} tb
		| mEnabled
			= (menus,tb)
		| otherwise
			# (nrMenus,msHs)= ulength mMenus
			# tb			= enablemenus (if (di==MDI) (nrMenus+1) (nrMenus-1)) osMenuBar tb
			= ({menus & mMenus=msHs,mEnabled=SystemAble},tb)
	where
		enablemenus :: !Int !OSMenuBar !*OSToolbox -> *OSToolbox
		enablemenus i osmenubar tb
			| i<0			= tb
			| otherwise		= enablemenus (i-1) osMenuBar (osEnableMenu i osMenuBar tb)

disableMenuSystem :: !(IOSt .l) -> IOSt .l
disableMenuSystem ioState
	# (di,ioState)	= ioStGetDocumentInterface ioState
	| di==NDI		= ioState
	| otherwise		= changeMenuSystemState True (disablemenusystem di) ioState
where
	disablemenusystem :: !DocumentInterface !OSMenuBar !(MenuHandles .pst) !*OSToolbox -> (!MenuHandles .pst,!*OSToolbox)
	disablemenusystem di osMenuBar menus=:{mEnabled,mMenus} tb
		| not mEnabled
			= (menus,tb)
		| otherwise
			# (nrMenus,msHs)= ulength mMenus
			# tb			= disablemenus (if (di==MDI) (nrMenus+1) (nrMenus-1)) osMenuBar tb
			= ({menus & mMenus=msHs,mEnabled=SystemUnable},tb)
	where
		disablemenus :: !Int !OSMenuBar !*OSToolbox -> *OSToolbox
		disablemenus i osMenuBar tb
			| i<0			= tb
			| otherwise		= disablemenus (i-1) osMenuBar (osDisableMenu i osMenuBar tb)


//	Enabling and Disabling of Menus:

enableMenus :: ![Id] !(IOSt .l) -> IOSt .l
enableMenus ids ioState
	# ids			= filter ((<>) windowMenuId) ids
	| isEmpty ids	= ioState
	| otherwise		= enablemenus ids ioState

disableMenus :: ![Id] !(IOSt .l) -> IOSt .l
disableMenus ids ioState
	# ids			= filter ((<>) windowMenuId) ids
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
	# (it,ioState)					= ioStGetIdTable pState.io
	# (maybeParent,it)				= getIdParent mId it
	| isNothing maybeParent
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# parent						= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# (pid,ioState)					= ioStGetIOId ioState
	| parent.idpIOId<>pid
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	| parent.idpId<>mId
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# (found,mDevice,ioState)		= ioStGetDevice MenuDevice ioState
	| not found
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# (osdInfo,ioState)				= ioStGetOSDInfo ioState
	  maybeOSMenuBar				= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar		// This condition should not occur
		= stdMenuFatalError "openMenuElements" "OSMenuBar could not be retrieved from OSDInfo"
	| otherwise
		# osMenuBar					= fromJust maybeOSMenuBar
		# (rt, ioState)				= ioStGetReceiverTable ioState
		# (tb, ioState)				= getIOToolbox ioState
		# pState					= {pState & io=ioState}
		  menus						= menuSystemStateGetMenuHandles mDevice
		# ((error,rt,it),menus,osMenuBar,pState)
									= addMenusItems (mId,Nothing) (max 0 pos) ls new pid rt it menus osMenuBar pState
		# ioState					= setIOToolbox (osDrawMenuBar osMenuBar tb) pState.io
		  mDevice					= MenuSystemState menus
		  osdInfo					= setOSDInfoOSMenuBar osMenuBar osdInfo
		# ioState					= ioStSetOSDInfo osdInfo ioState
		# ioState					= ioStSetDevice mDevice ioState
		# ioState					= ioStSetIdTable it ioState
		# ioState					= ioStSetReceiverTable rt ioState
		# pState					= {pState & io=ioState}
		= (error,pState)

openSubMenuElements :: !Id !Index .ls (m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)	| MenuElements m
openSubMenuElements sId pos ls new pState
	# (it,ioState)				= ioStGetIdTable pState.io
	# (maybeParent,it)			= getIdParent sId it
	| isNothing maybeParent
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# parent					= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# (pid,ioState)				= ioStGetIOId ioState
	| parent.idpIOId<>pid
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# (osdInfo,ioState)			= ioStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar	// This condition should not occur
		= stdMenuFatalError "openSubMenuElements" "OSMenuBar could not be retrieved from OSDInfo"
	| otherwise
		# osMenuBar				= fromJust maybeOSMenuBar
		# (rt,ioState)			= ioStGetReceiverTable ioState
		# (tb,ioState)			= getIOToolbox ioState
		# pState				= {pState & io=ioState}
		  menus					= menuSystemStateGetMenuHandles mDevice
		# ((error,rt,it),menus,osMenuBar,pState)
								= addMenusItems (parent.idpId,Just sId) (max 0 pos) ls new pid rt it menus osMenuBar pState
		# ioState				= setIOToolbox (osDrawMenuBar osMenuBar tb) pState.io
		  mDevice				= MenuSystemState menus
		  osdInfo				= setOSDInfoOSMenuBar osMenuBar osdInfo
		# ioState				= ioStSetOSDInfo osdInfo ioState
		# ioState				= ioStSetDevice mDevice ioState
		# ioState				= ioStSetIdTable it ioState
		# ioState				= ioStSetReceiverTable rt ioState
		# pState				= {pState & io=ioState}
		= (error,pState)

openRadioMenuItems :: !Id !Index ![MenuRadioItem (PSt .l)] !(IOSt .l) -> (!ErrorReport,!IOSt .l)
openRadioMenuItems rId pos radioItems ioState
	# (idtable,ioState)		= ioStGetIdTable ioState
	# (maybeParent,idtable)	= getIdParent rId idtable
	| isNothing maybeParent
		= (ErrorUnknownObject,ioStSetIdTable idtable ioState)
	# parent				= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= (ErrorUnknownObject,ioStSetIdTable idtable ioState)
	# (ioId,ioState)		= ioStGetIOId ioState
	| parent.idpIOId<>ioId
		= (ErrorUnknownObject,ioStSetIdTable idtable ioState)
	| isEmpty radioItems
		= (NoError,ioStSetIdTable idtable ioState)
	# radioIds				= filterMap (\(_,maybeId,_,_)->(isJust maybeId,fromJust maybeId)) radioItems
	# (ok,idtable)			= okMembersIdTable radioIds idtable
	| not ok
		= (ErrorIdsInUse,ioStSetIdTable idtable ioState)
	| otherwise
		# mId				= parent.idpId
		# (error,ioState)	= accessMenuSystemState True (addMenuRadioItems (mId,rId) (max 0 pos) radioItems) ioState
		# ioState			= ioStSetIdTable (snd (addIdsToIdTable (map (\id->(id,{idpIOId=ioId,idpDevice=MenuDevice,idpId=mId})) radioIds) idtable)) ioState
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
	# (idtable,ioState)		= ioStGetIdTable ioState
	# (maybeParent,idtable)	= getIdParent mId idtable
	# ioState				= ioStSetIdTable idtable ioState
	| isNothing maybeParent
		= ioState
	# parent				= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	| parent.idpIOId<>ioId || parent.idpId<>mId
		= ioState
	| otherwise
		= closemenuindexelements NotRemoveSpecialMenuElements False ioId (mId,Nothing) indices ioState

closeSubMenuIndexElements :: !Id ![Index] !(IOSt .l) -> IOSt .l
closeSubMenuIndexElements sId indices ioState
	# (idtable,ioState)		= ioStGetIdTable ioState
	# (maybeParent,idtable)	= getIdParent sId idtable
	# ioState				= ioStSetIdTable idtable ioState
	| isNothing maybeParent
		= ioState
	# parent				= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	| parent.idpIOId<>ioId
		= ioState
	| otherwise
		= closemenuindexelements NotRemoveSpecialMenuElements False ioId (parent.idpId,Just sId) indices ioState

closeRadioMenuIndexElements :: !Id ![Index] !(IOSt .l) -> IOSt .l
closeRadioMenuIndexElements rId indices ioState
	# (idtable,ioState)		= ioStGetIdTable ioState
	# (maybeParent,idtable)	= getIdParent rId idtable
	# ioState				= ioStSetIdTable idtable ioState
	| isNothing maybeParent
		= ioState
	# parent				= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	| parent.idpIOId<>ioId
		= ioState
	| otherwise
		= closemenuindexelements NotRemoveSpecialMenuElements True ioId (parent.idpId,Just rId) indices ioState


//	Determine the Ids and MenuTypes of all menus.

getMenus :: !(IOSt .l) -> (![(Id,MenuType)],!IOSt .l)
getMenus ioState
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= ([],ioState)
	| otherwise
		# mHs					= menuSystemStateGetMenuHandles mDevice
		  (idtypes,msHs)		= accessList getIdType mHs.mMenus
		# ioState				= ioStSetDevice (MenuSystemState {mHs & mMenus=msHs}) ioState
		= (tl idtypes,ioState)
where
	getIdType :: !(MenuStateHandle .pst) -> *((Id,MenuType),!MenuStateHandle .pst)
	getIdType msH
		# (id,msH)				= menuStateHandleGetMenuId msH
		= ((id,"Menu"),msH)


//	Determine the index position of a menu.

getMenuPos :: !Id !(IOSt .l) -> (!Maybe Index,!IOSt .l)
getMenuPos id ioState
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= (Nothing,ioState)
	| otherwise
		# mHs					= menuSystemStateGetMenuHandles mDevice
		  (optIndex,msHs)		= getmenuindex id 0 mHs.mMenus
		# ioState				= ioStSetDevice (MenuSystemState {mHs & mMenus=msHs}) ioState
		= (optIndex,ioState)
where
	getmenuindex :: !Id !Int ![MenuStateHandle .pst] -> (!Maybe Int,![MenuStateHandle .pst])
	getmenuindex id index [mH:mHs]
		# (menu_id,mH)			= menuStateHandleGetMenuId mH
		| id==menu_id
			= (Just index,[mH:mHs])
		| otherwise
			# (optIndex,mHs)	= getmenuindex id (index+1) mHs
			= (optIndex, [mH:mHs])
	getmenuindex _ _ _
		= (Nothing,[])


//	Set & Get the title of a menu.

setMenuTitle :: !Id !Title !(IOSt .l) -> IOSt .l
setMenuTitle id title ioState
	| id==windowMenuId	= ioState
	| otherwise			= setmenutitle id title ioState

getMenuTitle :: !Id !(IOSt .l) -> (!Maybe Title,!IOSt .l)
getMenuTitle id ioState
	= accessMenuHandles id menuStateHandleGetTitle ioState
