implementation module osmenu


//	Clean Object I/O library, version 1.2


import	StdBool, StdChar, StdClass, StdInt, StdString
import	menuCCall_12, menuCrossCall_12
from	osdocumentinterface	import OSMenuBar
from	ostypes				import OSWindowPtr, OSNoWindowPtr


//	Types for menus and menu elements:
::	OSMenuHandle	:== HMENU
::	OSMenu			:== HMENU
::	OSMenuItem		:== HITEM
::	OSMenuSeparator	:== HITEM

//	Dummy values:
OSNoMenu :== 0
OSNoMenuItem :== 0
OSNoMenuSeparator :== 0


/*	Initialisation:
*/
osInitialiseMenus :: !*OSToolbox -> *OSToolbox
osInitialiseMenus tb
	= winInitialiseMenus tb


//	Enabling and disabling menus and menu elements:

osDisableMenu :: !Int !OSMenuBar !*OSToolbox -> *OSToolbox
osDisableMenu zIndex osMenuBar=:{menuBar} tb
	= winChangeMenuAbility menuBar zIndex False tb

osEnableMenu :: !Int !OSMenuBar !*OSToolbox -> *OSToolbox
osEnableMenu zIndex osMenuBar=:{menuBar} tb
	= winChangeMenuAbility menuBar zIndex True tb

osEnableMenuItem :: !OSMenu !OSMenuItem !*OSToolbox -> *OSToolbox
osEnableMenuItem menuHandle item tb
	= winChangeItemAbility menuHandle item True tb

osDisableMenuItem :: !OSMenu !OSMenuItem !*OSToolbox -> *OSToolbox
osDisableMenuItem menuHandle item tb
	= winChangeItemAbility menuHandle item False tb


//	Changing and updating the menu bar:

osDrawMenuBar :: !OSMenuBar !*OSToolbox -> *OSToolbox
osDrawMenuBar {menuWindow,menuClient} tb
	= winDrawMenuBar menuWindow (if (menuClient==OSNoWindowPtr) 0 menuClient) tb

osMenuBarClear :: !*OSToolbox -> *OSToolbox
osMenuBarClear tb
	= tb

osMenuBarSet :: !OSMenuBar !*OSToolbox -> (!OSMenuBar,!*OSToolbox)
osMenuBarSet menuBar tb
	= (menuBar,tb)
	
osMenuInsert :: !Int !OSMenuNr !{#Char} !OSMenuBar !*OSToolbox -> (!OSMenu,!OSMenuBar,!*OSToolbox)
osMenuInsert index osMenuNr title menuBar tb
	# (menu,tb) = winCreatePopupMenuHandle tb
	= (menu,menuBar,winInsertMenu title True menu menuBar.menuBar index tb)
	
osSubMenuInsert :: !Int !OSMenuNr !{#Char} !OSMenu !*OSToolbox -> (!OSMenu, !OSMenu, !*OSToolbox)
osSubMenuInsert index osMenuNr title parentMenu tb
	# (menu,tb) = winCreatePopupMenuHandle tb
	= (menu,parentMenu,winInsertMenu title True menu parentMenu index tb)

osMenuRemove :: !OSMenu !OSMenuBar !*OSToolbox -> (!OSMenuBar, !*OSToolbox)
osMenuRemove menu menuBar=:{menuBar=hmenu} tb
	# tb	= winDeleteMenu hmenu menu tb
	# tb	= winDestroyMenu menu tb
	= (menuBar,tb)

osSubMenuRemove :: !OSMenu !OSMenu !*OSToolbox -> (!OSMenu,!*OSToolbox)
osSubMenuRemove submenu hmenu tb
	# tb	= winDeleteMenu hmenu submenu tb
	# tb	= winDestroyMenu submenu tb
	= (hmenu,tb)

osCreatePopUpMenu :: !*OSToolbox -> (!OSMenu,!*OSToolbox)
osCreatePopUpMenu tb
	= winCreatePopupMenuHandle tb

osTrackPopUpMenu :: !OSMenu !OSWindowPtr !*OSToolbox -> (!Bool,!*OSToolbox)
osTrackPopUpMenu menu framePtr tb
	= winTrackPopupMenu menu framePtr tb


//	Changing (sub)menus:
osAppendMenuItem :: !OSMenuBar !Int !OSMenu !{#Char} !Bool !Bool !Char !*OSToolbox -> (!OSMenuItem,!OSMenu,!*OSToolbox)
osAppendMenuItem {menuWindow} index menu title able mark key tb
	# title		= if (key <> '\0')
					(title +++ "\tCtrl+" +++ toString (toUpper key))
					title
	# (item,tb)	= winInsertMenuItem title able mark menu index tb
	| key <> '\0'
		= (item,menu,winAddMenuShortKey menuWindow item key tb)
	| otherwise
		= (item,menu,tb)

osAppendMenuSeparator :: !Int !OSMenu !*OSToolbox -> (!OSMenuSeparator,!OSMenu,!*OSToolbox)
osAppendMenuSeparator index menu tb
	# tb	= winInsertSeparator menu index tb
	= (OSNoMenuSeparator,menu,tb)

osChangeMenuTitle :: !OSMenuBar !OSMenu !{#Char} !*OSToolbox -> *OSToolbox
osChangeMenuTitle {menuBar} menu title tb
	= winModifyMenu title menu menuBar tb

osChangeMenuItemTitle :: !OSMenu !OSMenuItem !{#Char} !*OSToolbox -> *OSToolbox
osChangeMenuItemTitle menu item title tb
	= winModifyMenuItem title item menu tb

osMenuItemCheck :: !Bool !OSMenu !OSMenuItem !*OSToolbox -> *OSToolbox
osMenuItemCheck check menu item tb
	= winChangeMenuItemCheck menu item check tb

osMenuRemoveItem :: !OSMenuItem !OSMenu !*OSToolbox -> (!OSMenu,!*OSToolbox)
osMenuRemoveItem item menu tb
	= (menu,winRemoveMenuItem menu item tb)


//	Validation of (sub)menu (element) attributes:

osValidateMenuItemTitle :: !{#Char} -> {#Char}
osValidateMenuItemTitle title = title


/*	Two functions that generate free OS ids for menus and sub menus.
	If the functions fail, then the Bool result is False, and the Int result is 0. 
	Do not continue to create the (sub)menu.
*/
::	OSMenuNr	:== Int
::	OSSubMenuNr	:== Int

osNewMenuNr :: !*OSToolbox -> (!Bool,!OSMenuNr,!*OSToolbox)
osNewMenuNr tb
	= (True,0,tb)

osNewSubMenuNr :: !*OSToolbox -> (!Bool,!OSSubMenuNr,!*OSToolbox)
osNewSubMenuNr tb
	= (True,0,tb)