implementation module osmenu


//	Clean Object I/O library, version 1.2


import	StdBool, StdChar, StdClass, StdInt, StdString
import	menuCrossCall_12
from	oswindow	import OSWindowPtr, OSNoWindowPtr


//	Types for menus and menu elements:
::	MenuBar
	=	NoMenuBar
	|	MenuBar OSMenuBar
::	OSMenuBar
	=	{	menuBar		:: !HMENU
		,	menuWindow	:: !HWND
		,	menuClient	:: !HWND		// If MDI: client window; otherwise: OSNoWindowPtr
		}
::	OSMenuHandle	:== HMENU
::	OSMenu			:== HMENU
::	OSMenuItem		:== HITEM
::	OSMenuSeparator	:== HITEM

//	Dummy values:
OSNoMenu :== 0
OSNoMenuItem :== 0
OSNoMenuSeparator :== 0


OSMenuBarNew :: !HWND !HWND !HMENU -> OSMenuBar
OSMenuBarNew frameWindow clientWindow menu
	= {menuBar=menu, menuWindow=frameWindow, menuClient=clientWindow}

//	Enabling and disabling menus and menu elements:

OSDisableMenu :: !Int !OSMenuBar !*OSToolbox -> *OSToolbox
OSDisableMenu zIndex osMenuBar=:{menuBar} tb
	= WinChangeMenuAbility menuBar zIndex False tb

OSEnableMenu :: !Int !OSMenuBar !*OSToolbox -> *OSToolbox
OSEnableMenu zIndex osMenuBar=:{menuBar} tb
	= WinChangeMenuAbility menuBar zIndex True tb

OSEnableMenuItem :: !OSMenu !OSMenuItem !*OSToolbox -> *OSToolbox
OSEnableMenuItem menuHandle item tb
	= WinChangeItemAbility menuHandle item True tb

OSDisableMenuItem :: !OSMenu !OSMenuItem !*OSToolbox -> *OSToolbox
OSDisableMenuItem menuHandle item tb
	= WinChangeItemAbility menuHandle item False tb


//	Changing and updating the menu bar:

DrawMenuBar :: !OSMenuBar !*OSToolbox -> *OSToolbox
DrawMenuBar {menuWindow,menuClient} tb
	= WinDrawMenuBar menuWindow (if (menuClient==OSNoWindowPtr) 0 menuClient) tb

OSMenuBarClear :: !*OSToolbox -> *OSToolbox
OSMenuBarClear tb
	= tb

OSMenuBarSet :: !OSMenuBar !*OSToolbox -> (!OSMenuBar,!*OSToolbox)
OSMenuBarSet menuBar tb
	= (menuBar,tb)
	
OSMenuInsert :: !Int !OSMenuNr !{#Char} !OSMenuBar !*OSToolbox -> (!OSMenu,!OSMenuBar,!*OSToolbox)
OSMenuInsert index osMenuNr title menuBar tb
	# (menu,tb) = WinCreatePopupMenuHandle tb
	= (menu,menuBar,WinInsertMenu title True menu menuBar.menuBar index tb)
	
OSSubMenuInsert :: !Int !OSMenuNr !{#Char} !OSMenu !*OSToolbox -> (!OSMenu, !OSMenu, !*OSToolbox)
OSSubMenuInsert index osMenuNr title parentMenu tb
	# (menu,tb) = WinCreatePopupMenuHandle tb
	= (menu,parentMenu,WinInsertMenu title True menu parentMenu index tb)

OSMenuRemove :: !OSMenu !OSMenuBar !*OSToolbox -> (!OSMenuBar, !*OSToolbox)
OSMenuRemove menu menuBar=:{menuBar=hmenu} tb
	# tb	= WinDeleteMenu hmenu menu tb
	# tb	= WinDestroyMenu menu tb
	= (menuBar,tb)

OSSubMenuRemove :: !OSMenu !OSMenu !*OSToolbox -> (!OSMenu,!*OSToolbox)
OSSubMenuRemove submenu hmenu tb
	# tb	= WinDeleteMenu hmenu submenu tb
	# tb	= WinDestroyMenu submenu tb
	= (hmenu,tb)

OScreatePopUpMenu :: !*OSToolbox -> (!OSMenu,!*OSToolbox)
OScreatePopUpMenu tb
	= WinCreatePopupMenuHandle tb

OStrackPopUpMenu :: !OSMenu !OSWindowPtr !*OSToolbox -> (!Bool,!*OSToolbox)
OStrackPopUpMenu menu framePtr tb
	= WinTrackPopupMenu menu framePtr tb


//	Changing (sub)menus:
OSAppendMenuItem :: !OSMenuBar !Int !OSMenu !{#Char} !Bool !Bool !Char !*OSToolbox -> (!OSMenuItem,!OSMenu,!*OSToolbox)
OSAppendMenuItem {menuWindow} index menu title able mark key tb
	# title		= if (key <> '\0')
					(title +++ "\tCtrl+" +++ toString (toUpper key))
					title
	# (item,tb)	= WinInsertMenuItem title able mark menu index tb
	| key <> '\0'
		= (item,menu,WinAddMenuShortKey menuWindow item key tb)
	| otherwise
		= (item,menu,tb)

OSAppendMenuSeparator :: !Int !OSMenu !*OSToolbox -> (!OSMenuSeparator,!OSMenu,!*OSToolbox)
OSAppendMenuSeparator index menu tb
	# tb	= WinInsertSeparator menu index tb
	= (OSNoMenuSeparator,menu,tb)

OSChangeMenuTitle :: !OSMenuBar !OSMenu !{#Char} !*OSToolbox -> *OSToolbox
OSChangeMenuTitle {menuBar} menu title tb
	= WinModifyMenu title menu menuBar tb

OSChangeMenuItemTitle :: !OSMenu !OSMenuItem !{#Char} !*OSToolbox -> *OSToolbox
OSChangeMenuItemTitle menu item title tb
	= WinModifyMenuItem title item menu tb

OSMenuItemCheck :: !Bool !OSMenu !OSMenuItem !*OSToolbox -> *OSToolbox
OSMenuItemCheck check menu item tb
	= WinChangeMenuItemCheck menu item check tb

OSMenuRemoveItem :: !OSMenuItem !OSMenu !*OSToolbox -> (!OSMenu,!*OSToolbox)
OSMenuRemoveItem item menu tb
	= (menu,WinRemoveMenuItem menu item tb)


//	Validation of (sub)menu (element) attributes:

OSValidateMenuItemTitle :: !{#Char} -> {#Char}
OSValidateMenuItemTitle title = title


/*	Two functions that generate free OS ids for menus and sub menus.
	If the functions fail, then the Bool result is False, and the Int result is 0. 
	Do not continue to create the (sub)menu.
*/
::	OSMenuNr	:== Int
::	OSSubMenuNr	:== Int

OSNewMenuNr :: !*OSToolbox -> (!Bool,!OSMenuNr,!*OSToolbox)
OSNewMenuNr tb
	= (True,0,tb)

OSNewSubMenuNr :: !*OSToolbox -> (!Bool,!OSSubMenuNr,!*OSToolbox)
OSNewSubMenuNr tb
	= (True,0,tb)
