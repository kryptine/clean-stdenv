definition module osmenu


//	Clean Object I/O library, version 1.2


from	menuCrossCall_12	import HMENU, HITEM
from	osdocumentinterface	import OSMenuBar
from	ostoolbox			import OSToolbox
from	ostypes				import HWND, OSWindowPtr


//	Types for menus and menu elements:
::	OSMenu			:== HMENU
::	OSMenuItem		:== HITEM
::	OSMenuSeparator	:== HITEM

//	Dummy values:
OSNoMenu			:== 0
OSNoMenuItem		:== 0
OSNoMenuSeparator	:== 0


/*	Initialisation:
*/
osInitialiseMenus	:: !*OSToolbox -> *OSToolbox


/*	Enabling and disabling of menus and menu elements:
	os(Dis/En)ableMenu index menubar
		(dis/en)ables the top-level menu at the zero based index position of the menubar.
	os(Dis/En)ableMenuItem parentMenu menuitem 
		(dis/en)ables the menuitem that is part of the parentMenu.
*/
osDisableMenu		:: !Int !OSMenuBar		!*OSToolbox -> *OSToolbox
osEnableMenu		:: !Int !OSMenuBar		!*OSToolbox -> *OSToolbox
osEnableMenuItem	:: !OSMenu !OSMenuItem	!*OSToolbox -> *OSToolbox
osDisableMenuItem	:: !OSMenu !OSMenuItem	!*OSToolbox -> *OSToolbox


/*	Changing and updating the menubar:
	osDrawMenuBar
		redraws the menubar. This must be done after every change of the menubar (adding/removing).
	osMenuBarClear
		clears the menubar.
	osMenuBarSet
		dunno??
	osMenuInsert index menuNr title menubar 
		creates and inserts a new top-level menu at the indicated zero based index position.
		The new menu has the given title and the menuNr as retrieved by osNewMenuNr (below).
	osSubMenuInsert index menuNr title parentMenu 
		creates and inserts a new submenu at the indicated zero based index position.
		The new submenu has the given title and the menuNr as retrieved by osNewSubMenuNr (below).
	osMenuRemove menu menubar
		removes the indicated menu both 'logically' and 'physically' from the menubar.
	osSubMenuRemove submenu parentMenu
		removes the submenu both 'logically' and 'physically' from the parentMenu.
*/
osDrawMenuBar		:: !OSMenuBar							!*OSToolbox -> *OSToolbox
osMenuBarClear		:: !*OSToolbox -> *OSToolbox
osMenuBarSet		:: !OSMenuBar							!*OSToolbox -> (!OSMenuBar, !*OSToolbox)
osMenuInsert		:: !Int !OSMenuNr !{#Char} !OSMenuBar	!*OSToolbox -> (!OSMenu,!OSMenuBar,	!*OSToolbox)
osSubMenuInsert		:: !Int !OSMenuNr !{#Char} !OSMenu		!*OSToolbox -> (!OSMenu,!OSMenu,	!*OSToolbox)
osMenuRemove		:: !OSMenu !OSMenuBar					!*OSToolbox -> (!OSMenuBar,			!*OSToolbox)
osSubMenuRemove		:: !OSMenu !OSMenu						!*OSToolbox -> (!OSMenu,			!*OSToolbox)

/*	PopUpMenu functions:
	osCreatePopUpMenu creates a pop up menu.
	osTrackPopUpMenu  shows the pop up menu and handles user selection. 
		The Bool result is False if nothing was selected; True otherwise.
*/
osCreatePopUpMenu	:: !*OSToolbox -> (!OSMenu,!*OSToolbox)
osTrackPopUpMenu	:: !OSMenu !OSWindowPtr !*OSToolbox -> (!Bool,!*OSToolbox)


/*	Changing a (sub)menus and menu elements:
	osAppendMenuItem osmenubar index menu title able mark key
		adds a new menuitem to the given menu at the indicated zero based index position.
		The menuitem has the given title, selectstate, markstate, and shortcut key.
		The menu is element of the given osmenubar. 
	osAppendMenuSeparator index menu
		adds a new menuseparator to the given menu at the indicated zero based index position.
	osChangeMenuTitle menubar menu title
		sets the new title of the indicated top-level menu in the menubar.
	osChangeMenuItemTitle parentMenu menuitem title
		sets the new title of the indicated menuitem/submenu contained in the parentMenu.
	osMenuItemCheck check parentMenu menuitem
		marks the item iff check of the indicated menuitem contained in the parentMenu.
	osMenuRemoveItem menuitem parentMenu
		removes the menuitem 'logically' from the indicated parentMenu. The menuitem is not destroyed. CHECK APPLICATIONS!!
*/
osAppendMenuItem		:: !OSMenuBar !Int !OSMenu !{#Char} !Bool !Bool !Char
														!*OSToolbox -> (!OSMenuItem,	 !OSMenu,!*OSToolbox)
osAppendMenuSeparator	:: !Int !OSMenu					!*OSToolbox -> (!OSMenuSeparator,!OSMenu,!*OSToolbox)
osChangeMenuTitle		:: !OSMenuBar !OSMenu  !{#Char}	!*OSToolbox -> *OSToolbox
osChangeMenuItemTitle	:: !OSMenu !OSMenuItem !{#Char}	!*OSToolbox -> *OSToolbox
osMenuItemCheck			:: !Bool !OSMenu !OSMenuItem	!*OSToolbox -> *OSToolbox
osMenuRemoveItem		:: !OSMenuItem !OSMenu			!*OSToolbox -> (!OSMenu,!*OSToolbox)


/*	Validation of (sub)menu (element) attributes:
*/
osValidateMenuItemTitle :: !{#Char} -> {#Char}


/*	Two functions that generate free OS ids for menus and sub menus.
	If the functions fail, then the Bool result is False, and the Int result is 0. 
	Do not continue to create the (sub)menu.
*/
::	OSMenuNr	:== Int
::	OSSubMenuNr	:== Int

osNewMenuNr		:: !*OSToolbox -> (!Bool,!OSMenuNr,   !*OSToolbox)
osNewSubMenuNr	:: !*OSToolbox -> (!Bool,!OSSubMenuNr,!*OSToolbox)
