definition module osmenu


//	Clean Object I/O library, version 1.2


from	menuCrossCall_12	import HMENU, HITEM
from	osdocumentinterface	import OSMenuBar
from	ostoolbox			import OSToolbox
from	ostypes				import HWND
from	oswindow			import OSWindowPtr


//	Types for menus and menu elements:
/*::	MenuBar
	=	NoMenuBar
	|	MenuBar OSMenuBar
::	OSMenuBar
	=	{	menuBar		:: !HMENU
		,	menuWindow	:: !HWND
		,	menuClient	:: !HWND		// If MDI: client window; otherwise: OSNoWindowPtr
		}
*/
::	OSMenu			:== HMENU
::	OSMenuItem		:== HITEM
::	OSMenuSeparator	:== HITEM

//	Dummy values:
OSNoMenu			:== 0
OSNoMenuItem		:== 0
OSNoMenuSeparator	:== 0


/*	Creation of a OSMenuBar:
	OSMenuBarNew frameWindow clientWindow menu
		creates an OSMenuBar instance that can be used to manipulate menus.
PA---
OSMenuBarNew		:: !HWND !HWND !HMENU -> OSMenuBar
*/

/*	Enabling and disabling of menus and menu elements:
	OS(Dis/En)ableMenu index menubar
		(dis/en)ables the top-level menu at the zero based index position of the menubar.
	OS(Dis/En)ableMenuItem parentMenu menuitem 
		(dis/en)ables the menuitem that is part of the parentMenu.
*/
OSDisableMenu		:: !Int !OSMenuBar		!*OSToolbox -> *OSToolbox
OSEnableMenu		:: !Int !OSMenuBar		!*OSToolbox -> *OSToolbox
OSEnableMenuItem	:: !OSMenu !OSMenuItem	!*OSToolbox -> *OSToolbox
OSDisableMenuItem	:: !OSMenu !OSMenuItem	!*OSToolbox -> *OSToolbox


/*	Changing and updating the menubar:
	DrawMenuBar
		redraws the menubar. This must be done after every change of the menubar (adding/removing).
	OSMenuBarClear
		clears the menubar.
	OSMenuBarSet
		dunno??
	OSMenuInsert index menuNr title menubar 
		creates and inserts a new top-level menu at the indicated zero based index position.
		The new menu has the given title and the menuNr as retrieved by OSNewMenuNr (below).
	OSSubMenuInsert index menuNr title parentMenu 
		creates and inserts a new submenu at the indicated zero based index position.
		The new submenu has the given title and the menuNr as retrieved by OSNewSubMenuNr (below).
	OSMenuRemove menu menubar
		removes the indicated menu both 'logically' and 'physically' from the menubar.
	OSSubMenuRemove submenu parentMenu
		removes the submenu both 'logically' and 'physically' from the parentMenu.
*/
DrawMenuBar			:: !OSMenuBar							!*OSToolbox -> *OSToolbox
OSMenuBarClear		:: !*OSToolbox -> *OSToolbox
OSMenuBarSet		:: !OSMenuBar							!*OSToolbox -> (!OSMenuBar, !*OSToolbox)
OSMenuInsert		:: !Int !OSMenuNr !{#Char} !OSMenuBar	!*OSToolbox -> (!OSMenu,!OSMenuBar,	!*OSToolbox)
OSSubMenuInsert		:: !Int !OSMenuNr !{#Char} !OSMenu		!*OSToolbox -> (!OSMenu,!OSMenu,	!*OSToolbox)
OSMenuRemove		:: !OSMenu !OSMenuBar					!*OSToolbox -> (!OSMenuBar,			!*OSToolbox)
OSSubMenuRemove		:: !OSMenu !OSMenu						!*OSToolbox -> (!OSMenu,			!*OSToolbox)

/*	PopUpMenu functions:
	OScreatePopUpMenu creates a pop up menu.
	OStrackPopUpMenu  shows the pop up menu and handles user selection. 
		The Bool result is False if nothing was selected; True otherwise.
*/
OScreatePopUpMenu	:: !*OSToolbox -> (!OSMenu,!*OSToolbox)
OStrackPopUpMenu	:: !OSMenu !OSWindowPtr !*OSToolbox -> (!Bool,!*OSToolbox)


/*	Changing a (sub)menus and menu elements:
	OSAppendMenuItem osmenubar index menu title able mark key
		adds a new menuitem to the given menu at the indicated zero based index position.
		The menuitem has the given title, selectstate, markstate, and shortcut key.
		The menu is element of the given osmenubar. 
	OSAppendMenuSeparator index menu
		adds a new menuseparator to the given menu at the indicated zero based index position.
	OSChangeMenuTitle menubar menu title
		sets the new title of the indicated top-level menu in the menubar.
	OSChangeMenuItemTitle parentMenu menuitem title
		sets the new title of the indicated menuitem/submenu contained in the parentMenu.
	OSMenuItemCheck check parentMenu menuitem
		marks the item iff check of the indicated menuitem contained in the parentMenu.
	OSMenuRemoveItem menuitem parentMenu
		removes the menuitem 'logically' from the indicated parentMenu. The menuitem is not destroyed. CHECK APPLICATIONS!!
*/
OSAppendMenuItem		:: !OSMenuBar !Int !OSMenu !{#Char} !Bool !Bool !Char
														!*OSToolbox -> (!OSMenuItem,	 !OSMenu,!*OSToolbox)
OSAppendMenuSeparator	:: !Int !OSMenu					!*OSToolbox -> (!OSMenuSeparator,!OSMenu,!*OSToolbox)
OSChangeMenuTitle		:: !OSMenuBar !OSMenu  !{#Char}	!*OSToolbox -> *OSToolbox
OSChangeMenuItemTitle	:: !OSMenu !OSMenuItem !{#Char}	!*OSToolbox -> *OSToolbox
OSMenuItemCheck			:: !Bool !OSMenu !OSMenuItem	!*OSToolbox -> *OSToolbox
OSMenuRemoveItem		:: !OSMenuItem !OSMenu			!*OSToolbox -> (!OSMenu,!*OSToolbox)


/*	Validation of (sub)menu (element) attributes:
*/
OSValidateMenuItemTitle :: !{#Char} -> {#Char}


/*	Two functions that generate free OS ids for menus and sub menus.
	If the functions fail, then the Bool result is False, and the Int result is 0. 
	Do not continue to create the (sub)menu.
*/
::	OSMenuNr	:== Int
::	OSSubMenuNr	:== Int

OSNewMenuNr		:: !*OSToolbox -> (!Bool,!OSMenuNr,   !*OSToolbox)
OSNewSubMenuNr	:: !*OSToolbox -> (!Bool,!OSSubMenuNr,!*OSToolbox)
