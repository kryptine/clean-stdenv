definition module menucreate

//	Clean Object I/O library, version 1.2

import	StdMenuElementClass
import	menuhandle
from	iostate		import PSt, IOSt
from	ostoolbox	import OSToolbox
from	oswindow	import OSWindowPtr

/*	Creating menus:
	In case the Boolean result is False nothing has been created due to duplicate Ids.
*/
OpenMenu`		:: !Id       .ls !(Menu      m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | MenuElements m
createPopUpMenu :: !SystemId .ls !(PopUpMenu m .ls (PSt .l)) !(MenuHandles (PSt .l)) !ReceiverTable !IdTable !OSMenuBar !(PSt .l)
													-> (!Bool,!MenuHandles (PSt .l), !ReceiverTable,!IdTable,!OSMenuBar, !PSt .l)
													|  PopUpMenuElements m

// checkCurrentMenuSystem		::				  !Bool !*OSToolbox -> (!OSMenu,!*OSToolbox)
// redrawOnlyActiveMenuSystem	:: !OSMenu !Bool !*OSToolbox -> *OSToolbox

//	Build a new menu (createMenuElements), and extend an existing menu (extendMenu):

createMenuElements	:: !OSMenuBar !OSMenu !Int ![MenuElementHandle .ls .pst] ![Char] !*OSToolbox
									  -> (!Int,![MenuElementHandle .ls .pst],![Char],!*OSToolbox)
extendMenu			:: !OSMenuBar !OSMenu !Int ![MenuElementHandle .ls .pst] ![MenuElementHandle .ls .pst] ![Char] !*OSToolbox
																		 -> (![MenuElementHandle .ls .pst],![Char],!*OSToolbox)

// InsertPullDownPosition	:== 0
// InsertSubPosition		:== -1	
SystemAble				:== True
SystemUnable			:== False

closepopupmenu			:: !(MenuHandles .pst) -> MenuHandles .pst
disposeMenuItemHandle	:: !OSMenu !Int	!(MenuItemHandle    .ls .pst) !(![Char],!IdTable,!*OSToolbox) -> (![Char],!IdTable,!*OSToolbox)
disposeSubMenuHandles	::				!(MenuElementHandle .ls .pst)                    !*OSToolbox  -> *OSToolbox
disposeMenuHandles		:: !Bool		!(MenuHandles			.pst) !OSMenuBar         !*OSToolbox  -> (!OSMenuBar,!*OSToolbox)
disposeShortcutkeys		:: !OSWindowPtr !(MenuElementHandle .ls .pst) !(![Char],         !*OSToolbox) -> (![Char],   !*OSToolbox)
disposeMenuIds			:: !SystemId	!(MenuElementHandle .ls .pst) !(!ReceiverTable,  !IdTable)    -> (!ReceiverTable,!IdTable)


// GetMenuSystem			:: !(MenuHandles .pst) !*OSToolbox -> (!MenuHandles	.pst,!*OSToolbox)
// SetMenuSystem			:: !(MenuHandles .pst) !*OSToolbox -> (!MenuHandles	.pst,!*OSToolbox)

/*	validateMenu(Item)Title checks for empty titles and for titles that begin with a '-'.
	It furthermore removes single appearences of '&' and transforms '&&' to '&'.
*/
//validateMenuTitle		:: !String -> String
//validateMenuItemTitle	:: !String -> String
