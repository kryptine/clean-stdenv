definition module menuitems


//	Clean Object I/O library, version 1.2


import	StdMenuElementClass
import	menuhandle
from	id					import IdTable
from	iostate				import PSt, IOSt
from	receivertable		import ReceiverTable
from	systemid			import SystemId
from	osdocumentinterface	import OSDInfo
from	ostoolbox			import OSToolbox


//	Adding and removing menu elements to and from existing menus.

addMenusItems		:: !(!Id,Maybe Id) !Int .ls` (m .ls` (PSt .l .p)) !SystemId !ReceiverTable !IdTable !(MenuHandles (PSt .l .p)) !(PSt .l .p)
															 -> (!(!ErrorReport,!ReceiverTable,!IdTable),!MenuHandles (PSt .l .p),  !PSt .l .p)
															 |  MenuElements m
/*	addMenusItems (id,Just subid) _ _ items ...
		adds items to the SubMenu identified by subid which is contained in the Menu identified by id.
	addMenusItems (id,Nothing)    _ _ items ...
		adds items to the Menu identified by id.
*/

addMenuRadioItems :: !(!Id,Id) !Int [MenuRadioItem (PSt .l .p)] !(MenuHandles (PSt .l .p)) !*OSToolbox
												-> (!ErrorReport,!MenuHandles (PSt .l .p), !*OSToolbox)
/*	addMenuRadioItems (id,radioid) _ items ...
		adds items to the RadioMenu identified by radioid which is contained in the Menu identified by id.
*/


//	Adding and removing menu elements to and from existing menus.

removeMenusItems		:: !OSDInfo !Id ![Id] !SystemId !ReceiverTable !IdTable !(MenuHandles .ps) !*OSToolbox
												  -> (!(!ReceiverTable,!IdTable),!MenuHandles .ps, !*OSToolbox)
/*	removeMenusItems ...
*/

removeMenusIndexItems	:: !OSDInfo !Bool !Bool !(!Id,!Maybe Id) ![Int] !SystemId !ReceiverTable !IdTable !(MenuHandles .ps) !*OSToolbox
																			-> (!(!ReceiverTable,!IdTable),!MenuHandles .ps, !*OSToolbox)
/*	removeMenusIndexItems ...
*/
