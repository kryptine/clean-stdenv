definition module StdMenuElementClass


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdMenuElementClass defines the standard set of menu element instances.
//	********************************************************************************


import	StdMenuDef
from	StdPSt		import PSt, IOSt
from	menuhandle	import MenuElementState


/*	Menu elements for Menus:
*/
class MenuElements m where
	menuElementToHandles	:: !(m .ls (PSt .l .p)) !(PSt .l .p)
			-> (![MenuElementState .ls (PSt .l .p)], !PSt .l .p)
	getMenuElementType		::  (m .ls .pst)
			-> MenuElementType

instance MenuElements (AddLS	m)	| MenuElements m	// getMenuElementType==""
instance MenuElements (NewLS	m)	| MenuElements m	// getMenuElementType==""
instance MenuElements (ListLS	m)	| MenuElements m	// getMenuElementType==""
instance MenuElements NilLS								// getMenuElementType==""
instance MenuElements ((:+:) m1 m2)	| MenuElements m1
									& MenuElements m2	// getMenuElementType==""
instance MenuElements (SubMenu	m)	| MenuElements m
instance MenuElements RadioMenu
instance MenuElements MenuItem
instance MenuElements MenuSeparator

/*	Menu elements for PopUpMenus:
*/
class PopUpMenuElements m where
	popUpMenuElementToHandles	:: !(m .ls (PSt .l .p)) !(PSt .l .p)
				-> (![MenuElementState .ls (PSt .l .p)], !PSt .l .p)
	getPopUpMenuElementType		::  (m .ls .pst)
				-> MenuElementType

instance PopUpMenuElements (AddLS	m)		| PopUpMenuElements m	// getPopUpMenuElementType==""
instance PopUpMenuElements (NewLS	m)		| PopUpMenuElements m	// getPopUpMenuElementType==""
instance PopUpMenuElements (ListLS	m)		| PopUpMenuElements m	// getPopUpMenuElementType==""
instance PopUpMenuElements NilLS									// getPopUpMenuElementType==""
instance PopUpMenuElements ((:+:) m1 m2)	| PopUpMenuElements m1
											& PopUpMenuElements m2	// getPopUpMenuElementType==""
instance PopUpMenuElements RadioMenu
instance PopUpMenuElements MenuItem
instance PopUpMenuElements MenuSeparator
