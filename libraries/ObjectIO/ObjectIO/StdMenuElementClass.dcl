definition module StdMenuElementClass


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	StdMenuElementClass defines the standard set of menu element instances.
//	Author: Peter Achten
//	Modified: 8 October 2001 for Clean 2.0
//	********************************************************************************


import	StdMenuDef
from	StdPSt		import PSt, IOSt
from	menuhandle	import MenuElementState


/*	Menu elements for Menus:
*/
class MenuElements m where
	menuElementToHandles	:: !.(m .ls (PSt .l)) !(PSt .l)
			 -> (![MenuElementState .ls (PSt .l)], !PSt .l)
	getMenuElementType		::  .(m .ls .pst)
			 -> MenuElementType

instance MenuElements (AddLS	m)	| MenuElements m
instance MenuElements (NewLS	m)	| MenuElements m
instance MenuElements (ListLS	m)	| MenuElements m
instance MenuElements NilLS
instance MenuElements ((:+:) m1 m2)	| MenuElements m1
									& MenuElements m2
instance MenuElements (SubMenu	m)	| MenuElements m
instance MenuElements RadioMenu
instance MenuElements MenuItem
instance MenuElements MenuSeparator

/*	Menu elements for PopUpMenus:
*/
class PopUpMenuElements m where
	popUpMenuElementToHandles	:: !.(m .ls (PSt .l)) !(PSt .l)
				 -> (![MenuElementState .ls (PSt .l)], !PSt .l)
	getPopUpMenuElementType		::  .(m .ls .pst)
				 -> MenuElementType

instance PopUpMenuElements (AddLS	m)		| PopUpMenuElements m
instance PopUpMenuElements (NewLS	m)		| PopUpMenuElements m
instance PopUpMenuElements (ListLS	m)		| PopUpMenuElements m
instance PopUpMenuElements NilLS
instance PopUpMenuElements ((:+:) m1 m2)	| PopUpMenuElements m1
											& PopUpMenuElements m2
instance PopUpMenuElements RadioMenu
instance PopUpMenuElements MenuItem
instance PopUpMenuElements MenuSeparator

instance PopUpMenuElements (SubMenu	m)		| PopUpMenuElements m
