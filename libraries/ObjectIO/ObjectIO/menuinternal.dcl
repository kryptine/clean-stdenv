definition module menuinternal


//	Clean Object I/O library, version 1.2

//	The actual implementation of most of the StdMenu functions.


import	StdMenuElementClass
from	iostate		import IOSt, SystemId


RemoveSpecialMenuElements	:==	True		// For closemenuindexelements:        remove elements with special ids
NotRemoveSpecialMenuElements:==	False		// For closemenuindexelements: do not remove elements with special ids

closemenu				:: !Id												!(IOSt .l .p) -> IOSt .l .p
closemenuelements		:: !Id ![Id]										!(IOSt .l .p) -> IOSt .l .p
closemenuindexelements	:: !Bool !Bool !SystemId !(!Id,!Maybe Id) ![Index]	!(IOSt .l .p) -> IOSt .l .p
enablemenus				:: ![Id]											!(IOSt .l .p) -> IOSt .l .p
disablemenus			:: ![Id]											!(IOSt .l .p) -> IOSt .l .p
setmenutitle			:: !Id !Title										!(IOSt .l .p) -> IOSt .l .p
