definition module menuinternal


//	Clean Object I/O library, version 1.2

//	The actual implementation of most of the StdMenu functions.


import	StdMenuElementClass
from	iostate		import IOSt, SystemId


RemoveSpecialMenuElements	:==	True		// For closemenuindexelements:        remove elements with special ids
NotRemoveSpecialMenuElements:==	False		// For closemenuindexelements: do not remove elements with special ids

closemenu				:: !Id												!(IOSt .l) -> IOSt .l
closemenuelements		:: !Id ![Id]										!(IOSt .l) -> IOSt .l
closemenuindexelements	:: !Bool !Bool !SystemId !(!Id,!Maybe Id) ![Index]	!(IOSt .l) -> IOSt .l
enablemenus				:: ![Id]											!(IOSt .l) -> IOSt .l
disablemenus			:: ![Id]											!(IOSt .l) -> IOSt .l
setmenutitle			:: !Id !Title										!(IOSt .l) -> IOSt .l
