definition module menuwindowmenu


//	Version 1.2

//	The definition and implementation of the WindowMenu. 


from	iostate import PSt, IOSt
import	windowhandle


openWindowMenu							::										 !( PSt .l .p) ->  PSt .l .p
addWindowToWindowMenu					:: !Id !Title							 !(IOSt .l .p) -> IOSt .l .p
removeWindowFromWindowMenu				:: !Id									 !(IOSt .l .p) -> IOSt .l .p
validateWindowActivateForWindowMenu		:: !Id !(DialogLSHandle .ls (PSt .l .p)) !(IOSt .l .p)
											-> (!DialogLSHandle .ls (PSt .l .p), ! IOSt .l .p)
