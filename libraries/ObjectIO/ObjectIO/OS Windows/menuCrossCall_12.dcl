definition module menuCrossCall_12


from	StdString	import String
from	ostoolbox	import OSToolbox
from	ostypes		import HWND


::	HITEM	:==  Int
::	HMENU	:==  Int


WinCreatePopupMenuHandle::										!*OSToolbox -> (!HMENU, !*OSToolbox)
WinTrackPopupMenu		:: !HMENU !HWND							!*OSToolbox -> (!Bool,  !*OSToolbox)	// Bool: if True then item is selected; otherwise menu closed
WinInsertMenu			:: !String !Bool !HMENU !HMENU !Int		!*OSToolbox -> *OSToolbox
WinInsertMenuItem       :: !String !Bool !Bool !HMENU !Int		!*OSToolbox -> (!HITEM, !*OSToolbox)
WinInsertSeparator		:: !HMENU !Int							!*OSToolbox -> *OSToolbox
WinChangeMenuItemCheck  :: !HMENU !HITEM !Bool					!*OSToolbox -> *OSToolbox
WinModifyMenu			:: !String !HMENU !HMENU				!*OSToolbox -> *OSToolbox
WinModifyMenuItem       :: !String !HITEM !HMENU				!*OSToolbox -> *OSToolbox
WinDestroyMenu			:: !HMENU								!*OSToolbox -> *OSToolbox	// PA: new as crosscall
WinDeleteMenu			:: !HMENU !HITEM						!*OSToolbox -> *OSToolbox	// PA: new as crosscall
WinRemoveMenuItem       :: !HMENU !HITEM						!*OSToolbox -> *OSToolbox
WinChangeItemAbility    :: !HMENU !HITEM !Bool					!*OSToolbox -> *OSToolbox
WinChangeMenuAbility	:: !HMENU !Int   !Bool					!*OSToolbox -> *OSToolbox
WinDrawMenuBar			:: !HWND !HWND							!*OSToolbox -> *OSToolbox
WinAddMenuShortKey		:: !HWND !Int !Char						!*OSToolbox -> *OSToolbox	// PA: new
WinRemoveMenuShortKey	:: !HWND !Int							!*OSToolbox -> *OSToolbox	// PA: new
