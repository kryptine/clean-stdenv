definition module windowCCall_12


from	ostoolbox		import OSToolbox
from	ostypes			import HWND
from	rgnCCall_12		import HRGN
from	pictCCall_12	import HDC


WinInvalidateWindow	:: !HWND !*OSToolbox -> *OSToolbox
WinInvalidateRect	:: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
WinValidateRect		:: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
WinValidateRgn		:: !HWND !HRGN !*OSToolbox -> *OSToolbox

WinGetDC			:: !HWND !*OSToolbox -> (!HDC,!*OSToolbox)
WinReleaseDC		:: !HWND !(!HDC,!*OSToolbox) -> *OSToolbox
