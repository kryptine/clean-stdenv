implementation module windowCCall_12


from	ostoolbox		import OSToolbox
from	ostypes			import HWND
from	rgnCCall_12		import HRGN
from	pictCCall_12	import HDC


WinInvalidateWindow :: !HWND !*OSToolbox -> *OSToolbox
WinInvalidateWindow _ _
	= code
	{	
		.inline WinInvalidateWindow
			ccall WinInvalidateWindow "II-I"
		.end
	}

WinInvalidateRect :: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
WinInvalidateRect hwnd (left,top, right,bottom) tb
	= code
	{	
		.inline WinInvalidateRect
			ccall WinInvalidateRect "IIIIII-I"
		.end
	}

WinValidateRect :: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
WinValidateRect hwnd (left,top, right,bottom) tb
	= code
	{	
		.inline WinValidateRect
			ccall WinValidateRect "IIIIII-I"
		.end
	}

WinValidateRgn :: !HWND !HRGN !*OSToolbox -> *OSToolbox
WinValidateRgn hwnd rgn tb
	= code
	{	
		.inline WinValidateRgn
			ccall WinValidateRgn "III-I"
		.end
	}

WinGetDC :: !HWND !*OSToolbox -> (!HDC,!*OSToolbox)
WinGetDC _ _
	= code
	{
		.inline WinGetDC
			ccall WinGetDC "II-II"
		.end
	}

WinReleaseDC :: !HWND !(!HDC,!*OSToolbox) -> *OSToolbox
WinReleaseDC hwnd (hdc,tb)
	= code
	{
		.inline WinReleaseDC
			ccall WinReleaseDC "III-I"
		.end
	}
