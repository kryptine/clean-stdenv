implementation module rgnCCall_12


from	ostoolbox	import OSToolbox


::	HRGN	:==  Int


//	PA: CombineRgn() Styles.
RGN_AND				:== 1
RGN_OR				:== 2
RGN_XOR				:== 3
RGN_DIFF			:== 4
RGN_COPY			:== 5
//	PA: end of addition.


/*	PA: operations to create, modify and destroy regions.
*/
WinCreateRectRgn :: !Int !Int !Int !Int !*OSToolbox -> (!HRGN,!*OSToolbox)
WinCreateRectRgn _ _ _ _ _
	= code
	{	
		.inline WinCreateRectRgn
			ccall WinCreateRectRgn "IIIII-II"
		.end
	}

WinCreatePolygonRgn :: !Int !Int !Int !*OSToolbox -> (!HRGN,!*OSToolbox)
WinCreatePolygonRgn _ _ _ _
	= code
	{	
		.inline WinCreatePolygonRgn
			ccall WinCreatePolygonRgn "IIII-II"
		.end
	}

WinSetRgnToRect :: !Int !Int !Int !Int !HRGN !*OSToolbox -> (!HRGN,!*OSToolbox)
WinSetRgnToRect _ _ _ _ _ _
	= code
	{	
		.inline WinSetRgnToRect
			ccall WinSetRgnToRect "IIIIII-II"
		.end
	}

WinCombineRgn :: !HRGN !HRGN !HRGN !Int !*OSToolbox -> (!HRGN,!*OSToolbox)
WinCombineRgn _ _ _ _ _
	= code
	{	
		.inline WinCombineRgn
			ccall WinCombineRgn "IIIII-II"
		.end
	}

WinGetRgnBox :: !HRGN !*OSToolbox -> (!Int,!Int,!Int,!Int,!Bool,!Bool,!*OSToolbox)
WinGetRgnBox _ _
	= code
	{	
		.inline WinGetRgnBox
			ccall WinGetRgnBox "II-IIIIIII"
		.end
	}
