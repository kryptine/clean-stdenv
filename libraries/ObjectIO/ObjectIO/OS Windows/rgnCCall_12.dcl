definition module rgnCCall_12


from	ostoolbox	import OSToolbox


::	HRGN	:==  Int


//	PA: CombineRgn() Styles.
RGN_AND				:== 1
RGN_OR				:== 2
RGN_XOR				:== 3
RGN_DIFF			:== 4
RGN_COPY			:== 5
//	PA: end of addition.


// PA: operations to create, modify and destroy regions.
WinCreateRectRgn		:: !Int !Int !Int !Int !*OSToolbox -> (!HRGN,!*OSToolbox)
WinCreatePolygonRgn		:: !Int !Int !Int !*OSToolbox -> (!HRGN,!*OSToolbox)
WinSetRgnToRect			:: !Int !Int !Int !Int !HRGN !*OSToolbox -> (!HRGN,!*OSToolbox)
WinCombineRgn			:: !HRGN !HRGN !HRGN !Int !*OSToolbox -> (!HRGN,!*OSToolbox)
WinGetRgnBox			:: !HRGN !*OSToolbox -> (!Int,!Int,!Int,!Int,!Bool,!Bool,!*OSToolbox)
