definition module pictCCall_12


from	rgnCCall_12	import HRGN
from	ostoolbox	import OSToolbox
from	ostypes		import Rect


::	*PIC
	:== (	!HDC
		,	!*OSToolbox
		)
::	HDC
	:== Int
::	Pt
	:== (	!Int
		,	!Int
		)
::	RGBcolor
	:==	(	!Int
		,	!Int
		,	!Int
		)
::	Fnt
	:== (	!{#Char}
		,	!Int
		,	!Int
		)


iWhitePattern		:== 4
iLtGreyPattern		:== 3
iGreyPattern		:== 2
iDkGreyPattern		:== 1
iBlackPattern		:== 0

iModeNotBic			:== 7
iModeNotXor			:== 6
iModeNotOr			:== 5
iModeNotCopy		:== 4
iModeBic			:== 3
iModeXor			:== 2
iModeOr				:== 1
iModeCopy			:== 0

iStrikeOut			:== 8
iUnderline			:== 4
iItalic				:== 2
iBold				:== 1

//	PA: constants for drawing polygons.
ALTERNATE			:== 1
WINDING				:== 2
//	PA: end of addition.


/*	Win(Create/Destroy)ScreenHDC added to temporarily create a HDC of a screen.
	Never use these values for a window or control.
*/
WinCreateScreenHDC		:: !*OSToolbox -> PIC
WinDestroyScreenHDC		:: !PIC -> *OSToolbox

WinGetPicStringWidth	:: !{#Char} !PIC -> ( !Int, !PIC)
WinGetPicCharWidth		:: !Char !PIC -> ( !Int, !PIC)
WinGetStringWidth		:: !{#Char} !Fnt !Int !HDC !*OSToolbox -> ( !Int, !*OSToolbox)
WinGetCharWidth			:: !Char !Fnt !Int !HDC !*OSToolbox -> ( !Int, !*OSToolbox)

WinGetPicFontInfo		:: !PIC -> ( !Int, !Int, !Int, !Int, !PIC)
WinGetFontInfo			:: !Fnt !Int !HDC !*OSToolbox -> ( !Int, !Int, !Int, !Int, !*OSToolbox)
WinSetFontStyle			:: !Int !PIC ->  PIC
WinSetFontSize			:: !Int !PIC ->  PIC
WinSetFontName			:: !{#Char} !PIC ->  PIC
WinSetFont				:: !Fnt !PIC ->  PIC

/*	Routines to PRINT bitmaps (WinPrint(Resized)Bitmap).
	Routines to DRAW  bitmaps (WinDraw(Resized)Bitmap).
	Create a bitmap (WinCreateBitmap).
*/
WinPrintBitmap			:: !(!Int,!Int) !(!Int,!Int) !{#Char} !PIC -> PIC
WinPrintResizedBitmap	:: !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !{#Char} !PIC -> PIC
WinDrawBitmap			:: !(!Int,!Int) !(!Int,!Int) !Int !PIC -> PIC
WinDrawResizedBitmap	:: !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Int !PIC -> PIC
WinCreateBitmap			:: !Int !{#Char} !HDC !*OSToolbox -> (!Int,!*OSToolbox)


WinInvertPolygon		:: !PIC ->  PIC
WinErasePolygon			:: !PIC ->  PIC
WinFillPolygon			:: !PIC ->  PIC
WinDrawPolygon			:: !PIC ->  PIC
WinAddPolygonPoint		:: !Pt !*OSToolbox ->  *OSToolbox
WinStartPolygon			:: !Int !*OSToolbox ->  *OSToolbox
WinEndPolygon			:: !*OSToolbox -> *OSToolbox

WinAllocPolyShape		:: !Int !*OSToolbox -> (!Int,!*OSToolbox)
WinSetPolyPoint			:: !Int !Int !Int !Int !*OSToolbox -> *OSToolbox
WinFreePolyShape		:: !Int !*OSToolbox -> *OSToolbox


WinInvertWedge			:: !Rect !Pt !Pt !PIC ->  PIC
WinEraseWedge			:: !Rect !Pt !Pt !PIC ->  PIC
WinFillWedge			:: !Rect !Pt !Pt !PIC ->  PIC
WinDrawWedge			:: !Rect !Pt !Pt !PIC ->  PIC


WinInvertCircle			:: !Pt !Int !PIC ->  PIC
WinEraseCircle			:: !Pt !Int !PIC ->  PIC
WinFillCircle			:: !Pt !Int !PIC ->  PIC
WinDrawCircle			:: !Pt !Int !PIC ->  PIC


WinInvertOval			:: !Rect !PIC ->  PIC
WinEraseOval			:: !Rect !PIC ->  PIC
WinFillOval				:: !Rect !PIC ->  PIC
WinDrawOval				:: !Rect !PIC ->  PIC


WinInvertRoundRectangle	:: !Rect !Int !Int !PIC ->  PIC
WinEraseRoundRectangle	:: !Rect !Int !Int !PIC ->  PIC
WinFillRoundRectangle	:: !Rect !Int !Int !PIC ->  PIC
WinDrawRoundRectangle	:: !Rect !Int !Int !PIC ->  PIC


WinScrollRectangle		:: !Rect !Pt !PIC -> (!Rect,!PIC)
WinCopyRectangle		:: !Rect !Pt !PIC ->  PIC
WinCopyRectangleTo		:: !Rect !Pt !PIC ->  PIC
WinMoveRectangle		:: !Rect !Pt !PIC ->  PIC
WinMoveRectangleTo		:: !Rect !Pt !PIC ->  PIC


WinInvertRectangle		:: !Rect !PIC ->  PIC
WinEraseRectangle		:: !Rect !PIC ->  PIC
WinFillRectangle		:: !Rect !PIC ->  PIC
WinDrawRectangle		:: !Rect !PIC ->  PIC


WinDrawChar				:: !Int !PIC ->  PIC
WinDrawString			:: !{#Char} !PIC ->  PIC


WinDrawCCurve			:: !Rect !Pt !Pt !RGBcolor !PIC ->  PIC
WinDrawCLine			:: !Pt !Pt !RGBcolor !PIC ->  PIC
WinDrawCPoint			:: !Pt !RGBcolor !PIC ->  PIC
WinDrawCurve			:: !Rect !Pt !Pt !PIC ->  PIC
WinDrawLine				:: !Pt !Pt !PIC ->  PIC
WinDrawPoint			:: !Pt !PIC ->  PIC


WinLinePen				:: !Pt !PIC ->  PIC
WinLinePenTo			:: !Pt !PIC ->  PIC

WinMovePen				:: !Pt !PIC ->  PIC
WinMovePenTo			:: !Pt !PIC ->  PIC
WinGetPenPos			:: !PIC -> (!Int,!Int,!HDC,!*OSToolbox)

WinSetPenSize			:: !Int !PIC ->  PIC
WinSetPattern			:: !Int !PIC ->  PIC
WinSetMode				:: !Int !PIC ->  PIC
WinSetBackColor			:: !RGBcolor !PIC ->  PIC
WinSetPenColor			:: !RGBcolor !PIC ->  PIC

WinClipPicture			:: !Rect !PIC ->  PIC
WinClipRgnPicture		:: !HRGN !PIC -> PIC			//	Operation to set the clipping region
WinSetClipRgnPicture	:: !HRGN !PIC -> PIC			//	Operation to completely set the clipping region
WinGetClipRgnPicture	::       !PIC -> (!HRGN,!PIC)	//  Operation to retrieve the current clipping region

WinDeleteObject			:: !Int !*OSToolbox -> *OSToolbox

WinDonePicture			:: !PIC -> ( !Int, !Int, !RGBcolor, !RGBcolor, !Pt, !Fnt, !PIC)
WinInitPicture			:: !Int !Int !RGBcolor !RGBcolor !Pt !Fnt !Pt !PIC ->  PIC
