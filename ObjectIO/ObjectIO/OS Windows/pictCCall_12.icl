implementation module pictCCall_12


from	ostypes	import Rect
import	rgnCCall_12//, intrface_12


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


//	PA: Win(Create/Destroy)ScreenHDC added to temporarily create a HDC of a screen.
//		Never use these values for a window or control.
WinCreateScreenHDC :: !*OSToolbox -> PIC
WinCreateScreenHDC _
	= code
	{
		.inline WinCreateScreenHDC
			ccall WinCreateScreenHDC "I-II"
		.end
	}

WinDestroyScreenHDC :: !PIC -> *OSToolbox
WinDestroyScreenHDC _
	= code
	{
		.inline WinDestroyScreenHDC
			ccall WinDestroyScreenHDC "II-I"
		.end
	}

// MW: this is never used in the object IO
WinGetPicStringWidth ::  !{#Char} !PIC -> ( !Int, !PIC)
WinGetPicStringWidth _ _
	= code
	{
		.inline WinGetPicStringWidth
			ccall WinGetPicStringWidth "SII-III"
		.end
	}

WinGetPicCharWidth ::  !Char !PIC -> ( !Int, !PIC)
WinGetPicCharWidth _ _
	= code
	{
		.inline WinGetPicCharWidth
			ccall WinGetPicCharWidth "III-III"
		.end
	}
// END MW

WinGetStringWidth ::  !{#Char} !Fnt !Int !HDC !*OSToolbox -> ( !Int, !*OSToolbox)
WinGetStringWidth _ _ _ _ _
	= code
	{
		.inline WinGetStringWidth
			ccall WinGetStringWidth "SSIIIII-II"
		.end
	}

WinGetCharWidth ::  !Char !Fnt !Int !HDC !*OSToolbox -> ( !Int, !*OSToolbox)
WinGetCharWidth _ _ _ _ _
	= code
	{
		.inline WinGetCharWidth
			ccall WinGetCharWidth "ISIIIII-II"
		.end
	}

WinGetPicFontInfo ::  !PIC -> ( !Int, !Int, !Int, !Int, !PIC)
WinGetPicFontInfo _
	= code
	{
		.inline WinGetPicFontInfo
			ccall WinGetPicFontInfo "II-IIIIII"
		.end
	}

WinGetFontInfo ::  !Fnt !Int !HDC !*OSToolbox -> ( !Int, !Int, !Int, !Int, !*OSToolbox)
WinGetFontInfo _ _ _ _
	= code
	{
		.inline WinGetFontInfo
			ccall WinGetFontInfo "SIIIII-IIIII"
		.end
	}

WinSetFontStyle ::  !Int !PIC ->  PIC
WinSetFontStyle _ _
	= code
	{
		.inline WinSetFontStyle
			ccall WinSetFontStyle "III-II"
		.end
	}

WinSetFontSize ::  !Int !PIC ->  PIC
WinSetFontSize _ _
	= code
	{
		.inline WinSetFontSize
			ccall WinSetFontSize "III-II"
		.end
	}

WinSetFontName ::  !{#Char} !PIC ->  PIC
WinSetFontName _ _
	= code
	{
		.inline WinSetFontName
			ccall WinSetFontName "SII-II"
		.end
	}

WinSetFont ::  !Fnt !PIC ->  PIC
WinSetFont _ _
	= code
	{
		.inline WinSetFont
			ccall WinSetFont "SIIII-II"
		.end
	}


//	PA: Routines to PRINT bitmaps.
WinPrintBitmap :: !(!Int,!Int) !(!Int,!Int) !{#Char} !PIC -> PIC
WinPrintBitmap _ _ _ _
	= code
	{
		.inline WinPrintBitmap
			ccall WinPrintBitmap "IIIISII-II"
		.end
	}

WinPrintResizedBitmap :: !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !{#Char} !PIC -> PIC
WinPrintResizedBitmap _ _ _ _ _
	= code
	{
		.inline WinPrintResizedBitmap
			ccall WinPrintResizedBitmap "IIIIIISII-II"
		.end
	}

//	PA: Routines to DRAW bitmaps.
WinDrawBitmap :: !(!Int,!Int) !(!Int,!Int) !Int !PIC -> PIC
WinDrawBitmap _ _ _ _
	= code
	{
		.inline WinDrawBitmap
			ccall WinDrawBitmap "IIIIIII-II"
		.end
	}

WinDrawResizedBitmap :: !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Int !PIC -> PIC
WinDrawResizedBitmap _ _ _ _ _
	= code
	{
		.inline WinDrawResizedBitmap
			ccall WinDrawResizedBitmap "IIIIIIIII-II"
		.end
	}

WinCreateBitmap :: !Int !{#Char} !HDC !*OSToolbox -> (!Int,!*OSToolbox)
WinCreateBitmap _ _ _ _
	= code
	{
		.inline WinCreateBitmap
			ccall WinCreateBitmap "ISII-II"
		.end
	}

WinInvertPolygon ::  !PIC ->  PIC
WinInvertPolygon _
	= code
	{
		.inline WinInvertPolygon
			ccall WinInvertPolygon "II-II"
		.end
	}

WinErasePolygon ::  !PIC ->  PIC
WinErasePolygon _
	= code
	{
		.inline WinErasePolygon
			ccall WinErasePolygon "II-II"
		.end
	}

WinFillPolygon ::  !PIC ->  PIC
WinFillPolygon _
	= code
	{
		.inline WinFillPolygon
			ccall WinFillPolygon "II-II"
		.end
	}

WinDrawPolygon ::  !PIC ->  PIC
WinDrawPolygon _
	= code
	{
		.inline WinDrawPolygon
			ccall WinDrawPolygon "II-II"
		.end
	}

WinAddPolygonPoint ::  !Pt !*OSToolbox ->  *OSToolbox
WinAddPolygonPoint _ _
	= code
	{
		.inline WinAddPolygonPoint
			ccall WinAddPolygonPoint "III-I"
		.end
	}

WinStartPolygon ::  !Int !*OSToolbox ->  *OSToolbox
WinStartPolygon _ _
	= code
	{
		.inline WinStartPolygon
			ccall WinStartPolygon "II-I"
		.end
	}

WinEndPolygon :: !*OSToolbox -> *OSToolbox
WinEndPolygon _
	= code
	{	
		.inline WinEndPolygon
			ccall WinEndPolygon "I-I"
		.end
	}

/*	Operations to create, modify, and destroy polygon shapes.
*/
WinAllocPolyShape :: !Int !*OSToolbox -> (!Int,!*OSToolbox)
WinAllocPolyShape _ _
	= code
	{	
		.inline WinAllocPolyShape
			ccall WinAllocPolyShape "II-II"
		.end
	}

WinSetPolyPoint :: !Int !Int !Int !Int !*OSToolbox -> *OSToolbox
WinSetPolyPoint _ _ _ _ _
	= code
	{	
		.inline WinSetPolyPoint
			ccall WinSetPolyPoint "IIIII-I"
		.end
	}

WinFreePolyShape :: !Int !*OSToolbox -> *OSToolbox
WinFreePolyShape _ _
	= code
	{	
		.inline WinFreePolyShape
			ccall WinFreePolyShape "II-I"
		.end
	}


WinInvertWedge ::  !Rect !Pt !Pt !PIC ->  PIC
WinInvertWedge _ _ _ _
	= code
	{
		.inline WinInvertWedge
			ccall WinInvertWedge "IIIIIIIIII-II"
		.end
	}

WinEraseWedge ::  !Rect !Pt !Pt !PIC ->  PIC
WinEraseWedge _ _ _ _
	= code
	{
		.inline WinEraseWedge
			ccall WinEraseWedge "IIIIIIIIII-II"
		.end
	}

WinFillWedge ::  !Rect !Pt !Pt !PIC ->  PIC
WinFillWedge _ _ _ _
	= code
	{
		.inline WinFillWedge
			ccall WinFillWedge "IIIIIIIIII-II"
		.end
	}

WinDrawWedge ::  !Rect !Pt !Pt !PIC ->  PIC
WinDrawWedge _ _ _ _
	= code
	{
		.inline WinDrawWedge
			ccall WinDrawWedge "IIIIIIIIII-II"
		.end
	}


WinInvertCircle ::  !Pt !Int !PIC ->  PIC
WinInvertCircle _ _ _
	= code
	{
		.inline WinInvertCircle
			ccall WinInvertCircle "IIIII-II"
		.end
	}

WinEraseCircle ::  !Pt !Int !PIC ->  PIC
WinEraseCircle _ _ _
	= code
	{
		.inline WinEraseCircle
			ccall WinEraseCircle "IIIII-II"
		.end
	}

WinFillCircle ::  !Pt !Int !PIC ->  PIC
WinFillCircle _ _ _
	= code
	{
		.inline WinFillCircle
			ccall WinFillCircle "IIIII-II"
		.end
	}

WinDrawCircle ::  !Pt !Int !PIC ->  PIC
WinDrawCircle _ _ _
	= code
	{
		.inline WinDrawCircle
			ccall WinDrawCircle "IIIII-II"
		.end
	}


WinInvertOval ::  !Rect !PIC ->  PIC
WinInvertOval _ _
	= code
	{
		.inline WinInvertOval
			ccall WinInvertOval "IIIIII-II"
		.end
	}

WinEraseOval ::  !Rect !PIC ->  PIC
WinEraseOval _ _
	= code
	{
		.inline WinEraseOval
			ccall WinEraseOval "IIIIII-II"
		.end
	}

WinFillOval ::  !Rect !PIC ->  PIC
WinFillOval _ _
	= code
	{
		.inline WinFillOval
			ccall WinFillOval "IIIIII-II"
		.end
	}

WinDrawOval ::  !Rect !PIC ->  PIC
WinDrawOval _ _
	= code
	{
		.inline WinDrawOval
			ccall WinDrawOval "IIIIII-II"
		.end
	}


WinInvertRoundRectangle ::  !Rect !Int !Int !PIC ->  PIC
WinInvertRoundRectangle _ _ _ _
	= code
	{
		.inline WinInvertRoundRectangle
			ccall WinInvertRoundRectangle "IIIIIIII-II"
		.end
	}

WinEraseRoundRectangle ::  !Rect !Int !Int !PIC ->  PIC
WinEraseRoundRectangle _ _ _ _
	= code
	{
		.inline WinEraseRoundRectangle
			ccall WinEraseRoundRectangle "IIIIIIII-II"
		.end
	}

WinFillRoundRectangle ::  !Rect !Int !Int !PIC ->  PIC
WinFillRoundRectangle _ _ _ _
	= code
	{
		.inline WinFillRoundRectangle
			ccall WinFillRoundRectangle "IIIIIIII-II"
		.end
	}

WinDrawRoundRectangle ::  !Rect !Int !Int !PIC ->  PIC
WinDrawRoundRectangle _ _ _ _
	= code
	{
		.inline WinDrawRoundRectangle
			ccall WinDrawRoundRectangle "IIIIIIII-II"
		.end
	}


WinScrollRectangle :: !Rect !Pt !PIC -> (!Rect,!PIC)
WinScrollRectangle _ _ _
	= code
	{
		.inline WinScrollRectangle
			ccall WinScrollRectangle "IIIIIIII-IIIIII"
		.end
	}

WinCopyRectangle ::  !Rect !Pt !PIC ->  PIC
WinCopyRectangle _ _ _
	= code
	{
		.inline WinCopyRectangle
			ccall WinCopyRectangle "IIIIIIII-II"
		.end
	}

WinCopyRectangleTo ::  !Rect !Pt !PIC ->  PIC
WinCopyRectangleTo _ _ _
	= code
	{
		.inline WinCopyRectangleTo
			ccall WinCopyRectangleTo "IIIIIIII-II"
		.end
	}

WinMoveRectangle ::  !Rect !Pt !PIC ->  PIC
WinMoveRectangle _ _ _
	= code
	{
		.inline WinMoveRectangle
			ccall WinMoveRectangle "IIIIIIII-II"
		.end
	}

WinMoveRectangleTo ::  !Rect !Pt !PIC ->  PIC
WinMoveRectangleTo _ _ _
	= code
	{
		.inline WinMoveRectangleTo
			ccall WinMoveRectangleTo "IIIIIIII-II"
		.end
	}


WinInvertRectangle ::  !Rect !PIC ->  PIC
WinInvertRectangle _ _
	= code
	{
		.inline WinInvertRectangle
			ccall WinInvertRectangle "IIIIII-II"
		.end
	}

WinEraseRectangle ::  !Rect !PIC ->  PIC
WinEraseRectangle _ _
	= code
	{
		.inline WinEraseRectangle
			ccall WinEraseRectangle "IIIIII-II"
		.end
	}

WinFillRectangle ::  !Rect !PIC ->  PIC
WinFillRectangle _ _
	= code
	{
		.inline WinFillRectangle
			ccall WinFillRectangle "IIIIII-II"
		.end
	}

WinDrawRectangle ::  !Rect !PIC ->  PIC
WinDrawRectangle _ _
	= code
	{
		.inline WinDrawRectangle
			ccall WinDrawRectangle "IIIIII-II"
		.end
	}


WinDrawChar ::  !Int !PIC ->  PIC
WinDrawChar _ _
	= code
	{
		.inline WinDrawChar
			ccall WinDrawChar "III-II"
		.end
	}

WinDrawString ::  !{#Char} !PIC ->  PIC
WinDrawString _ _
	= code
	{
		.inline WinDrawString
			ccall WinDrawString "SII-II"
		.end
	}


WinDrawCCurve ::  !Rect !Pt !Pt !RGBcolor !PIC ->  PIC
WinDrawCCurve _ _ _ _ _
	= code
	{
		.inline WinDrawCCurve
			ccall WinDrawCCurve "IIIIIIIIIIIII-II"
		.end
	}

WinDrawCLine ::  !Pt !Pt !RGBcolor !PIC ->  PIC
WinDrawCLine _ _ _ _
	= code
	{
		.inline WinDrawCLine
			ccall WinDrawCLine "IIIIIIIII-II"
		.end
	}

WinDrawCPoint ::  !Pt !RGBcolor !PIC ->  PIC
WinDrawCPoint _ _ _
	= code
	{
		.inline WinDrawCPoint
			ccall WinDrawCPoint "IIIIIII-II"
		.end
	}

WinDrawCurve ::  !Rect !Pt !Pt !PIC ->  PIC
WinDrawCurve _ _ _ _
	= code
	{
		.inline WinDrawCurve
			ccall WinDrawCurve "IIIIIIIIII-II"
		.end
	}

WinDrawLine ::  !Pt !Pt !PIC ->  PIC
WinDrawLine _ _ _
	= code
	{
		.inline WinDrawLine
			ccall WinDrawLine "IIIIII-II"
		.end
	}

WinDrawPoint ::  !Pt !PIC ->  PIC
WinDrawPoint _ _
	= code
	{
		.inline WinDrawPoint
			ccall WinDrawPoint "IIII-II"
		.end
	}


WinLinePen ::  !Pt !PIC ->  PIC
WinLinePen _ _
	= code
	{
		.inline WinLinePen
			ccall WinLinePen "IIII-II"
		.end
	}

WinLinePenTo ::  !Pt !PIC ->  PIC
WinLinePenTo _ _
	= code
	{
		.inline WinLinePenTo
			ccall WinLinePenTo "IIII-II"
		.end
	}

WinMovePen ::  !Pt !PIC ->  PIC
WinMovePen _ _
	= code
	{
		.inline WinMovePen
			ccall WinMovePen "IIII-II"
		.end
	}

WinMovePenTo ::  !Pt !PIC ->  PIC
WinMovePenTo _ _
	= code
	{
		.inline WinMovePenTo
			ccall WinMovePenTo "IIII-II"
		.end
	}

WinGetPenPos :: !PIC -> (!Int,!Int,!HDC,!*OSToolbox)
WinGetPenPos _
	= code
	{	
		.inline WinGetPenPos
			ccall WinGetPenPos "II-IIII"
		.end
	}


WinSetPenSize ::  !Int !PIC ->  PIC
WinSetPenSize _ _
	= code
	{
		.inline WinSetPenSize
			ccall WinSetPenSize "III-II"
		.end
	}

WinSetPattern ::  !Int !PIC ->  PIC
WinSetPattern _ _
	= code
	{
		.inline WinSetPattern
			ccall WinSetPattern "III-II"
		.end
	}

WinSetMode ::  !Int !PIC ->  PIC
WinSetMode _ _
	= code
	{
		.inline WinSetMode
			ccall WinSetMode "III-II"
		.end
	}

WinSetBackColor ::  !RGBcolor !PIC ->  PIC
WinSetBackColor _ _
	= code
	{
		.inline WinSetBackColor
			ccall WinSetBackColor "IIIII-II"
		.end
	}

WinSetPenColor ::  !RGBcolor !PIC ->  PIC
WinSetPenColor _ _
	= code
	{
		.inline WinSetPenColor
			ccall WinSetPenColor "IIIII-II"
		.end
	}


WinClipPicture ::  !Rect !PIC ->  PIC
WinClipPicture _ _
	= code
	{
		.inline WinClipPicture
			ccall WinClipPicture "IIIIII-II"
		.end
	}

//	PA: operation to set the clipping region.
WinClipRgnPicture :: !HRGN !PIC -> PIC
WinClipRgnPicture _ _
	= code
	{	
		.inline WinClipRgnPicture
			ccall WinClipRgnPicture "III-II"
		.end
	}

//	PA+++: new operation to set the complete clipping region.
WinSetClipRgnPicture :: !HRGN !PIC -> PIC
WinSetClipRgnPicture _ _
	= code
	{
		.inline WinSetClipRgnPicture
			ccall WinSetClipRgnPicture "III-II"
		.end
	}

//	PA+++: new operation to retrieve the current clipping region.
WinGetClipRgnPicture :: !PIC -> (!HRGN,!PIC)
WinGetClipRgnPicture _
	= code
	{
		.inline WinGetClipRgnPicture
			ccall WinGetClipRgnPicture "II-III"
		.end
	}

WinDeleteObject :: !Int !*OSToolbox -> *OSToolbox
WinDeleteObject _ _
	= code
	{	
		.inline WinDeleteObject
			ccall WinDeleteObject "II-I"
		.end
	}


WinDonePicture :: !PIC -> (!Int,!Int,!RGBcolor,!RGBcolor,!Pt,!Fnt,!PIC)
WinDonePicture _
	= code
	{
		.inline WinDonePicture
			ccall WinDonePicture "II-IIIIIIIIIISIIII"
		.end
	}

WinInitPicture :: !Int !Int !RGBcolor !RGBcolor !Pt !Fnt !Pt !PIC -> PIC
WinInitPicture _ _ _ _ _ _ _ _
	= code
	{
		.inline WinInitPicture
			ccall WinInitPicture "IIIIIIIIIISIIIIII-II"
		.end
	}
