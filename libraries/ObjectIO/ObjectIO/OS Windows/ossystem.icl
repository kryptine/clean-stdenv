implementation module ossystem			// for Windows

//	Clean Object I/O library, version 1.2

import	StdBool, StdInt, StdReal, StdString
import	StdMaybe
import	clCCall_12, clCrossCall_12, windowCrossCall_12
import	osdocumentinterface, osfont
from	ostypes	import Rect


::	OSWindowMetrics
	=	{	osmFont				:: Font				// The internal Font used in Windows for controls
		,	osmFontMetrics		:: (Int,Int,Int)	// The ascent, descent, leading of osmFont
		,	osmHeight			:: Int				// The height of the internal Font
		,	osmHorMargin		:: Int				// The default horizontal margin
		,	osmVerMargin		:: Int				// The default vertical   margin
		,	osmHorItemSpace		:: Int				// The default horizontal item space
		,	osmVerItemSpace		:: Int				// The default vertical   item space
		,	osmHSliderHeight	:: Int				// The default height of a horizontal slider control
		,	osmVSliderWidth		:: Int				// The default width  of a vertical   slider control
		}

OSdirseparator	:==	'\\'				// OS separator between folder- and filenames in a pathname

OShomepath :: !String -> String
OShomepath fname = theApplicationPath +++ fname

OSapplicationpath :: !String -> String
OSapplicationpath fname = theApplicationPath +++ fname

theApplicationPath =: path
where
	ptr		= WinGetAppPath 
	(path,_)= WinGetCStringAndFree ptr 99

OSnewlineChars			:== "\xD\xA" // MW11++

OStickspersecond :== 1000				// OS max resolution of ticks per second

OSmmToHPixels :: !Real -> Int
OSmmToHPixels mm = toInt ( (mm/25.4) * toReal WinGetHorzResolution )

OSmmToVPixels :: !Real -> Int
OSmmToVPixels mm = toInt ( (mm/25.4) * toReal WinGetVertResolution )

OSmaxScrollWindowSize :: (!Int,!Int)
OSmaxScrollWindowSize = WinMaxScrollWindowSize

OSmaxFixedWindowSize :: (!Int,!Int)
OSmaxFixedWindowSize = WinMaxFixedWindowSize

OSscreenrect :: !*OSToolbox -> (!Rect,!*OSToolbox)
OSscreenrect tb
	# (screenWidth, tb)	= WinScreenXSize tb
	# (screenHeight,tb)	= WinScreenYSize tb
	= ({rleft=0,rtop=0,rright=screenWidth,rbottom=screenHeight},tb)

OSprintSetupTypical		:: Bool // MW11++
OSprintSetupTypical = False

OSrefreshDesktop :: !*OSToolbox -> *OSToolbox
OSrefreshDesktop tb
	= WinRefreshDesktop tb

OSGetProcessWindowDimensions :: !OSDInfo !*OSToolbox -> (!Rect,!*OSToolbox)
OSGetProcessWindowDimensions osdinfo tb
	# maybeOSInfo		= getOSDInfoOSInfo osdinfo
	| isNothing maybeOSInfo
		= OSscreenrect tb
	| otherwise
		# osinfo		= fromJust maybeOSInfo
		# ((x,y),tb)	= WinGetWindowPos  osinfo.osFrame  tb
		# ((w,h),tb)	= WinGetClientSize osinfo.osClient tb
		= ({rleft=x,rtop=y,rright=x+w,rbottom=y+h},tb)

OSDefaultWindowMetrics :: !*OSToolbox -> (!OSWindowMetrics,!*OSToolbox)
OSDefaultWindowMetrics tb
	# (font,tb)							= OSdialogfont tb
	# ((ascent,descent,leading,_),tb)	= OSgetfontmetrics False 0 font tb
	  height							= ascent+descent+leading
	  unit								= (toReal height)/8.0
	  margin							= toInt (unit*7.0)
	  itemspace							= toInt (unit*4.0)
	# (scrollWidth,scrollHeight,tb)		= WinScrollbarSize tb
	= (	{	osmFont				= font
		,	osmFontMetrics		= (ascent,descent,leading)
		,	osmHeight			= height
		,	osmHorMargin		= margin
		,	osmVerMargin		= margin
		,	osmHorItemSpace		= itemspace
		,	osmVerItemSpace		= itemspace
		,	osmHSliderHeight	= scrollHeight
		,	osmVSliderWidth		= scrollWidth
		}
	  ,	tb
	  )

/*	OSstripOuterSize isMDI isResizable (width,height)
		returns (dw,dh) required to add/subtract to view size/outer size in order to obtain
		outer size/view size.
*/
OSstripOuterSize :: !Bool !Bool !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSstripOuterSize isMDI isResizable tb
	| isMDI
		# (dw,dh,tb)	= WinMDIClientToOuterSizeDims styleFlags tb
		= ((dw,dh),tb)
	| otherwise
		# (dw,dh,tb)	= WinSDIClientToOuterSizeDims styleFlags tb
		= ((dw,dh),tb)
where
	styleFlags			= if isResizable WS_THICKFRAME 0
