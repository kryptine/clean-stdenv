definition module ossystem

//	Clean Object I/O library, version 1.2

import	StdString
from	StdMaybe			import Maybe, Just, Nothing
from	menuCrossCall_12	import HMENU
from	osdocumentinterface	import OSDInfo, OSMDInfo, OSSDInfo, OSInfo, OSToolbar, OSToolbarHandle
from	osfont				import Font
from	ostoolbox			import OSToolbox
from	ostypes				import Rect, HWND

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

OSdirseparator			:== '\\'

osHomepath				:: !String -> String
osApplicationpath		:: !String -> String
OSnewlineChars			:== "\xD\xA" // MW11++

OStickspersecond		:== 1000

osMMtoHPixels			:: !Real -> Int
osMMtoVPixels			:: !Real -> Int
osMaxScrollWindowSize	:: (!Int,!Int)
osMaxFixedWindowSize	:: (!Int,!Int)
osScreenrect			:: !*OSToolbox -> (!Rect,!*OSToolbox)

osPrintSetupTypical		:: Bool // MW11++

// osGetProcessWindowDimensions returns Rect of process window in terms of screen coordinates
osGetProcessWindowDimensions :: !OSDInfo !*OSToolbox -> (!Rect,!*OSToolbox)


osDefaultWindowMetrics	:: !*OSToolbox -> (!OSWindowMetrics,!*OSToolbox)

/*	osStripOuterSize isMDI isResizable
		returns (dw,dh) required to add/subtract to view/outer size in order to obtain outer/view size.
*/
osStripOuterSize		:: !Bool !Bool !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)