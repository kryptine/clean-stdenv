implementation module ossystem			// for Windows

//	Clean Object I/O library, version 1.2

import	StdInt, StdReal, StdString
import	clCCall_12
from	ostypes	import Rect

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
