definition module ossystem

//	Clean Object I/O library, version 1.2

from	StdString	import String
from	ostypes		import Rect
from	ostoolbox	import OSToolbox

OSdirseparator			:== '\\'

OShomepath				:: !String -> String
OSapplicationpath		:: !String -> String
OSnewlineChars			:== "\xD\xA" // MW11++

OStickspersecond		:== 1000

OSmmToHPixels			:: !Real -> Int
OSmmToVPixels			:: !Real -> Int
OSmaxScrollWindowSize	:: (!Int,!Int)
OSmaxFixedWindowSize	:: (!Int,!Int)
OSscreenrect			:: !*OSToolbox -> (!Rect,!*OSToolbox)

OSprintSetupTypical		:: Bool // MW11++

OSrefreshDesktop		:: !*OSToolbox -> *OSToolbox
