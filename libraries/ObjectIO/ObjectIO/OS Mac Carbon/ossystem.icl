implementation module ossystem

import StdInt, StdTuple, StdReal
import ostoolbox,ostypes,osfont, osdocumentinterface
import pointer
from	quickdraw	import LMGetScrHRes, LMGetScrVRes, QScreenRect

//import StdDebug, dodebug

::	OSWindowMetrics
	=	{	osmFont				:: !Font				// The internal Font used in Windows for controls
		,	osmFontMetrics		:: !(!Int,!Int,!Int)	// The ascent, descent, leading of osmFont
		,	osmHeight			:: !Int					// The height of the internal Font
		,	osmHorMargin		:: !Int					// The default horizontal margin
		,	osmVerMargin		:: !Int					// The default vertical   margin
		,	osmHorItemSpace		:: !Int					// The default horizontal item space
		,	osmVerItemSpace		:: !Int					// The default vertical   item space
		,	osmHSliderHeight	:: !Int					// The default height of a horizontal slider control
		,	osmVSliderWidth		:: !Int					// The default width  of a vertical   slider control
		}

OSdirseparator			:== ':'

osHomepath :: !String -> String
osHomepath fname
	= fname

import StdArray, files, osdirectory
osApplicationpath :: !String -> String
osApplicationpath fname
	= FStartUpDir +++ ":" +++ fname
where
	FStartUpDir :: String
	FStartUpDir
		| result==0
			= pathName % (0,size pathName-2)
	where
		(result,wd_vref_num,directory_id,tb1)	= HGetVol NewToolbox;
		(pathName,_)	= Get_directory_path wd_vref_num directory_id "" tb1;
	

OSnewlineChars			:== "\xD"

OStickspersecond		:== 60

mmperinch		:== 25.4
ScrollBarWidth		:== 16									// Conventional width of a scroll bar
TitleBarWidth		:== 20									// Conventional width of a window's title bar
MenuBarWidth		:== 20									// Conventional width of the Mac menu bar
WindowScreenBorder	:== 4									// Conventional distance between window and screen

osMMtoHPixels :: !Real -> Int
osMMtoHPixels mm
//	= toInt ((mm*toReal (fst (LoadWord ScrnHResAddress OSNewToolbox)))/mmperinch)
	= toInt ((mm*toReal LMGetScrHRes)/mmperinch)

osMMtoVPixels :: !Real -> Int
osMMtoVPixels mm
//	= toInt ((mm*toReal (fst (LoadWord ScrnVResAddress OSNewToolbox)))/mmperinch)
	= toInt ((mm*toReal LMGetScrVRes)/mmperinch)

osMaxScrollWindowSize :: (!Int,!Int)	// moet je eigenlijk dynamisch evalueren aangezien window op verschillende schermen kan staan...
osMaxScrollWindowSize
	=	(	sR-ScrollBarWidth-dScrwW+1
		,	sB-ScrollBarWidth-dScrwW-TitleBarWidth-MenuBarWidth+1
		)
where	dScrwW			= WindowScreenBorder<<1
		(_,_, sR,sB,_)	= QScreenRect OSNewToolbox

osMaxFixedWindowSize :: (!Int,!Int)
osMaxFixedWindowSize
	=	(	w+ScrollBarWidth-1
		,	h+ScrollBarWidth-1
		)
where	(w,h)			= osMaxScrollWindowSize

osScreenrect :: !*OSToolbox -> (!OSRect,!*OSToolbox)
osScreenrect tb
	# (sl,st,sr,sb, tb)	= QScreenRect tb
	// subtract menubar from top???
//	#! tb = trace_n ("OSscreenrect "+++toString (sl,st,sr,sb)) tb
	= ({rleft=sl,rtop=st,rright=sr,rbottom=sb-MenuBarWidth},tb)
where
	dScrwW			= WindowScreenBorder<<1

osPrintSetupTypical :: Bool
osPrintSetupTypical = True

osGetProcessWindowDimensions :: !OSDInfo !*OSToolbox -> (!OSRect,!*OSToolbox)
osGetProcessWindowDimensions osd tb
	# (sl,st,sr,sb, tb)	= QScreenRect tb
	= ({rleft=sl,rtop=st,rright=sr,rbottom=sb-MenuBarWidth},tb)

osDefaultWindowMetrics	:: !*OSToolbox -> (!OSWindowMetrics,!*OSToolbox)
osDefaultWindowMetrics tb
	# (font,tb)							= osDialogfont tb
	# ((ascent,descent,leading,_),tb)	= osGetfontmetrics False 0 font tb
	# height							= ascent+descent+leading
	=	(
		{ osmFont				= font
		, osmFontMetrics		= (ascent,descent,leading)
		, osmHeight				= height
		, osmHorMargin			= 10
		, osmVerMargin			= 10
		, osmHorItemSpace		= 10
		, osmVerItemSpace		= 10
		, osmHSliderHeight		= 16
		, osmVSliderWidth		= 16
		}, tb)

osStripOuterSize		:: !Bool !Bool !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osStripOuterSize mdi resize tb = ((if resize 16 0,if resize 16 0),tb)

