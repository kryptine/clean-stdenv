implementation module toolbar


//	Clean object I/O library, version 1.2


import StdBool, StdInt, StdMisc
import osbitmap, ostoolbar, oswindow
import commondef, iostate, sdisize, StdProcessAttribute


toolbarFatalError :: String String -> .x
toolbarFatalError function error
	= FatalError function "toolbar" error


openToolbar :: !(IOSt .l .p) -> IOSt .l .p
openToolbar ioState
	# (osdInfo,ioState)			= IOStGetOSDInfo ioState
	  di						= case osdInfo of
	  								OSMDInfo _	-> MDI
	  								OSSDInfo _	-> SDI
	  								OSNoInfo	-> NDI
	| di==NDI
		= ioState
	# (atts,ioState)			= IOStGetProcessAttributes ioState
	  (hasToolbarAtt,toolbarAtt)= Select isProcessToolbar undef atts
	| not hasToolbarAtt
		= ioState
	# toolbar					= getProcessToolbarAtt toolbarAtt
	| di==SDI
		= openSDIToolbar toolbar osdInfo ioState
	| otherwise
		= openMDIToolbar toolbar osdInfo ioState
where
	openSDIToolbar :: ![ToolbarItem (PSt .l .p)] !OSDInfo !(IOSt .l .p) -> IOSt .l .p
	openSDIToolbar items (OSSDInfo info=:{ossdFrame,ossdToolbar}) ioState
		| isJust ossdToolbar
			= toolbarFatalError "openSDIToolbar" "toolbar already present"
		# (oldSize,_,ioState)	= getSDIWindowSize ioState
		# (tb,ioState)			= getIOToolbox ioState
		# ((tbPtr,tbHeight),tb)	= OScreateToolbar False ossdFrame (toTuple reqSize) tb
		| tbPtr==OSNoWindowPtr
			= toolbarFatalError "openSDIToolbar" "toolbar could not be created"
		| otherwise
			# (_,tb)			= StateMap2 (openToolbarItem tbPtr) items (1,tb)
			  ostoolbar			= {toolbarPtr=tbPtr,toolbarHeight=tbHeight}
			  info				= {info & ossdToolbar=Just ostoolbar}
			# ioState			= setIOToolbox tb ioState
			# ioState			= IOStSetOSDInfo (OSSDInfo info) ioState
			# ioState			= resizeSDIWindow ossdFrame oldSize {oldSize & h=oldSize.h-tbHeight} ioState
			= ioState
	where
		reqSize					= getBitmapsSize items
	openSDIToolbar _ _ _
		= toolbarFatalError "openSDIToolbar" "OSSDInfo alternative expected"
	
	openMDIToolbar :: ![ToolbarItem (PSt .l .p)] !OSDInfo !(IOSt .l .p) -> IOSt .l .p
	openMDIToolbar items (OSMDInfo info=:{osmdFrame,osmdClient,osmdToolbar}) ioState
		| isJust osmdToolbar
			= toolbarFatalError "openMDIToolbar" "toolbar already present"
		# (tb,ioState)			= getIOToolbox ioState
		# ((tbPtr,tbHeight),tb)	= OScreateToolbar True osmdFrame (toTuple reqSize) tb
		| tbPtr==OSNoWindowPtr
			= toolbarFatalError "openMDIToolbar" "toolbar could not be created"
		| otherwise
			# (_,tb)			= StateMap2 (openToolbarItem tbPtr) items (1,tb)
			  ostoolbar			= {toolbarPtr=tbPtr,toolbarHeight=tbHeight}
			  info				= {info & osmdToolbar=Just ostoolbar}
			# ioState			= setIOToolbox tb ioState
			# ioState			= IOStSetOSDInfo (OSMDInfo info) ioState
			= ioState
	where
		reqSize					= getBitmapsSize items
	openMDIToolbar _ _ _
		= toolbarFatalError "openMDIToolbar" "OSMDInfo alternative expected"

	openToolbarItem	:: !OSToolbarHandle !(ToolbarItem (PSt .l .p)) !(!Int,!*OSToolbox) -> (!Int,!*OSToolbox)
	openToolbarItem tbPtr (ToolbarItem bitmap tooltip _) (index,tb)
		= (index+1,OScreateBitmapToolbarItem tbPtr (fromBitmap bitmap) index tb)
	openToolbarItem tbPtr ToolbarSeparator (index,tb)
		= (index,OScreateToolbarSeparator tbPtr tb)

getBitmapsSize :: ![ToolbarItem .ps] -> Size
getBitmapsSize items
	= StateMap2 maxBitmapSize items {w=OSdefaultToolbarHeight,h=OSdefaultToolbarHeight}
where
	maxBitmapSize :: !(ToolbarItem .ps) !Size -> Size
	maxBitmapSize item size
		= {w=max itemsize.w size.w,h=max itemsize.h size.h}
	where
		itemsize	= case item of
						ToolbarItem bitmap _ _	-> getBitmapSize bitmap
						_						-> zero
