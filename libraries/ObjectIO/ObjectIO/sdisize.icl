implementation module sdisize


//	Clean Object I/O library, version 1.2

import	StdBool, StdClass, StdInt
import	oswindow
import	iostate, windowaccess, windowupdate
from	commondef	import FatalError
//
import ostypes
//

sdisizeFatalError :: String String -> .x
sdisizeFatalError rule error = FatalError rule "sdisize" error


//	getSDIWindowSize retrieves the current size of the WindowViewFrame if this is a SDI process
getSDIWindowSize :: !(IOSt .l) -> (!Size,!OSWindowPtr,!IOSt .l)
getSDIWindowSize ioState
	# (osdInfo,ioState)		= IOStGetOSDInfo ioState
	  isSDI					= getOSDInfoDocumentInterface osdInfo==SDI
	  wPtr					= case (getOSDInfoOSInfo osdInfo) of
		  						Just info -> info.osFrame
		  						_         -> OSNoWindowPtr
	| not isSDI
		= (zero,wPtr,ioState)
	| otherwise
		// PA: here we have to use OSgetWindowViewFrameSize, because it is the only reliable way to determine proper viewframe size. 
		# ((w,h),ioState)	= accIOToolbox (OSgetWindowViewFrameSize wPtr) ioState
		= ({w=w,h=h},wPtr,ioState)

/*	resizeSDIWindow wPtr oldviewframesize newviewframesize
		resizes the SDI window so the viewframe does not change in size. 
		oldviewframesize is the size of the ViewFrame(!) before the menu/toolbar was created.
		newviewframesize is the size of the ViewFrame(!) after  the menu/toolbar was created.
		Note that:
			oldviewframesize.h <> newviewframesize.h && oldviewframesize.w == newviewframesize.w
*/
resizeSDIWindow :: !OSWindowPtr !Size !Size !(IOSt .l) -> IOSt .l
resizeSDIWindow wPtr {h=oldHeight} newFrameSize=:{h=newHeight} ioState
	# (osdInfo,ioState)			= IOStGetOSDInfo ioState
	  isSDI						= getOSDInfoDocumentInterface osdInfo==SDI
	| not isSDI
		= sdisizeFatalError "resizeSDIWindow" "not an SDI process"
	# (framePtr,clientPtr)		= case (getOSDInfoOSInfo osdInfo) of
	  								Just {osFrame,osClient} -> (osFrame,osClient)
	  								_                       -> (OSNoWindowPtr,OSNoWindowPtr)
	| wPtr<>framePtr
		= sdisizeFatalError "resizeSDIWindow" "SDIWindow frame could not be located"
	# (tb,ioState)				= getIOToolbox ioState
	# ((oldw,oldh),tb)			= OSgetWindowSize framePtr tb
	# tb						= OSsetWindowSize framePtr (oldw,oldh+oldHeight-newHeight) True tb
	| newHeight>oldHeight	// menus take up less space
		= setIOToolbox tb ioState
	# (ok,wDevice,ioState)		= IOStGetDevice WindowDevice ioState
	| not ok
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID clientPtr) windows
	| not found
		= IOStSetDevice (WindowSystemState windows) ioState
	| otherwise
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (wsH,tb)				= updateSDIWindow wMetrics oldHeight newFrameSize wsH tb
		  windows				= setWindowHandlesWindow wsH windows
		# ioState				= IOStSetDevice (WindowSystemState windows) ioState
		= setIOToolbox tb ioState
where
	// Note that oldH>newSize.h
	updateSDIWindow :: !OSWindowMetrics !Int !Size !(WindowStateHandle .ps) !*OSToolbox -> (!WindowStateHandle .ps,!*OSToolbox)
	updateSDIWindow wMetrics oldH newSize wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		# (wH,tb)				= updatewindow wMetrics updateInfo wH tb			// Update the background
		# (wH,tb)				= updaterectcontrols wMetrics newArea wPtr  wH tb	// Update the controls
		= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	where
		wPtr					= wshIds.wPtr
		newArea					= {zero & rright=newSize.w,rbottom=oldH}
		updateInfo				= {	updWIDS			= wshIds
								  ,	updWindowArea	= newArea
								  ,	updControls		= []
								  ,	updGContext		= Nothing
								  }
	updateSDIWindow _ _ _ _ _
		= sdisizeFatalError "updateSDIWindow" "unexpected window placeholder"
