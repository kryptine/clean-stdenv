implementation module documentinterface


//	Clean Object I/O library, version 1.2


import	StdBool, StdFunc, StdMisc
import	osdocumentinterface, osmenu
import	commondef, iostate, menudevice, menuevent


documentinterfaceFatalError :: String String -> .x
documentinterfaceFatalError function error
	= FatalError function "documentinterface" error

/*	setOSDInfoInMenuDevice stores process document interface information in the menu device.
*/
setOSDInfoInMenuDevice :: !OSDInfo !(PSt .l .p) -> PSt .l .p
setOSDInfoInMenuDevice OSNoInfo pState
	= pState
setOSDInfoInMenuDevice osdinfo pState
	# pState			= MenuFunctions.dOpen pState
	# (mDevice,ioState)	= IOStGetDevice MenuDevice pState.io
	  mHs				= MenuSystemStateGetMenuHandles mDevice
	  mHs				= {mHs & mOSMenuBar=OSMenuBarNew frame client menubar}
	# ioState			= IOStSetDevice (MenuSystemState mHs) ioState
	= {pState & io=ioState}
where
	(frame,client,menubar)	= case osdinfo of
								OSMDInfo info	-> (info.osmdFrame,info.osmdClient,info.osmdMenubar)
								OSSDInfo info	-> (info.ossdFrame,info.ossdClient,info.ossdMenubar)
								OSNoInfo		-> documentinterfaceFatalError "setOSDInfoInMenuDevice" "unexpected OSDInfo argument"

closeOSDInfo :: !(IOSt .l .p) -> IOSt .l .p
closeOSDInfo ioState
	# (osdinfo,ioState)	= IOStGetOSDInfo ioState
	  close				= case osdinfo of
	  						OSMDInfo info	-> OScloseMDI info
	  						OSSDInfo info	-> OScloseSDI info
	  						_				-> id
	= appIOToolbox close ioState
