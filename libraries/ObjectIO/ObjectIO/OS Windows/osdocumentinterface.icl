implementation module osdocumentinterface


//	Clean object I/O library, version 1.2


import	StdMaybe, StdTuple
import	clCrossCall_12, ostoolbar, oswindow
from	commondef	import FatalError,String
from	StdIOCommon	import DocumentInterface, MDI, SDI, NDI


::	OSDInfo
	=	OSMDInfo OSMDInfo
	|	OSSDInfo OSSDInfo
	|	OSNoInfo
::	OSMDInfo
	=	{	osmdFrame		:: !HWND			// The frame window of the MDI frame window
		,	osmdToolbar		:: !Maybe OSToolbar	// The toolbar of the MDI frame window (Nothing if no toolbar)
		,	osmdClient		:: !HWND			// The client window of the MDI frame window
		,	osmdMenubar		:: !HMENU			// The menu bar of the MDI frame window
		,	osmdWindowMenu	:: !HMENU			// The Window menu in the menu bar
		}
::	OSSDInfo
	=	{	ossdFrame		:: !HWND			// The frame window of the SDI frame window
		,	ossdToolbar		:: !Maybe OSToolbar	// The toolbar of the SDI frame window (Nothing if no toolbar)
		,	ossdClient		:: !HWND			// The client window of the SDI frame window
		,	ossdMenubar		:: !HMENU			// The menu bar of the SDI frame window
		}


osdocumentinterfaceFatalError :: String String -> .x
osdocumentinterfaceFatalError function error
	= FatalError function "osdocumentinterface" error


getOSDInfoDocumentInterface :: !OSDInfo -> DocumentInterface
getOSDInfoDocumentInterface (OSMDInfo _)	= MDI
getOSDInfoDocumentInterface (OSSDInfo _)	= SDI
getOSDInfoDocumentInterface OSNoInfo		= NDI


/*	OSopenMDI creates the infrastructure of an MDI process.
		If the first Bool argument is True, then the frame window is shown, otherwise it is hidden.
		The second Bool indicates whether the process accepts file open events.
*/
OSopenMDI :: !Bool !Bool !*OSToolbox -> (!OSMDInfo,!*OSToolbox)
OSopenMDI show acceptFileOpen tb
	# createCci			= Rq2Cci CcRqCREATEMDIFRAMEWINDOW (toInt show) (toInt acceptFileOpen)
	# (returncci,tb)	= IssueCleanRequest2 osCreateMDIWindowCallback createCci tb
	  (framePtr,clientPtr,menuBar,windowMenu)
		  				= case returncci.ccMsg of
			  				CcRETURN4	-> (returncci.p1,returncci.p2,returncci.p3,returncci.p4)
			  				CcWASQUIT	-> (OSNoWindowPtr,OSNoWindowPtr,OSNoWindowPtr,OSNoWindowPtr)
			  				msg			-> osdocumentinterfaceFatalError "OSopenMDI" ("CcRETURN4 expected instead of "+++toString msg)
	= ({osmdFrame=framePtr,osmdToolbar=Nothing,osmdClient=clientPtr,osmdMenubar=menuBar,osmdWindowMenu=windowMenu},tb)
where
	osCreateMDIWindowCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
	osCreateMDIWindowCallback {ccMsg=CcWmDEACTIVATE} tb
		= (Return0Cci,tb)
	osCreateMDIWindowCallback {ccMsg=CcWmACTIVATE} tb
		= (Return0Cci,tb)
	osCreateMDIWindowCallback {ccMsg} tb
		= osdocumentinterfaceFatalError "osCreateMDIWindowCallback" ("received message nr:"+++toString ccMsg)

OScloseMDI :: !OSMDInfo !*OSToolbox -> *OSToolbox
OScloseMDI {osmdFrame} tb
	= snd (IssueCleanRequest2 (osDestroyProcessWindowCallback "OScloseMDI") (Rq1Cci CcRqDESTROYWINDOW osmdFrame) tb)

OSopenSDI :: !Bool !*OSToolbox -> (!OSSDInfo,!*OSToolbox)
OSopenSDI acceptFileOpen tb
	# createCci			= Rq1Cci CcRqCREATESDIFRAMEWINDOW (toInt acceptFileOpen)
	# (returncci,tb)	= IssueCleanRequest2 osCreateSDIWindowCallback createCci tb
	  (framePtr,menuBar)= case returncci.ccMsg of
	  						CcRETURN2	-> (returncci.p1,returncci.p2)
	  						CcWASQUIT	-> (OSNoWindowPtr,OSNoWindowPtr)
	  						msg			-> osdocumentinterfaceFatalError "OSopenSDI" ("CcRETURN2 expected instead of "+++toString msg)
	= ({ossdFrame=framePtr,ossdToolbar=Nothing,ossdClient=OSNoWindowPtr,ossdMenubar=menuBar},tb)
where
	osCreateSDIWindowCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
	osCreateSDIWindowCallback {ccMsg=CcWmDEACTIVATE} tb
		= (Return0Cci,tb)
	osCreateSDIWindowCallback {ccMsg=CcWmACTIVATE} tb
		= (Return0Cci,tb)
	osCreateSDIWindowCallback {ccMsg} tb
		= osdocumentinterfaceFatalError "osCreateSDIWindowCallback" ("received message nr:"+++toString ccMsg)

OScloseSDI :: !OSSDInfo !*OSToolbox -> *OSToolbox
OScloseSDI {ossdFrame} tb
	= snd (IssueCleanRequest2 (osDestroyProcessWindowCallback "OScloseSDI") (Rq1Cci CcRqDESTROYWINDOW ossdFrame) tb)

osDestroyProcessWindowCallback :: String !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
osDestroyProcessWindowCallback function {ccMsg=CcWmDEACTIVATE} tb
	= (Return0Cci,tb)
osDestroyProcessWindowCallback function {ccMsg=CcWmACTIVATE} tb
	= (Return0Cci,tb)
osDestroyProcessWindowCallback function {ccMsg=CcWmKEYBOARD} tb
	= (Return0Cci,tb)
osDestroyProcessWindowCallback function {ccMsg} tb
	= osdocumentinterfaceFatalError function ("received message nr:"+++toString ccMsg)

//	getOSDInfoOSToolbar retrieves the OSToolbar, if any.
getOSDInfoOSToolbar :: !OSDInfo -> Maybe OSToolbar
getOSDInfoOSToolbar (OSMDInfo {osmdToolbar}) = osmdToolbar
getOSDInfoOSToolbar (OSSDInfo {ossdToolbar}) = ossdToolbar
getOSDInfoOSToolbar _						 = Nothing
