definition module osdocumentinterface

//	Clean object I/O library, version 1.2

import	StdMaybe
from	menuCrossCall_12	import HMENU
from	ostoolbox			import OSToolbox
from	ostoolbar			import OSToolbar, OSToolbarHandle
from	ostypes				import HWND
from	StdIOCommon			import DocumentInterface, MDI, SDI, NDI

::	OSDInfo
	=	OSMDInfo !OSMDInfo
	|	OSSDInfo !OSSDInfo
	|	OSNoInfo
::	OSMDInfo
	=	{	osmdOSInfo		:: !OSInfo			// The general document interface infrastructure
		,	osmdWindowMenu	:: !HMENU			// The Window menu in the MDI menu bar
		}
::	OSSDInfo
	=	{	ossdOSInfo		:: !OSInfo			// The general document interface infrastructure
		}
::	OSInfo
	=	{	osFrame			:: !HWND			// The frame window of the (M/S)DI frame window
		,	osToolbar		:: !Maybe OSToolbar	// The toolbar of the (M/S)DI frame window (Nothing if no toolbar)
		,	osClient		:: !HWND			// The client window of the (M/S)DI frame window
		,	osMenuBar		:: !HMENU			// The menu bar of the (M/S)DI frame window
		}
::	OSMenuBar
	=	{	menuBar			:: !HMENU
		,	menuWindow		:: !HWND
		,	menuClient		:: !HWND
		}

/*	emptyOSDInfo creates a OSDInfo with dummy values for the argument document interface.
*/
emptyOSDInfo :: !DocumentInterface -> OSDInfo

/*	getOSDInfoDocumentInterface returns the DocumentInterface of the argument OSDInfo.
*/
getOSDInfoDocumentInterface :: !OSDInfo -> DocumentInterface

/*	getOSDInfoOSMenuBar returns the OSMenuBar info from the argument OSDInfo.
	setOSDInfoOSMenuBar sets the OSMenuBar info in the OSDInfo.
*/
getOSDInfoOSMenuBar ::            !OSDInfo -> Maybe OSMenuBar
setOSDInfoOSMenuBar :: !OSMenuBar !OSDInfo -> OSDInfo

/*	getOSDInfoOSInfo returns the OSInfo from the argument OSDInfo if present.
	setOSDInfoOSInfo sets the OSInfo in the OSDInfo.
*/
getOSDInfoOSInfo ::         !OSDInfo -> Maybe OSInfo
setOSDInfoOSInfo :: !OSInfo !OSDInfo -> OSDInfo

/*	OSopenMDI  creates  the infrastructure of a MDI process.
		If the first Bool argument is True, then the frame window is shown, otherwise it is hidden.
		The second Bool indicates whether the process accepts file open events.
	OSopenSDI  creates the infrastructure of a SDI process.
		The Bool argument indicates whether the process accepts file open events.
	OScloseOSDInfo destroys the infrastructure.
*/
OSopenMDI     :: !Bool !Bool !*OSToolbox -> (!OSDInfo,!*OSToolbox)
OSopenSDI     ::       !Bool !*OSToolbox -> (!OSDInfo,!*OSToolbox)
OScloseOSDInfo:: !OSDInfo    !*OSToolbox -> *OSToolbox

/*	getOSDInfoOSToolbar retrieves the OSToolbar, if any.
*/
getOSDInfoOSToolbar :: !OSDInfo -> Maybe OSToolbar
