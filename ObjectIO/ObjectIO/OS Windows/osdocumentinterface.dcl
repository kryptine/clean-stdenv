definition module osdocumentinterface

//	Clean object I/O library, version 1.2

import	StdMaybe
from	menuCrossCall_12	import HMENU
from	ostoolbox			import OSToolbox
from	ostoolbar			import OSToolbar, OSToolbarHandle
from	ostypes				import HWND
from	StdIOCommon			import DocumentInterface, MDI, SDI, NDI

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

/*	getOSDInfoDocumentInterface returns the DocumentInterface of the argument OSDInfo.
*/
getOSDInfoDocumentInterface :: !OSDInfo -> DocumentInterface

/*	OSopenMDI  creates  the infrastructure of a MDI process.
		If the first Bool argument is True, then the frame window is shown, otherwise it is hidden.
		The second Bool indicates whether the process accepts file open events.
	OScloseMDI destroys the infrastructure of a MDI process.
	OSopenSDI  creates the infrastructure of a SDI process.
		The Bool argument indicates whether the process accepts file open events.
	OScloseSDI destroys the infrastructure of a SDI process.
*/
OSopenMDI :: !Bool !Bool !*OSToolbox -> (!OSMDInfo,!*OSToolbox)
OScloseMDI:: !OSMDInfo   !*OSToolbox -> *OSToolbox
OSopenSDI ::       !Bool !*OSToolbox -> (!OSSDInfo,!*OSToolbox)
OScloseSDI:: !OSSDInfo   !*OSToolbox -> *OSToolbox

//	getOSDInfoOSToolbar retrieves the OSToolbar, if any.
getOSDInfoOSToolbar :: !OSDInfo -> Maybe OSToolbar
