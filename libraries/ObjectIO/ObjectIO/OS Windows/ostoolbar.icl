implementation module ostoolbar


//	Clean object I/O library, version 1.2

//	Operations to add and remove tools.

import	StdMisc, StdTuple
from	osbitmap		import OSBitmap, OSgetBitmapSize, OSgetBitmapContent
from	ostypes			import HWND, OSWindowPtr, OSNoWindowPtr
from	pictCCall_12	import WinCreateBitmap
import	clCrossCall_12, windowCrossCall_12


::	OSToolbar
	=	{	toolbarPtr		:: !OSToolbarHandle		// The toolbar of the frame window (zero if no toolbar)
		,	toolbarHeight	:: !Int					// The height of the toolbar       (zero if no toolbar)
		}
::	OSToolbarHandle
	:==	HWND

OSdefaultToolbarHeight :== 16	// The default height of the toolbar

/*	OScreateToolbar wPtr height
		creates a toolbar in the argument window with the given size of the bitmap images.
		The return Int is the actual height of the toolbar. 
*/
OScreateToolbar :: !Bool !HWND !(!Int,!Int) !*OSToolbox -> (!(!OSToolbarHandle,!Int),!*OSToolbox)
OScreateToolbar forMDI hwnd (w,h) tb
	# (rcci,tb)		= IssueCleanRequest2 (ErrorCallback2 "OScreateToolbar") (Rq3Cci (if forMDI CcRqCREATEMDITOOLBAR CcRqCREATESDITOOLBAR) hwnd w h) tb
	  tbPtr_Height	= case rcci.ccMsg of
						CcRETURN2	-> (rcci.p1,rcci.p2)
						CcWASQUIT	-> (OSNoWindowPtr,0)
						other		-> abort "[OScreateToolbar] expected CcRETURN1 value."
	= (tbPtr_Height,tb)

OScreateBitmapToolbarItem :: !OSToolbarHandle !OSBitmap !Int !*OSToolbox -> *OSToolbox
OScreateBitmapToolbarItem tbPtr osBitmap index tb
	# (hdc, tb)	= WinGetDC tbPtr tb
	# (hbmp,tb)	= WinCreateBitmap w contents hdc tb
	# (_,tb)	= IssueCleanRequest2 (ErrorCallback2 "OScreateBitmapToolbarItem") (Rq3Cci CcRqCREATETOOLBARITEM tbPtr hbmp index) tb
	# tb		= WinReleaseDC tbPtr (hdc,tb)
	= tb
where
	(w,_)		= OSgetBitmapSize    osBitmap
	contents	= OSgetBitmapContent osBitmap

OScreateToolbarSeparator :: !OSToolbarHandle !*OSToolbox -> *OSToolbox
OScreateToolbarSeparator tbPtr tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "OScreateBitmapToolbarSeparator") (Rq1Cci CcRqCREATETOOLBARSEPARATOR tbPtr) tb)
