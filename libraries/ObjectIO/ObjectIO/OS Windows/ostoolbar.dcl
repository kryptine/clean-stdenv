definition module ostoolbar

//	Clean object I/O library, version 1.2

//	Operations to add and remove tools.

from	osbitmap	import OSBitmap
from	ostoolbox	import OSToolbox
from	ostypes		import HWND

::	OSToolbar
	=	{	toolbarPtr		:: !OSToolbarHandle		// The toolbar of the frame window (zero if no toolbar)
		,	toolbarHeight	:: !Int					// The height of the toolbar       (zero if no toolbar)
		}
::	OSToolbarHandle
	:==	HWND

OSdefaultToolbarHeight :== 16	// The default height of the toolbar

/*	osCreateToolbar forMDI wPtr (width,height)
		creates a toolbar in the argument window that contains buttons of the given width and height. 
		The forMDI is True in case the toolbar must be created for a MDI process, and False otherwise.
		The return Int is the actual height of the toolbar.
	osCreateBitmapToolbarItem toolbarPtr bitmap index
		adds a button with the given bitmap to the toolbar. The index must be the button item number.
	osCreateToolbarSeparator toolbarPtr
		adds a separator to the toolbar.
*/
osCreateToolbar				:: !Bool !HWND !(!Int,!Int)			!*OSToolbox -> (!(!OSToolbarHandle,!Int),!*OSToolbox)
osCreateBitmapToolbarItem	:: !OSToolbarHandle !OSBitmap !Int	!*OSToolbox -> *OSToolbox
osCreateToolbarSeparator	:: !OSToolbarHandle					!*OSToolbox -> *OSToolbox
