definition module ostooltip

//	Clean Object I/O library, version 1.2

//	Operations to add and remove tooltip controls and areas.

import	StdString
from	ostoolbox	import OSToolbox
from	oswindow	import OSWindowPtr

/*	Tooltip controls are added and removed by OSaddControlTooltip and OSremoveControlTooltip.
	The first  OSWindowPtr argument identifies the parent window.
	The second OSWindowPtr argument identifies the control.
	The String argument is the tooltip text.
*/
OSaddControlToolTip		:: !OSWindowPtr !OSWindowPtr !String	!*OSToolbox -> *OSToolbox
OSremoveControlToolTip	:: !OSWindowPtr !OSWindowPtr			!*OSToolbox -> *OSToolbox
