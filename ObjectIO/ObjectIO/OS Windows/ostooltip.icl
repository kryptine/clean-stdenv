implementation module ostooltip

//	Clean Object I/O library, version 1.2

//	Operations to add and remove tooltip controls and areas.

/*	Tooltip controls are added and removed by OSaddControlTooltip and OSremoveControlTooltip.
	The first  OSWindowPtr argument identifies the parent window.
	The second OSWindowPtr argument identifies the control.
	The String argument is the tooltip text.
*/

import	StdTuple
import	clCrossCall_12
from	clCCall_12	import winMakeCString, winReleaseCString, CSTR
from	oswindow	import OSWindowPtr

osIgnoreCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
osIgnoreCallback _ tb 
	= (return0Cci,tb)

osAddControlToolTip :: !OSWindowPtr !OSWindowPtr !String !*OSToolbox -> *OSToolbox
osAddControlToolTip parentPtr controlPtr tip tb
	# (textptr,tb)	= winMakeCString tip tb
	# cci			= Rq3Cci CcRqADDCONTROLTIP parentPtr controlPtr textptr
	# tb			= snd (issueCleanRequest2 osIgnoreCallback cci tb)
	= winReleaseCString textptr tb

osRemoveControlToolTip :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osRemoveControlToolTip parentPtr controlPtr tb
	= snd (issueCleanRequest2 osIgnoreCallback (Rq2Cci CcRqDELCONTROLTIP parentPtr controlPtr) tb)
