definition module windowclipstate


//	Clean Object I/O library, version 1.2


import	wstate
from	oswindow	import OSWindowMetrics


disposeClipState:: !ClipState !*OSToolbox -> *OSToolbox
/*	disposeClipState disposes the system resources associated with the ClipState.
	The ClipState should not be used anymore after this operation.
*/

validateWindowClipState   :: !OSWindowMetrics !Bool !OSWindowPtr !(WindowHandle .ls .ps) !*OSToolbox -> (!WindowHandle .ls .ps,!*OSToolbox)
validateWindowClipState`  :: !OSWindowMetrics !Bool !OSWindowPtr !WindowHandle`          !*OSToolbox -> (!WindowHandle`,       !*OSToolbox)
forceValidWindowClipState :: !OSWindowMetrics !Bool !OSWindowPtr !(WindowHandle .ls .ps) !*OSToolbox -> (!WindowHandle .ls .ps,!*OSToolbox)
forceValidWindowClipState`:: !OSWindowMetrics !Bool !OSWindowPtr !WindowHandle`          !*OSToolbox -> (!WindowHandle`,       !*OSToolbox)
/*	validateWindowClipState(`) wMetrics allClipStates wPtr windowHandle
		checks that the optional ClipState of the window is valid (clipOk field is True).
		If not, the ClipState is recalculated and the former value is disposed(!!).
		In case allClipStates holds, then this is done recursively for all compound controls.
	forceValidWindowClipState(`) wMetrics allClipStates wPtr windowHandle
		always recalculates the ClipState and disposes the former value(!!).
		In case allClipStates holds, then this is done recursively for all compound controls.
*/

invalidateWindowClipState   :: !(WindowHandle .ls .ps)	-> WindowHandle .ls .ps
invalidateWindowClipState`  :: !WindowHandle`			-> WindowHandle`
/*	validateWindowClipState(`) invalidate the ClipState of the given window.
*/

validateCompoundClipState   :: !OSWindowMetrics !Bool !OSWindowPtr !(Maybe Id) !(WItemHandle .ls .ps) !*OSToolbox -> (!WItemHandle .ls .ps,!*OSToolbox)
validateCompoundClipState`  :: !OSWindowMetrics !Bool !OSWindowPtr !(Maybe Id) !WItemHandle`          !*OSToolbox -> (!WItemHandle`,       !*OSToolbox)
forceValidCompoundClipState :: !OSWindowMetrics !Bool !OSWindowPtr !(Maybe Id) !(WItemHandle .ls .ps) !*OSToolbox -> (!WItemHandle .ls .ps,!*OSToolbox)
forceValidCompoundClipState`:: !OSWindowMetrics !Bool !OSWindowPtr !(Maybe Id) !WItemHandle`          !*OSToolbox -> (!WItemHandle`,       !*OSToolbox)
/*	validateCompoundClipState(`) wMetrics allClipStates wPtr defaultId compoundControl
		checks that the optional ClipState of the compound control is valid (clipOk field is True).
		If not, the ClipState is recalculated and the former value is disposed(!!).
		In case allClipStates holds, then this is done recursively for all compound controls.
	forceValidCompoundClipState(`) wMetrics allClipStates wPtr defaultId compoundControl
		always recalculates the ClipState and disposes the former value(!!).
		In case allClipStates holds, then this is done recursively for all compound controls.
*/

invalidateCompoundClipState :: !(WItemHandle .ls .ps) -> WItemHandle .ls .ps
invalidateCompoundClipState`:: !WItemHandle`		  -> WItemHandle`
/*	invalidateCompoundClipState(`) invalidate the ClipState of the given compound control.
*/
