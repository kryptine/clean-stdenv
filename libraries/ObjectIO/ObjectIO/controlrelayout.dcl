definition module controlrelayout


//	Clean Object I/O library, version 1.2


import	relayout, wstate


relayoutControls :: !OSWindowMetrics !Bool !Rect !Rect !Point2 !Point2 !OSWindowPtr !(Maybe Id) ![WElementHandle .ls .pst] ![WElementHandle .ls .pst]
					!*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
relayoutControls`:: !OSWindowMetrics !Bool !Rect !Rect !Point2 !Point2 !OSWindowPtr !(Maybe Id) ![WElementHandle`]         ![WElementHandle`]
					!*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
/*	relayoutControls(`) wMetrics isAble oldFrame newFrame oldParentPos newParentPos parentPtr defaultId old new
	resizes, moves, and updates changed WElementHandle(`)s. 
		isAble							is True iff the parent window/compound is Able.
		oldFrame						is the clipping rect of the parent window/compound at the original location and size.
		newFrame						is the clipping rect of the parent window/compound at the new location and size.
		oldParentPos and newParentPos	are the positions of the respective parent window/compound of the elements.
		parentPtr						is the parent window/dialog.
		defaultId						is the optional Id of the default control.
		old								contains the elements at their original location and size.
		new								contains the elements at their new location and size.
	relayoutControls(`) assumes that the two lists contain elements that are identical except for size and position.
		If this is not the case, a runtime error will occur.
	relayoutControls(`) assumes that the ClipStates of all compound elements are valid.
	The return OSRgnHandle is the area of the window that requires to be updated (use updatewindowbackgrounds [windowupdate] for this purpose).
*/
