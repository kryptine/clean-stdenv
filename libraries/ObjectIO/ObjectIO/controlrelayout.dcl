definition module controlrelayout


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	relayout, wstate


relayoutControls :: !OSWindowMetrics !OSWindowPtr !(Maybe Id) !Bool !Bool !(!OSRect,!Point2,!Vector2,![WElementHandle`])
                                                                          !(!OSRect,!Point2,!Vector2,!*[WElementHandle .ls .pst]) 
                                                                          !*OSToolbox
                             -> (!OSRgnHandle,!*[WElementHandle .ls .pst],!*OSToolbox)
relayoutControls` :: !OSWindowMetrics !OSWindowPtr !(Maybe Id) !Bool !Bool !(!OSRect,!Point2,!Vector2,![WElementHandle`])
                                                                           !(!OSRect,!Point2,!Vector2,![WElementHandle`]) 
                                                                           !*OSToolbox
                                                          -> (!OSRgnHandle,!*OSToolbox)
/*	relayoutControls(`) wMetrics parentPtr defaultId isAble isVisible (oldFrame,oldParentPos,oldCompoundPos,old) 
                                                                      (newFrame,newParentPos,newCompoundPos,new)
	resizes, moves, and updates changed WElementHandle(`)s. 
		parentPtr							is the parent window/dialog.
		defaultId							is the optional Id of the default control.
		isAble								is True iff the parent window/compound is Able.
		isVisible							is True iff the parent window/compound is shown.
		oldFrame							is the clipping rect of the parent window/compound at the original location and size.
		newFrame							is the clipping rect of the parent window/compound at the new location and size.
		oldParentPos   and newParentPos		are the positions of the respective parent window/compound/layout of the elements.
		oldCompoundPos and newCompoundPos	are the positions of the respective parent window/compound of the elements.
		old									contains the elements at their original location and size.
		new									contains the elements at their new location and size.
	relayoutControls(`) assumes that the two lists contain elements that are identical except for size and position.
		If this is not the case, a runtime error will occur.
	relayoutControls(`) assumes that the ClipStates of all compound elements are valid.
	The return OSRgnHandle is the area of the window that requires to be updated (use updatewindowbackgrounds [windowupdate] for this purpose).
*/
