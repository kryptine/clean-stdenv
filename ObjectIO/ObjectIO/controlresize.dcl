definition module controlresize


//	Clean Object I/O library, version 1.2


import	ossystem
import	wstate


resizeControls :: !OSWindowMetrics !Bool !Bool !WIDS !Origin !Size !Size !(WindowHandle .ls .pst) !*OSToolbox
																	  -> (!WindowHandle .ls .pst, !*OSToolbox)
/*	resizeControls wMetrics isActive updateAll wids oldOrigin oldSize newSize theWindow
		recalculates the size and positions of the controls of the window.
		The OSWindowMetrics	argument are the window metrics of the platform.
		The Bool			argument isActive is True iff theWindow is the active window.
		The Bool			argument updateAll is True if theWindow must be completely updated.
		The WIDS			argument identifies the window.
		The oldOrigin		argument must be the old origin of the window.
		The oldSize			argument must be the old size of the window (excluding scrollbars). (Note: can be larger than old view frame!)
		The newSize			argument must be the new size of the window (excluding scrollbars). (Note: can be larger than new view frame!)
		The theWindow		argument must not be a window placeholder. 
	resizeControls assumes that the window itself already has the proper new size.
*/
