definition module controllayout


//	Clean Object I/O library, version 1.2


from	ostoolbox	import OSToolbox
from	ostypes		import Rect
from	oswindow	import OSWindowMetrics
import	wstate


layoutControls :: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Size !Size ![(ViewDomain,Point2)] ![WElementHandle .ls .ps] !*OSToolbox
																								   -> (!Size,![WElementHandle .ls .ps],!*OSToolbox)
layoutControls`:: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Size !Size ![(ViewDomain,Point2)] ![WElementHandle`] !*OSToolbox
																								   -> (!Size,![WElementHandle`],!*OSToolbox)
/*	layoutControls(`) calculates the layout of the controls given the horizontal and vertical margins, item spaces, 
	prefered size, minimum size of the area in which the controls are positioned. The list of (ViewDomain,Point2) contains
	the current view domains and origins of all parent objects in ascending order. 
	The result size is the actual size.
*/
