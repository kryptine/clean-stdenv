definition module oswindow


//	Clean Object I/O library, version 1.2


import StdMaybe, StdOverloaded, StdString
import ostypes
from osdocumentinterface	import OSDInfo, OSMDInfo, OSSDInfo, OSInfo, OSToolbar, OSToolbarHandle, HMENU, HWND
from osevent				import OSEvents, OSEvent, CrossCallInfo
from osfont					import Font
from osrgn					import OSRgnHandle
from ossystem				import OSWindowMetrics
from ostoolbox				import OSToolbox
from ospicture				import OSPictContext


/*	System dependent constants:
*/
OSControlTitleSpecialChars :== []					// Special prefix characters that should be removed


/*	System dependent metrics:
*/
OSMinWindowSize					:: (!Int,!Int)
OSMinCompoundSize				:: (!Int,!Int)


/*	Initialisation:
*/
OSinitialiseWindows	:: !*OSToolbox -> *OSToolbox


/*	Determine the size of controls:
	OSgetButtonControlSize windowmetrics title
		returns the size(height) of the ButtonControl that has the given title.
	OSgetTextControlSize windowmetrics title
		returns the size of the TextControl   that has the given title.
	OSgetEditControlSize windowmetrics width nr
		returns the size of the EditControl that has the given width and should show nr of lines.
	OSgetPopUpControlSize windowmetrics items
		returns the size of the PopUpControl that thas the given list of items.
	OSget(Radio/Check)ControlItemSize windowmetrics title
		returns the size of the (Radio/Check)ControlItem that has the given title.
	OSget(Radio/Check)ControlItemHeight windowmetrics
		returns the height of an individual (Radio/Check)ControlItem.
	OSgetSliderControlSize windowmetrics isHorizontal length
		returns the correct size of the SliderControl given its direction (True iff Horizontal) and length.
*/
OSgetButtonControlSize		:: !OSWindowMetrics !String		!*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetTextControlSize		:: !OSWindowMetrics !String		!*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetEditControlSize		:: !OSWindowMetrics !Int !Int	!*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetPopUpControlSize		:: !OSWindowMetrics ![String]	!*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetRadioControlItemSize	:: !OSWindowMetrics !String		!*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetCheckControlItemSize	:: !OSWindowMetrics !String		!*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetSliderControlSize		:: !OSWindowMetrics !Bool !Int -> (!Int,!Int)

/*	Determine the height of controls.
*/
OSgetButtonControlHeight	:: !OSWindowMetrics			-> Int
OSgetTextControlHeight		:: !OSWindowMetrics			-> Int
OSgetEditControlHeight		:: !OSWindowMetrics !Int	-> Int
OSgetPopUpControlHeight		:: !OSWindowMetrics			-> Int
OSgetRadioControlItemHeight	:: !OSWindowMetrics			-> Int
OSgetCheckControlItemHeight	:: !OSWindowMetrics			-> Int

/*	Determine the minimum width of controls.
*/
OSgetButtonControlMinWidth		:: !OSWindowMetrics -> Int
OSgetTextControlMinWidth		:: !OSWindowMetrics -> Int
OSgetEditControlMinWidth		:: !OSWindowMetrics -> Int
OSgetPopUpControlMinWidth		:: !OSWindowMetrics -> Int
OSgetRadioControlItemMinWidth	:: !OSWindowMetrics -> Int
OSgetCheckControlItemMinWidth	:: !OSWindowMetrics -> Int
OSgetSliderControlMinWidth		:: !OSWindowMetrics -> Int


/*	Window creation functions.
	OScreateDialog	isModal
					isClosable title pos size behindPtr
					getcontrolfocus createcontrols updatecontrols osdocinfo controlinfo
					creates a dialog with the given title, position and size. 
					The isModal		argument is True iff the dialog is modal.
					The behindPtr	argument is OSNoWindowPtr if the dialog must be created topmost;
									it is an OSWindowPtr if it must be placed behind a given window.
	OScreateWindow	isResizable hScrollInfo vScrollInfo minSize maxSize
					isClosable title pos size
					getcontrolfocus createcontrols updatecontrols osdocinfo behindPtr controlinfo
					creates a window with the given title, position and size. 
					The isResizable	argument is True iff the window is user resizable.
					The hScrollInfo	argument represents the horizontal scrollbar of the window.
					The vScrollInfo	argument represents the vertical   scrollbar of the window.
					The minSize		argument is the minimum size of the window.
					The maxSize		argument is the maximum size of the window.
					The return OSWindowPtrs (result 3,4) are the OSWindowPtrs of the scrollbars.
	The isClosable		argument is True iff the window/dialog is user closeable.
	The title			argument is the title of the window/dialog.
	The pos				argument is the position of the window/dialog.
	The size			argument is the size of the window/dialog, including scrollbars, excluding title bar and frame.
	The getcontrolfocus	argument function returns the handle to the control that 
						has the input focus.
	The createcontrols	argument function creates the controls of the window/dialog, 
						given the handle to the created window/dialog and the proper control information.
	The updatecontrols	argument function updates the customised controls of the window/dialog.
	The osdocinfo		argument gives the document interface of the parent process.
	The return [DelayActivationInfo] are the OSWindowPtrs of windows/dialogs that have become (in)active (in that order).
	The return  OSWindowPtr is the OSWindowPtr of the created window/dialog.
	The return OSDInfo is the validated OSDInfo of the parent process.
*/

OScreateDialog :: !Bool
				  !Bool !String !(!Int,!Int) !(!Int,!Int) !OSWindowPtr
				  !(u:s->*(OSWindowPtr,u:s))
				  !(OSWindowPtr-> u:s -> u:(*OSToolbox -> *(u:s,*OSToolbox)))
				  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !OSDInfo !u:s !*OSToolbox
			   -> (![DelayActivationInfo],!OSWindowPtr,!u:s,!*OSToolbox)
OScreateWindow :: !OSWindowMetrics !Bool !ScrollbarInfo !ScrollbarInfo !(!Int,!Int) !(!Int,!Int)
				  !Bool !String !(!Int,!Int) !(!Int,!Int)
				  !(u:s->*(OSWindowPtr,u:s))
				  !(OSWindowPtr-> u:s -> u:(*OSToolbox -> *(u:s,*OSToolbox)))
				  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !OSDInfo !OSWindowPtr !u:s !*OSToolbox
			   -> (![DelayActivationInfo],!OSWindowPtr,!OSWindowPtr,!OSWindowPtr,!OSDInfo,!u:s,!*OSToolbox)
OScreateModalDialog :: !Bool !String !OSDInfo !(Maybe OSWindowPtr) !(u:s -> (OSEvents,u:s))
																   !((OSEvents,u:s)-> u:s)
																   !(OSEvent -> u:s -> *([Int],u:s))
						!u:s !*OSToolbox
			  -> (!Bool,!u:s,!*OSToolbox)


/*	Control creation functions:
	OScreateRadioControl parentWindow parentPos title able pos size selected isfirst
		creates a RadioControl in the window identified by parentWindow. 
	OScreateCheckControl parentWindow parentPos title able pos size selected isfirst
		creates a CheckControl in the window identified by parentWindow.
	OScreateEmptyPopUpControl parentWindow parentPos able pos nrItems keySensitive
		creates an initially empty PopUpControl that will display nrItems elements.
		The boolean keySensitive holds iff the PopUpControl should respond to keyboard input (is editable).
		The first OSWindowPtr is the PopUpControl, the second OSWindowPtr is the EditControl (if editable).
	OScreatePopUpControlItem parentPopUp pos able title selected
		adds an item title to the parentPopUp PopUpControl. The pos argument determines the location of 
		the item title. If (-1), the item is appended, otherwise it is created behind the item with the
		given pos index. The return Int is its zero based index.
	OScreateSliderControl parentWindow parentPos show able horizontal pos size range
		creates a horizontal (True) or vertical (False) SliderControl in the window identified by parentWindow.
	OScreateTextControl parentWindow parentPos text pos size
		creates a TextControl in the window identified by parentWindow.
	OScreateEditControl parentWindow parentPos text able isKeySensitive pos size
		creates an EditControl in the window identified by parentWindow.
	OScreateButtonControl parentWindow parentPos title able pos size okOrCancel
		creates a ButtonControl in the window identified by parentWindow.
	OScreateCustomButtonControl parentWindow parentPos able pos size okOrCancel
		creates a CustomButtonControl in the window identified by parentWindow.
	OScreateCustomControl parentWindow parentPos able pos size 
		creates a CustomControl in the window identified by parentWindow.
	OScreateCompoundControl parentWindow parentPos show able isTransparent pos size hScrollInfo vScrollInfo
		creates a CompoundControl in the window identified by parentWindow.
		The Boolean isTransparent should be True iff the CompoundControl has no ControlLook attribute.
*/
::	OKorCANCEL
	=	OK | CANCEL | NORMAL

OScreateRadioControl		:: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Bool !Bool !*OSToolbox
																					 -> (!OSWindowPtr,!*OSToolbox)
OScreateCheckControl		:: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Bool !Bool !*OSToolbox
																					 -> (!OSWindowPtr,!*OSToolbox)
OScreateEmptyPopUpControl	:: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Int !Bool !*OSToolbox
																	  -> (!OSWindowPtr,!OSWindowPtr,!*OSToolbox)
OScreatePopUpControlItem	:: !OSWindowPtr !Int !Bool !String !Bool !*OSToolbox -> (!Int,!*OSToolbox)
OScreateSliderControl		:: !OSWindowPtr !(!Int,!Int) !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int) !(!Int,!Int,!Int,!Int) !*OSToolbox
																										   -> (!OSWindowPtr,!*OSToolbox)
OScreateTextControl			:: !OSWindowPtr !(!Int,!Int) !String !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox
																   -> (!OSWindowPtr,!*OSToolbox)
OScreateEditControl			:: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox
																							-> (!OSWindowPtr,!*OSToolbox)
OScreateButtonControl		:: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !OKorCANCEL !*OSToolbox
																			   -> (!OSWindowPtr,!*OSToolbox)
OScreateCustomButtonControl	:: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !OKorCANCEL !*OSToolbox
																	   -> (!OSWindowPtr,!*OSToolbox)
OScreateCustomControl		:: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox
																 -> (!OSWindowPtr,!*OSToolbox)

::	ScrollbarInfo
	=	{	cbiHasScroll	:: !Bool				// The scrollbar exists
		,	cbiPos			:: (Int,Int)			// Its position within the parent
		,	cbiSize			:: (Int,Int)			// Its size within the parent
		,	cbiState		:: (Int,Int,Int,Int)	// Its (min,thumb,max,thumbsize) settings
		}

OScreateCompoundControl		:: !OSWindowMetrics !OSWindowPtr !(!Int,!Int) !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int)
										 !ScrollbarInfo !ScrollbarInfo !*OSToolbox
							-> (!OSWindowPtr,!OSWindowPtr,!OSWindowPtr,!*OSToolbox)


/*	Window destruction operations.
	OSdestroyWindow osDInfo isModal isWindow window
		destroys the window identified by window. 
		The first Boolean isModal is True iff the window is Modal.
		The second Boolean isWindow is True iff the window is a Window.
*/
//OSdestroyWindow :: !OSDInfo !Bool !Bool !OSWindowPtr !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
OSdestroyWindow :: !OSDInfo !Bool !Bool !OSWindowPtr !(OSEvent -> .s -> ([Int],.s)) !.s !*OSToolbox
														  -> (![DelayActivationInfo],.s,!*OSToolbox)


/*	Control destruction operations.
*/
OSdestroyRadioControl		:: !OSWindowPtr	!*OSToolbox -> *OSToolbox
OSdestroyCheckControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyPopUpControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroySliderControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyTextControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyEditControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyButtonControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyCustomButtonControl:: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyCustomControl		:: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyCompoundControl	:: !OSWindowPtr !*OSToolbox -> *OSToolbox


/*	Control update operations.
	OSupdateRadioControl	area parentWindow theControl updates the area of theControl in parentWindow
	OSupdateCheckControl	area parentWindow theControl updates the area of theControl in parentWindow
	OSupdatePopUpControl	area parentWindow theControl updates the area of theControl in parentWindow
	OSupdateSliderControl	area parentWindow theControl updates the area of theControl in parentWindow
	OSupdateTextControl		area parentWindow theControl updates the area of theControl in parentWindow
	OSupdateEditControl		area parentWindow theControl updates the area of theControl in parentWindow
	OSupdateButtonControl	area parentWindow theControl updates the area of theControl in parentWindow
	OSupdateCompoundControl area parentWindow theControl updates the area of theControl in parentWindow
*/
OSupdateRadioControl		:: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateCheckControl		:: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdatePopUpControl		:: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateSliderControl		:: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateTextControl			:: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateEditControl			:: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateButtonControl		:: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateCompoundControl		:: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox


/*	Control clipping operations.
	OSclipRadioControl        parentWindow parentPos area pos size generates the clipping region of a radio control within area.
	OSclipCheckControl        parentWindow parentPos area pos size generates the clipping region of a check control within area.
	OSclipPopUpControl        parentWindow parentPos area pos size generates the clipping region of a pop up control within area.
	OSclipSliderControl       parentWindow parentPos area pos size generates the clipping region of a slider control within area.
	OSclipTextControl         parentWindow parentPos area pos size generates the clipping region of a text control within area.
	OSclipEditControl         parentWindow parentPos area pos size generates the clipping region of a edit control within area.
	OSclipButtonControl       parentWindow parentPos area pos size generates the clipping region of a button control within area.
	OSclipCustomButtonControl parentWindow parentPos area pos size generates the clipping region of a custom button control within area.
	OSclipCustomControl       parentWindow parentPos area pos size generates the clipping region of a custom control within area.
	OSclipCompoundControl     parentWindow parentPos area pos size generates the clipping region of a compound control within area.
*/
OSclipRadioControl			:: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipCheckControl			:: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipPopUpControl			:: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipSliderControl			:: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipTextControl			:: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipEditControl			:: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipButtonControl			:: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipCustomButtonControl	:: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipCustomControl			:: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipCompoundControl		:: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)


/*	Window graphics context access operations.
	OSgrabWindowPictContext theWindow
		returns the graphics context that must be used to update the window.
	OSreleaseWindowPictContext theWindow theContext
		releases the graphics context.
	OSgrabControlPictContext theWindow theControl
		returns the graphics context that must be used to update the control.
	OSreleaseControlPictContext theControl theContext
		releases the graphics context.
*/
OSgrabWindowPictContext		:: !OSWindowPtr					!*OSToolbox -> (!OSPictContext,!*OSToolbox)
OSgrabControlPictContext	:: !OSWindowPtr !OSWindowPtr	!*OSToolbox -> (!OSPictContext,!*OSToolbox)
OSreleaseWindowPictContext	:: !OSWindowPtr !OSPictContext	!*OSToolbox -> *OSToolbox
OSreleaseControlPictContext :: !OSWindowPtr !OSPictContext	!*OSToolbox -> *OSToolbox


/*	Scrollbar operations.
*/

/*	toOSscrollbarRange (domainMin,viewMin,domainMax) viewSize
		maps the (domainMin,viewMin,domainMax) viewSize values to proper OS values (osRangeMin,osThumb,osRangeMax,osThumbSize).
	fromOSscrollbarRange (domainMin,domainMax) osThumb
		maps the osThumb value between the (domainMin,domainMax) values.
	These values are also valid for CompoundControls.
	Both functions assume that:domainMin<=viewMin<= domainMax 
						  and osRangeMin<=osThumb<=osRangeMax.
	OSscrollbarIsVisible (domainMin,domainMax) viewSize
		determines whether the scrollbar is visible given these settings.
	OSscrollbarsAreVisible wMetrics windowDomain size (hasHScroll,hasVScroll)
		determines the visibility of the horizontal/vertical scrollbars given the domain, size, and presence.
*/
toOSscrollbarRange		:: !(!Int,!Int,!Int) !Int -> (!Int,!Int,!Int,!Int)
fromOSscrollbarRange	:: !(!Int,!Int) !Int -> Int
OSscrollbarIsVisible	:: !(!Int,!Int) !Int -> Bool
OSscrollbarsAreVisible	:: !OSWindowMetrics !Rect !(!Int,!Int) !(!Bool,!Bool) -> (!Bool,!Bool)


/*	Window access operations.
*/

/*	OSsetWindowSliderThumb theWindow isHorizontal thumb redraw
		sets the thumb value of the horizontal/vertical slider of the given window.
	OSsetWindowSliderThumbSize theWindow isHorizontal size redraw
		sets the view size of the horizontal/vertical slider of the given window.
	OSsetWindowSlider theWindow isHorizontal (osRangeMin,osThumb,osRangeMax,osThumbSize)
		sets all values of the horizontal/vertical slider of the given window.
*/
OSsetWindowSliderThumb		:: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetWindowSliderThumbSize	:: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetWindowSlider			:: !OSWindowMetrics !OSWindowPtr !Bool !(!Int,!Int,!Int,!Int) !(!Int,!Int) !*OSToolbox -> *OSToolbox


/*	OSinvalidateWindow theWindow
		invalidates the window identified by theWindow, forcing an update event for the entire contents.
	OSinvalidateWindowRect theWindow part
		invalidates the part of the window identified by theWindow, forcing an update event for that part.
	OSvalidateWindowRect theWindow part
		validates the Rect part of the window identified by theWindow, eliminating the need to update that part.
	OSvalidateWindowRgn theWindow part
		validate the Rgn part of the window identified by the theWindow, eliminating the need to update that part.
*/
OSinvalidateWindow		:: !OSWindowPtr					!*OSToolbox -> *OSToolbox
OSinvalidateWindowRect	:: !OSWindowPtr !Rect			!*OSToolbox -> *OSToolbox
OSvalidateWindowRect	:: !OSWindowPtr !Rect			!*OSToolbox -> *OSToolbox
OSvalidateWindowRgn		:: !OSWindowPtr !OSRgnHandle	!*OSToolbox -> *OSToolbox


/*	OS(dis/en)ableWindow theWindow
		(dis/en)able the window identified by theWindow.
		The Boolean tuple indicates whether the window has a horizontal/vertical scrollbar.
		The last Boolean argument indicates whether the window is (dis/en)abled because of a modal dialogue.
*/
OSdisableWindow			:: !OSWindowPtr !(!Bool,!Bool) !Bool !*OSToolbox -> *OSToolbox
OSenableWindow			:: !OSWindowPtr !(!Bool,!Bool) !Bool !*OSToolbox -> *OSToolbox

/*	OSactivateWindow osdInfo thisWindow handleEvents info
		activates thisWindow. The handleEvents function is applied when updates are required.
	OSactivateControl parentWindow theControl
		activates theControl which is in parentWindow. 
	OSstackWindow thisWindow behindWindow
		moves the window identified by thisWindow behind the window identified by behindWindow.
		OSstackWindow assumes that thisWindow and behindWindow are valid values.
*/
//OSactivateWindow	:: !OSDInfo     !OSWindowPtr	!*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
OSactivateWindow	:: !OSDInfo !OSWindowPtr !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !.s !*OSToolbox
															   -> (![DelayActivationInfo],!.s,!*OSToolbox)
OSactivateControl	:: !OSWindowPtr !OSWindowPtr	!*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
//OSstackWindow		:: !OSWindowPtr !OSWindowPtr	!*OSToolbox -> *OSToolbox
OSstackWindow		:: !OSWindowPtr !OSWindowPtr !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !.s !*OSToolbox
																   -> (![DelayActivationInfo],!.s,!*OSToolbox)

/*	OShideWindow thisWindow activate
		hides the window. If the Boolean activate is True then a new window is made the active window.
	OSshowWindow thisWindow activate
		shows the window. If the Boolean activate is True then the window is also made the active 
		window. If the Boolean activate is False then the stacking order is not changed.
*/
OShideWindow	:: !OSWindowPtr !Bool !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
OSshowWindow	:: !OSWindowPtr !Bool !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)

/*	OSsetWindowCursor sets the new cursor shape.
*/
OSsetWindowCursor :: !OSWindowPtr !Int !*OSToolbox -> *OSToolbox

/*	OSgetWindowPos           returns the current position of the window.
	OSgetWindowViewFrameSize returns the current size of the window view frame.
	OSgetWindowSize          returns the current size of the window including bounds.
	OSsetWindowPos           sets the position of the window.
	OSsetWindowViewFrameSize sets the size of the window view frame.
	OSsetWindowSize          sets the size of the window.
*/
OSgetWindowPos			:: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetWindowViewFrameSize:: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetWindowSize         :: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSsetWindowPos			:: !OSWindowPtr !(!Int,!Int) !Bool !Bool !*OSToolbox -> *OSToolbox
OSsetWindowViewFrameSize:: !OSWindowPtr !(!Int,!Int)             !*OSToolbox -> *OSToolbox
OSsetWindowSize			:: !OSWindowPtr !(!Int,!Int) !Bool       !*OSToolbox -> *OSToolbox

/*	OSsetWindowTitle sets the title of the window.
*/
OSsetWindowTitle:: !OSWindowPtr !String !*OSToolbox -> *OSToolbox


/*	Control access operations.
*/

/*	On compound controls:
	OSinvalidateCompound compoundPtr
		invalidates the compound control, forcing an update event for the entire contents.
	OSinvalidateCompoundRect compoundPtr part
		invalidates the part of the compound control, forcing an update event for that part.
	OSsetCompoundSliderThumb compoundPtr isHorizontal thumb (maxx,maxy) redraw
		sets the thumb value of the horizontal/vertical slider of the given compound control.
		(maxx,maxy) are the maximum x and y coordinates of the enclosing rectangle of the slider.
	OSsetCompoundSliderThumbSize compoundPtr isHorizontal size (maxx,maxy) redraw
		sets the view size of the horizontal/vertical slider of the given compound control.
		(maxx,maxy) are the maximum x and y coordinates of the enclosing rectangle of the slider.
	OSsetCompoundSlider compoundPtr isHorizontal (osRangeMin,osThumb,osRangeMax,osThumbSize)
		sets all slider values of the horizontal/vertical slider of the given compound control.
	OSsetCompoundSelect parentWindow compoundPtr clipRect (hasHScroll,hasVScroll) toAble
		enables the compound control (if toAble), or disables the compound control (if (not toAble)), while clipping.
	OSsetCompoundShow parentWindow compoundPtr clipRect show
		shows the compound control (if show), or hides the compound control (if (not show)), while clipping.
	OSsetCompoundPos parentWindow parentPos compoundPtr pos size update
		sets the new position of the compound control and updates the control if update holds. 
	OSsetCompoundSize parentWindow parentPos compoundPtr pos size update
		sets the new size of the compound control and updates the control if update holds.
	OSCompoundMovesControls
		is True iff moving a compound control also moves its elements.
*/
OSinvalidateCompound			:: !OSWindowPtr																!*OSToolbox -> *OSToolbox
OSinvalidateCompoundRect		:: !OSWindowPtr !Rect														!*OSToolbox -> *OSToolbox
OSsetCompoundSliderThumb		:: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool				!*OSToolbox -> *OSToolbox
OSsetCompoundSliderThumbSize	:: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool				!*OSToolbox -> *OSToolbox
OSsetCompoundSlider				:: !OSWindowMetrics !OSWindowPtr !Bool !(!Int,!Int,!Int,!Int) !(!Int,!Int)	!*OSToolbox -> *OSToolbox
OSsetCompoundSelect				:: !OSWindowPtr !OSWindowPtr !Rect !(!Bool,!Bool) !Bool						!*OSToolbox -> *OSToolbox
OSsetCompoundShow				:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetCompoundPos				:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
OSsetCompoundSize				:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
OSCompoundMovesControls			:== True


/*	On slider controls:
	OSsetSliderThumb parentWindow sliderPtr clipRect redraw (min,thumb,max)
		sets the thumb value of the slider control, while clipping.
	OSsetSliderControlSelect parentWindow sliderPtr clipRect toAble
		enables the slider control (if toAble), or disables the slider control (if (not toAble)), while clipping.
	OSsetSliderControlShow parentWindow sliderPtr clipRect show
		shows the slider control (if show), or hides the slider control (if (not show)), while clipping.
	OSsetSliderControlPos parentWindow parentPos sliderPtr pos size update
		sets the new position of the slider control and updates the control if update holds.
	OSsetSliderControlSize parentWindow parentPos sliderPtr pos size update
		sets the new size of the slider control and updates the control if update holds.
*/
OSsetSliderThumb				:: !OSWindowPtr !OSWindowPtr !Rect !Bool !(!Int,!Int,!Int)					!*OSToolbox -> *OSToolbox
OSsetSliderControlSelect		:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetSliderControlShow			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetSliderControlPos			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
OSsetSliderControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox

/*	On radio controls:
	OSsetRadioControl parentWindow current new clipRect
		removes the selection from current and sets the selection to new, while clipping.
	OSsetRadioControlSelect parentWindow radioPtr clipRect toAble
		enables the radio control (if toAble), or disables the radio control (if (not toAble)), while clipping.
	OSsetRadioControlShow parentWindow radioPtr clipRect show
		shows the radio control (if show), or hides the radio control (if (not show)), while clipping.
	OSsetRadioControlPos parentWindow parentPos radioPtr pos size update
		sets the new position of the radio control and updates the control if update holds.
	OSsetRadioControlSize parentWindow parentPos radioPtr pos size update
		sets the new size of the radio control and updates the control if update holds.
*/
OSsetRadioControl				:: !OSWindowPtr !OSWindowPtr !OSWindowPtr !Rect								!*OSToolbox -> *OSToolbox
OSsetRadioControlSelect			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetRadioControlShow			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetRadioControlPos			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
OSsetRadioControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox

/*	On check controls:
	OSsetCheckControl parentWindow checkPtr clipRect marked
		sets the check mark (if marked) or removes the check mark (if not marked) of the check control, while clipping.
	OSsetCheckControlSelect parentWindow checkPtr clipRect toAble
		enables the check control (if toAble), or disables the check control (if (not toAble)), while clipping.
	OSsetCheckControlShow parentWindow checkPtr clipRect show
		shows the check control (if show), or hides the check control (if (not show)), while clipping.
	OSsetCheckControlPos parentWindow parentPos checkPtr pos size update
		sets the new position of the check control and updates the control if update holds.
	OSsetCheckControlSize parentWindow parentPos checkPtr pos size update
		sets the new size of the check control and updates the control if update holds.
*/
OSsetCheckControl				:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetCheckControlSelect			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetCheckControlShow			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetCheckControlPos			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int)	!Bool	!*OSToolbox -> *OSToolbox
OSsetCheckControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int)	!Bool	!*OSToolbox -> *OSToolbox

/*	On pop up controls:
	OSsetPopUpControl parentWindow popupPtr clipRect current new newtext shown
		removes the selection from current and sets the selection to new, while clipping. Both indices are zero based!
	OSsetPopUpControlSelect parentWindow popupPtr clipRect toAble
		enables the pop up control (if toAble), or disables the pop up control (if (not toAble)), while clipping.
	OSsetPopUpControlShow parentWindow popupPtr clipRect show
		shows the pop up control (if show), or hides the pop up control (if (not show)), while clipping.
	OSsetPopUpControlPos parentWindow parentPos popupPtr pos size update
		sets the new position of the pop up control and updates the control if update holds.
	OSsetPopUpControlSize parentWindow parentPos popupPtr pos size update
		sets the new size of the pop up control and updates the control if update holds.
*/
OSsetPopUpControl				:: !OSWindowPtr !OSWindowPtr !Rect !Rect !Int !Int !String !Bool			!*OSToolbox -> *OSToolbox
OSsetPopUpControlSelect			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetPopUpControlShow			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetPopUpControlPos			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
OSsetPopUpControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox

/*	On edit controls:
	OSsetEditControlText parentWindow editPtr clipRect itemRect shown text 
		sets the text of the shown edit control while clipping.
	OSgetEditControlText parentWindow editPtr 
		returns the current content of the edit control.
	OSsetEditControlCursor parentWindow editPtr clipRect itemRect pos
		sets the cursor position at pos of the edit control while clipping.
	OSsetEditControlSelect parentWindow editPtr clipRect toAble
		enables the edit control (if toAble), or disables the edit control (if (not toAble)), while clipping.
	OSsetEditControlShow parentWindow editPtr clipRect show
		shows the edit control (if show), or hides the edit control (if (not show)), while clipping.
	OSsetEditControlPos parentWindow parentPos editPtr pos size update
		sets the new position of the edit control and updates the control if update holds.
	OSsetEditControlSize parentWindow parentPos editPtr pos size update
		sets the new size of the edit control and updates the control if update holds.
*/
OSsetEditControlText			:: !OSWindowPtr !OSWindowPtr !Rect !Rect !Bool !String						!*OSToolbox -> *OSToolbox
OSgetEditControlText			:: !OSWindowPtr !OSWindowPtr												!*OSToolbox -> (!String,!*OSToolbox) 
OSsetEditControlCursor			:: !OSWindowPtr !OSWindowPtr !Rect !Rect !Int								!*OSToolbox -> *OSToolbox
OSsetEditControlSelect			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetEditControlShow			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetEditControlPos				:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
OSsetEditControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox

/*	On text controls:
	OSsetTextControlText parentWindow textPtr clipRect itemRect shown text
		sets the text of the shown edit control while clipping.
	OSsetTextControlSelect parentWindow textPtr clipRect toAble
		enables the text control (if toAble), or disables the text control (if (not toAble)), while clipping.
	OSsetTextControlShow parentWindow textPtr clipRect show
		shows the text control (if show), or hides the text control (if (not show)), while clipping.
	OSsetTextControlPos parentWindow parentPos textPtr pos size update
		sets the new position of the text control and updates the control if update holds.
	OSsetTextControlSize parentWindow parentPos textPtr pos size update
		sets the new size of the text control and updates the control if update holds.
*/
OSsetTextControlText			:: !OSWindowPtr !OSWindowPtr !Rect !Rect !Bool !String						!*OSToolbox -> *OSToolbox
OSsetTextControlSelect			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetTextControlShow			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetTextControlPos				:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
OSsetTextControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox

/*	On button controls:
	OSsetButtonControlText parentWindow buttonPtr clipRect text
		sets the text of the button control while clipping.
	OSsetButtonControlSelect parentWindow buttonPtr clipRect toAble
		enables the button control (if toAble), or disables the button control (if (not toAble)), while clipping.
	OSsetButtonControlShow parentWindow buttonPtr clipRect show
		shows the button control (if show), or hides the button control (if (not show)), while clipping.
	OSsetButtonControlPos parentWindow parentPos buttonPtr pos size update
		sets the new position of the button control and updates the control if update holds.
	OSsetButtonControlSize parentWindow parentPos buttonPtr pos size update
		sets the new size of the button control and updates the control if update holds.
*/
OSsetButtonControlText			:: !OSWindowPtr !OSWindowPtr !Rect !String									!*OSToolbox -> *OSToolbox
OSsetButtonControlSelect		:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetButtonControlShow			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetButtonControlPos			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
OSsetButtonControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox

/*	On custom button controls:
	OSsetCustomButtonControlSelect parentWindow buttonPtr clipRect toAble
		enables the custom button control (if toAble), or disables the custom button control (if (not toAble)), while clipping.
	OSsetCustomButtonControlShow parentWindow buttonPtr clipRect show
		shows the custom button control (if show), or hides the custom button control (if (not show)), while clipping.
	OSsetCustomButtonControlPos parentWindow parentPos buttonPtr pos size update
		sets the new position of the custom button control and updates the custom button if update holds.
	OSsetCustomButtonControlSize parentWindow parentPos buttonPtr pos size update
		sets the new size of the custom button control and updates the custom button if update holds.
*/
OSsetCustomButtonControlSelect	:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetCustomButtonControlShow	:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetCustomButtonControlPos		:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
OSsetCustomButtonControlSize	:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox

/*	On custom controls:
	OSsetCustomControlSelect parentWindow controlPtr clipRect toAble
		enables the custom control (if toAble), or disables the custom control (if (not toAble)), while clipping.
	OSsetCustomControlShow parentWindow controlPtr clipRect show
		shows the custom control (if show), or hides the custom control (if (not show)), while clipping.
	OSsetCustomControlPos parentWindow parentPos controlPtr pos size update
		sets the new position of the custom control and updates the control if update holds.
	OSsetCustomControlSize parentWindow parentPos controlPtr pos size update
		sets the new size of the custom control and updates the control if update holds.
*/
OSsetCustomControlSelect		:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetCustomControlShow			:: !OSWindowPtr !OSWindowPtr !Rect !Bool									!*OSToolbox -> *OSToolbox
OSsetCustomControlPos			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
OSsetCustomControlSize			:: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool	!*OSToolbox -> *OSToolbox
