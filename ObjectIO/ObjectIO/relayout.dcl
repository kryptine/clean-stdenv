definition module relayout

//	Clean Object I/O library, version 1.2

import	windowhandle
import	osfont, ossystem, ostoolbox, ostypes


::	RelayoutItem
	=	{	rliItemKind		:: !ControlKind			// The control kind
		,	rliItemPtr		:: !OSWindowPtr			// The ptr to the item
		,	rliItemPos		:: !Point2				// The exact position of the item
		,	rliItemSize		:: !Size				// The exact size of the item
		,	rliItemSelect	:: !Bool				// The item is Able (True) or Unable (False)
		,	rliItemShow		:: !Bool				// The item is visible (True) or invisible (False)
		,	rliItemInfo		:: CompoundInfo			// If the control kind is IsCompoundControl: its CompoundInfo; otherwise undefined
		,	rliItemLook		:: LookInfo				// If the control kind is IsCustom(Button)Control: its LookInfo; otherwise undefined
		,	rliItems		:: ![RelayoutItem]		// If the control kind is Is(Compound/Layout)Control: its elements; otherwise: []
		}

relayoutItems :: !OSWindowMetrics !Rect !Rect !Point2 !Point2 !OSWindowPtr ![RelayoutItem] ![RelayoutItem] !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
/*	relayoutItems resizes and moves changed items.
		The two Rect   arguments are the window frames in which the elements reside.
		The two Point2 arguments are the positions of the parent window/compound.
		The OSWindowPtr is the parent window/dialog.
		The first  RelayoutItem list contains the elements at their original location and size.
		The second RelayoutItem list contains the elements at their new location and size.
	Assumptions: 
		* The two lists contain elements that are identical except for size and position
		* (Radio/Check)Controls are flattened and have rliItemKind Is(Radio/Check)Control
		* The ClipStates of CompoundControls are valid.
	relayoutItems returns the background region that needs to be updated.
*/
