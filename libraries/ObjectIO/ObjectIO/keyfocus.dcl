definition module keyfocus


//	Clean Object I/O library, version 1.2


import	StdMaybe


::	KeyFocus
	=	{	kfItem		:: !Maybe Int	// Case (Just nr): the item with (wItemNr nr) has the keyboard input focus; Nothing: no item has focus
		,	kfItems		:: ![FocusItem]	// The items of the window that can have the keyboard input focus
		}
::	FocusItem
	=	{	focusNr		:: !Int			// The item nr of the item
		,	focusShow	:: !Bool		// Flag: True iff item is visible
		}

newFocusItems				:: ![FocusItem]								-> KeyFocus

openFocusItems				:: !(Maybe Int) ![FocusItem]	!KeyFocus	-> KeyFocus
closeFocusItems				:: ![Int]						!KeyFocus	-> KeyFocus
showFocusItems				:: ![Int]						!KeyFocus	-> KeyFocus
hideFocusItems				:: ![Int]						!KeyFocus	-> KeyFocus

getCurrentFocusItem			::								!KeyFocus	-> Maybe Int
setNoFocusItem				::								!KeyFocus	-> KeyFocus
setNewFocusItem				:: !Int							!KeyFocus	-> KeyFocus
setNextFocusItem			:: !(Maybe Int)					!KeyFocus	-> (!Maybe Int,!KeyFocus)
