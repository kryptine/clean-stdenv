implementation module keyfocus


//	Clean Object I/O library, version 1.2


import	StdInt, StdBool, StdList, StdTuple, StdFunc, StdMisc
import	commondef
import	StdMaybe


::	KeyFocus
	=	{	kfItem		:: !Maybe Int	// Case (Just nr): the item with (dItemNr nr) has the keyboard input focus; Nothing: no item has focus
		,	kfItems		:: ![FocusItem]	// The items of the window that can have the keyboard input focus
		}
::	FocusItem
	=	{	focusNr		:: !Int			// The item nr of the item
		,	focusShow	:: !Bool		// Flag: True iff item is visible
		}

isShownFocusItem :: !FocusItem -> Bool
isShownFocusItem {focusShow} = focusShow

eqFocusItemNr :: !Int !FocusItem -> Bool
eqFocusItemNr nr {focusNr} = nr==focusNr

newFocusItems :: ![FocusItem] -> KeyFocus
newFocusItems items
	# (found,item)	= Select isShownFocusItem undef items
	= {	kfItem		= if found (Just item.focusNr) Nothing
	  ,	kfItems		= items
	  }

openFocusItems :: !(Maybe Int) ![FocusItem] !KeyFocus -> KeyFocus
openFocusItems (Just behind) new kf=:{kfItems}
	= {kf & kfItems=openFocusItems` behind new kfItems}
where
	openFocusItems` :: !Int ![FocusItem] ![FocusItem] -> [FocusItem]
	openFocusItems` behind new [item=:{focusNr}:items]
		| behind==focusNr	= [item:new++items]
		| otherwise			= [item:openFocusItems` behind new items]
	openFocusItems` _ new _
		= new
openFocusItems _ items kf=:{kfItems}
	= {kf & kfItems=kfItems++items}

closeFocusItems :: ![Int] !KeyFocus -> KeyFocus
closeFocusItems nrs kf=:{kfItem,kfItems}
	= {	kf & kfItems	= closeFocusItems` nrs kfItems
		   , kfItem		= if (isNothing kfItem) kfItem
						 (if (isMember (fromJust kfItem) nrs) Nothing kfItem)
	  }
where
	closeFocusItems` :: ![Int] ![FocusItem] -> [FocusItem]
	closeFocusItems` nrs items
		| isEmpty nrs || isEmpty items
			= []
		# (item,items)	= HdTl items
		  (found,nrs)	= RemoveCheck item.focusNr nrs
		| found
			= closeFocusItems` nrs items
		| otherwise
			= [item:closeFocusItems` nrs items]

showFocusItems :: ![Int] !KeyFocus -> KeyFocus
showFocusItems nrs kf=:{kfItems}
	= {kf & kfItems=setShowFocusItems True nrs kfItems}

setShowFocusItems :: !Bool ![Int] ![FocusItem] -> [FocusItem]
setShowFocusItems show nrs items
	| isEmpty nrs || isEmpty items
		= []
	# (item,items)	= HdTl items
	  (found,nrs)	= RemoveCheck item.focusNr nrs
	| found
		= [{item & focusShow=show}:setShowFocusItems show nrs items]
	| otherwise
		= [ item:setShowFocusItems show nrs items]

hideFocusItems :: ![Int] !KeyFocus -> KeyFocus
hideFocusItems nrs kf=:{kfItem,kfItems}
	= {	kf & kfItems	= setShowFocusItems False nrs kfItems
		   , kfItem		= if (isNothing kfItem) kfItem
						 (if (isMember (fromJust kfItem) nrs) Nothing kfItem)
	  }

getCurrentFocusItem :: !KeyFocus -> Maybe Int
getCurrentFocusItem {kfItem} = kfItem

setNoFocusItem :: !KeyFocus -> KeyFocus
setNoFocusItem kf = {kf & kfItem=Nothing}

setNewFocusItem :: !Int !KeyFocus -> KeyFocus
/*	PA: previous implementation checked for existence of new in kfItems.
		Leave this to the user side. 
setNewFocusItem new kf=:{kfItem,kfItems}
	| isJust kfItem && fromJust kfItem==new
		= kf
	# (found,item)	= Select (eqFocusItemNr new) undef kfItems
	| not found
		= kf
	| item.focusShow
		= {kf & kfItem=Just new}
	| otherwise
		= kf
*/
setNewFocusItem new kf
	= {kf & kfItem=Just new}

setNextFocusItem :: !(Maybe Int) !KeyFocus -> (!Maybe Int,!KeyFocus)
setNextFocusItem (Just behind) kf=:{kfItems}
	# (before,item_after)	= span (not o (eqFocusItemNr behind)) kfItems
	| isEmpty item_after
		= (Nothing,kf)
	# (found,item)	= Select isShownFocusItem undef (tl item_after)
	| found
		= (Just item.focusNr,{kf & kfItem=Just item.focusNr})
	# (found,item)	= Select isShownFocusItem undef before
	| found
		= (Just item.focusNr,{kf & kfItem=Just item.focusNr})
	| otherwise
		= (Nothing,kf)
setNextFocusItem _ kf=:{kfItems}
	# (found,item)	= Select isShownFocusItem undef kfItems
	| not found
		= (Nothing,{kf & kfItem=Nothing})
	| otherwise
		= (Just item.focusNr,{kf & kfItem=Just item.focusNr})
