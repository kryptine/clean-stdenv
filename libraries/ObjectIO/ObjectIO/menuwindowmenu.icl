implementation module menuwindowmenu


//	Version 1.2

//	The definition and implementation of the WindowMenu. 


import	StdInt, StdBool, StdClass, StdList, StdTuple, StdFunc, StdEnum, StdMisc
import	StdMenu, StdMenuElement, StdWindow, StdPSt, StdProcess, StdIOCommon
import	menuinternal, menucreate, id, windowdefaccess, windowaccess
// RWS +++
import commondef

menuwindowmenuError :: String String -> .x
menuwindowmenuError rule error = Error rule "menuwindowmenu" error


/*	openWindowMenu creates the WindowMenu. This menu contains atleast the following elements:
	-	MenuItem "&Cascade":
			Reorder the current list of windows from left-top to right-bottom.
	-	MenuItem "Tile &Horizontally":
			Reorder the current list of windows from top to bottom.
	-	MenuItem "&Tile Vertically":
			Reorder the current list of windows from left to right.
	-	MenuSeparator
	-	RadioMenu:
			Display all current open windows (hidden and shown). Selection activates and
			shows the indicated window.
*/
openWindowMenu :: !(PSt .l .p) -> PSt .l .p
openWindowMenu pState=:{io}
	# (id_types,ioState)	= getMenus io
	  ids					= map fst id_types
	| Contains ((==) wMenuId) ids
	= {pState & io=ioState}
	# (error,ioState)		= OpenMenu` (Just wMenuId) undef (wMenuDef (length ids)) ioState
	| error==NoError
	= {pState & io=ioState}
	= menuwindowmenuError "openWindowMenu" "Fatal error: could not open the WindowMenu"
where
	wMenuId		= WindowMenuId
	wMenuDef i	= Menu "Windows"
					(	MenuItem "&Cascade"				[MenuSelectState Unable,MenuId WindowMenuCascadeId,	MenuFunction (noLS cascade)]
					:+:	MenuItem "Tile &Horizontally"	[MenuSelectState Unable,MenuId WindowMenuTileHId,	MenuFunction (noLS tileH)]
					:+:	MenuItem "&Tile Vertically"		[MenuSelectState Unable,MenuId WindowMenuTileVId,	MenuFunction (noLS tileV)]
					:+:	RadioMenu [] 0					[MenuId WindowMenuRadioId]
					)
					[	MenuId		wMenuId
					,	MenuIndex	i
					]
	
	cascade :: !(PSt .l .p) -> PSt .l .p
	cascade pState
		# (wIds,pState)		= accPIO getWindowsStack pState
		  nrWindows			= length wIds
		| nrWindows==0
		= pState
		# (pwSize,pState)	= accPIO getProcessWindowSize pState
		  wMargin			= pwSize.w/5
		  hMargin			= pwSize.h/5
		  (dx,dy)			= (TitleBarWidth,TitleBarWidth)
		  n					= min (wMargin/dx+1) (hMargin/dy+1)
		  virtualWidth		= max MinWindowW (pwSize.w-(n-1)*dx)
		  virtualHeight		= max MinWindowH (pwSize.h-(n-1)*dy)
		  virtualSize		= {w=virtualWidth-dx,h=virtualHeight-dy}
		  (front,rest)		= HdTl wIds
		  posWindows		= zip2 [(pos dx dy n i,virtualSize) \\ i<-[0..nrWindows-2]] (reverse rest)
		  pState			= StateMap2 rearrange posWindows pState
		  pState			= setWindowViewSize front virtualSize pState
		  pState			= setWindowPos front (LeftTop,pos dx dy n (nrWindows-1)) pState
		= pState
	where
		pos :: !Int !Int !Int !Int -> Vector
		pos dx dy n i	= {vx=dx*(i mod n),vy=dy*(i mod n)}
	
	tileH :: !(PSt .l .p) -> PSt .l .p
	tileH pState
		# (wIds,pState)		= accPIO getWindowsStack pState
		| isEmpty wIds
		= pState
		# (pwSize,pState)	= accPIO getProcessWindowSize pState
		  nrWindows			= length wIds
		  columns			= smallestNrColumns (MinWindowH+TitleBarWidth) pwSize.h nrWindows
		  perfecttiling		= nrWindows rem columns==0
		  percolumn			= if perfecttiling (nrWindows/columns) (nrWindows/columns+1)
		  leftovers			= if perfecttiling 0 (nrWindows-(columns-1)*percolumn)
		  allWidth			= max MinWindowW (pwSize.w/columns)
		  leftHeight		= max MinWindowH (pwSize.h/leftovers)
		  restHeight		= max MinWindowH (pwSize.h/percolumn)
		  leftSize			= {w=allWidth,h=leftHeight-TitleBarWidth}
		  restSize			= {w=allWidth,h=restHeight-TitleBarWidth}
		  (front,rest)		= HdTl wIds
		  (l,r)				= Split (leftovers-1) rest
		  leftWindows		= zip2 [({vx=0,vy=i*leftHeight},leftSize) \\ i<-[1..]] l
		  restWindows		= zip2 [(pos allWidth restHeight percolumn i,restSize) \\ i<-[(if perfecttiling 1 percolumn)..]] r
		  pState			= StateMap2 rearrange (reverse (leftWindows++restWindows)) pState
		  firstSize			= if perfecttiling restSize leftSize
		  pState			= setWindowViewSize front firstSize pState
		  pState			= setWindowPos  front (LeftTop,zero) pState
		= pState
	where
		pos :: !Int !Int !Int !Int -> Vector
		pos w h n i		= {vx=w*(i/n),vy=h*(i mod n)}
		
		smallestNrColumns :: !Int !Int !Int -> Int
		smallestNrColumns minHeight pwHeight nrWindows
			= smallestNrColumns` 1
		where
			smallestNrColumns` :: !Int -> Int
			smallestNrColumns` columns
				| (columns*pwHeight)/nrWindows>=minHeight
				= columns
				= smallestNrColumns` (columns+1)
	
	tileV :: !(PSt .l .p) -> PSt .l .p
	tileV pState
		# (wIds,pState)	= accPIO getWindowsStack pState
		| isEmpty wIds
		= pState
		# (pwSize,pState)	= accPIO getProcessWindowSize pState
		  nrWindows			= length wIds
		  rows				= smallestNrRows MinWindowW pwSize.w nrWindows
		  perfecttiling		= nrWindows rem rows==0
		  perrow			= if perfecttiling (nrWindows/rows) (nrWindows/rows+1)
		  leftovers			= if perfecttiling 0 (nrWindows-(rows-1)*perrow)
		  allHeight			= max MinWindowH (pwSize.h/rows)
		  topWidth			= max MinWindowW (pwSize.w/leftovers)
		  restWidth			= max MinWindowW (pwSize.w/perrow)
		  topSize			= {w=topWidth, h=allHeight-TitleBarWidth}
		  restSize			= {w=restWidth,h=allHeight-TitleBarWidth}
		  (front,rest)		= HdTl wIds
		  (l,r)				= Split (leftovers-1) rest
		  topWindows		= zip2 [({vx=i*topWidth,vy=zero},topSize) \\ i<-[1..]] l
		  restWindows		= zip2 [(pos restWidth allHeight perrow i,restSize) \\ i<-[(if perfecttiling 1 perrow)..]] r
		  pState			= StateMap2 rearrange (reverse (topWindows++restWindows)) pState
		  firstSize			= if perfecttiling restSize topSize
		  pState			= setWindowViewSize front firstSize pState
		  pState			= setWindowPos  front (LeftTop,zero) pState
		= pState
	where
		pos :: !Int !Int !Int !Int -> Vector
		pos w h n i		= {vx=w*(i mod n),vy=h*(i/n)}
		
		smallestNrRows :: !Int !Int !Int -> Int
		smallestNrRows minWidth pwWidth nrWindows
			= smallestNrRows` 1
		where
			smallestNrRows` :: !Int -> Int
			smallestNrRows` rows
				| (rows*pwWidth)/nrWindows>=minWidth
				= rows
				= smallestNrRows` (rows+1)
	
	rearrange :: !(!(!Vector,!Size),!Id) !(PSt .l .p) -> PSt .l .p
	rearrange ((newPos,newSize),id) pState
		# (curSize,ioState)		= getWindowViewSize id pState.io
		  (Just curPos,ioState)	= getWindowPos id ioState
		  pState				= {pState & io=ioState}
		| curSize==newSize && curPos==newPos
		= pState
		| curSize==newSize
		= setWindowPos id (LeftTop,newPos) pState
		| curPos==newPos
		= setWindowViewSize id newSize pState
		= showWindows [id] (setWindowPos id (LeftTop,newPos) (setWindowViewSize id newSize (hideWindows [id] pState)))


/*	addWindowToWindowMenu adds a new item to the RadioMenu of the WindowMenu if present. 
	The Id argument is the id of the window that should be added, and the Title argument its title. 
*/
addWindowToWindowMenu :: !Id !Title !(IOSt .l .p) -> IOSt .l .p
addWindowToWindowMenu windowId windowTitle ioState
	# (document,ioState)	= IOStGetDocumentInterface ioState
	| document<>MDI
	= ioState
	# (optMState,ioState)	= getMenu WindowMenuId ioState
	| isNothing optMState
	= ioState
	# mState				= fromJust optMState
	  wIds					= map (fromJust o snd) (getCompoundMenuElementTypes WindowMenuRadioId mState)
	  titles				= map (fromJust o snd) (getMenuElementTitles wIds mState)
	  index					= findInsertIndex windowTitle titles
	  radioItem				= (windowTitle,Just windowId,Nothing,activateWindow windowId o showWindows [windowId])
	  (error,ioState)		= openRadioMenuItems WindowMenuId WindowMenuRadioId index [radioItem] ioState
	| error<>NoError
	= menuwindowmenuError "addWindowToWindowMenu" "Fatal error: could not add MenuItem to WindowMenu"
	| not (isEmpty wIds)
	= ioState
	# ioState				= setMenu WindowMenuId 
								[enableMenuElements [WindowMenuCascadeId,WindowMenuTileHId,WindowMenuTileVId]] ioState
	  itemIds				= map snd (getMenuElementTypes mState)
	  (_,index)				= findMenuSeparatorIndex WindowMenuTileVId itemIds
	  (error,ioState)		= openMenuElements WindowMenuId index undef (MenuSeparator [MenuId WindowMenuSeparatorId]) ioState
	| error<>NoError
	= menuwindowmenuError "addWindowToWindowMenu" "Fatal error: could not add MenuSeparator to WindowMenu"
	= ioState


/*	removeWindowFromWindowMenu removes the window entry from the WindowMenu if present.
*/
removeWindowFromWindowMenu :: !Id !(IOSt .l .p) -> IOSt .l .p
removeWindowFromWindowMenu id ioState
	# (document,ioState)	= IOStGetDocumentInterface ioState
	| document<>MDI
	= ioState
	# (optMState,ioState)	= getMenu WindowMenuId ioState
	| isNothing optMState
	= ioState
	# mState				= fromJust optMState
	  wIds					= map (fromJust o snd) (getCompoundMenuElementTypes WindowMenuRadioId mState)
	  (found,index)			= findCloseIndex id wIds
	| not found
	= ioState
	# ioState				= closemenuindexelements RemoveSpecialMenuElements True (WindowMenuId,Just WindowMenuRadioId) [index] ioState
	| length wIds<>1
	= ioState
	# ioState				= setMenu WindowMenuId 
								[disableMenuElements [WindowMenuCascadeId,WindowMenuTileHId,WindowMenuTileVId]] ioState
	  ioState				= closemenuelements WindowMenuId [WindowMenuSeparatorId] ioState
	= ioState


/*	validateWindowActivateForWindowMenu takes care that if this interactive process is a MDI process,
	and the DialogLSHandle represents a Windows instance that the WindowActivate function of the
	DialogLSHandle will select the proper RadioMenuItem of the WindowMenu if present before any other 
	actions are taken.
*/
validateWindowActivateForWindowMenu :: !Id !(DialogLSHandle .ls (PSt .l .p)) !(IOSt .l .p)
										-> (!DialogLSHandle .ls (PSt .l .p),  !IOSt .l .p)
validateWindowActivateForWindowMenu id dlsH=:{dlsHandle=dH=:{dhAtts,dhKind}} ioState
	| dhKind<>IsWindow
	= (dlsH,ioState)
	# (document,ioState)= IOStGetDocumentInterface ioState
	| document<>MDI
	= (dlsH,ioState)
	# (found,att,atts)	= Remove iswindowactivate (WindowActivate I) dhAtts
	| not found
	= ({dlsH & dlsHandle={dH & dhAtts=[WindowActivate (noLS (selectWindow id)):atts]}},ioState)
	# activateF			= getwindowactivatefunction att
	= ({dlsH & dlsHandle={dH & dhAtts=[WindowActivate (activateF o (noLS (selectWindow id))):atts]}},ioState)
where
	selectWindow :: !Id !(PSt .l .p) -> PSt .l .p
	selectWindow id pState=:{io}
		# (document,ioState)= IOStGetDocumentInterface io
		| document<>MDI
		= {pState & io=ioState}
		# ioState			= setMenu WindowMenuId [selectRadioMenuItem WindowMenuRadioId id] ioState
		= {pState & io=ioState}


//	Index locating functions.

findInsertIndex :: x ![x] -> Int	| Ord x
findInsertIndex x ys
	= findInsertIndex` 0 x ys
where
	findInsertIndex` :: !Int x ![x] -> Int	| Ord x
	findInsertIndex` index x [y:ys]
		| x<=y
		= index
		= findInsertIndex` (index+1) x ys
	findInsertIndex` index _ _
		= index+1

findCloseIndex :: x ![x] -> (!Bool,!Int)	| Eq x
findCloseIndex id ids
	= findCloseIndex` 1 id ids
where
	findCloseIndex` :: !Int x ![x] -> (!Bool,!Int)	| Eq x
	findCloseIndex` index x [y:ys]
		| x==y
		= (True,index)
		= findCloseIndex` (index+1) x ys
	findCloseIndex` index _ _
		= (False,index)

findMenuSeparatorIndex :: x ![Maybe x] -> (!Bool,!Int)	| Eq x
findMenuSeparatorIndex id opt_ids
	= findMenuSeparatorIndex` 1 id opt_ids
where
	findMenuSeparatorIndex` :: !Int x ![Maybe x] -> (!Bool,!Int)	| Eq x
	findMenuSeparatorIndex` index x [y:ys]
		| isJust y && x==fromJust y
		= (True,index)
		= findMenuSeparatorIndex` (index+1) x ys
	findMenuSeparatorIndex` index _ _
		= (False,index)
