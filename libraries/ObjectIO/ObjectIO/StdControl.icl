implementation module StdControl


//	Clean Object I/O library, version 1.2

//	Operations to change controls using their Ids only.


import	StdBool, StdList, StdMisc, StdTuple
import	commondef, controlaccess, controlinternal, id, iostate, windowaccess, wstate
from	windowclipstate	import invalidateWindowClipState`
from	wstateaccess	import iswindowitemspace`, getwindowitemspace`,
								iswindowhmargin`,  getwindowhmargin`,
								iswindowvmargin`,  getwindowvmargin`
from	ostoolbox	import OSNewToolbox


/*	The function isOkControlId can be used to filter out the proper IdParent records.
*/
isOkControlId :: !SystemId !(.x,!Maybe IdParent) -> (!Bool,(.x,Id))
isOkControlId ioId (x,Just {idpIOId,idpDevice,idpId})
	= (ioId==idpIOId && idpDevice==WindowDevice,(x,idpId))
isOkControlId _ _
	= (False,undef)

/*	gatherWindowIds collects all first Ids (ControlId) that belong to the same second Id (WindowId).
	gatherWindowIds` does the same, except that not only ControlIds are collected, but also their data item.
*/
gatherWindowIds :: ![(Id,Id)] -> [([Id],Id)]
gatherWindowIds [(cId,wId):ids]
	= [([cId:cIds],wId):cIds_wIds]
where
	(cIds,ids`)	= gatherControlsIds wId ids
	cIds_wIds	= gatherWindowIds ids`
	
	gatherControlsIds :: !Id ![(Id,Id)] -> ([Id],[(Id,Id)])
	gatherControlsIds wId [(cId,wId`):ids]
		| wId==wId`	= ([cId:cIds],ids`)
		| otherwise	= (cIds,[(cId,wId`):ids`])
	where
		(cIds,ids`)	= gatherControlsIds wId ids
	gatherControlsIds _ _
		= ([],[])
gatherWindowIds []
	= []

gatherWindowIds` :: ![((Id,.x),Id)] -> [([(Id,.x)],Id)]
gatherWindowIds` [((cId,x),wId):ids]
	= [([(cId,x):cIds],wId):cIds_wIds]
where
	(cIds,ids`)	= gatherControlsIds wId ids
	cIds_wIds	= gatherWindowIds` ids`
	
	gatherControlsIds :: !Id ![((Id,.x),Id)] -> ([(Id,.x)],[((Id,.x),Id)])
	gatherControlsIds wId [((cId,x),wId`):ids]
		| wId==wId`	= ([(cId,x):cIds],ids`)
		| otherwise	= (cIds,[((cId,x),wId`):ids`])
	where
		(cIds,ids`)	= gatherControlsIds wId ids
	gatherControlsIds _ _
		= ([],[])
gatherWindowIds` []
	= []


//	The WState window representation record:

::	WState
	=	{	wIds	:: !WIDS
		,	wRep	:: !WindowHandle`
		,	wTb		:: !.OSToolbox
		,	wMetrics:: !OSWindowMetrics
		}


getWindow :: !Id !(IOSt .l) -> (!Maybe WState, !IOSt .l)
getWindow windowId ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID windowId) windows
	| not found
		= (Nothing,ioState)
	| otherwise
		# (tb,ioState)			= getIOToolbox ioState
		# (wsH`,wsH,tb)			= retrieveWindowHandle` wsH tb
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		  windows				= setWindowHandlesWindow wsH windows
		# ioState				= setIOToolbox tb ioState
		# ioState				= IOStSetDevice (WindowSystemState windows) ioState
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		= (Just {wIds=wids,wRep=wsH`,wTb=OSNewToolbox,wMetrics=wMetrics},ioState)

getParentWindow :: !Id !(IOSt .l) -> (!Maybe WState, !IOSt .l)
getParentWindow controlId ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	  maybeParent		= getIdParent controlId idtable
	| isNothing maybeParent
		= (Nothing,ioState)
	# parent			= fromJust maybeParent
	# (ioId,ioState)	= IOStGetIOId ioState
	| ioId==parent.idpIOId && parent.idpDevice==WindowDevice
		= getWindow parent.idpId ioState
	| otherwise
		= (Nothing,ioState)

setWindow :: !Id !(IdFun *WState) !(IOSt .l) -> IOSt .l
setWindow windowId f ioState
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= WindowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID windowId) windows
	| not found
		= ioState
	| otherwise
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		# (wsH`,wsH,tb)			= retrieveWindowHandle` wsH tb
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		# {wRep=wsH`,wTb=tb}	= f {wIds=wids,wRep=wsH`,wTb=tb,wMetrics=wMetrics}
		  wsH					= insertWindowHandle` wsH` wsH
		  windows				= setWindowHandlesWindow wsH windows
		# ioState				= setIOToolbox tb ioState
		# ioState				= IOStSetDevice (WindowSystemState windows) ioState
		= ioState


//	Show/Hide controls.

showControls :: ![Id] !(IOSt .l) -> IOSt .l
showControls ids ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	  cIds_wIds			= FilterMap (isOkControlId ioId) (zip2 ids (getIdParents ids idtable))
	  cIds_wIds			= gatherWindowIds cIds_wIds
	| isEmpty cIds_wIds	= ioState
	| otherwise			= StrictSeq [setWindow wId (setControlsShowState` True cIds) \\ (cIds,wId)<-cIds_wIds] ioState

showControl :: !Id !(IOSt .l) -> IOSt .l
showControl id ioState = showControls [id] ioState

hideControls :: ![Id] !(IOSt .l) -> IOSt .l
hideControls ids ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	  cIds_wIds			= FilterMap (isOkControlId ioId) (zip2 ids (getIdParents ids idtable))
	  cIds_wIds			= gatherWindowIds cIds_wIds
	| isEmpty cIds_wIds	= ioState
	| otherwise			= StrictSeq [setWindow wId (setControlsShowState` False cIds) \\ (cIds,wId)<-cIds_wIds] ioState

hideControl :: !Id !(IOSt .l) -> IOSt .l
hideControl id ioState = hideControls [id] ioState

setControlsShowState` :: !Bool ![Id] !*WState -> *WState
setControlsShowState` show ids wState=:{wIds={wPtr},wRep,wTb,wMetrics}
	# (wH,tb)	= setcontrolsshowstate ids show wMetrics wPtr wRep wTb
	  wH		= invalidateWindowClipState` wH
	= {wState & wRep=wH,wTb=tb}


/*	Enabling/Disabling of controls.
*/
enableControls :: ![Id] !(IOSt .l) -> IOSt .l
enableControls ids ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	  cIds_wIds			= FilterMap (isOkControlId ioId) (zip2 ids (getIdParents ids idtable))
	  cIds_wIds			= gatherWindowIds cIds_wIds
	| isEmpty cIds_wIds	= ioState
	| otherwise			= StrictSeq [setWindow wId (enableControls` cIds) \\ (cIds,wId)<-cIds_wIds] ioState
where
	enableControls` :: ![Id] !*WState -> *WState
	enableControls` ids wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)		= enablecontrols ids False wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}

enableControl :: !Id !(IOSt .l) -> IOSt .l
enableControl id ioState = enableControls [id] ioState

disableControls :: ![Id] !(IOSt .l) -> IOSt .l
disableControls ids ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	  cIds_wIds			= FilterMap (isOkControlId ioId) (zip2 ids (getIdParents ids idtable))
	  cIds_wIds			= gatherWindowIds cIds_wIds
	| isEmpty cIds_wIds	= ioState
	| otherwise			= StrictSeq [setWindow wId (disableControls` cIds) \\ (cIds,wId)<-cIds_wIds] ioState
where
	disableControls` :: ![Id] !*WState -> *WState
	disableControls` ids wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)		= disablecontrols ids False wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}

disableControl :: !Id !(IOSt .l) -> IOSt .l
disableControl id ioState = disableControls [id] ioState


//	Marking/Unmarking of check controls.

markCheckControlItems :: !Id ![Index] !(IOSt .l) -> IOSt .l
markCheckControlItems cId indexs ioState
	= setControlsMarkState Mark cId indexs ioState

unmarkCheckControlItems :: !Id ![Index] !(IOSt .l) -> IOSt .l
unmarkCheckControlItems cId indexs ioState
	= setControlsMarkState NoMark cId indexs ioState

setControlsMarkState :: !MarkState !Id ![Index] !(IOSt .l) -> IOSt .l
setControlsMarkState mark cId indexs ioState
	| isEmpty indexs	= ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	  maybeParent		= getIdParent cId idtable
	| not (fst (isOkControlId ioId (cId,maybeParent)))
						= ioState
	| otherwise			= setWindow (fromJust maybeParent).idpId (setControlsMarkState` mark cId indexs) ioState
where
	setControlsMarkState` :: !MarkState !Id ![Index] !*WState -> *WState
	setControlsMarkState` mark id indexs wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)		= setcontrolsmarkstate id mark indexs wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}


//	Selecting/Unselecting a radio control.

selectRadioControlItem :: !Id !Index !(IOSt .l) -> IOSt .l
selectRadioControlItem cId index ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	  maybeParent		= getIdParent cId idtable
	| not (fst (isOkControlId ioId (cId,maybeParent)))
						= ioState
	| otherwise			= setWindow (fromJust maybeParent).idpId (selectRadioControlItem` cId index) ioState
where
	selectRadioControlItem` :: !Id !Index !*WState -> *WState
	selectRadioControlItem` id index wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)	= selectradiocontrol id index wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}


//	Select a pop up menu item.

selectPopUpControlItem :: !Id !Index !(IOSt .l) -> IOSt .l
selectPopUpControlItem cId index ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	  maybeParent		= getIdParent cId idtable
	| not (fst (isOkControlId ioId (cId,maybeParent)))
						= ioState
	| otherwise			= setWindow (fromJust maybeParent).idpId (selectPopUpControlItem` cId index) ioState
where
	selectPopUpControlItem` :: !Id !Index !*WState -> *WState
	selectPopUpControlItem` id index wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)		= selectpopupitem id index wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}


//	Move the orientation of a CompoundControl.

moveControlViewFrame :: !Id Vector2 !(IOSt .l) -> IOSt .l
moveControlViewFrame cId v ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	  maybeParent		= getIdParent cId idtable
	| not (fst (isOkControlId ioId (cId,maybeParent)))
						= ioState
	| otherwise			= setWindow (fromJust maybeParent).idpId (moveControlViewFrame` cId v) ioState
where
	moveControlViewFrame` :: !Id Vector2 !*WState -> *WState
	moveControlViewFrame` id v wState=:{wIds,wRep,wTb,wMetrics}
		# (wH,tb)	= movecontrolviewframe id v wMetrics wIds wRep wTb
		  wH		= invalidateWindowClipState` wH
		= {wState & wRep=wH,wTb=tb}


//	Set a new view domain of a CompoundControl.

setControlViewDomain :: !Id ViewDomain !(IOSt .l) -> IOSt .l
setControlViewDomain cId newDomain ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	  maybeParent		= getIdParent cId idtable
	| not (fst (isOkControlId ioId (cId,maybeParent)))
						= ioState
	| otherwise			= setWindow (fromJust maybeParent).idpId (setControlViewDomain` cId newDomain) ioState
where
	setControlViewDomain` :: !Id !ViewDomain !*WState -> *WState
	setControlViewDomain` id newDomain wState=:{wIds,wRep,wTb,wMetrics}
		# (wH,tb)	= setcontrolviewdomain id newDomain wMetrics wIds wRep wTb
		= {wState & wRep=wH,wTb=tb}


//	Set the ScrollFunction of a CompoundControl.

setControlScrollFunction :: !Id Direction ScrollFunction !(IOSt .l) -> IOSt .l
setControlScrollFunction cId direction scrollFun ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	  maybeParent		= getIdParent cId idtable
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= ioState
	| otherwise
		= setWindow (fromJust maybeParent).idpId (setControlScrollFunction` cId direction scrollFun) ioState
where
	setControlScrollFunction` :: !Id !Direction ScrollFunction !*WState -> *WState
	setControlScrollFunction` id direction scrollFun wState=:{wRep}
		# wH			= setcontrolscrollfun id direction scrollFun wRep
		= {wState & wRep=wH}


//	Change the text of (Text/Edit/Button)Control.

setControlTexts :: ![(Id,String)] !(IOSt .l) -> IOSt .l
setControlTexts cid_texts ioState
	# (idtable,ioState)			= IOStGetIdTable ioState
	# (ioId,ioState)			= IOStGetIOId ioState
	  (cids,_)					= unzip cid_texts
	  cid_texts_wIds			= FilterMap (isOkControlId ioId) (zip2 cid_texts (getIdParents cids idtable))
	  cid_texts_wIds			= gatherWindowIds` cid_texts_wIds
	| isEmpty cid_texts_wIds	= ioState
	| otherwise					= StrictSeq [setWindow wId (setControlTexts` cid_texts) \\ (cid_texts,wId)<-cid_texts_wIds] ioState
where
	setControlTexts` :: ![(Id,String)] !*WState -> *WState
	setControlTexts` texts wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)	= setcontroltexts texts wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}

setControlText :: !Id !String !(IOSt .l) -> IOSt .l
setControlText id text ioState = setControlTexts [(id,text)] ioState


//	Set the cursor position of an EditControl.

setEditControlCursor :: !Id !Int !(IOSt .l) -> IOSt .l
setEditControlCursor cId pos ioState
	# (idtable,ioState)	= IOStGetIdTable ioState
	# (ioId,ioState)	= IOStGetIOId ioState
	  maybeParent		= getIdParent cId idtable
	| not (fst (isOkControlId ioId (cId,maybeParent)))
						= ioState
	| otherwise			= setWindow (fromJust maybeParent).idpId (setEditControlCursor` cId pos) ioState
where
	setEditControlCursor` :: !Id !Int !*WState -> *WState
	setEditControlCursor` id pos wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)	= seteditcontrolcursor id pos wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}


/*	Change the Look of the corresponding (Custom(Button)/Compound)Controls and redraw
	only if the first Boolean is True.
*/
setControlLooks :: ![(Id,Bool,(Bool,Look))] !(IOSt .l) -> IOSt .l
setControlLooks cid_looks ioState
	# (idtable,ioState)			= IOStGetIdTable ioState
	# (ioId,ioState)			= IOStGetIOId ioState
	  cid_looks					= [(cid,(redraw,look)) \\ (cid,redraw,look)<-cid_looks]
	  (cids,_)					= unzip cid_looks
	  cid_looks_wIds			= FilterMap (isOkControlId ioId) (zip2 cid_looks (getIdParents cids idtable))
	  cid_looks_wIds			= gatherWindowIds` cid_looks_wIds
	| isEmpty cid_looks_wIds	= ioState
	| otherwise					= StrictSeq [	setWindow wId (setControlLooks` [(cid,redraw,look) \\ (cid,(redraw,look))<-cid_looks])
											\\	(cid_looks,wId)<-cid_looks_wIds
											]	ioState
where
	setControlLooks` :: ![(Id,Bool,(Bool,Look))] !*WState -> *WState
	setControlLooks` looks wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)	= setcontrolslook looks wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}

setControlLook :: !Id !Bool (Bool,Look) !(IOSt .l) -> IOSt .l
setControlLook id redraw newlook ioState = setControlLooks [(id,redraw,newlook)] ioState


//	Change the SliderState and redraw the settings of the SliderControls.

setSliderStates :: ![(Id,IdFun SliderState)] !(IOSt .l) -> IOSt .l
setSliderStates cid_fs ioState
	# (idtable,ioState)		= IOStGetIdTable ioState
	# (ioId,ioState)		= IOStGetIOId ioState
	  (cids,_)				= unzip cid_fs
	  cid_funs_wIds			= FilterMap (isOkControlId ioId) (zip2 cid_fs (getIdParents cids idtable))
	  cid_funs_wIds			= gatherWindowIds` cid_funs_wIds
	| isEmpty cid_funs_wIds	= ioState
	| otherwise				= StrictSeq [setWindow wId (setSliderStates` cid_funs) \\ (cid_funs,wId)<-cid_funs_wIds] ioState
where
	setSliderStates` :: ![(Id,IdFun SliderState)] !*WState -> *WState
	setSliderStates` id_fs wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)	= setsliderstates id_fs wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}

setSliderState :: !Id (IdFun SliderState) !(IOSt .l) -> IOSt .l
setSliderState id fun ioState = setSliderStates [(id,fun)] ioState


//	Change the thumb value of the SliderState of a SliderControl. 

setSliderThumbs :: ![(Id,Int)] !(IOSt .l) -> IOSt .l
setSliderThumbs cid_thumbs ioState
	= setSliderStates (map (\(cid,thumb)->(cid,\state->{state & sliderThumb=thumb})) cid_thumbs) ioState

setSliderThumb :: !Id Int !(IOSt .l) -> IOSt .l
setSliderThumb id thumb ioState = setSliderThumbs [(id,thumb)] ioState


//	Draw in a (Custom(Button)/Compound)Control.

appControlPicture :: !Id !.(IdFun *Picture) !(IOSt .l) -> IOSt .l
appControlPicture cId drawfun ioState
	= snd (accControlPicture cId (\p->(undef,drawfun p)) ioState)

accControlPicture :: !Id !.(St *Picture .x) !(IOSt .l) -> (!Maybe .x,!IOSt .l)
accControlPicture cId drawfun ioState
	# (idtable,ioState)			= IOStGetIdTable ioState
	# (ioId,ioState)			= IOStGetIOId ioState
	  maybeParent				= getIdParent cId idtable
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= (Nothing,ioState)
	# (found,wDevice,ioState)	= IOStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	| otherwise
		# windows				= WindowSystemStateGetWindowHandles wDevice
		  wId					= (fromJust maybeParent).idpId
		  (_,wsH,windows)		= getWindowHandlesWindow (toWID wId) windows
		# (wMetrics,ioState)	= IOStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		# (wsH`,wsH,tb)			= retrieveWindowHandle` wsH tb
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		# (maybe_result,wsH`,tb)= drawincontrol cId drawfun wMetrics wids.wPtr wsH` tb
		  wsH					= insertWindowHandle` wsH` wsH
		  windows				= setWindowHandlesWindow wsH windows
		# ioState				= setIOToolbox tb ioState
		# ioState				= IOStSetDevice (WindowSystemState windows) ioState
		= (maybe_result,ioState)


//	Access operations on WState:

getWStateControls :: !WState -> [WElementHandle`]
getWStateControls {wRep={whItems`}}
	= whItems`

getControlTypes :: !WState -> [(ControlType,Maybe Id)]
getControlTypes wstate
	= getcontrolstypes (getWStateControls wstate)

getCompoundTypes :: !Id !WState -> [(ControlType,Maybe Id)]
getCompoundTypes id wstate
	= getcompoundstypes id (getWStateControls wstate)


// snd3thd3	:: !(.a,.b,.c) -> (.b,.c)								// (t2,t3) of (t1,t2,t3)
snd3thd3 tuple :== (t2,t3) where (_,t2,t3) = tuple

getControlLayouts :: ![Id] !WState -> [(Bool,(Maybe ItemPos,Vector2))]
getControlLayouts ids wstate
	= map snd3thd3 (snd (getcontrolslayouts (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= (Nothing,zero)

getControlLayout :: !Id !WState -> (Bool,(Maybe ItemPos,Vector2))
getControlLayout id wstate = hd (getControlLayouts [id] wstate)

getControlViewSizes :: ![Id] !WState -> [(Bool,Size)]
getControlViewSizes ids wstate=:{wMetrics}
	= map snd3thd3 (snd (getcontrolsviewsizes wMetrics (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= zero

getControlViewSize :: !Id !WState -> (Bool,Size)
getControlViewSize id wstate = hd (getControlViewSizes [id] wstate)

getControlOuterSizes :: ![Id] !WState -> [(Bool,Size)]
getControlOuterSizes ids wstate=:{wMetrics}
	= map snd3thd3 (snd (getcontrolsoutersizes wMetrics (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= zero

getControlOuterSize :: !Id !WState -> (Bool,Size)
getControlOuterSize id wstate = hd (getControlOuterSizes [id] wstate)

getControlSelectStates :: ![Id] !WState -> [(Bool,SelectState)]
getControlSelectStates ids wstate
	= map snd3thd3 (snd (getcontrolsselects (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Able

getControlSelectState :: !Id !WState -> (Bool,SelectState)
getControlSelectState id wstate = hd (getControlSelectStates [id] wstate)

getControlShowStates :: ![Id] !WState -> [(Bool,Bool)]
getControlShowStates ids wstate
	= map snd3thd3 (snd (getcontrolsshowstates (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= False

getControlShowState :: !Id !WState -> (Bool,Bool)
getControlShowState id wstate = hd (getControlShowStates [id] wstate)

getControlTexts :: ![Id] !WState -> [(Bool,Maybe String)]
getControlTexts ids wstate
	= map snd3thd3 (snd (getcontrolstexts (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getControlText :: !Id !WState -> (Bool,Maybe String)
getControlText id wstate = hd (getControlTexts [id] wstate)

getControlNrLines :: ![Id] !WState -> [(Bool,Maybe NrLines)]
getControlNrLines ids wstate
	= map snd3thd3 (snd (getcontrolsnrlines (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getControlNrLine :: !Id !WState -> (Bool,Maybe NrLines)
getControlNrLine id wstate = hd (getControlNrLines [id] wstate)

getControlLooks :: ![Id] !WState -> [(Bool,Maybe (Bool,Look))]
getControlLooks ids wstate
	= map snd3thd3 (snd (getcontrolslooks (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getControlLook :: !Id !WState -> (Bool,Maybe (Bool,Look))
getControlLook id wstate = hd (getControlLooks [id] wstate)

getControlMinimumSizes :: ![Id] !WState -> [(Bool,Maybe Size)]
getControlMinimumSizes ids wstate
	= map snd3thd3 (snd (getcontrolsminsizes (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getControlMinimumSize :: !Id !WState -> (Bool,Maybe Size)
getControlMinimumSize id wstate = hd (getControlMinimumSizes [id] wstate)

getControlResizes :: ![Id] !WState -> [(Bool,Maybe ControlResizeFunction)]
getControlResizes ids wstate
	= map snd3thd3 (snd (getcontrolsresizes (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getControlResize :: !Id !WState -> (Bool,Maybe ControlResizeFunction)
getControlResize id wstate = hd (getControlResizes [id] wstate)

getRadioControlItems :: ![Id] !WState -> [(Bool,Maybe [String])]
getRadioControlItems ids wstate
	= map snd3thd3 (snd (getradioitems (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getRadioControlItem :: !Id !WState -> (Bool,Maybe [String])
getRadioControlItem id wstate = hd (getRadioControlItems [id] wstate)

getRadioControlSelections :: ![Id] !WState -> [(Bool,Maybe Index)]
getRadioControlSelections ids wstate
	= map snd3thd3 (snd (getradiocontrolsmarks (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getRadioControlSelection :: !Id !WState -> (Bool,Maybe Index)
getRadioControlSelection id wstate = hd (getRadioControlSelections [id] wstate)

getCheckControlItems :: ![Id] !WState -> [(Bool,Maybe [String])]
getCheckControlItems ids wstate
	= map snd3thd3 (snd (getcheckitems (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getCheckControlItem :: !Id !WState -> (Bool,Maybe [String])
getCheckControlItem id wstate = hd (getCheckControlItems [id] wstate)

getCheckControlSelections :: ![Id] !WState -> [(Bool,Maybe [Index])]
getCheckControlSelections ids wstate
	= map snd3thd3 (snd (getcheckcontrolsmarks (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getCheckControlSelection :: !Id !WState -> (Bool,Maybe [Index])
getCheckControlSelection id wstate = hd (getCheckControlSelections [id] wstate)

getPopUpControlItems :: ![Id] !WState -> [(Bool,Maybe [String])]
getPopUpControlItems ids wstate
	= map snd3thd3 (snd (getpopupitems (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getPopUpControlItem :: !Id !WState -> (Bool,Maybe [String])
getPopUpControlItem id wstate = hd (getPopUpControlItems [id] wstate)

getPopUpControlSelections :: ![Id] !WState -> [(Bool,Maybe Index)]
getPopUpControlSelections ids wstate
	= map snd3thd3 (snd (getselectedpopupitems (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getPopUpControlSelection :: !Id !WState -> (Bool,Maybe Index)
getPopUpControlSelection id wstate = hd (getPopUpControlSelections [id] wstate)

getSliderDirections :: ![Id] !WState -> [(Bool,Maybe Direction)]
getSliderDirections ids wstate
	= map snd3thd3 (snd (getslidersdirections (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getSliderDirection :: !Id !WState -> (Bool,Maybe Direction)
getSliderDirection id wstate = hd (getSliderDirections [id] wstate)

getSliderStates :: ![Id] !WState -> [(Bool,Maybe SliderState)]
getSliderStates ids wstate
	= map snd3thd3 (snd (getslidersstates (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getSliderState :: !Id !WState -> (Bool,Maybe SliderState)
getSliderState id wstate = hd (getSliderStates [id] wstate)

getControlViewFrames :: ![Id] !WState -> [(Bool,Maybe ViewFrame)]
getControlViewFrames ids wstate=:{wMetrics}
	= map snd3thd3 (snd (getcontrolsframes wMetrics (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getControlViewFrame :: !Id !WState -> (Bool,Maybe ViewFrame)
getControlViewFrame id wstate = hd (getControlViewFrames [id] wstate)

getControlViewDomains :: ![Id] !WState -> [(Bool,Maybe ViewDomain)]
getControlViewDomains ids wstate
	= map snd3thd3 (snd (getcontrolsdomains (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getControlViewDomain :: !Id !WState -> (Bool,Maybe ViewDomain)
getControlViewDomain id wstate = hd (getControlViewDomains [id] wstate)

getControlScrollFunctions :: ![Id] !WState -> [(Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))]
getControlScrollFunctions ids wstate
	= map snd3thd3 (snd (getscrollfunctions (getWStateControls wstate) (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing

getControlScrollFunction :: !Id !WState -> (Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))
getControlScrollFunction id wstate = hd (getControlScrollFunctions [id] wstate)

getControlItemSpaces :: ![Id] !WState -> [(Bool,Maybe (Int,Int))]
getControlItemSpaces ids {wRep={whItems`,whAtts`},wMetrics={osmHorItemSpace,osmVerItemSpace}}
	= map snd3thd3 (snd (getcontrolsspaces spaces whItems` (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing
	spaces		= getwindowitemspace` (snd (Select iswindowitemspace` (WindowItemSpace` osmHorItemSpace osmVerItemSpace) whAtts`))

getControlItemSpace :: !Id !WState -> (Bool,Maybe (Int,Int))
getControlItemSpace id wstate = hd (getControlItemSpaces [id] wstate)

getControlMargins :: ![Id] !WState -> [(Bool,Maybe ((Int,Int),(Int,Int)))]
getControlMargins ids {wRep={whKind`,whItems`,whAtts`},wMetrics={osmHorMargin,osmVerMargin}}
	= map snd3thd3 (snd (getcontrolsmargins (hMargins,vMargins) whItems` (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Nothing
	(hMargin,vMargin)
				= if (whKind`==IsDialog) (osmHorMargin,osmVerMargin) (0,0)
	hMargins	= getwindowhmargin` (snd (Select iswindowhmargin` (WindowHMargin` hMargin hMargin) whAtts`))
	vMargins	= getwindowvmargin` (snd (Select iswindowvmargin` (WindowVMargin` vMargin vMargin) whAtts`))

getControlMargin :: !Id !WState -> (Bool,Maybe ((Int,Int),(Int,Int)))
getControlMargin id wstate = hd (getControlMargins [id] wstate)
