definition module StdControl


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdControl specifies all control operations.
//	********************************************************************************


from	StdFunc		import St
import	StdControlDef, StdMaybe
from	StdPSt		import PSt, IOSt


/*	Functions that change the state of controls.
	Only those Id arguments that refer to controls within the same interactive 
	process are used to change the corresponding controls.
*/

showControls			:: ![Id]						!(IOSt .l .p) -> IOSt .l .p
showControl				:: ! Id							!(IOSt .l .p) -> IOSt .l .p
hideControls			:: ![Id]						!(IOSt .l .p) -> IOSt .l .p
hideControl				:: ! Id							!(IOSt .l .p) -> IOSt .l .p
/*	(show/hide)Control(s) makes the indicated control(s) visible/invisible. 
	Hiding a control overrides the visibility of its elements, which become 
		invisible. 
	Showing a hidden control re-establishes the visibility state of its elements.
*/

enableControls			:: ![Id]						!(IOSt .l .p) -> IOSt .l .p
enableControl			:: ! Id							!(IOSt .l .p) -> IOSt .l .p
disableControls			:: ![Id]						!(IOSt .l .p) -> IOSt .l .p
disableControl			:: ! Id							!(IOSt .l .p) -> IOSt .l .p
/*	(en/dis)ableControl(s) (en/dis)ables the indicated control(s).
	Disabling a control overrides the SelectStates of its elements, which become 
		unselectable.
	Enabling a disabled control re-establishes the SelectStates of its elements.
*/

markCheckControlItems	:: !Id ![Index]					!(IOSt .l .p) -> IOSt .l .p
unmarkCheckControlItems	:: !Id ![Index]					!(IOSt .l .p) -> IOSt .l .p
/*	(unm/m)arkCheckControlItems unmarks/marks the indicated check items of the given
	CheckControl. Indices range from 1 to the number of check items. Illegal indices
	are ignored.
*/

selectRadioControlItem	:: !Id  !Index					!(IOSt .l .p) -> IOSt .l .p
/*	selectRadioControlItem marks the indicated radio item of a RadioControl, causing
	the mark of the previously marked radio item to disappear. The item is given by 
	the Id of the RadioControl and its index position (counted from 1). 
*/

selectPopUpControlItem	:: !Id  !Index					!(IOSt .l .p) -> IOSt .l .p
/*	selectPopUpControlItem marks the indicated popup item of a PopUpControl, causing
	the mark of the previously marked popup item to disappear. The item is given by 
	the Id of the PopUpControl and its index position (counted from 1).
*/

moveControlViewFrame	:: !Id Vector2					!(IOSt .l .p) -> IOSt .l .p
/*	moveControlViewFrame moves the orientation of the CompoundControl over the given
	vector, and updates the control if necessary. The control frame is not moved 
	outside the ViewDomain of the control. MoveControlViewFrame has no effect if the
	indicated control has no ControlDomain attribute.
*/

setControlViewDomain	:: !Id ViewDomain				!(IOSt .l .p) -> IOSt .l .p
/*	setControlViewDomain sets the view domain of the indicated CompoundControl as 
	given. The control view frame is moved such that a maximum portion of the view 
	domain is visible. The control is not resized.
	In case of unknown Ids, or non CompoundControls, setControlViewDomain has no 
	effect.
*/

setControlScrollFunction:: !Id Direction ScrollFunction !(IOSt .l .p) -> IOSt .l .p
/*	setControlScrollFunction set the ScrollFunction of the indicated CompoundControl
	in the given Direction if it has one.
	In all other cases, setControlScrollFunction has no effect.
*/

setControlTexts			:: ![(Id,String)]				!(IOSt .l .p) -> IOSt .l .p
setControlText			:: !Id !String					!(IOSt .l .p) -> IOSt .l .p
/*	setControlText(s) sets the text of the indicated (Text/Edit/Button)Control(s). 
	If the indicated control is a (Text/Button)Control, then AltKey are interpreted 
	by the system.
	If the indicated control is an EditControl, then the text is taken as it is.
*/

setEditControlCursor	:: !Id !Int						!(IOSt .l .p) -> IOSt .l .p
/*	setEditControlCursor sets the cursor at position @2 of the current content of 
	the EditControl.
	In case @2<0, then the cursor is set at the start of the current content.
	In case @2>size content, then the cursor is set at the end of the current 
	content.
*/

setControlLooks			:: ![(Id, Bool,(Bool,Look))]	!(IOSt .l .p) -> IOSt .l .p
setControlLook			::   !Id !Bool (Bool,Look)		!(IOSt .l .p) -> IOSt .l .p
/*	setControlLook(s) sets the (render,look) attribute of the indicated 
	(Custom(Button)/Compound)Control(s). If this concerns a transparant 
	CompoundControl then it becomes non-transparant.
	An indicated control is only redrawn if the first Boolean is True. 
*/

setSliderStates			:: ![(Id, IdFun SliderState)]	!(IOSt .l .p) -> IOSt .l .p
setSliderState			::   !Id (IdFun SliderState)	!(IOSt .l .p) -> IOSt .l .p
setSliderThumbs			:: ![(Id,Int)]					!(IOSt .l .p) -> IOSt .l .p
setSliderThumb			::   !Id Int					!(IOSt .l .p) -> IOSt .l .p
/*	setSliderState(s)
		applies the function to the current SliderState of the indicated 
		SliderControl(s) and redraws the settings if necessary.
	setSliderThumb(s)
		sets the new thumb value of the indicated SliderControl(s) and redraws the 
		settings if necessary.
*/

appControlPicture		:: !Id !.(IdFun *Picture)		!(IOSt .l .p) -> IOSt .l .p
accControlPicture		:: !Id !.(St *Picture .x)		!(IOSt .l .p)
										   -> (!Maybe .x,!IOSt .l .p)
/*	(app/acc)ControlPicture applies the given drawing function to the Picture of
	the indicated (Custom(Button)/Compound)Control. If the CompoundControl is 
	transparant, or the indicated control could not be found then this operation 
	has no effect. In that case, accControlPicture also returns Nothing.
*/


/*	Access functions on WState. To read the state of a control, a WState is 
	required which can be obtained by the getWindow function. The WState value 
	represents the state of a window or dialogue at that particular moment.
*/

::	WState

getWindow				:: !Id !(IOSt .l .p) -> (!Maybe WState, !IOSt .l .p)
getParentWindow			:: !Id !(IOSt .l .p) -> (!Maybe WState, !IOSt .l .p)
/*	getWindow returns a read-only WState for the indicated window.
		In case the indicated window does not exist Nothing is returned.
	getParentWindow returns a read-only WState for the parent window/dialogue
		of the indicated control. In case the Id does not correspond with a
		control, Nothing is returned. 
*/

getControlTypes			::		!WState -> [(ControlType,Maybe Id)]
getCompoundTypes		:: !Id	!WState -> [(ControlType,Maybe Id)]
/*	getControlTypes
		yields the list of ControlTypes of the component controls of this window. 
	getCompoundTypes
		yields the list of ControlTypes of the component controls of this 
		CompoundControl. 
	For both functions (Just id) is yielded if the component control has a 
	(ControlId id) attribute, and Nothing otherwise. Component controls are not 
	collected recursively through CompoundControls.
	If the indicated CompoundControl is not a CompoundControl, then [] is yielded.
*/

/*	Functions that return the current state of controls. 
	For each access there is one singular and one plural version. In case of the
	plural version the result list is of equal length as the argument Id list. Each 
	result list element corresponds in order with the argument Id list. 
	In both versions the first Boolean result is False in case of invalid Ids (if so
	dummy values are returned - see comment).
	Important: controls with no ControlId attribute, or illegal ids, can not be 
	found in the WState!
*/


getControlLayouts		:: ![Id] !WState -> [(Bool,(Maybe ItemPos,Vector2))]
getControlLayout		:: ! Id  !WState ->  (Bool,(Maybe ItemPos,Vector2))
/*	getControlLayout(s) yields (Just ControlPos) if the indicated control had a 
	ControlPos attribute and Nothing otherwise. The Vector2 offset is the exact 
	current location of the indicated control (LeftTop,OffsetVector offset). 
*/

getControlViewSizes		:: ![Id] !WState -> [(Bool,Size)]
getControlViewSize		:: ! Id  !WState ->  (Bool,Size)
getControlOuterSizes	:: ![Id] !WState -> [(Bool,Size)]
getControlOuterSize		:: ! Id  !WState ->  (Bool,Size)
/*	getControlViewSize(s) yields the current ViewFrame size of the indicated 
		control. Note that this is the exact size of the control for any control 
		other than the CompoundControl. In case of unknown Ids zero is returned.
	getControlOuterSize(s) yields the current ControlOuterSize of the indicated
		control. Note that this is the exact size of the control. In case of unknown
		Ids zero is returned. 
*/

getControlSelectStates	:: ![Id] !WState -> [(Bool,SelectState)]
getControlSelectState	:: ! Id  !WState ->  (Bool,SelectState)
/*	getControlSelectState(s) yields the current SelectState of the indicated 
	control. In case of unknown Ids Able is returned.
*/

getControlShowStates	:: ![Id] !WState -> [(Bool,Bool)]
getControlShowState		:: ! Id  !WState ->  (Bool,Bool)
/*	getControlShowState(s) yields True if the indicated control is visible, and 
	False otherwise. The latter is also returned in case of unknown Ids.
*/

getControlTexts			:: ![Id] !WState -> [(Bool,Maybe String)]
getControlText			:: ! Id  !WState ->  (Bool,Maybe String)
/*	getControlText(s) yields (Just text) of the indicated (PopUp/Text/Edit/Button)
	Control. If the control is not such a control, then Nothing is yielded. 
*/

getControlNrLines		:: ![Id] !WState -> [(Bool,Maybe NrLines)]
getControlNrLine		:: ! Id  !WState ->  (Bool,Maybe NrLines)
/*	getControlNrLine(s) yields (Just nrlines) of the indicated EditControl. 
	If the control is not such a control, then Nothing is yielded.
*/

getControlLooks			:: ![Id] !WState -> [(Bool,Maybe (Bool,Look))]
getControlLook			:: ! Id  !WState ->  (Bool,Maybe (Bool,Look))
/*	getControlLook(s) yields the (render/look) of the indicated 
	(Custom/CustomButton/Compound)Control. If the control is not such a control, or
	is a transparant CompoundControl, then Nothing is yielded.
*/

getControlMinimumSizes	:: ![Id] !WState -> [(Bool,Maybe Size)]
getControlMinimumSize	:: ! Id  !WState ->  (Bool,Maybe Size)
/*	getControlMinimumSize(s) yields (Just minimumsize) if the indicated control had
	a ControlMinimumSize attribute and Nothing otherwise. 
*/

getControlResizes		:: ![Id] !WState -> [(Bool,Maybe ControlResizeFunction)]
getControlResize		:: ! Id  !WState ->  (Bool,Maybe ControlResizeFunction)
/*	getControlResize(s) yields (Just resizefunction) if the indicated control had a
	ControlResize attribute and Nothing otherwise.
*/

getRadioControlItems	:: ![Id] !WState -> [(Bool,Maybe [String])]
getRadioControlItem		:: ! Id  !WState ->  (Bool,Maybe [String])
/*	getRadioControlItem(s) yields the TextLines of the items of the indicated 
	RadioControl. If the control is not such a control, then Nothing is yielded.
*/

getRadioControlSelections:: ![Id] !WState -> [(Bool,Maybe Index)]
getRadioControlSelection :: ! Id  !WState ->  (Bool,Maybe Index)
/*	getRadioControlSelection(s) yields the index of the selected radio item of the 
	indicated RadioControl. If the control is not such a control, then Nothing is 
	yielded.
*/

getCheckControlItems	:: ![Id] !WState -> [(Bool,Maybe [String])]
getCheckControlItem		:: ! Id  !WState ->  (Bool,Maybe [String])
/*	getCheckControlItem(s) yields the TextLines of the items of the indicated 
	CheckControl. If the control is not such a control, then Nothing is yielded.
*/

getCheckControlSelections:: ![Id] !WState -> [(Bool,Maybe [Index])]
getCheckControlSelection :: ! Id  !WState ->  (Bool,Maybe [Index])
/*	getCheckControlSelection(s) yields the indices of the selected checkitems of the
	indicated CheckControl. If the control is not such a control, then Nothing is 
	yielded.
*/

getPopUpControlItems	:: ![Id] !WState -> [(Bool,Maybe [String])]
getPopUpControlItem		:: ! Id  !WState ->  (Bool,Maybe [String])
/*	getPopUpControlItem(s) yields the TextLines of the items of the indicated 
	PopUpControl. If the control is not such a control, then Nothing is yielded.
*/

getPopUpControlSelections:: ![Id] !WState -> [(Bool,Maybe Index)]
getPopUpControlSelection :: ! Id  !WState ->  (Bool,Maybe Index)
/*	getPopUpControlSelection(s) yields the Index of the indicated PopUpControl.
	If the control is not such a control, then Nothing is yielded.
*/

getSliderDirections		:: ![Id] !WState -> [(Bool,Maybe Direction)]
getSliderDirection		:: ! Id  !WState ->  (Bool,Maybe Direction)
/*	getSliderDirection(s) yields (Just Direction) of the indicated SliderControl. 
	If the control is not such a control, then Nothing is yielded.
*/

getSliderStates			:: ![Id] !WState -> [(Bool,Maybe SliderState)]
getSliderState			:: ! Id  !WState ->  (Bool,Maybe SliderState)
/*	getSliderState(s) yields (Just SliderState) of the indicated SliderControl. 
	If the control is not such a control, then Nothing is yielded. 
*/

getControlViewFrames	:: ![Id] !WState -> [(Bool,Maybe ViewFrame)]
getControlViewFrame		:: ! Id  !WState ->  (Bool,Maybe ViewFrame)
/*	getControlViewFrame(s) yields (Just ViewFrame) of the indicated CompoundControl.
	If the control is not such a control, then Nothing is yielded.
*/

getControlViewDomains	:: ![Id] !WState -> [(Bool,Maybe ViewDomain)]
getControlViewDomain	:: ! Id  !WState ->  (Bool,Maybe ViewDomain)
/*	getControlViewDomain(s) yields (Just ViewDomain) of the indicated 
	CompoundControl. If the control is not such a control, then Nothing is yielded.
*/

getControlScrollFunctions
						:: ![Id] !WState
						-> [(Bool,Maybe ((Direction,Maybe ScrollFunction)
										,(Direction,Maybe ScrollFunction)
										))]
getControlScrollFunction:: ! Id  !WState
						->  (Bool,Maybe ((Direction,Maybe ScrollFunction)
										,(Direction,Maybe ScrollFunction)
										))
/*	getControlScrollFunction(s) yields the ScrollFunctions of the indicated
	CompoundControl. If the control is not such a control, then Nothing is yielded.
*/

getControlItemSpaces	:: ![Id] !WState -> [(Bool,Maybe (Int,Int))]
getControlItemSpace		:: ! Id  !WState ->  (Bool,Maybe (Int,Int))
/*	getControlItemSpace(s) yields (Just (horizontal space,vertical space)) of the 
	indicated CompoundControl. If the control is not such a control, then Nothing 
	is yielded. 
*/

getControlMargins		:: ![Id] !WState -> [(Bool,Maybe ((Int,Int),(Int,Int)))]
getControlMargin		:: ! Id  !WState ->  (Bool,Maybe ((Int,Int),(Int,Int)))
/*	getControlMargins yields (Just (ControlHMargin,ControlVMargin)) of the 
	indicated CompoundControl. If the control is not such a control, then Nothing 
	is yielded.
*/
