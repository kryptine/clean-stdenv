implementation module oswindow


//	Clean Object I/O library, version 1.2


import	StdBool, StdInt, StdReal, StdClass, StdOverloaded, StdList, StdMisc, StdTuple
import	clCrossCall_12, clCCall_12, windowCCall_12, windowCrossCall_12
import	osdocumentinterface, osevent, osfont, ospicture, osrgn, ossystem, ostypes
from	menuCrossCall_12	import HMENU
from	commondef			import fatalError,intersectRects,rectSize,fromTuple,toTuple4,subVector


oswindowFatalError :: String String -> .x
oswindowFatalError function error
	= fatalError function "oswindow" error


/*	System dependent constants:
*/
OSControlTitleSpecialChars
	:== []											// Special prefix characters that should be removed


/*	System dependent metrics:
*/

osMinWindowSize :: (!Int,!Int)
osMinWindowSize = winMinimumWinSize

osMinCompoundSize :: (!Int,!Int)
osMinCompoundSize = (0,0)	// PA: (0,0)<--WinMinimumWinSize (Check if this safe)


/*	Initialisation:
*/
osInitialiseWindows :: !*OSToolbox -> *OSToolbox
osInitialiseWindows tb
	= winInitialiseWindows tb


/*	Determine the size of controls.
*/
osGetButtonControlSize :: !OSWindowMetrics !String !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetButtonControlSize wMetrics=:{osmFont,osmHeight} text tb
	# (widths,tb)	= osGetfontstringwidths False 0 [text] osmFont tb
	  width			= hd widths
	= ((2*osmHeight+width,osGetButtonControlHeight wMetrics),tb)

osGetButtonControlHeight :: !OSWindowMetrics -> Int
osGetButtonControlHeight {osmHeight} = 2*osmHeight

osGetTextControlSize :: !OSWindowMetrics !String !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetTextControlSize wMetrics=:{osmFont,osmHeight} text tb
	# (widths,tb)	= osGetfontstringwidths False 0 [text] osmFont tb
	  width			= hd widths
	= ((width+osmHeight/4,osGetTextControlHeight wMetrics),tb)

osGetTextControlHeight :: !OSWindowMetrics -> Int
osGetTextControlHeight {osmHeight} = osmHeight+osmHeight/2

osGetEditControlSize :: !OSWindowMetrics !Int !Int !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetEditControlSize wMetrics width nrlines tb
	= ((width,osGetEditControlHeight wMetrics nrlines),tb)

osGetEditControlHeight :: !OSWindowMetrics !Int -> Int
osGetEditControlHeight {osmHeight} nrlines = osmHeight/2+osmHeight*nrlines

osGetPopUpControlSize :: !OSWindowMetrics ![String] !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetPopUpControlSize wMetrics=:{osmFont,osmHeight} items tb
	# (widths,tb)	= osGetfontstringwidths False 0 items osmFont tb
	  maxwidth		= listmax widths
	= ((maxwidth+2*osmHeight+osmHeight/2,osGetPopUpControlHeight wMetrics),tb)
where
	listmax :: ![Int] -> Int
	listmax [x:xs]	= foldr max x xs
	listmax _		= 0

osGetPopUpControlHeight :: !OSWindowMetrics -> Int
osGetPopUpControlHeight {osmHeight} = osmHeight+osmHeight/2+2

osGetRadioControlItemSize :: !OSWindowMetrics !String !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetRadioControlItemSize wMetrics=:{osmFont,osmHeight} text tb
	# (widths,tb)	= osGetfontstringwidths False 0 [text] osmFont tb
	  width			= hd widths
	= ((width+2*osmHeight+osmHeight/2,osGetRadioControlItemHeight wMetrics),tb)

osGetRadioControlItemHeight :: !OSWindowMetrics -> Int
osGetRadioControlItemHeight {osmHeight}
	= osmHeight+osmHeight/2

osGetCheckControlItemSize :: !OSWindowMetrics !String !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetCheckControlItemSize wMetrics=:{osmFont,osmHeight} text tb
	# (widths,tb)	= osGetfontstringwidths False 0 [text] osmFont tb
	  width			= hd widths
	= ((width+2*osmHeight+osmHeight/2,osGetCheckControlItemHeight wMetrics),tb)

osGetCheckControlItemHeight :: !OSWindowMetrics -> Int
osGetCheckControlItemHeight {osmHeight}
	= osmHeight+osmHeight/2

osGetSliderControlSize :: !OSWindowMetrics !Bool !Int -> (!Int,!Int)
osGetSliderControlSize wMetrics isHorizontal length
	| isHorizontal	= (length,wMetrics.osmHSliderHeight)
	| otherwise		= (wMetrics.osmVSliderWidth,length)



/*	Determine the minimum width of controls.
*/
osGetButtonControlMinWidth :: !OSWindowMetrics -> Int
osGetButtonControlMinWidth {osmHeight} = 2*osmHeight

osGetTextControlMinWidth :: !OSWindowMetrics -> Int
osGetTextControlMinWidth {osmHeight} = osmHeight/4

osGetEditControlMinWidth :: !OSWindowMetrics -> Int
osGetEditControlMinWidth _ = 0

osGetPopUpControlMinWidth :: !OSWindowMetrics -> Int
osGetPopUpControlMinWidth {osmHeight} = 2*osmHeight+osmHeight/2

osGetRadioControlItemMinWidth :: !OSWindowMetrics -> Int
osGetRadioControlItemMinWidth {osmHeight} = 2*osmHeight+osmHeight/2

osGetCheckControlItemMinWidth :: !OSWindowMetrics -> Int
osGetCheckControlItemMinWidth {osmHeight} = 2*osmHeight+osmHeight/2

osGetSliderControlMinWidth :: !OSWindowMetrics -> Int
osGetSliderControlMinWidth _ = 0


/*	Window creation functions.
*/
osCreateDialog :: !Bool !Bool !String !(!Int,!Int) !(!Int,!Int) !OSWindowPtr
				  !(u:s->*(OSWindowPtr,u:s))
				  !(OSWindowPtr->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !OSDInfo !u:s !*OSToolbox
			   -> (![DelayActivationInfo],!OSWindowPtr,!u:s,!*OSToolbox)
osCreateDialog isModal isClosable title pos size behindPtr get_focus create_controls update_controls osdinfo control_info tb
	# (textPtr,tb)	= winMakeCString title tb
	  createcci		= Rq4Cci CcRqCREATEDIALOG textPtr parentptr (if (behindPtr==OSNoWindowPtr) 0 behindPtr) (toInt isModal)
	# (returncci,(control_info,delay_info),tb)
					= issueCleanRequest (osCreateDialogCallback get_focus create_controls update_controls)
										createcci
										(control_info,[]) tb
	# tb			= winReleaseCString textPtr tb
	  wPtr			= case returncci.ccMsg of
	  					CcRETURN1	-> returncci.p1
	  					CcWASQUIT	-> OSNoWindowPtr
	  					_			-> oswindowCreateError 1 "osCreateDialog"
	= (reverse delay_info,wPtr,control_info,tb)
where
	parentptr		= case (getOSDInfoOSInfo osdinfo) of
						Nothing        -> 0
						Just {osFrame} -> osFrame
	
	osCreateDialogCallback :: !(u:s->*(OSWindowPtr,u:s))
							  !(OSWindowPtr->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
							  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
							  !CrossCallInfo !*(u:s,[DelayActivationInfo]) !*OSToolbox
						  -> (!CrossCallInfo,!*(u:s,[DelayActivationInfo]),!*OSToolbox)
	osCreateDialogCallback _ _ _ {ccMsg=CcWmPAINT,p1=hwnd} s tb
		= //trace_n "osCreateDialogCallback CcWmPAINT" 
		  (return0Cci, s, winFakePaint hwnd tb)
	osCreateDialogCallback _ _ _ {ccMsg=CcWmACTIVATE,p1=hwnd} (control_info,delay_info) tb
		= //trace_n "osCreateDialogCallback CcWmACTIVATE" 
		  (return0Cci, (control_info,[DelayActivatedWindow hwnd:delay_info]), tb)
	osCreateDialogCallback _ _ _ {ccMsg=CcWmDEACTIVATE,p1=hwnd} (control_info,delay_info) tb
		= //trace_n "osCreateDialogCallback CcWmDEACTIVATE" 
		  (return0Cci, (control_info,[DelayDeactivatedWindow hwnd:delay_info]), tb)
	osCreateDialogCallback get_focus create_controls _ {ccMsg=CcWmINITDIALOG,p1=hwnd} (control_info,delay_info) tb
		# (control_info,tb)			= create_controls hwnd control_info tb
		# (defhandle,control_info)	= get_focus control_info
		  (x,y)						= pos
		  (w,h)						= size
		  r5cci						= return5Cci x y w h (if (defhandle==OSNoWindowPtr) 0 defhandle)
		= (r5cci, (control_info,delay_info), tb)
	osCreateDialogCallback _ _ update_controls {ccMsg=CcWmDRAWCONTROL,p1=hdlog,p2=hctrl,p3=hdc} (control_info,delay_info) tb
		# (control_info,tb)			= update_controls hdlog hctrl hdc control_info tb
		= (return0Cci, (control_info,delay_info), tb)
	osCreateDialogCallback _ _ _ {ccMsg=CcWmKEYBOARD} s tb
		= //trace_n "osCreateDialogCallback CcWmKEYBOARD"
		  (return0Cci, s, tb)
	osCreateDialogCallback _ _ _ {ccMsg=CcWmSETFOCUS} s tb
		= //trace_n "osCreateDialogCallback CcWmSETFOCUS"
		  (return0Cci, s, tb)
	osCreateDialogCallback _ _ _ {ccMsg=CcWmKILLFOCUS} s tb
		= //trace_n "osCreateDialogCallback CcWmKILLFOCUS"
		  (return0Cci, s, tb)
	osCreateDialogCallback _ _ _ {ccMsg} s tb
		= oswindowFatalError "osCreateDialogCallback" ("unknown message type ("+++toString ccMsg+++")")

osCreateWindow :: !OSWindowMetrics !Bool !ScrollbarInfo !ScrollbarInfo !(!Int,!Int) !(!Int,!Int)
				  !Bool !String !(!Int,!Int) !(!Int,!Int)
				  !(u:s->*(OSWindowPtr,u:s))
				  !(OSWindowPtr->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !OSDInfo !OSWindowPtr !u:s !*OSToolbox
			   -> (![DelayActivationInfo],!OSWindowPtr,!OSWindowPtr,!OSWindowPtr,!OSDInfo,!u:s,!*OSToolbox)
osCreateWindow	wMetrics isResizable hInfo=:{cbiHasScroll=hasHScroll} vInfo=:{cbiHasScroll=hasVScroll} minSize maxSize
				isClosable title pos size
				get_focus
				create_controls
				update_controls
				osdInfo behindPtr control_info tb
	| di==MDI
		# (textPtr,tb)	= winMakeCString title tb
		  styleFlags	= WS_SYSMENU
		  					bitor WS_OVERLAPPED
		  					bitor (if hasHScroll  WS_HSCROLL    0)
		  					bitor (if hasVScroll  WS_VSCROLL    0)
		  					bitor (if isResizable WS_THICKFRAME 0)
		  				//	bitor WS_CLIPCHILDREN
		  createcci		= Rq6Cci CcRqCREATEMDIDOCWINDOW textPtr osinfo.osClient behindPtr (x<<16+(y<<16)>>16) (w<<16+(h<<16)>>16) styleFlags
		# (returncci,(control_info,delay_info),tb)
						= issueCleanRequest (osCreateWindowCallback isResizable minSize maxSize create_controls update_controls)
											createcci
											(control_info,[]) tb
		# tb			= winReleaseCString textPtr tb
		  wPtr			= case returncci.ccMsg of
		  					CcRETURN1	-> returncci.p1
		  					CcWASQUIT	-> OSNoWindowPtr
		  					_			-> oswindowCreateError 1 "osCreateWindow (MDI)"
		# tb			= setScrollRangeAndPos hasHScroll False wMetrics SB_HORZ hInfo.cbiState (0,0) wPtr tb
		# tb			= setScrollRangeAndPos hasVScroll False wMetrics SB_VERT vInfo.cbiState (0,0) wPtr tb
		= (reverse delay_info,wPtr,OSNoWindowPtr,OSNoWindowPtr,osdInfo,control_info,tb)

	| di==SDI
		# (textPtr,tb)	= winMakeCString title tb		// PA+++
		  styleFlags	= (if hasHScroll WS_HSCROLL 0) bitor (if hasVScroll WS_VSCROLL 0)
		  createcci		= Rq6Cci CcRqCREATESDIDOCWINDOW textPtr osFrame (x<<16+(y<<16)>>16) w h styleFlags
		# (returncci,(control_info,delay_info),tb)
						= issueCleanRequest (osCreateWindowCallback isResizable minSize maxSize create_controls update_controls)
											createcci
											(control_info,[]) tb
		# tb			= winReleaseCString textPtr tb	// PA+++
		  clientPtr		= case returncci.ccMsg of
		  					CcRETURN1	-> returncci.p1
		  					CcWASQUIT	-> OSNoWindowPtr
		  					_			-> oswindowCreateError 1 "osCreateWindow (SDI)"
		  osdInfo		= setOSDInfoOSInfo {osinfo & osClient=clientPtr} osdInfo
		# tb			= setScrollRangeAndPos hasHScroll False wMetrics SB_HORZ hInfo.cbiState (0,0) clientPtr tb
		# tb			= setScrollRangeAndPos hasVScroll False wMetrics SB_VERT vInfo.cbiState (0,0) clientPtr tb
	//	# tb			= osSetWindowTitle osFrame title tb
		= (reverse delay_info,clientPtr,OSNoWindowPtr,OSNoWindowPtr,osdInfo,control_info,tb)
	
	| otherwise
		= oswindowFatalError "osCreateWindow" "unexpected OSDInfo (OSNoInfo) argument"
where
	(x,y)			= pos	// packed into one 32-bit integer
	(w,h)			= size
	di				= getOSDInfoDocumentInterface osdInfo
	osinfo			= fromJust (getOSDInfoOSInfo  osdInfo)
	osFrame			= osinfo.osFrame

osCreateWindowCallback :: !Bool !(!Int,!Int) !(!Int,!Int) 
						  !(OSWindowPtr->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
						  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
						  !CrossCallInfo !*(u:s,[DelayActivationInfo]) !*OSToolbox
					  -> (!CrossCallInfo,!*(u:s,[DelayActivationInfo]),!*OSToolbox)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmPAINT,p1=hwnd} s tb
	= //trace "osCreateWindowCallback CcWmPAINT"
	  (return0Cci, s, winFakePaint hwnd tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmACTIVATE,p1=hwnd} (control_info,delay_info) tb
	= //trace "osCreateWindowCallback CcWmACTIVATE" 
	  (return0Cci, (control_info,[DelayActivatedWindow hwnd:delay_info]), tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmDEACTIVATE,p1=hwnd} (control_info,delay_info) tb
	= //trace "osCreateWindowCallback CcWmDEACTIVATE" 
	  (return0Cci, (control_info,[DelayDeactivatedWindow hwnd:delay_info]), tb)
osCreateWindowCallback _ _ _ create_controls _ {ccMsg=CcWmCREATE,p1=hwnd} (control_info,deactivates) tb
	# (control_info,tb)			= create_controls hwnd control_info tb
	= (return0Cci, (control_info,deactivates), tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmNEWHTHUMB,p1=hwnd,p2=thumb} s tb
	= //trace "osCreateWindowCallback CcWmNEWHTHUMB" 
	  (return0Cci, s, tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmNEWVTHUMB,p1=hwnd,p2=thumb} s tb
	= //trace "osCreateWindowCallback CcWmNEWVTHUMB" 
	  (return0Cci, s, tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmSIZE,p1=hwnd,p2=width,p3=height} s tb
	= //trace ("osCreateWindowCallback CcWmSIZE "+++toString (width,height)) 
	  (return0Cci, s, tb)
osCreateWindowCallback _ _ _ _ update_controls {ccMsg=CcWmDRAWCONTROL,p1=hwnd,p2=hctrl,p3=hdc} (control_info,delay_info) tb
	# (control_info,tb)			= update_controls hwnd hctrl hdc control_info tb
	= //trace ("osCreateWindowCallback CcWmDRAWCONTROL "+++toString (hwnd,hctrl,hdc))
	  (return0Cci, (control_info,delay_info), tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmKILLFOCUS} s tb
	= //trace "osCreateWindowCallback CcWmKILLFOCUS" 
	  (return0Cci, s, tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg=CcWmKEYBOARD,p1=hwnd,p2=hctrl,p3=char,p4=ks,p5=mods} s tb
	= //trace "osCreateWindowCallback CcWmKEYBOARD "+++toString (hwnd,hctrl,char,ks,mods))
	  (return0Cci, s,tb)
osCreateWindowCallback _ _ _ _ _ {ccMsg} s tb
	= oswindowFatalError "osCreateWindowCallback" ("unknown message type ("+++toString ccMsg+++")")


/*	PA: new function that creates modal dialog and handles events until termination. 
		The Bool result is True iff no error occurred. 
*/
osCreateModalDialog :: !Bool !String !OSDInfo !(Maybe OSWindowPtr) !(u:s -> (OSEvents,u:s)) !((OSEvents,u:s)-> u:s) !(OSEvent -> u:s -> *([Int],u:s))
						!u:s !*OSToolbox
			  -> (!Bool,!u:s,!*OSToolbox)
osCreateModalDialog isClosable title osdinfo currentActiveModal getOSEvents setOSEvents handleOSEvents s tb
	# (textPtr,tb)		= winMakeCString title tb
	  createcci			= Rq2Cci CcRqCREATEMODALDIALOG textPtr parentptr
	# (returncci,s,tb)	= issueCleanRequest (osCreateModalDialogCallback getOSEvents setOSEvents handleOSEvents) createcci s tb
	# tb				= winReleaseCString textPtr tb
	  ok				= case returncci.ccMsg of
	  						CcRETURN1	-> returncci.p1==0
	  						CcWASQUIT	-> True
		  					_			-> oswindowCreateError 1 "osCreateModalDialog"
	= (ok,s,tb)
where
	parentptr			= if (isNothing currentActiveModal)
							(case (getOSDInfoOSInfo osdinfo) of
								Just info -> info.osFrame
								nothing   -> 0
							)
							(fromJust currentActiveModal)
	
	osCreateModalDialogCallback :: !(u:s -> (OSEvents,u:s)) !((OSEvents,u:s)-> u:s) !(OSEvent -> u:s -> *([Int],u:s)) 
									!CrossCallInfo !u:s !*OSToolbox
								-> (!CrossCallInfo,!u:s,!*OSToolbox)
	osCreateModalDialogCallback getOSEvents setOSEvents handleOSEvents osEvent s tb
		# (replyToOS,s)				= handleOSEvents osEvent s
		| not (isEmpty replyToOS)	// information must be returned to OS
			= (setReplyInOSEvent replyToOS,s,tb)
		# (osEvents, s)				= getOSEvents s
		# (noDelayEvents,osEvents)	= osIsEmptyEvents osEvents
		| noDelayEvents
			= (setReplyInOSEvent replyToOS,setOSEvents (osEvents,s),tb)
		| otherwise
			# (osEvent,osEvents)	= osRemoveEvent osEvents
			# s						= setOSEvents (osEvents,s)
			= osCreateModalDialogCallback getOSEvents setOSEvents handleOSEvents osEvent s tb


/*	Control creation functions.
*/
oswindowCreateError :: Int String -> .x
oswindowCreateError arity function
	= oswindowFatalError function ("Expected CcRETURN"+++toString arity+++" value.\n")

osIgnoreCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
osIgnoreCallback ccinfo=:{ccMsg=CcWmPAINT,p1=hwnd} tb
	= (return0Cci,winFakePaint hwnd tb)//WinEndPaint hwnd (WinBeginPaint hwnd tb))
osIgnoreCallback ccinfo tb 
	= (return0Cci,tb)

osIgnoreCallback` :: !CrossCallInfo ![DelayActivationInfo] !*OSToolbox -> (!CrossCallInfo,![DelayActivationInfo],!*OSToolbox)
osIgnoreCallback` {ccMsg=CcWmPAINT,p1=hwnd} s tb
	= (return0Cci,s,winFakePaint hwnd tb)//WinEndPaint hwnd (WinBeginPaint hwnd tb))
osIgnoreCallback` {ccMsg=CcWmACTIVATE,p1=hwnd} delayinfo tb
	= (return0Cci,[DelayActivatedWindow hwnd:delayinfo],tb)
osIgnoreCallback` {ccMsg=CcWmDEACTIVATE,p1=hwnd} delayinfo tb
	= (return0Cci,[DelayDeactivatedWindow hwnd:delayinfo],tb)
osIgnoreCallback` _ s tb
	= (return0Cci,s,tb)

/*	OKorCANCEL type is used to tell Windows that a (Custom)ButtonControl is 
	the OK, CANCEL, or normal button.
*/
::	OKorCANCEL
	=	OK | CANCEL | NORMAL

instance toInt OKorCANCEL where
	toInt OK     = ISOKBUTTON
	toInt CANCEL = ISCANCELBUTTON
	toInt NORMAL = ISNORMALBUTTON
instance toString OKorCANCEL where
	toString OK     = "OK"
	toString CANCEL = "CANCEL"
	toString NORMAL = "NORMAL"

osCreateRadioControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Bool !Bool !*OSToolbox
																						   -> (!OSWindowPtr,!*OSToolbox)
osCreateRadioControl parentWindow parentPos title show able (x,y) (w,h) selected isfirst tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATERADIOBUT parentWindow x y w h (toInt isfirst)
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  radioPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateRadioControl"
	# tb			= winSetWindowTitle radioPtr title tb
	# tb			= winCheckControl   radioPtr selected tb
	# tb			= winEnableControl  radioPtr able tb
	# tb			= winShowControl	radioPtr show tb
	= (radioPtr,tb)

osCreateCheckControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Bool !Bool !*OSToolbox
																						   -> (!OSWindowPtr,!*OSToolbox)
osCreateCheckControl parentWindow parentPos title show able (x,y) (w,h) selected isfirst tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATECHECKBOX parentWindow x y w h (toInt isfirst)
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  checkPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateCheckControl"
	# tb			= winSetWindowTitle checkPtr title tb
	# tb			= winCheckControl   checkPtr selected tb
	# tb			= winEnableControl  checkPtr able tb
	# tb			= winShowControl	checkPtr show tb
	= (checkPtr,tb)

MaxComboboxWidth		:== 65535		// System maximum for width  of combo box
MaxComboboxHeight		:==	65535		// System maximum for height of combo box
MaxComboElementsVisible	:==	15			// If there are <=MaxComboElementsVisible then show all elements
MaxComboElementsScroll	:==	12			// otherwise, show MaxComboElementsScroll elements

osCreateEmptyPopUpControl :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Int !Bool !*OSToolbox
	-> (!OSWindowPtr,!OSWindowPtr,!*OSToolbox)
osCreateEmptyPopUpControl parentWindow parentPos show able (x,y) (w,h) nrItems isEditable tb
	# (x,y)				= (x-fst parentPos,y-snd parentPos)
	# (screenRect,tb)	= osScreenrect tb
	# (wMetrics,tb)		= osDefaultWindowMetrics tb
	  screenSize		= rectSize screenRect
	  height			= wMetrics.osmHeight
	  okNrItems			= if (nrItems<=MaxComboElementsVisible) nrItems MaxComboElementsScroll
	  overall_h			= min screenSize.h (min MaxComboboxHeight (h + (okNrItems+1)*(height+2)))
	  overall_w			= min screenSize.w (min MaxComboboxWidth w)
	  createcci			= Rq6Cci CcRqCREATEPOPUP parentWindow x y overall_w overall_h (toInt isEditable)
	# (returncci,tb)	= issueCleanRequest2 osIgnoreCallback createcci tb
	  (popUpPtr,editPtr)= case returncci.ccMsg of
							CcRETURN2	-> (returncci.p1, returncci.p2)
							CcWASQUIT	-> (OSNoWindowPtr,OSNoWindowPtr)
							_			-> oswindowCreateError 2 "osCreateEmptyPopUpControl"
	# tb				= winEnableControl popUpPtr able tb
	# tb				= winShowControl   popUpPtr show tb
	= (popUpPtr,editPtr,tb)

osCreatePopUpControlItem :: !OSWindowPtr !Int !Bool !String !Bool !*OSToolbox -> (!Int,!*OSToolbox)
osCreatePopUpControlItem parentPopUp pos able title selected tb
	# (textPtr,tb)	= winMakeCString title tb
	  addcci		= Rq5Cci CcRqADDTOPOPUP parentPopUp textPtr (toInt able) (toInt selected) pos
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback addcci tb
	# tb			= winReleaseCString textPtr tb
	  index			= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> 0
						_			-> oswindowCreateError 1 "osCreatePopUpControlItem"
	= (index,tb)

osCreateSliderControl :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int) !(!Int,!Int,!Int,!Int) !*OSToolbox
																									 -> (!OSWindowPtr,!*OSToolbox)
osCreateSliderControl parentWindow parentPos show able horizontal (x,y) (w,h) (min,thumb,max,thumbSize) tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATESCROLLBAR parentWindow x y w h (toInt horizontal)
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  sliderPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateSliderControl"
	# tb			= winSetScrollRange sliderPtr SB_CTL min max False tb
	# tb			= winSetScrollPos   sliderPtr SB_CTL thumb (x+w) (y+h) (if horizontal h w) tb
	# tb			= winEnableControl  sliderPtr able tb
	# tb			= winShowControl	sliderPtr show tb
	= (sliderPtr,tb)

osCreateTextControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
osCreateTextControl parentWindow parentPos text show (x,y) (w,h) tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq5Cci CcRqCREATESTATICTXT parentWindow x y w h
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  textPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateTextControl"
	# tb			= winSetWindowTitle textPtr text tb
	# tb			= winShowControl	textPtr show tb
	= (textPtr,tb)

osCreateEditControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
osCreateEditControl parentWindow parentPos text show able isKeySensitive (x,y) (w,h) tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	# (wMetrics,tb)	= osDefaultWindowMetrics tb
	  nrLines		= (h-wMetrics.osmHeight)/wMetrics.osmHeight		//toInt ((toReal h) / (1.5*(toReal wMetrics.osmHeight)))
	  isMultiLine	= nrLines>1
	  editflags		= (if isMultiLine EDITISMULTILINE 0) + (if isKeySensitive EDITISKEYSENSITIVE 0)
	  createcci		= Rq6Cci CcRqCREATEEDITTXT parentWindow x y w h editflags
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  editPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateEditControl"
	# tb			= winSetWindowTitle editPtr text tb
	# tb			= winEnableControl	editPtr able tb
	# tb			= winShowControl	editPtr show tb
	= (editPtr,tb)

osCreateButtonControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !OKorCANCEL !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
osCreateButtonControl parentWindow parentPos title show able (x,y) (w,h) okOrCancel tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATEBUTTON parentWindow x y w h (toInt okOrCancel)
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  buttonPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateButtonControl"
	# tb			= winSetWindowTitle buttonPtr title tb
	# tb			= winEnableControl  buttonPtr able tb
	# tb			= winShowControl	buttonPtr show tb
	= (buttonPtr,tb)

osCreateCustomButtonControl :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !OKorCANCEL !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
osCreateCustomButtonControl parentWindow parentPos show able (x,y) (w,h) okOrCancel tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATEICONBUT parentWindow x y w h (toInt okOrCancel)
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  buttonPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateCustomButtonControl"
	# tb			= winEnableControl	buttonPtr able tb
	# tb			= winShowControl	buttonPtr show tb
	= (buttonPtr,tb)

osCreateCustomControl :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
osCreateCustomControl parentWindow parentPos show able (x,y) (w,h) tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq5Cci CcRqCREATECUSTOM parentWindow x y w h
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  customPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateCustomControl"
	# tb			= winEnableControl	customPtr able tb
	# tb			= winShowControl	customPtr show tb
	= (customPtr,tb)

::	ScrollbarInfo
	=	{	cbiHasScroll	:: !Bool				// The scrollbar exists
		,	cbiPos			:: (Int,Int)			// Its position within the parent
		,	cbiSize			:: (Int,Int)			// Its size within the parent
		,	cbiState		:: (Int,Int,Int,Int)	// Its (min,thumb,max,thumbsize) settings
		}

osCreateCompoundControl ::  !OSWindowMetrics !OSWindowPtr !(!Int,!Int) !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int)
							!ScrollbarInfo
							!ScrollbarInfo
							!*OSToolbox
						 -> (!OSWindowPtr,!OSWindowPtr,!OSWindowPtr,!*OSToolbox)
osCreateCompoundControl wMetrics parentWindow parentPos show able isTransparent (x,y) (w,h)
						hInfo=:{cbiHasScroll=hasHScroll}
						vInfo=:{cbiHasScroll=hasVScroll} tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  scrollFlags	= (if hasHScroll WS_HSCROLL 0) bitor (if hasVScroll WS_VSCROLL 0)
	  createcci		= Rq6Cci CcRqCREATECOMPOUND parentWindow (x<<16+(y<<16)>>16) w h scrollFlags (toInt isTransparent)
	# (returncci,tb)= issueCleanRequest2 osIgnoreCallback createcci tb
	  compoundPtr	= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "osCreateCompoundControl"
	# tb			= setScrollRangeAndPos hasHScroll False wMetrics SB_HORZ hInfo.cbiState (0,0) compoundPtr tb
	# tb			= setScrollRangeAndPos hasVScroll False wMetrics SB_VERT vInfo.cbiState (0,0) compoundPtr tb
	# tb			= winSetSelectStateWindow	compoundPtr (hasHScroll,hasVScroll) able False tb
	# tb			= winShowControl			compoundPtr show tb
	= (compoundPtr,OSNoWindowPtr,OSNoWindowPtr,tb)

setScrollRangeAndPos :: !Bool Bool OSWindowMetrics Int (Int,Int,Int,Int) (Int,Int) OSWindowPtr !*OSToolbox -> *OSToolbox
setScrollRangeAndPos hasScroll redraw wMetrics iBar state maxcoords wPtr tb
	| not hasScroll
		= tb
	# tb	= winSetScrollRange     wPtr iBar min max   False tb
	# tb	= winSetScrollPos       wPtr iBar thumb     0 0 0 tb
	| redraw
		= winSetScrollThumbSize wPtr iBar thumbsize maxx maxy extent tb
	| otherwise
		= winSetScrollThumbSize wPtr iBar thumbsize 0 0 0 tb
where
	(min,thumb,max,thumbsize)	= state
	(maxx,maxy)					= maxcoords
	horizontal					= iBar==SB_HORZ
	extent						= if horizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth


/*	Window destruction operations.
	PA: osDestroyWindow checks the process document interface and applies the appropriate destruction operation.
*/
/*	PA: previous implementation of osDestroyWindow without update handling.
osDestroyWindow :: !OSDInfo !Bool !Bool !OSWindowPtr !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
osDestroyWindow (OSMDInfo {osmdFrame,osmdClient}) isModal isWindow wPtr tb
	# (_,delayInfo,tb)	= issueCleanRequest osDelayCallback destroycci [] tb
	= (reverse delayInfo,tb)
where
	destroycci	= if isWindow (Rq3Cci CcRqDESTROYMDIDOCWINDOW osmdFrame osmdClient wPtr)
				 (if isModal  (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
							  (Rq1Cci CcRqDESTROYWINDOW wPtr))
osDestroyWindow (OSSDInfo _) isModal isWindow wPtr tb
	# (_,delayInfo,tb)	= issueCleanRequest osDelayCallback destroycci [] tb//(Rq1Cci CcRqDESTROYWINDOW wPtr) [] tb
	= (reverse delayInfo,tb)
where
	destroycci	= if isModal (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
							 (Rq1Cci CcRqDESTROYWINDOW wPtr)
osDestroyWindow OSNoInfo isModal isWindow wPtr tb
	| isWindow		/* This condition should never occur (NDI processes have only dialogues). */
		= oswindowFatalError "osDestroyWindow" "trying to destroy window of NDI process"
	| otherwise
		# (_,delayInfo,tb)= issueCleanRequest osDelayCallback destroycci [] tb//(Rq1Cci CcRqDESTROYWINDOW wPtr) [] tb
		= (reverse delayInfo,tb)
where
	destroycci	= if isModal (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
							 (Rq1Cci CcRqDESTROYWINDOW wPtr)

osDelayCallback :: !CrossCallInfo ![DelayActivationInfo] !*OSToolbox
			   -> (!CrossCallInfo,![DelayActivationInfo],!*OSToolbox)
osDelayCallback {ccMsg=CcWmPAINT,p1=wPtr} s tb
	= (return0Cci,s,winFakePaint wPtr tb)
osDelayCallback {ccMsg=CcWmACTIVATE,p1=wPtr} delayinfo tb
	= (return0Cci,[DelayActivatedWindow wPtr:delayinfo],tb)
osDelayCallback {ccMsg=CcWmDEACTIVATE,p1=wPtr} delayinfo tb
	= (return0Cci,[DelayDeactivatedWindow wPtr:delayinfo],tb)
osDelayCallback {ccMsg} s tb
	| expected	= (return0Cci,s,tb)
	| otherwise	= oswindowFatalError "osDelayCallback" ("unexpected delay message "+++toString ccMsg)
where
	expected	= case ccMsg of
					CcWmCLOSE			-> True
					CcWmDRAWCONTROL		-> True
					CcWmIDLETIMER		-> True
					CcWmKEYBOARD		-> True
					CcWmKILLFOCUS		-> True
					CcWmMOUSE			-> True
					CcWmSETFOCUS		-> True
					CcWmSIZE			-> True
					_					-> False
*/
osDestroyWindow :: !OSDInfo !Bool !Bool !OSWindowPtr !(OSEvent -> .s -> ([Int],.s)) !.s !*OSToolbox
														  -> (![DelayActivationInfo],.s,!*OSToolbox)
osDestroyWindow osdInfo isModal isWindow wPtr handleOSEvent state tb
	| di==MDI
		# destroycci				= if isWindow (Rq3Cci CcRqDESTROYMDIDOCWINDOW osFrame osClient wPtr)
									 (if isModal  (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
												  (Rq1Cci CcRqDESTROYWINDOW wPtr))
		# (_,(delayInfo,state),tb)	= issueCleanRequest (osDelayCallback handleOSEvent) destroycci ([],state) tb
		= (reverse delayInfo,state,tb)
	| di==SDI
		# destroycci				= if isModal (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
												 (Rq1Cci CcRqDESTROYWINDOW wPtr)
		# (_,(delayInfo,state),tb)	= issueCleanRequest (osDelayCallback handleOSEvent) destroycci ([],state) tb
		= (reverse delayInfo,state,tb)
	// It's a NDI process
	| isWindow		/* This condition should never occur (NDI processes have only dialogues). */
		= oswindowFatalError "osDestroyWindow" "trying to destroy window of NDI process"
	| otherwise
		# destroycci				= if isModal (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
												 (Rq1Cci CcRqDESTROYWINDOW wPtr)
		# (_,(delayInfo,state),tb)	= issueCleanRequest (osDelayCallback handleOSEvent) destroycci ([],state) tb
		= (reverse delayInfo,state,tb)
where
	di								= getOSDInfoDocumentInterface osdInfo
	{osFrame,osClient}				= fromJust (getOSDInfoOSInfo  osdInfo)

osDelayCallback :: !(OSEvent -> .s -> ([Int],.s)) !CrossCallInfo !(![DelayActivationInfo],.s) !*OSToolbox
											  -> (!CrossCallInfo,!(![DelayActivationInfo],.s),!*OSToolbox)
osDelayCallback handleOSEvent osEvent=:{ccMsg} (delayinfo,s) tb
	| toBeHandled
		# (replyToOS,s)	= handleOSEvent osEvent s
		= (setReplyInOSEvent replyToOS,(delayinfo,s),tb)
	| ccMsg==CcWmACTIVATE
		= (return0Cci,([DelayActivatedWindow osEvent.p1:delayinfo],s),tb)
	| ccMsg==CcWmDEACTIVATE
		= (return0Cci,([DelayDeactivatedWindow osEvent.p1:delayinfo],s),tb)
	| toBeSkipped
		= (return0Cci,(delayinfo,s),tb)
	| otherwise
		= oswindowFatalError "osDelayCallback" ("unexpected delay message "+++toString ccMsg)
where
	toBeHandled	= case ccMsg of
					CcWmPAINT			-> True
					CcWmDRAWCONTROL		-> True
					CcWmKEYBOARD		-> True
					CcWmKILLFOCUS		-> True
					CcWmMOUSE			-> True
					CcWmSETFOCUS		-> True
					other				-> False
	toBeSkipped	= case ccMsg of
					CcWmCLOSE			-> True
					CcWmIDLETIMER		-> True
					CcWmSIZE			-> True
					other				-> False


/*	Control destruction operations.
*/
destroycontrol :: !OSWindowPtr !*OSToolbox -> *OSToolbox
destroycontrol wPtr tb
	= snd (issueCleanRequest2 osDestroyControlCallback (Rq1Cci CcRqDESTROYWINDOW wPtr) tb)
where
	osDestroyControlCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
	osDestroyControlCallback info=:{ccMsg} tb
		| ccMsg==CcWmPAINT
			= (return0Cci,winFakePaint info.p1 tb)//WinEndPaint info.p1 (WinBeginPaint info.p1 tb))
		| expected
			= (return0Cci,tb)
		| otherwise
			= oswindowFatalError "osDestroyControlCallback" ("unexpected message "+++toString ccMsg)
	where
		expected	= case ccMsg of
						CcWmACTIVATE		-> True
						CcWmBUTTONCLICKED	-> True
						CcWmCOMBOSELECT		-> True
						CcWmCOMMAND			-> True
						CcWmDEACTIVATE		-> True
						CcWmDRAWCONTROL		-> True
						CcWmIDLETIMER		-> True
						CcWmKEYBOARD		-> True
						CcWmKILLFOCUS		-> True
						CcWmSETFOCUS		-> True
						_					-> False

osDestroyRadioControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyRadioControl wPtr tb = destroycontrol wPtr tb

osDestroyCheckControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyCheckControl wPtr tb = destroycontrol wPtr tb

osDestroyPopUpControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyPopUpControl wPtr tb = destroycontrol wPtr tb

osDestroySliderControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroySliderControl wPtr tb = destroycontrol wPtr tb

osDestroyTextControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyTextControl wPtr tb = destroycontrol wPtr tb

osDestroyEditControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyEditControl wPtr tb = destroycontrol wPtr tb

osDestroyButtonControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyButtonControl wPtr tb = destroycontrol wPtr tb

osDestroyCustomButtonControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyCustomButtonControl wPtr tb = destroycontrol wPtr tb

osDestroyCustomControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyCustomControl wPtr tb = destroycontrol wPtr tb

osDestroyCompoundControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osDestroyCompoundControl wPtr tb = destroycontrol wPtr tb


/*	Control update operations.
*/
osUpdateRadioControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateRadioControl area parentWindow theControl tb = updatecontrol theControl area tb

osUpdateCheckControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateCheckControl area parentWindow theControl tb = updatecontrol theControl area tb

osUpdatePopUpControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdatePopUpControl area parentWindow theControl tb = updatecontrol theControl area tb

osUpdateSliderControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateSliderControl area parentWindow theControl tb = updatecontrol theControl area tb

osUpdateTextControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateTextControl area parentWindow theControl tb = updatecontrol theControl area tb

osUpdateEditControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateEditControl area parentWindow theControl tb = updatecontrol theControl area tb

osUpdateButtonControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateButtonControl area parentWindow theControl tb = updatecontrol theControl area tb

osUpdateCompoundControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osUpdateCompoundControl area parentWindow theControl tb = updatecontrol theControl area tb

updatecontrol :: !OSWindowPtr !Rect !*OSToolbox -> *OSToolbox
updatecontrol theControl rect tb = winUpdateWindowRect theControl (toTuple4 rect) tb


/*	Control clipping operations.
*/
oscliprectrgn :: !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
oscliprectrgn parent_pos=:(parent_x,parent_y) rect (x,y) (w,h) tb
	= osnewrectrgn (intersectRects area item) tb
where
	area	= subVector (fromTuple parent_pos) rect
	x`		= x-parent_x
	y`		= y-parent_y
	item	= {rleft=x`,rtop=y`,rright=x`+w,rbottom=y`+h}

osClipRadioControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipRadioControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipCheckControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipCheckControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipPopUpControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipPopUpControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipSliderControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipSliderControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipTextControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipTextControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipEditControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipEditControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipButtonControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipButtonControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipCustomButtonControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipCustomButtonControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipCustomControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipCustomControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

osClipCompoundControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osClipCompoundControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

/*	Window graphics context access operations.
*/
osGrabWindowPictContext :: !OSWindowPtr !*OSToolbox -> (!OSPictContext,!*OSToolbox)
osGrabWindowPictContext wPtr tb
	= winGetDC wPtr tb

osReleaseWindowPictContext :: !OSWindowPtr !OSPictContext !*OSToolbox -> *OSToolbox
osReleaseWindowPictContext wPtr hdc tb
	= winReleaseDC wPtr (hdc,tb)


/*	Window access operations.
*/
toOSscrollbarRange :: !(!Int,!Int,!Int) !Int -> (!Int,!Int,!Int,!Int)
toOSscrollbarRange (domainMin,viewMin,domainMax) viewSize
	= (osRangeMin,osThumb,osRangeMax,osThumbSize+1)
where
	(osRangeMin,osRangeMax)	= toOSRange (domainMin,domainMax)
	range					=  domainMax- domainMin
	osRange					= osRangeMax-osRangeMin
	osThumb					= inRange osRangeMin osRange (viewMin-domainMin) range
	osThumbSize				= if (viewSize>=range) osRange (toInt (((toReal viewSize)/(toReal range))*(toReal osRange)))

fromOSscrollbarRange :: !(!Int,!Int) !Int -> Int
fromOSscrollbarRange (domainMin,domainMax) osThumb
	= inRange domainMin range (osThumb-osRangeMin) osRange
where
	(osRangeMin,osRangeMax)	= toOSRange (domainMin,domainMax)
	range					=  domainMax- domainMin
	osRange					= osRangeMax-osRangeMin

osScrollbarIsVisible :: !(!Int,!Int) !Int -> Bool
osScrollbarIsVisible (domainMin,domainMax) viewSize
	= viewSize<domainMax-domainMin

osScrollbarsAreVisible :: !OSWindowMetrics !Rect !(!Int,!Int) !(!Bool,!Bool) -> (!Bool,!Bool)
osScrollbarsAreVisible {osmHSliderHeight,osmVSliderWidth} {rleft=xMin,rtop=yMin,rright=xMax,rbottom=yMax} (width,height) (hasHScroll,hasVScroll)
	= visScrollbars (False,False)
					(hasHScroll && (osScrollbarIsVisible hRange width),hasVScroll && (osScrollbarIsVisible vRange height))
where
	hRange	= (xMin,xMax)
	vRange	= (yMin,yMax)
	
	visScrollbars :: !(!Bool,!Bool) !(!Bool,!Bool) -> (!Bool,!Bool)
	visScrollbars (showH1,showV1) (showH2,showV2)
		| showH1==showH2 && showV1==showV2
			= (showH1,showV1)
		| otherwise
			= visScrollbars (showH2,showV2) (showH,showV)
	where
		showH	= if showV2 (hasHScroll && osScrollbarIsVisible hRange (width -osmVSliderWidth )) showH2
		showV	= if showH2 (hasVScroll && osScrollbarIsVisible vRange (height-osmHSliderHeight)) showV2

toOSRange :: !(!Int,!Int) -> (!Int,!Int)
toOSRange (min,max)
	= (OSSliderMin,if (range<=OSSliderRange) (OSSliderMin+range) OSSliderMax)
where
	range = max-min

inRange :: !Int !Int !Int !Int -> Int
inRange destMin destRange sourceValue sourceRange
	= destMin + (toInt (((toReal sourceValue) / (toReal sourceRange)) * (toReal destRange)))

OSSliderMin		:== 0			// 0
OSSliderMax		:== 32767		// MaxSigned2ByteInt
OSSliderRange	:== 32767		// OSSliderMax-OSSliderMin


osSetWindowSliderThumb :: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetWindowSliderThumb wMetrics theWindow isHorizontal thumb (maxx,maxy) redraw tb
	= winSetScrollPos theWindow (if isHorizontal SB_HORZ SB_VERT) thumb maxx maxy extent tb
where
	extent	= if isHorizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth

osSetWindowSliderThumbSize :: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetWindowSliderThumbSize wMetrics theWindow isHorizontal size (maxx,maxy) redraw tb
	= winSetScrollThumbSize theWindow (if isHorizontal SB_HORZ SB_VERT) size maxx maxy extent tb
where
	extent	= if isHorizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth

osSetWindowSlider :: !OSWindowMetrics !OSWindowPtr !Bool !(!Int,!Int,!Int,!Int) !(!Int,!Int) !*OSToolbox -> *OSToolbox
osSetWindowSlider wMetrics theWindow isHorizontal state maxcoords tb
	= setScrollRangeAndPos True True wMetrics (if isHorizontal SB_HORZ SB_VERT) state maxcoords theWindow tb

osInvalidateWindow :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osInvalidateWindow theWindow tb
	= winInvalidateWindow theWindow tb

osInvalidateWindowRect :: !OSWindowPtr !Rect !*OSToolbox -> *OSToolbox
osInvalidateWindowRect theWindow rect tb
	= winInvalidateRect theWindow (toTuple4 rect) tb

osValidateWindowRect :: !OSWindowPtr !Rect !*OSToolbox -> *OSToolbox
osValidateWindowRect theWindow rect tb
	= winValidateRect theWindow (toTuple4 rect) tb

osValidateWindowRgn :: !OSWindowPtr !OSRgnHandle !*OSToolbox -> *OSToolbox
osValidateWindowRgn theWindow rgn tb
	= winValidateRgn theWindow rgn tb

osDisableWindow :: !OSWindowPtr !(!Bool,!Bool) !Bool !*OSToolbox -> *OSToolbox
osDisableWindow theWindow scrollInfo modalContext tb
	= winSetSelectStateWindow theWindow scrollInfo False modalContext tb

osEnableWindow :: !OSWindowPtr !(!Bool,!Bool) !Bool !*OSToolbox -> *OSToolbox
osEnableWindow theWindow scrollInfo modalContext tb
	= winSetSelectStateWindow theWindow scrollInfo True modalContext tb

osActivateWindow :: !OSDInfo !OSWindowPtr !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !.s !*OSToolbox
	-> (![DelayActivationInfo],!.s,!*OSToolbox)
osActivateWindow osdInfo thisWindow handleOSEvent state tb
	# (_,(delayinfo,state),tb)	= issueCleanRequest (osCallback handleOSEvent) (Rq3Cci CcRqACTIVATEWINDOW (toInt isMDI) clientPtr thisWindow) ([],state) tb
	= (reverse delayinfo,state,tb)
where
	isMDI				= getOSDInfoDocumentInterface osdInfo==MDI
	clientPtr			= case (getOSDInfoOSInfo osdInfo) of
							Just {osClient} -> osClient
							nothing         -> oswindowFatalError "osActivateWindow" "illegal DocumentInterface context"
	
/*	osCallback delays activate and deactivate events.
	All other events are passed to the callback function.
*/	osCallback :: !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !CrossCallInfo !(![DelayActivationInfo],!.s) !*OSToolbox
		-> (!CrossCallInfo,!(![DelayActivationInfo],!.s),!*OSToolbox)
	osCallback handleOSEvent osEvent=:{ccMsg,p1,p2} (delayinfo,s) tb
		| isDelayEvent
			= (return0Cci,([delayEvent:delayinfo],s),tb)
		| otherwise
			# (s,tb)	= handleOSEvent osEvent (s,tb)
			= (return0Cci,(delayinfo,s),tb)
	where
		(isDelayEvent,delayEvent)	= case ccMsg of
										CcWmACTIVATE   -> (True,DelayActivatedWindow    p1)
										CcWmDEACTIVATE -> (True,DelayDeactivatedWindow  p1)
										CcWmKILLFOCUS  -> (True,DelayDeactivatedControl p1 p2)
										CcWmSETFOCUS   -> (True,DelayActivatedControl   p1 p2)
										_              -> (False,undef)


osActivateControl :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
osActivateControl parentWindow controlPtr tb
	# (_,delayinfo,tb)	= issueCleanRequest osIgnoreCallback` (Rq1Cci CcRqACTIVATECONTROL controlPtr) [] tb
	= (reverse delayinfo,tb)
where
	osIgnoreCallback` :: !CrossCallInfo ![DelayActivationInfo] !*OSToolbox -> (!CrossCallInfo,![DelayActivationInfo],!*OSToolbox)
	osIgnoreCallback` {ccMsg=CcWmPAINT,p1=hwnd} s tb
		= (return0Cci,s,winFakePaint hwnd tb)//winEndPaint hwnd (winBeginPaint hwnd tb))
	osIgnoreCallback` {ccMsg=CcWmKILLFOCUS,p1=hwnd,p2=cptr} delayinfo tb
		= (return0Cci,[DelayDeactivatedControl hwnd cptr:delayinfo],tb)
	osIgnoreCallback` {ccMsg=CcWmSETFOCUS,p1=hwnd,p2=cptr} delayinfo tb
		= (return0Cci,[DelayActivatedControl hwnd cptr:delayinfo],tb)
	osIgnoreCallback` _ s tb
		= (return0Cci,s,tb)

/*	PA: previous implementation of osStackWindow ignored window updates and resizes. This is fixed below.
osStackWindow :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
osStackWindow thisWindow behindWindow tb
	= winRestackWindow thisWindow behindWindow tb

winRestackWindow :: !HWND !HWND !*OSToolbox -> *OSToolbox
winRestackWindow theWindow behindWindow tb
	= snd (issueCleanRequest2 (errorCallback2 "WinRestackWindow") (Rq2Cci CcRqRESTACKWINDOW theWindow behindWindow) tb)
*/

osStackWindow :: !OSWindowPtr !OSWindowPtr !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !.s !*OSToolbox
	-> (![DelayActivationInfo],!.s,!*OSToolbox)
osStackWindow thisWindow behindWindow handleOSEvent state tb
	# (_,(delayinfo,state),tb)	= issueCleanRequest (osCallback handleOSEvent) (Rq2Cci CcRqRESTACKWINDOW thisWindow behindWindow) ([],state) tb
	= (reverse delayinfo,state,tb)
where
/*	osCallback delays activate and deactivate events.
	All other events are passed to the callback function. 
	PA: is now identical to osActivateWindow!!
*/	osCallback :: !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !CrossCallInfo !(![DelayActivationInfo],!.s) !*OSToolbox
		-> (!CrossCallInfo,!(![DelayActivationInfo],!.s),!*OSToolbox)
	osCallback handleOSEvent {ccMsg=CcWmACTIVATE,p1=hwnd} (delayinfo,s) tb
		= (return0Cci,([DelayActivatedWindow hwnd:delayinfo],s),tb)
	osCallback handleOSEvent {ccMsg=CcWmDEACTIVATE,p1=hwnd} (delayinfo,s) tb
		= (return0Cci,([DelayDeactivatedWindow hwnd:delayinfo],s),tb)
	osCallback handleOSEvent osEvent (delayinfo,s) tb
		# (s,tb)	= handleOSEvent osEvent (s,tb)
		= (return0Cci,(delayinfo,s),tb)

osHideWindow :: !OSWindowPtr !Bool !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
osHideWindow wPtr activate tb
	# (_,delayinfo,tb)	= issueCleanRequest osIgnoreCallback` (Rq3Cci CcRqSHOWWINDOW wPtr (toInt False) (toInt activate)) [] tb
	= (reverse delayinfo,tb)

osShowWindow :: !OSWindowPtr !Bool !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
osShowWindow wPtr activate tb
	# (_,delayinfo,tb)	= issueCleanRequest osIgnoreCallback` (Rq3Cci CcRqSHOWWINDOW wPtr (toInt True) (toInt activate)) [] tb
	= (reverse delayinfo,tb)

osSetWindowCursor :: !OSWindowPtr !Int !*OSToolbox -> *OSToolbox
osSetWindowCursor wPtr cursorCode tb
	= winSetWindowCursor wPtr cursorCode tb

osGetWindowPos :: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetWindowPos wPtr tb
	= winGetWindowPos wPtr tb

osGetWindowViewFrameSize :: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetWindowViewFrameSize wPtr tb
	= winGetClientSize wPtr tb

osGetWindowSize :: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
osGetWindowSize wPtr tb
	= winGetWindowSize wPtr tb

osSetWindowPos :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !*OSToolbox -> *OSToolbox
osSetWindowPos wPtr pos update inclScrollbars tb
	= winSetWindowPos wPtr pos update inclScrollbars tb

osSetWindowViewFrameSize :: !OSWindowPtr !(!Int,!Int) !*OSToolbox -> *OSToolbox
osSetWindowViewFrameSize wPtr size tb
	= winSetClientSize wPtr size tb

osSetWindowSize	:: !OSWindowPtr !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetWindowSize wPtr size update tb
	= winSetWindowSize wPtr size update tb

osSetWindowTitle :: !OSWindowPtr !String !*OSToolbox -> *OSToolbox
osSetWindowTitle wPtr title tb
	= winSetWindowTitle wPtr title tb


/*	Control access operations.
*/
//	On compound controls:

osInvalidateCompound :: !OSWindowPtr !*OSToolbox -> *OSToolbox
osInvalidateCompound compoundPtr tb
	= winInvalidateWindow compoundPtr tb

osInvalidateCompoundRect :: !OSWindowPtr !Rect !*OSToolbox -> *OSToolbox
osInvalidateCompoundRect compoundPtr rect tb
	= winInvalidateRect compoundPtr (toTuple4 rect) tb

osSetCompoundSliderThumb :: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCompoundSliderThumb wMetrics compoundPtr isHorizontal thumb (maxx,maxy) redraw tb
	= winSetScrollPos compoundPtr (if isHorizontal SB_HORZ SB_VERT) thumb maxx` maxy` extent tb
where
	(maxx`,maxy`,extent)	= if redraw (maxx,maxy,if isHorizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth) (0,0,0)

osSetCompoundSliderThumbSize :: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCompoundSliderThumbSize wMetrics compoundPtr isHorizontal size (maxx,maxy) redraw tb
	= winSetScrollThumbSize compoundPtr (if isHorizontal SB_HORZ SB_VERT) size maxx` maxy` extent tb
where
	(maxx`,maxy`,extent)	= if redraw (maxx,maxy,if isHorizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth) (0,0,0)

osSetCompoundSlider :: !OSWindowMetrics !OSWindowPtr !Bool !(!Int,!Int,!Int,!Int) !(!Int,!Int) !*OSToolbox -> *OSToolbox
osSetCompoundSlider wMetrics compoundPtr isHorizontal state maxcoords tb
	= setScrollRangeAndPos True True wMetrics (if isHorizontal SB_HORZ SB_VERT) state maxcoords compoundPtr tb

osSetCompoundSelect :: !OSWindowPtr !OSWindowPtr !Rect !(!Bool,!Bool) !Bool !*OSToolbox -> *OSToolbox
osSetCompoundSelect _ compoundPtr _ scrollInfo select tb
	= winSetSelectStateWindow compoundPtr scrollInfo select False tb
//	= winEnableControl compoundPtr scrollInfo select tb

osSetCompoundShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetCompoundShow _ compoundPtr _ show tb
	= winShowControl compoundPtr show tb

osSetCompoundPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCompoundPos _ (parent_x,parent_y) compoundPtr (x,y) _ update tb
	= winSetWindowPos compoundPtr (x-parent_x,y-parent_y) update True tb

osSetCompoundSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCompoundSize _ _ compoundPtr _ size update tb
	= winSetWindowSize compoundPtr size update tb

OSCompoundMovesControls :== True


//	On slider controls:

osSetSliderThumb :: !OSWindowPtr !OSWindowPtr !Rect !Bool !(!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
osSetSliderThumb _ cPtr _ redraw (min,thumb,max) tb
	= winSetScrollPos cPtr SB_CTL thumb 0 0 0 tb//redraw tb

osSetSliderControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetSliderControlSelect _ cPtr _ select tb
	= winEnableControl cPtr select tb

osSetSliderControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetSliderControlShow _ cPtr _ show tb
	= winShowControl cPtr show tb

osSetSliderControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetSliderControlPos _ (parent_x,parent_y) sliderPtr (x,y) _ update tb
	= winSetWindowPos sliderPtr (x-parent_x,y-parent_y) update False tb

osSetSliderControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetSliderControlSize _ _ sliderPtr _ size update tb
	= winSetWindowSize sliderPtr size update tb


//	On radio controls:

osSetRadioControl :: !OSWindowPtr !OSWindowPtr !OSWindowPtr !Rect !*OSToolbox -> *OSToolbox
osSetRadioControl _ current new _ tb
	= winCheckControl new True (winCheckControl current False tb)

osSetRadioControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetRadioControlSelect _ cPtr _ select tb
	= winEnableControl cPtr select tb

osSetRadioControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetRadioControlShow _ cPtr _ show tb
	= winShowControl cPtr show tb

osSetRadioControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetRadioControlPos _ (parent_x,parent_y) radioPtr (x,y) _ update tb
	= winSetWindowPos radioPtr (x-parent_x,y-parent_y) update False tb

osSetRadioControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetRadioControlSize _ _ radioPtr _ size update tb
	= winSetWindowSize radioPtr size update tb


//	On check controls:

osSetCheckControl :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetCheckControl _ cPtr _ check tb
	= winCheckControl cPtr check tb

osSetCheckControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetCheckControlSelect _ cPtr _ select tb
	= winEnableControl cPtr select tb

osSetCheckControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetCheckControlShow _ cPtr _ show tb
	= winShowControl cPtr show tb

osSetCheckControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCheckControlPos _ (parent_x,parent_y) checkPtr (x,y) _ update tb
	= winSetWindowPos checkPtr (x-parent_x,y-parent_y) update False tb

osSetCheckControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCheckControlSize _ _ checkPtr _ size update tb
	= winSetWindowSize checkPtr size update tb


//	On pop up controls:

osSetPopUpControl :: !OSWindowPtr !OSWindowPtr !Rect !Rect !Int !Int !String !Bool !*OSToolbox -> *OSToolbox
osSetPopUpControl _ pPtr _ _ _ new _ _ tb
	= winSelectPopupItem pPtr (new-1) tb

osSetPopUpControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetPopUpControlSelect _ pPtr _ select tb
	= winEnableControl pPtr select tb

osSetPopUpControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetPopUpControlShow _ pPtr _ show tb
	= winShowControl pPtr show tb

osSetPopUpControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetPopUpControlPos _ (parent_x,parent_y) popupPtr (x,y) _ update tb
	= winSetWindowPos popupPtr (x-parent_x,y-parent_y) update False tb

osSetPopUpControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetPopUpControlSize _ _ popupPtr _ size update tb
	= winSetWindowSize popupPtr size update tb


//	On edit controls:

osSetEditControlText :: !OSWindowPtr !OSWindowPtr !Rect !Rect !Bool !String !*OSToolbox -> *OSToolbox
osSetEditControlText _ ePtr _ _ _ text tb
	= winSetWindowTitle ePtr text tb

osGetEditControlText :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!String,!*OSToolbox) 
osGetEditControlText _ ePtr tb
	= winGetWindowText ePtr tb

osSetEditControlCursor :: !OSWindowPtr !OSWindowPtr !Rect !Rect !Int !*OSToolbox -> *OSToolbox
osSetEditControlCursor _ ePtr _ _ pos tb
	= winSetEditSelection ePtr pos (pos+1) tb

osSetEditControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetEditControlSelect _ ePtr _ select tb
	= winEnableControl ePtr select tb

osSetEditControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetEditControlShow _ ePtr _ show tb
	= winShowControl ePtr show tb

osSetEditControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetEditControlPos _ (parent_x,parent_y) editPtr (x,y) _ update tb
	= winSetWindowPos editPtr (x-parent_x,y-parent_y) update False tb

osSetEditControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetEditControlSize _ _ editPtr _ size update tb
	= winSetWindowSize editPtr size update tb


//	On text controls:

osSetTextControlText :: !OSWindowPtr !OSWindowPtr !Rect !Rect !Bool !String !*OSToolbox -> *OSToolbox
osSetTextControlText _ tPtr _ _ _ text tb
	= winSetWindowTitle tPtr text tb

osSetTextControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetTextControlSelect _ tPtr _ select tb
	= winEnableControl tPtr select tb

osSetTextControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetTextControlShow _ tPtr _ show tb
	= winShowControl tPtr show tb

osSetTextControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetTextControlPos _ (parent_x,parent_y) textPtr (x,y) _ update tb
	= winSetWindowPos textPtr (x-parent_x,y-parent_y) update False tb

osSetTextControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetTextControlSize _ _ textPtr _ size update tb
	= winSetWindowSize textPtr size update tb


//	On button controls:

osSetButtonControlText :: !OSWindowPtr !OSWindowPtr !Rect !String !*OSToolbox -> *OSToolbox
osSetButtonControlText _ bPtr _ text tb
	= winSetWindowTitle bPtr text tb

osSetButtonControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetButtonControlSelect _ bPtr _ select tb
	= winEnableControl bPtr select tb

osSetButtonControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetButtonControlShow _ bPtr _ show tb
	= winShowControl bPtr show tb

osSetButtonControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetButtonControlPos _ (parent_x,parent_y) buttonPtr (x,y) _ update tb
	= winSetWindowPos buttonPtr (x-parent_x,y-parent_y) update False tb

osSetButtonControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetButtonControlSize _ _ buttonPtr _ size update tb
	= winSetWindowSize buttonPtr size update tb


//	On custom button controls:

osSetCustomButtonControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetCustomButtonControlSelect _ cPtr _ select tb
	= winEnableControl cPtr select tb

osSetCustomButtonControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetCustomButtonControlShow _ cPtr _ show tb
	= winShowControl cPtr show tb

osSetCustomButtonControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCustomButtonControlPos _ (parent_x,parent_y) cPtr (x,y) _ update tb
	= winSetWindowPos cPtr (x-parent_x,y-parent_y) update False tb

osSetCustomButtonControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCustomButtonControlSize _ _ cPtr _ size update tb
	= winSetWindowSize cPtr size update tb


//	On custom controls:

osSetCustomControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetCustomControlSelect _ cPtr _ select tb
	= winEnableControl cPtr select tb

osSetCustomControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
osSetCustomControlShow _ cPtr _ show tb
	= winShowControl cPtr show tb

osSetCustomControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCustomControlPos _ (parent_x,parent_y) customPtr (x,y) _ update tb
	= winSetWindowPos customPtr (x-parent_x,y-parent_y) update False tb

osSetCustomControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
osSetCustomControlSize _ _ customPtr _ size update tb
	= winSetWindowSize customPtr size update tb
