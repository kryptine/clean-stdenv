implementation module oswindow


//	Clean Object I/O library, version 1.2


import	StdBool, StdInt, StdReal, StdClass, StdOverloaded, StdList, StdMisc, StdTuple
import	clCrossCall_12, clCCall_12, windowCCall_12, windowCrossCall_12
import	osdocumentinterface, osevent, osfont, ospicture, osrgn, ossystem, ostypes
from	menuCrossCall_12	import HMENU
from	commondef			import FatalError,IntersectRects,RectSize,fromTuple,toTuple4,subVector,HdTl
//import StdDebug,tracetypes


oswindowFatalError :: String String -> .x
oswindowFatalError function error
	= FatalError function "oswindow" error


/*	System dependent constants:
*/
OSControlTitleSpecialChars
	:== []											// Special prefix characters that should be removed


/*	System dependent metrics:
*/

OSMinWindowSize :: (!Int,!Int)
OSMinWindowSize = WinMinimumWinSize

OSMinCompoundSize :: (!Int,!Int)
OSMinCompoundSize = (0,0)	// PA: (0,0)<--WinMinimumWinSize (Check if this safe)


/*	Determine the size of controls.
*/
OSgetButtonControlSize :: !OSWindowMetrics !String !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetButtonControlSize wMetrics=:{osmFont,osmHeight} text tb
	# (widths,tb)	= OSgetfontstringwidths False 0 [text] osmFont tb
	  width			= hd widths
	= ((2*osmHeight+width,OSgetButtonControlHeight wMetrics),tb)

OSgetButtonControlHeight :: !OSWindowMetrics -> Int
OSgetButtonControlHeight {osmHeight} = 2*osmHeight

OSgetTextControlSize :: !OSWindowMetrics !String !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetTextControlSize wMetrics=:{osmFont,osmHeight} text tb
	# (widths,tb)	= OSgetfontstringwidths False 0 [text] osmFont tb
	  width			= hd widths
	= ((width+osmHeight/4,OSgetTextControlHeight wMetrics),tb)

OSgetTextControlHeight :: !OSWindowMetrics -> Int
OSgetTextControlHeight {osmHeight} = osmHeight+osmHeight/2

OSgetEditControlSize :: !OSWindowMetrics !Int !Int !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetEditControlSize wMetrics width nrlines tb
	= ((width,OSgetEditControlHeight wMetrics nrlines),tb)

OSgetEditControlHeight :: !OSWindowMetrics !Int -> Int
OSgetEditControlHeight {osmHeight} nrlines = osmHeight/2+osmHeight*nrlines

OSgetPopUpControlSize :: !OSWindowMetrics ![String] !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetPopUpControlSize wMetrics=:{osmFont,osmHeight} items tb
	# (widths,tb)	= OSgetfontstringwidths False 0 items osmFont tb
	  maxwidth		= listmax widths
	= ((maxwidth+2*osmHeight+osmHeight/2,OSgetPopUpControlHeight wMetrics),tb)
where
	listmax :: ![Int] -> Int
	listmax [x:xs]	= foldr max x xs
	listmax _		= 0

OSgetPopUpControlHeight :: !OSWindowMetrics -> Int
OSgetPopUpControlHeight {osmHeight} = osmHeight+osmHeight/2+2

OSgetRadioControlItemSize :: !OSWindowMetrics !String !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetRadioControlItemSize wMetrics=:{osmFont,osmHeight} text tb
	# (widths,tb)	= OSgetfontstringwidths False 0 [text] osmFont tb
	  width			= hd widths
	= ((width+2*osmHeight+osmHeight/2,OSgetRadioControlItemHeight wMetrics),tb)

OSgetRadioControlItemHeight :: !OSWindowMetrics -> Int
OSgetRadioControlItemHeight {osmHeight}
	= osmHeight+osmHeight/2

OSgetCheckControlItemSize :: !OSWindowMetrics !String !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetCheckControlItemSize wMetrics=:{osmFont,osmHeight} text tb
	# (widths,tb)	= OSgetfontstringwidths False 0 [text] osmFont tb
	  width			= hd widths
	= ((width+2*osmHeight+osmHeight/2,OSgetCheckControlItemHeight wMetrics),tb)

OSgetCheckControlItemHeight :: !OSWindowMetrics -> Int
OSgetCheckControlItemHeight {osmHeight}
	= osmHeight+osmHeight/2

OSgetSliderControlSize :: !OSWindowMetrics !Bool !Int -> (!Int,!Int)
OSgetSliderControlSize wMetrics isHorizontal length
	| isHorizontal	= (length,wMetrics.osmHSliderHeight)
	| otherwise		= (wMetrics.osmVSliderWidth,length)



/*	Determine the minimum width of controls.
*/
OSgetButtonControlMinWidth :: !OSWindowMetrics -> Int
OSgetButtonControlMinWidth {osmHeight} = 2*osmHeight

OSgetTextControlMinWidth :: !OSWindowMetrics -> Int
OSgetTextControlMinWidth {osmHeight} = osmHeight/4

OSgetEditControlMinWidth :: !OSWindowMetrics -> Int
OSgetEditControlMinWidth _ = 0

OSgetPopUpControlMinWidth :: !OSWindowMetrics -> Int
OSgetPopUpControlMinWidth {osmHeight} = 2*osmHeight+osmHeight/2

OSgetRadioControlItemMinWidth :: !OSWindowMetrics -> Int
OSgetRadioControlItemMinWidth {osmHeight} = 2*osmHeight+osmHeight/2

OSgetCheckControlItemMinWidth :: !OSWindowMetrics -> Int
OSgetCheckControlItemMinWidth {osmHeight} = 2*osmHeight+osmHeight/2

OSgetSliderControlMinWidth :: !OSWindowMetrics -> Int
OSgetSliderControlMinWidth _ = 0


/*	Window creation functions.
*/
::	DelayActivationInfo
	=	DelayActivatedWindow	OSWindowPtr				// the window has become active
	|	DelayDeactivatedWindow	OSWindowPtr				// the window has become inactive
	|	DelayActivatedControl	OSWindowPtr OSWindowPtr	// the control (@2) in window (@1) has become active
	|	DelayDeactivatedControl	OSWindowPtr OSWindowPtr	// the control (@2) in window (@1) has become inactive

OScreateDialog :: !Bool !Bool !String !(!Int,!Int) !(!Int,!Int) !OSWindowPtr
				  !(u:s->*(OSWindowPtr,u:s))
				  !(OSWindowPtr->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !OSDInfo !u:s !*OSToolbox
			   -> (![DelayActivationInfo],!OSWindowPtr,!u:s,!*OSToolbox)
OScreateDialog isModal isClosable title pos size behindPtr get_focus create_controls update_controls osdinfo control_info tb
	# (textPtr,tb)	= WinMakeCString title tb
	  createcci		= Rq4Cci CcRqCREATEDIALOG textPtr parentptr (if (behindPtr==OSNoWindowPtr) 0 behindPtr) (toInt isModal)
	# (returncci,(control_info,delay_info),tb)
					= IssueCleanRequest (OScreateDialogCallback get_focus create_controls update_controls)
										createcci
										(control_info,[]) tb
	# tb			= WinReleaseCString textPtr tb
	  wPtr			= case returncci.ccMsg of
	  					CcRETURN1	-> returncci.p1
	  					CcWASQUIT	-> OSNoWindowPtr
	  					_			-> oswindowCreateError 1 "OScreateDialog"
	= (reverse delay_info,wPtr,control_info,tb)
where
	parentptr		= case (getOSDInfoOSInfo osdinfo) of
						Nothing        -> 0
						Just {osFrame} -> osFrame
	
	OScreateDialogCallback :: !(u:s->*(OSWindowPtr,u:s))
							  !(OSWindowPtr->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
							  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
							  !CrossCallInfo !*(u:s,[DelayActivationInfo]) !*OSToolbox
						  -> (!CrossCallInfo,!*(u:s,[DelayActivationInfo]),!*OSToolbox)
	OScreateDialogCallback _ _ _ {ccMsg=CcWmPAINT,p1=hwnd} s tb
		= //trace_n "OScreateDialogCallback CcWmPAINT" 
		  (Return0Cci, s, WinFakePaint hwnd tb)
	OScreateDialogCallback _ _ _ {ccMsg=CcWmACTIVATE,p1=hwnd} (control_info,delay_info) tb
		= //trace_n "OScreateDialogCallback CcWmACTIVATE" 
		  (Return0Cci, (control_info,[DelayActivatedWindow hwnd:delay_info]), tb)
	OScreateDialogCallback _ _ _ {ccMsg=CcWmDEACTIVATE,p1=hwnd} (control_info,delay_info) tb
		= //trace_n "OScreateDialogCallback CcWmDEACTIVATE" 
		  (Return0Cci, (control_info,[DelayDeactivatedWindow hwnd:delay_info]), tb)
	OScreateDialogCallback get_focus create_controls _ {ccMsg=CcWmINITDIALOG,p1=hwnd} (control_info,delay_info) tb
		# (control_info,tb)			= create_controls hwnd control_info tb
		# (defhandle,control_info)	= get_focus control_info
		  (x,y)						= pos
		  (w,h)						= size
		  r5cci						= Return5Cci x y w h (if (defhandle==OSNoWindowPtr) 0 defhandle)
		= (r5cci, (control_info,delay_info), tb)
	OScreateDialogCallback _ _ update_controls {ccMsg=CcWmDRAWCONTROL,p1=hdlog,p2=hctrl,p3=hdc} (control_info,delay_info) tb
		# (control_info,tb)			= update_controls hdlog hctrl hdc control_info tb
		= (Return0Cci, (control_info,delay_info), tb)
	OScreateDialogCallback _ _ _ {ccMsg=CcWmKEYBOARD} s tb
		= //trace_n "OScreateDialogCallback CcWmKEYBOARD"
		  (Return0Cci, s, tb)
	OScreateDialogCallback _ _ _ {ccMsg=CcWmSETFOCUS} s tb
		= //trace_n "OScreateDialogCallback CcWmSETFOCUS"
		  (Return0Cci, s, tb)
	OScreateDialogCallback _ _ _ {ccMsg=CcWmKILLFOCUS} s tb
		= //trace_n "OScreateDialogCallback CcWmKILLFOCUS"
		  (Return0Cci, s, tb)
	OScreateDialogCallback _ _ _ {ccMsg} s tb
		= oswindowFatalError "OScreateDialogCallback" ("unknown message type ("+++toString ccMsg+++")")

OScreateWindow :: !OSWindowMetrics !Bool !ScrollbarInfo !ScrollbarInfo !(!Int,!Int) !(!Int,!Int)
				  !Bool !String !(!Int,!Int) !(!Int,!Int)
				  !(u:s->*(OSWindowPtr,u:s))
				  !(OSWindowPtr->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
				  !OSDInfo !OSWindowPtr !u:s !*OSToolbox
			   -> (![DelayActivationInfo],!OSWindowPtr,!OSWindowPtr,!OSWindowPtr,!OSDInfo,!u:s,!*OSToolbox)
OScreateWindow	wMetrics isResizable hInfo=:{cbiHasScroll=hasHScroll} vInfo=:{cbiHasScroll=hasVScroll} minSize maxSize
				isClosable title pos size
				get_focus
				create_controls
				update_controls
				osdInfo behindPtr control_info tb
	| di==MDI
		# (textPtr,tb)	= WinMakeCString title tb
		  styleFlags	= WS_SYSMENU
		  					bitor WS_OVERLAPPED
		  					bitor (if hasHScroll  WS_HSCROLL    0)
		  					bitor (if hasVScroll  WS_VSCROLL    0)
		  					bitor (if isResizable WS_THICKFRAME 0)
		  				//	bitor WS_CLIPCHILDREN
		  createcci		= Rq6Cci CcRqCREATEMDIDOCWINDOW textPtr osinfo.osClient behindPtr (x<<16+(y<<16)>>16) (w<<16+(h<<16)>>16) styleFlags
		# (returncci,(control_info,delay_info),tb)
						= IssueCleanRequest (OScreateWindowCallback isResizable minSize maxSize create_controls update_controls)
											createcci
											(control_info,[]) tb
		# tb			= WinReleaseCString textPtr tb
		  wPtr			= case returncci.ccMsg of
		  					CcRETURN1	-> returncci.p1
		  					CcWASQUIT	-> OSNoWindowPtr
		  					_			-> oswindowCreateError 1 "OScreateWindow (MDI)"
		# tb			= setScrollRangeAndPos hasHScroll False wMetrics SB_HORZ hInfo.cbiState (0,0) wPtr tb
		# tb			= setScrollRangeAndPos hasVScroll False wMetrics SB_VERT vInfo.cbiState (0,0) wPtr tb
		= (reverse delay_info,wPtr,OSNoWindowPtr,OSNoWindowPtr,osdInfo,control_info,tb)

	| di==SDI
		# (textPtr,tb)	= WinMakeCString title tb		// PA+++
		  styleFlags	= (if hasHScroll WS_HSCROLL 0) bitor (if hasVScroll WS_VSCROLL 0)
		  createcci		= Rq6Cci CcRqCREATESDIDOCWINDOW textPtr osFrame (x<<16+(y<<16)>>16) w h styleFlags
		# (returncci,(control_info,delay_info),tb)
						= IssueCleanRequest (OScreateWindowCallback isResizable minSize maxSize create_controls update_controls)
											createcci
											(control_info,[]) tb
		# tb			= WinReleaseCString textPtr tb	// PA+++
		  clientPtr		= case returncci.ccMsg of
		  					CcRETURN1	-> returncci.p1
		  					CcWASQUIT	-> OSNoWindowPtr
		  					_			-> oswindowCreateError 1 "OScreateWindow (SDI)"
		  osdInfo		= setOSDInfoOSInfo {osinfo & osClient=clientPtr} osdInfo
		# tb			= setScrollRangeAndPos hasHScroll False wMetrics SB_HORZ hInfo.cbiState (0,0) clientPtr tb
		# tb			= setScrollRangeAndPos hasVScroll False wMetrics SB_VERT vInfo.cbiState (0,0) clientPtr tb
	//	# tb			= OSsetWindowTitle osFrame title tb
		= (reverse delay_info,clientPtr,OSNoWindowPtr,OSNoWindowPtr,osdInfo,control_info,tb)
	
	| otherwise
		= oswindowFatalError "OScreateWindow" "unexpected OSDInfo (OSNoInfo) argument"
where
	(x,y)			= pos	// packed into one 32-bit integer
	(w,h)			= size
	di				= getOSDInfoDocumentInterface osdInfo
	osinfo			= fromJust (getOSDInfoOSInfo  osdInfo)
	osFrame			= osinfo.osFrame

OScreateWindowCallback :: !Bool !(!Int,!Int) !(!Int,!Int) 
						  !(OSWindowPtr->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
						  !(OSWindowPtr->OSWindowPtr->OSPictContext->u:s->u:(*OSToolbox->*(u:s,*OSToolbox)))
						  !CrossCallInfo !*(u:s,[DelayActivationInfo]) !*OSToolbox
					  -> (!CrossCallInfo,!*(u:s,[DelayActivationInfo]),!*OSToolbox)
/*	PA: This alternative replaced by WinFakePaint function.
OScreateWindowCallback _ _ _ _ _ {ccMsg=CcWmPAINT,p1=hwnd} s tb
	= //trace "OScreateWindowCallback CcWmPAINT" 
	  (Return0Cci, s, OSdummyWindowUpdate hwnd tb)
where
	OSdummyWindowUpdate :: !OSWindowPtr !*OSToolbox -> *OSToolbox
	OSdummyWindowUpdate wPtr tb
		# (hdc,tb) = WinBeginPaint wPtr tb
		= WinEndPaint wPtr (hdc,tb)
*/
OScreateWindowCallback _ _ _ _ _ {ccMsg=CcWmPAINT,p1=hwnd} s tb
	= //trace "OScreateWindowCallback CcWmPAINT"
	  (Return0Cci, s, WinFakePaint hwnd tb)
OScreateWindowCallback _ _ _ _ _ {ccMsg=CcWmACTIVATE,p1=hwnd} (control_info,delay_info) tb
	= //trace "OScreateWindowCallback CcWmACTIVATE" 
	  (Return0Cci, (control_info,[DelayActivatedWindow hwnd:delay_info]), tb)
OScreateWindowCallback _ _ _ _ _ {ccMsg=CcWmDEACTIVATE,p1=hwnd} (control_info,delay_info) tb
	= //trace "OScreateWindowCallback CcWmDEACTIVATE" 
	  (Return0Cci, (control_info,[DelayDeactivatedWindow hwnd:delay_info]), tb)
OScreateWindowCallback _ _ _ create_controls _ {ccMsg=CcWmCREATE,p1=hwnd} (control_info,deactivates) tb
	# (control_info,tb)			= create_controls hwnd control_info tb
	= (Return0Cci, (control_info,deactivates), tb)
OScreateWindowCallback _ _ _ _ _ {ccMsg=CcWmNEWHTHUMB,p1=hwnd,p2=thumb} s tb
	= //trace "OScreateWindowCallback CcWmNEWHTHUMB" 
	  (Return0Cci, s, tb)
OScreateWindowCallback _ _ _ _ _ {ccMsg=CcWmNEWVTHUMB,p1=hwnd,p2=thumb} s tb
	= //trace "OScreateWindowCallback CcWmNEWVTHUMB" 
	  (Return0Cci, s, tb)
OScreateWindowCallback _ _ _ _ _ {ccMsg=CcWmSIZE,p1=hwnd,p2=width,p3=height} s tb
	= //trace ("OScreateWindowCallback CcWmSIZE "+++toString (width,height)) 
	  (Return0Cci, s, tb)
OScreateWindowCallback _ _ _ _ update_controls {ccMsg=CcWmDRAWCONTROL,p1=hwnd,p2=hctrl,p3=hdc} (control_info,delay_info) tb
	# (control_info,tb)			= update_controls hwnd hctrl hdc control_info tb
	= //trace ("OScreateWindowCallback CcWmDRAWCONTROL "+++toString (hwnd,hctrl,hdc))
	  (Return0Cci, (control_info,delay_info), tb)
OScreateWindowCallback _ _ _ _ _ {ccMsg=CcWmKILLFOCUS} s tb
	= //trace "OScreateWindowCallback CcWmKILLFOCUS" 
	  (Return0Cci, s, tb)
OScreateWindowCallback _ _ _ _ _ {ccMsg=CcWmKEYBOARD,p1=hwnd,p2=hctrl,p3=char,p4=ks,p5=mods} s tb
	= //trace "OScreateWindowCallback CcWmKEYBOARD "+++toString (hwnd,hctrl,char,ks,mods))
	  (Return0Cci, s,tb)
OScreateWindowCallback _ _ _ _ _ {ccMsg} s tb
	= oswindowFatalError "OScreateWindowCallback" ("unknown message type ("+++toString ccMsg+++")")


/*	PA: new function that creates modal dialog and handles events until termination. 
		The Bool result is True iff no error occurred. 
*/
OScreateModalDialog :: !Bool !String !OSDInfo !(Maybe OSWindowPtr) !(u:s -> (OSEvents,u:s)) !((OSEvents,u:s)-> u:s) !(OSEvent -> u:s -> *([Int],u:s))
						!u:s !*OSToolbox
			  -> (!Bool,!u:s,!*OSToolbox)
OScreateModalDialog isClosable title osdinfo currentActiveModal getOSEvents setOSEvents handleOSEvents s tb
	# (textPtr,tb)		= WinMakeCString title tb
	  createcci			= Rq2Cci CcRqCREATEMODALDIALOG textPtr parentptr
	# (returncci,s,tb)	= IssueCleanRequest (OScreateModalDialogCallback getOSEvents setOSEvents handleOSEvents) createcci s tb
	# tb				= WinReleaseCString textPtr tb
	  ok				= case returncci.ccMsg of
	  						CcRETURN1	-> returncci.p1==0
	  						CcWASQUIT	-> True
		  					_			-> oswindowCreateError 1 "OScreateModalDialog"
	= (ok,s,tb)
where
	parentptr			= if (isNothing currentActiveModal)
							(case (getOSDInfoOSInfo osdinfo) of
								Just info -> info.osFrame
								nothing   -> 0
							)
							(fromJust currentActiveModal)
	
	OScreateModalDialogCallback :: !(u:s -> (OSEvents,u:s)) !((OSEvents,u:s)-> u:s) !(OSEvent -> u:s -> *([Int],u:s)) 
									!CrossCallInfo !u:s !*OSToolbox
								-> (!CrossCallInfo,!u:s,!*OSToolbox)
	OScreateModalDialogCallback getOSEvents setOSEvents handleOSEvents osEvent s tb
		# (replyToOS,s)				= handleOSEvents osEvent s
		| not (isEmpty replyToOS)	// information must be returned to OS
			= (setReplyInOSEvent replyToOS,s,tb)
		# (osEvents, s)				= getOSEvents s
		# (noDelayEvents,osEvents)	= OSisEmptyEvents osEvents
		| noDelayEvents
			= (setReplyInOSEvent replyToOS,setOSEvents (osEvents,s),tb)
		| otherwise
			# (osEvent,osEvents)	= OSremoveEvent osEvents
			# s						= setOSEvents (osEvents,s)
			= OScreateModalDialogCallback getOSEvents setOSEvents handleOSEvents osEvent s tb

// Mike //
OScreateGameWindow :: !Bool !(!Int,!Int) !Int !*OSToolbox -> (![DelayActivationInfo],!OSWindowPtr,!*OSToolbox)
OScreateGameWindow fullscreen size bpp tb
	# createcci		= {ccMsg=CcRqCREATEGAMEWINDOW,p1=w,p2=h,p3=bpp,p4=toInt fullscreen,p5=0,p6=0}
	# (returncci,delay_info,tb)
					= IssueCleanRequest OScreateGameWindowCallback createcci [] tb
	  wPtr			= case returncci.ccMsg of
						CcRETURN1   -> returncci.p1
						CcWASQUIT   -> OSNoWindowPtr
						_           -> oswindowCreateError 1 "OScreateGameWindow"
	= (reverse delay_info,wPtr,tb)
	/* ddPtr removed !!!     PA: check if ddPtr is still required!!
	  (wPtr,ddPtr)  = case returncci.ccMsg of
	                    CcRETURN2   -> (returncci.p1,returncci.p2)
	                    CcWASQUIT   -> (OSNoWindowPtr,OSNoWindowPtr)
	                    _           -> oswindowCreateError 1 "OScreateGameWindow"
	= (reverse delay_info,wPtr,ddPtr,tb)
	*/
where
    (w,h)           = size

    OScreateGameWindowCallback :: !CrossCallInfo ![DelayActivationInfo] !*OSToolbox
                              -> (!CrossCallInfo,![DelayActivationInfo],!*OSToolbox)
    OScreateGameWindowCallback {ccMsg=CcWmPAINT,p1=hwnd} s tb
        = //trace "OScreateGameWindowCallback CcWmPAINT"
          (Return0Cci, s, WinFakePaint hwnd tb)//OSdummyWindowUpdate hwnd tb)
/*    where
        OSdummyWindowUpdate :: !OSWindowPtr !*OSToolbox -> *OSToolbox
        OSdummyWindowUpdate wPtr tb
            # (hdc,tb) = WinBeginPaint wPtr tb
            = WinEndPaint wPtr (hdc,tb) */
    OScreateGameWindowCallback {ccMsg=CcWmACTIVATE,p1=hwnd} delay_info tb
        = //trace "OScreateGameWindowCallback CcWmACTIVATE"
          (Return0Cci, [DelayActivatedWindow hwnd:delay_info], tb)
    OScreateGameWindowCallback {ccMsg=CcWmDEACTIVATE,p1=hwnd} delay_info tb
        = //trace "OScreateGameWindowCallback CcWmDEACTIVATE"
          (Return0Cci, [DelayDeactivatedWindow hwnd:delay_info], tb)
    OScreateGameWindowCallback {ccMsg=CcWmCREATE,p1=hwnd} delay_info tb
        = (Return0Cci, delay_info, tb)
    OScreateGameWindowCallback {ccMsg=CcWmSIZE,p1=hwnd,p2=width,p3=height} s tb
        = //trace ("OScreateGameWindowCallback CcWmSize "+++toString (width,height))
          (Return0Cci, s, tb)
    OScreateGameWindowCallback {ccMsg} s tb
        = oswindowFatalError "OScreateGameWindowCallback" ("unknown message type ("+++toString ccMsg+++")")
//

/*	Control creation functions.
*/
oswindowCreateError :: Int String -> .x
oswindowCreateError arity function
	= oswindowFatalError function ("Expected CcRETURN"+++toString arity+++" value.\n")

osIgnoreCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
osIgnoreCallback ccinfo=:{ccMsg=CcWmPAINT,p1=hwnd} tb
	= (Return0Cci,WinFakePaint hwnd tb)//WinEndPaint hwnd (WinBeginPaint hwnd tb))
osIgnoreCallback ccinfo tb 
	= (Return0Cci,tb)

osIgnoreCallback` :: !CrossCallInfo ![DelayActivationInfo] !*OSToolbox -> (!CrossCallInfo,![DelayActivationInfo],!*OSToolbox)
osIgnoreCallback` {ccMsg=CcWmPAINT,p1=hwnd} s tb
	= (Return0Cci,s,WinFakePaint hwnd tb)//WinEndPaint hwnd (WinBeginPaint hwnd tb))
osIgnoreCallback` {ccMsg=CcWmACTIVATE,p1=hwnd} delayinfo tb
	= (Return0Cci,[DelayActivatedWindow hwnd:delayinfo],tb)
osIgnoreCallback` {ccMsg=CcWmDEACTIVATE,p1=hwnd} delayinfo tb
	= (Return0Cci,[DelayDeactivatedWindow hwnd:delayinfo],tb)
osIgnoreCallback` _ s tb
	= (Return0Cci,s,tb)

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

OScreateRadioControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Bool !Bool !*OSToolbox
																						   -> (!OSWindowPtr,!*OSToolbox)
OScreateRadioControl parentWindow parentPos title show able (x,y) (w,h) selected isfirst tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATERADIOBUT parentWindow x y w h (toInt isfirst)
	# (returncci,tb)= IssueCleanRequest2 osIgnoreCallback createcci tb
	  radioPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "OScreateRadioControl"
	# tb			= WinSetWindowTitle radioPtr title tb
	# tb			= WinCheckControl   radioPtr selected tb
	# tb			= WinEnableControl  radioPtr able tb
	# tb			= WinShowControl	radioPtr show tb
	= (radioPtr,tb)

OScreateCheckControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Bool !Bool !*OSToolbox
																						   -> (!OSWindowPtr,!*OSToolbox)
OScreateCheckControl parentWindow parentPos title show able (x,y) (w,h) selected isfirst tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATECHECKBOX parentWindow x y w h (toInt isfirst)
	# (returncci,tb)= IssueCleanRequest2 osIgnoreCallback createcci tb
	  checkPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "OScreateCheckControl"
	# tb			= WinSetWindowTitle checkPtr title tb
	# tb			= WinCheckControl   checkPtr selected tb
	# tb			= WinEnableControl  checkPtr able tb
	# tb			= WinShowControl	checkPtr show tb
	= (checkPtr,tb)

MaxComboboxWidth		:== 65535		// System maximum for width  of combo box
MaxComboboxHeight		:==	65535		// System maximum for height of combo box
MaxComboElementsVisible	:==	15			// If there are <=MaxComboElementsVisible then show all elements
MaxComboElementsScroll	:==	12			// otherwise, show MaxComboElementsScroll elements

OScreateEmptyPopUpControl :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !Int !Bool !*OSToolbox
	-> (!OSWindowPtr,!OSWindowPtr,!*OSToolbox)
OScreateEmptyPopUpControl parentWindow parentPos show able (x,y) (w,h) nrItems isEditable tb
	# (x,y)				= (x-fst parentPos,y-snd parentPos)
	# (screenRect,tb)	= OSscreenrect tb
	# (wMetrics,tb)		= OSDefaultWindowMetrics tb
	  screenSize		= RectSize screenRect
	  height			= wMetrics.osmHeight
	  okNrItems			= if (nrItems<=MaxComboElementsVisible) nrItems MaxComboElementsScroll
	  overall_h			= min screenSize.h (min MaxComboboxHeight (h + (okNrItems+1)*(height+2)))
	  overall_w			= min screenSize.w (min MaxComboboxWidth w)
	  createcci			= Rq6Cci CcRqCREATEPOPUP parentWindow x y overall_w overall_h (toInt isEditable)
	# (returncci,tb)	= IssueCleanRequest2 osIgnoreCallback createcci tb
	  (popUpPtr,editPtr)= case returncci.ccMsg of
							CcRETURN2	-> (returncci.p1, returncci.p2)
							CcWASQUIT	-> (OSNoWindowPtr,OSNoWindowPtr)
							_			-> oswindowCreateError 2 "OScreateEmptyPopUpControl"
	# tb				= WinEnableControl popUpPtr able tb
	# tb				= WinShowControl   popUpPtr show tb
	= (popUpPtr,editPtr,tb)

OScreatePopUpControlItem :: !OSWindowPtr !Int !Bool !String !Bool !*OSToolbox -> (!Int,!*OSToolbox)
OScreatePopUpControlItem parentPopUp pos able title selected tb
	# (textPtr,tb)	= WinMakeCString title tb
	  addcci		= Rq5Cci CcRqADDTOPOPUP parentPopUp textPtr (toInt able) (toInt selected) pos
	# (returncci,tb)= IssueCleanRequest2 osIgnoreCallback addcci tb
	# tb			= WinReleaseCString textPtr tb
	  index			= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> 0
						_			-> oswindowCreateError 1 "OScreatePopUpControlItem"
	= (index,tb)

OScreateSliderControl :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int) !(!Int,!Int,!Int,!Int) !*OSToolbox
																									 -> (!OSWindowPtr,!*OSToolbox)
OScreateSliderControl parentWindow parentPos show able horizontal (x,y) (w,h) (min,thumb,max,thumbSize) tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATESCROLLBAR parentWindow x y w h (toInt horizontal)
	# (returncci,tb)= IssueCleanRequest2 osIgnoreCallback createcci tb
	  sliderPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "OScreateSliderControl"
	# tb			= WinSetScrollRange sliderPtr SB_CTL min max False tb
	# tb			= WinSetScrollPos   sliderPtr SB_CTL thumb (x+w) (y+h) (if horizontal h w) tb
	# tb			= WinEnableControl  sliderPtr able tb
	# tb			= WinShowControl	sliderPtr show tb
	= (sliderPtr,tb)

OScreateTextControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
OScreateTextControl parentWindow parentPos text show (x,y) (w,h) tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq5Cci CcRqCREATESTATICTXT parentWindow x y w h
	# (returncci,tb)= IssueCleanRequest2 osIgnoreCallback createcci tb
	  textPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "OScreateTextControl"
	# tb			= WinSetWindowTitle textPtr text tb
	# tb			= WinShowControl	textPtr show tb
	= (textPtr,tb)

OScreateEditControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
OScreateEditControl parentWindow parentPos text show able isKeySensitive (x,y) (w,h) tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	# (wMetrics,tb)	= OSDefaultWindowMetrics tb
	  nrLines		= (h-wMetrics.osmHeight)/wMetrics.osmHeight		//toInt ((toReal h) / (1.5*(toReal wMetrics.osmHeight)))
	  isMultiLine	= nrLines>1
	  editflags		= (if isMultiLine EDITISMULTILINE 0) + (if isKeySensitive EDITISKEYSENSITIVE 0)
	  createcci		= Rq6Cci CcRqCREATEEDITTXT parentWindow x y w h editflags
	# (returncci,tb)= IssueCleanRequest2 osIgnoreCallback createcci tb
	  editPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "OScreateEditControl"
	# tb			= WinSetWindowTitle editPtr text tb
	# tb			= WinEnableControl	editPtr able tb
	# tb			= WinShowControl	editPtr show tb
	= (editPtr,tb)

OScreateButtonControl :: !OSWindowPtr !(!Int,!Int) !String !Bool !Bool !(!Int,!Int) !(!Int,!Int) !OKorCANCEL !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
OScreateButtonControl parentWindow parentPos title show able (x,y) (w,h) okOrCancel tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATEBUTTON parentWindow x y w h (toInt okOrCancel)
	# (returncci,tb)= IssueCleanRequest2 osIgnoreCallback createcci tb
	  buttonPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "OScreateButtonControl"
	# tb			= WinSetWindowTitle buttonPtr title tb
	# tb			= WinEnableControl  buttonPtr able tb
	# tb			= WinShowControl	buttonPtr show tb
	= (buttonPtr,tb)

OScreateCustomButtonControl :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !OKorCANCEL !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
OScreateCustomButtonControl parentWindow parentPos show able (x,y) (w,h) okOrCancel tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq6Cci CcRqCREATEICONBUT parentWindow x y w h (toInt okOrCancel)
	# (returncci,tb)= IssueCleanRequest2 osIgnoreCallback createcci tb
	  buttonPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "OScreateCustomButtonControl"
	# tb			= WinEnableControl	buttonPtr able tb
	# tb			= WinShowControl	buttonPtr show tb
	= (buttonPtr,tb)

OScreateCustomControl :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSWindowPtr,!*OSToolbox)
OScreateCustomControl parentWindow parentPos show able (x,y) (w,h) tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  createcci		= Rq5Cci CcRqCREATECUSTOM parentWindow x y w h
	# (returncci,tb)= IssueCleanRequest2 osIgnoreCallback createcci tb
	  customPtr		= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "OScreateCustomControl"
	# tb			= WinEnableControl	customPtr able tb
	# tb			= WinShowControl	customPtr show tb
	= (customPtr,tb)

::	ScrollbarInfo
	=	{	cbiHasScroll	:: !Bool				// The scrollbar exists
		,	cbiPos			:: (Int,Int)			// Its position within the parent
		,	cbiSize			:: (Int,Int)			// Its size within the parent
		,	cbiState		:: (Int,Int,Int,Int)	// Its (min,thumb,max,thumbsize) settings
		}

OScreateCompoundControl ::  !OSWindowMetrics !OSWindowPtr !(!Int,!Int) !Bool !Bool !Bool !(!Int,!Int) !(!Int,!Int)
							!ScrollbarInfo
							!ScrollbarInfo
							!*OSToolbox
						 -> (!OSWindowPtr,!OSWindowPtr,!OSWindowPtr,!*OSToolbox)
OScreateCompoundControl wMetrics parentWindow parentPos show able isTransparent (x,y) (w,h)
						hInfo=:{cbiHasScroll=hasHScroll}
						vInfo=:{cbiHasScroll=hasVScroll} tb
	# (x,y)			= (x-fst parentPos,y-snd parentPos)
	  scrollFlags	= (if hasHScroll WS_HSCROLL 0) bitor (if hasVScroll WS_VSCROLL 0)
	  createcci		= Rq6Cci CcRqCREATECOMPOUND parentWindow (x<<16+(y<<16)>>16) w h scrollFlags (toInt isTransparent)
	# (returncci,tb)= IssueCleanRequest2 osIgnoreCallback createcci tb
	  compoundPtr	= case returncci.ccMsg of
						CcRETURN1	-> returncci.p1
						CcWASQUIT	-> OSNoWindowPtr
						_			-> oswindowCreateError 1 "OScreateCompoundControl"
	# tb			= setScrollRangeAndPos hasHScroll False wMetrics SB_HORZ hInfo.cbiState (0,0) compoundPtr tb
	# tb			= setScrollRangeAndPos hasVScroll False wMetrics SB_VERT vInfo.cbiState (0,0) compoundPtr tb
	# tb			= WinSetSelectStateWindow	compoundPtr (hasHScroll,hasVScroll) able False tb
	# tb			= WinShowControl			compoundPtr show tb
	= (compoundPtr,OSNoWindowPtr,OSNoWindowPtr,tb)

setScrollRangeAndPos :: !Bool Bool OSWindowMetrics Int (Int,Int,Int,Int) (Int,Int) OSWindowPtr !*OSToolbox -> *OSToolbox
setScrollRangeAndPos hasScroll redraw wMetrics iBar state maxcoords wPtr tb
	| not hasScroll
		= tb
	# tb	= WinSetScrollRange     wPtr iBar min max   False tb
	# tb	= WinSetScrollPos       wPtr iBar thumb     0 0 0 tb
	| redraw
		= WinSetScrollThumbSize wPtr iBar thumbsize maxx maxy extent tb
	| otherwise
		= WinSetScrollThumbSize wPtr iBar thumbsize 0 0 0 tb
where
	(min,thumb,max,thumbsize)	= state
	(maxx,maxy)					= maxcoords
	horizontal					= iBar==SB_HORZ
	extent						= if horizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth


/*	Window destruction operations.
	PA: OSdestroyWindow checks the process document interface and applies the appropriate destruction operation.
*/
/*	PA: previous implementation of OSdestroyWindow without update handling.
OSdestroyWindow :: !OSDInfo !Bool !Bool !OSWindowPtr !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
OSdestroyWindow (OSMDInfo {osmdFrame,osmdClient}) isModal isWindow wPtr tb
	# (_,delayInfo,tb)	= IssueCleanRequest osDelayCallback destroycci [] tb
	= (reverse delayInfo,tb)
where
	destroycci	= if isWindow (Rq3Cci CcRqDESTROYMDIDOCWINDOW osmdFrame osmdClient wPtr)
				 (if isModal  (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
							  (Rq1Cci CcRqDESTROYWINDOW wPtr))
OSdestroyWindow (OSSDInfo _) isModal isWindow wPtr tb
	# (_,delayInfo,tb)	= IssueCleanRequest osDelayCallback destroycci [] tb//(Rq1Cci CcRqDESTROYWINDOW wPtr) [] tb
	= (reverse delayInfo,tb)
where
	destroycci	= if isModal (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
							 (Rq1Cci CcRqDESTROYWINDOW wPtr)
OSdestroyWindow OSNoInfo isModal isWindow wPtr tb
	| isWindow		/* This condition should never occur (NDI processes have only dialogues). */
		= oswindowFatalError "OSdestroyWindow" "trying to destroy window of NDI process"
	| otherwise
		# (_,delayInfo,tb)= IssueCleanRequest osDelayCallback destroycci [] tb//(Rq1Cci CcRqDESTROYWINDOW wPtr) [] tb
		= (reverse delayInfo,tb)
where
	destroycci	= if isModal (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
							 (Rq1Cci CcRqDESTROYWINDOW wPtr)

osDelayCallback :: !CrossCallInfo ![DelayActivationInfo] !*OSToolbox
			   -> (!CrossCallInfo,![DelayActivationInfo],!*OSToolbox)
osDelayCallback {ccMsg=CcWmPAINT,p1=wPtr} s tb
	= (Return0Cci,s,WinFakePaint wPtr tb)
osDelayCallback {ccMsg=CcWmACTIVATE,p1=wPtr} delayinfo tb
	= (Return0Cci,[DelayActivatedWindow wPtr:delayinfo],tb)
osDelayCallback {ccMsg=CcWmDEACTIVATE,p1=wPtr} delayinfo tb
	= (Return0Cci,[DelayDeactivatedWindow wPtr:delayinfo],tb)
osDelayCallback {ccMsg} s tb
	| expected	= (Return0Cci,s,tb)
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
OSdestroyWindow :: !OSDInfo !Bool !Bool !OSWindowPtr !(OSEvent -> .s -> ([Int],.s)) !.s !*OSToolbox
														  -> (![DelayActivationInfo],.s,!*OSToolbox)
OSdestroyWindow osdInfo isModal isWindow wPtr handleOSEvent state tb
	| di==MDI
		# destroycci				= if isWindow (Rq3Cci CcRqDESTROYMDIDOCWINDOW osFrame osClient wPtr)
									 (if isModal  (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
												  (Rq1Cci CcRqDESTROYWINDOW wPtr))
		# (_,(delayInfo,state),tb)	= IssueCleanRequest (osDelayCallback handleOSEvent) destroycci ([],state) tb
		= (reverse delayInfo,state,tb)
	| di==SDI
		# destroycci				= if isModal (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
												 (Rq1Cci CcRqDESTROYWINDOW wPtr)
		# (_,(delayInfo,state),tb)	= IssueCleanRequest (osDelayCallback handleOSEvent) destroycci ([],state) tb
		= (reverse delayInfo,state,tb)
	// It's a NDI process
	| isWindow		/* This condition should never occur (NDI processes have only dialogues). */
		= oswindowFatalError "OSdestroyWindow" "trying to destroy window of NDI process"
	| otherwise
		# destroycci				= if isModal (Rq1Cci CcRqDESTROYMODALDIALOG wPtr)
												 (Rq1Cci CcRqDESTROYWINDOW wPtr)
		# (_,(delayInfo,state),tb)	= IssueCleanRequest (osDelayCallback handleOSEvent) destroycci ([],state) tb
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
		= (Return0Cci,([DelayActivatedWindow osEvent.p1:delayinfo],s),tb)
	| ccMsg==CcWmDEACTIVATE
		= (Return0Cci,([DelayDeactivatedWindow osEvent.p1:delayinfo],s),tb)
	| toBeSkipped
		= (Return0Cci,(delayinfo,s),tb)
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
	= snd (IssueCleanRequest2 osDestroyControlCallback (Rq1Cci CcRqDESTROYWINDOW wPtr) tb)
where
	osDestroyControlCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
	osDestroyControlCallback info=:{ccMsg} tb
		| ccMsg==CcWmPAINT
			= (Return0Cci,WinFakePaint info.p1 tb)//WinEndPaint info.p1 (WinBeginPaint info.p1 tb))
		| expected
			= (Return0Cci,tb)
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

OSdestroyRadioControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyRadioControl wPtr tb = destroycontrol wPtr tb

OSdestroyCheckControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyCheckControl wPtr tb = destroycontrol wPtr tb

OSdestroyPopUpControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyPopUpControl wPtr tb = destroycontrol wPtr tb

OSdestroySliderControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroySliderControl wPtr tb = destroycontrol wPtr tb

OSdestroyTextControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyTextControl wPtr tb = destroycontrol wPtr tb

OSdestroyEditControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyEditControl wPtr tb = destroycontrol wPtr tb

OSdestroyButtonControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyButtonControl wPtr tb = destroycontrol wPtr tb

OSdestroyCustomButtonControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyCustomButtonControl wPtr tb = destroycontrol wPtr tb

OSdestroyCustomControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyCustomControl wPtr tb = destroycontrol wPtr tb

OSdestroyCompoundControl :: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSdestroyCompoundControl wPtr tb = destroycontrol wPtr tb


/*	Control update operations.
*/
OSupdateRadioControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateRadioControl area parentWindow theControl tb = updatecontrol theControl area tb

OSupdateCheckControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateCheckControl area parentWindow theControl tb = updatecontrol theControl area tb

OSupdatePopUpControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdatePopUpControl area parentWindow theControl tb = updatecontrol theControl area tb

OSupdateSliderControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateSliderControl area parentWindow theControl tb = updatecontrol theControl area tb

OSupdateTextControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateTextControl area parentWindow theControl tb = updatecontrol theControl area tb

OSupdateEditControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateEditControl area parentWindow theControl tb = updatecontrol theControl area tb

OSupdateButtonControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateButtonControl area parentWindow theControl tb = updatecontrol theControl area tb

OSupdateCompoundControl :: !Rect !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSupdateCompoundControl area parentWindow theControl tb = updatecontrol theControl area tb

updatecontrol :: !OSWindowPtr !Rect !*OSToolbox -> *OSToolbox
updatecontrol theControl rect tb = WinUpdateWindowRect theControl (toTuple4 rect) tb


/*	Control clipping operations.
*/
oscliprectrgn :: !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
oscliprectrgn parent_pos=:(parent_x,parent_y) rect (x,y) (w,h) tb
	= osnewrectrgn (IntersectRects area item) tb
where
	area	= subVector (fromTuple parent_pos) rect
	x`		= x-parent_x
	y`		= y-parent_y
	item	= {rleft=x`,rtop=y`,rright=x`+w,rbottom=y`+h}

OSclipRadioControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipRadioControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

OSclipCheckControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipCheckControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

OSclipPopUpControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipPopUpControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

OSclipSliderControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipSliderControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

OSclipTextControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipTextControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

OSclipEditControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipEditControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

OSclipButtonControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipButtonControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

OSclipCustomButtonControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipCustomButtonControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

OSclipCustomControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipCustomControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

OSclipCompoundControl :: !OSWindowPtr !(!Int,!Int) !Rect !(!Int,!Int) !(!Int,!Int) !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
OSclipCompoundControl _ parentPos area itemPos itemSize tb = oscliprectrgn parentPos area itemPos itemSize tb

/*	Window graphics context access operations.
*/
OSgrabWindowPictContext :: !OSWindowPtr !*OSToolbox -> (!OSPictContext,!*OSToolbox)
OSgrabWindowPictContext wPtr tb
	= WinGetDC wPtr tb

OSgrabControlPictContext :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSPictContext,!*OSToolbox)
OSgrabControlPictContext wPtr cPtr tb
	= WinGetDC cPtr tb

OSreleaseWindowPictContext :: !OSWindowPtr !OSPictContext !*OSToolbox -> *OSToolbox
OSreleaseWindowPictContext wPtr hdc tb
	= WinReleaseDC wPtr (hdc,tb)

OSreleaseControlPictContext :: !OSWindowPtr !OSPictContext !*OSToolbox -> *OSToolbox
OSreleaseControlPictContext cPtr hdc tb
	= WinReleaseDC cPtr (hdc,tb)


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

OSscrollbarIsVisible :: !(!Int,!Int) !Int -> Bool
OSscrollbarIsVisible (domainMin,domainMax) viewSize
	= viewSize<domainMax-domainMin

OSscrollbarsAreVisible :: !OSWindowMetrics !Rect !(!Int,!Int) !(!Bool,!Bool) -> (!Bool,!Bool)
OSscrollbarsAreVisible {osmHSliderHeight,osmVSliderWidth} {rleft=xMin,rtop=yMin,rright=xMax,rbottom=yMax} (width,height) (hasHScroll,hasVScroll)
	= visScrollbars (False,False)
					(hasHScroll && (OSscrollbarIsVisible hRange width),hasVScroll && (OSscrollbarIsVisible vRange height))
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
		showH	= if showV2 (hasHScroll && OSscrollbarIsVisible hRange (width -osmVSliderWidth )) showH2
		showV	= if showH2 (hasVScroll && OSscrollbarIsVisible vRange (height-osmHSliderHeight)) showV2

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


OSsetWindowSliderThumb :: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetWindowSliderThumb wMetrics theWindow isHorizontal thumb (maxx,maxy) redraw tb
	= WinSetScrollPos theWindow (if isHorizontal SB_HORZ SB_VERT) thumb maxx maxy extent tb
where
	extent	= if isHorizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth

OSsetWindowSliderThumbSize :: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetWindowSliderThumbSize wMetrics theWindow isHorizontal size (maxx,maxy) redraw tb
	= WinSetScrollThumbSize theWindow (if isHorizontal SB_HORZ SB_VERT) size maxx maxy extent tb
where
	extent	= if isHorizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth

OSsetWindowSlider :: !OSWindowMetrics !OSWindowPtr !Bool !(!Int,!Int,!Int,!Int) !(!Int,!Int) !*OSToolbox -> *OSToolbox
OSsetWindowSlider wMetrics theWindow isHorizontal state maxcoords tb
	= setScrollRangeAndPos True True wMetrics (if isHorizontal SB_HORZ SB_VERT) state maxcoords theWindow tb

OSinvalidateWindow :: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSinvalidateWindow theWindow tb
	= WinInvalidateWindow theWindow tb

OSinvalidateWindowRect :: !OSWindowPtr !Rect !*OSToolbox -> *OSToolbox
OSinvalidateWindowRect theWindow rect tb
	= WinInvalidateRect theWindow (toTuple4 rect) tb

OSvalidateWindowRect :: !OSWindowPtr !Rect !*OSToolbox -> *OSToolbox
OSvalidateWindowRect theWindow rect tb
	= WinValidateRect theWindow (toTuple4 rect) tb

OSvalidateWindowRgn :: !OSWindowPtr !OSRgnHandle !*OSToolbox -> *OSToolbox
OSvalidateWindowRgn theWindow rgn tb
	= WinValidateRgn theWindow rgn tb

OSdisableWindow :: !OSWindowPtr !(!Bool,!Bool) !Bool !*OSToolbox -> *OSToolbox
OSdisableWindow theWindow scrollInfo modalContext tb
	= WinSetSelectStateWindow theWindow scrollInfo False modalContext tb

OSenableWindow :: !OSWindowPtr !(!Bool,!Bool) !Bool !*OSToolbox -> *OSToolbox
OSenableWindow theWindow scrollInfo modalContext tb
	= WinSetSelectStateWindow theWindow scrollInfo True modalContext tb

OSactivateWindow :: !OSDInfo !OSWindowPtr !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !.s !*OSToolbox
	-> (![DelayActivationInfo],!.s,!*OSToolbox)
OSactivateWindow osdInfo thisWindow handleOSEvent state tb
	# (_,(delayinfo,state),tb)	= IssueCleanRequest (osCallback handleOSEvent) (Rq3Cci CcRqACTIVATEWINDOW (toInt isMDI) clientPtr thisWindow) ([],state) tb
	= (reverse delayinfo,state,tb)
where
	isMDI				= getOSDInfoDocumentInterface osdInfo==MDI
	clientPtr			= case (getOSDInfoOSInfo osdInfo) of
							Just {osClient} -> osClient
							nothing         -> oswindowFatalError "OSactivateWindow" "illegal DocumentInterface context"
	
/*	osCallback delays activate and deactivate events.
	All other events are passed to the callback function.
	PA: is now identical to OSstackWindow!!
*/	osCallback :: !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !CrossCallInfo !(![DelayActivationInfo],!.s) !*OSToolbox
		-> (!CrossCallInfo,!(![DelayActivationInfo],!.s),!*OSToolbox)
	osCallback handleOSEvent {ccMsg=CcWmACTIVATE,p1=hwnd} (delayinfo,s) tb
		= (Return0Cci,([DelayActivatedWindow hwnd:delayinfo],s),tb)
	osCallback handleOSEvent {ccMsg=CcWmDEACTIVATE,p1=hwnd} (delayinfo,s) tb
		= (Return0Cci,([DelayDeactivatedWindow hwnd:delayinfo],s),tb)
	osCallback handleOSEvent osEvent (delayinfo,s) tb
		# (s,tb)	= handleOSEvent osEvent (s,tb)
		= (Return0Cci,(delayinfo,s),tb)


OSactivateControl :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
OSactivateControl parentWindow controlPtr tb
	# (_,delayinfo,tb)	= IssueCleanRequest osIgnoreCallback` (Rq1Cci CcRqACTIVATECONTROL controlPtr) [] tb
	= (reverse delayinfo,tb)
where
	osIgnoreCallback` :: !CrossCallInfo ![DelayActivationInfo] !*OSToolbox -> (!CrossCallInfo,![DelayActivationInfo],!*OSToolbox)
	osIgnoreCallback` {ccMsg=CcWmPAINT,p1=hwnd} s tb
		= (Return0Cci,s,WinFakePaint hwnd tb)//WinEndPaint hwnd (WinBeginPaint hwnd tb))
	osIgnoreCallback` {ccMsg=CcWmKILLFOCUS,p1=hwnd,p2=cptr} delayinfo tb
		= (Return0Cci,[DelayDeactivatedControl hwnd cptr:delayinfo],tb)
	osIgnoreCallback` {ccMsg=CcWmSETFOCUS,p1=hwnd,p2=cptr} delayinfo tb
		= (Return0Cci,[DelayActivatedControl hwnd cptr:delayinfo],tb)
	osIgnoreCallback` _ s tb
		= (Return0Cci,s,tb)

/*	PA: previous implementation of OSstackWindow ignored window updates and resizes. This is fixed below.
OSstackWindow :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> *OSToolbox
OSstackWindow thisWindow behindWindow tb
	= WinRestackWindow thisWindow behindWindow tb

WinRestackWindow :: !HWND !HWND !*OSToolbox -> *OSToolbox
WinRestackWindow theWindow behindWindow tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinRestackWindow") (Rq2Cci CcRqRESTACKWINDOW theWindow behindWindow) tb)
*/

OSstackWindow :: !OSWindowPtr !OSWindowPtr !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !.s !*OSToolbox
	-> (![DelayActivationInfo],!.s,!*OSToolbox)
OSstackWindow thisWindow behindWindow handleOSEvent state tb
	# (_,(delayinfo,state),tb)	= IssueCleanRequest (osCallback handleOSEvent) (Rq2Cci CcRqRESTACKWINDOW thisWindow behindWindow) ([],state) tb
	= (reverse delayinfo,state,tb)
where
/*	osCallback delays activate and deactivate events.
	All other events are passed to the callback function. 
	PA: is now identical to OSactivateWindow!!
*/	osCallback :: !(OSEvent->(.s,*OSToolbox)->(.s,*OSToolbox)) !CrossCallInfo !(![DelayActivationInfo],!.s) !*OSToolbox
		-> (!CrossCallInfo,!(![DelayActivationInfo],!.s),!*OSToolbox)
	osCallback handleOSEvent {ccMsg=CcWmACTIVATE,p1=hwnd} (delayinfo,s) tb
		= (Return0Cci,([DelayActivatedWindow hwnd:delayinfo],s),tb)
	osCallback handleOSEvent {ccMsg=CcWmDEACTIVATE,p1=hwnd} (delayinfo,s) tb
		= (Return0Cci,([DelayDeactivatedWindow hwnd:delayinfo],s),tb)
	osCallback handleOSEvent osEvent (delayinfo,s) tb
		# (s,tb)	= handleOSEvent osEvent (s,tb)
		= (Return0Cci,(delayinfo,s),tb)

OShideWindow :: !OSWindowPtr !Bool !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
OShideWindow wPtr activate tb
	# (_,delayinfo,tb)	= IssueCleanRequest osIgnoreCallback` (Rq3Cci CcRqSHOWWINDOW wPtr (toInt False) (toInt activate)) [] tb
	= (reverse delayinfo,tb)

OSshowWindow :: !OSWindowPtr !Bool !*OSToolbox -> (![DelayActivationInfo],!*OSToolbox)
OSshowWindow wPtr activate tb
	# (_,delayinfo,tb)	= IssueCleanRequest osIgnoreCallback` (Rq3Cci CcRqSHOWWINDOW wPtr (toInt True) (toInt activate)) [] tb
	= (reverse delayinfo,tb)

OSsetWindowCursor :: !OSWindowPtr !Int !*OSToolbox -> *OSToolbox
OSsetWindowCursor wPtr cursorCode tb
	= WinSetWindowCursor wPtr cursorCode tb

OSgetWindowPos :: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetWindowPos wPtr tb
	= WinGetWindowPos wPtr tb

OSgetWindowViewFrameSize :: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetWindowViewFrameSize wPtr tb
	= WinGetClientSize wPtr tb

OSgetWindowSize :: !OSWindowPtr !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
OSgetWindowSize wPtr tb
	= WinGetWindowSize wPtr tb

OSsetWindowPos :: !OSWindowPtr !(!Int,!Int) !Bool !Bool !*OSToolbox -> *OSToolbox
OSsetWindowPos wPtr pos update inclScrollbars tb
	= WinSetWindowPos wPtr pos update inclScrollbars tb

OSsetWindowViewFrameSize :: !OSWindowPtr !(!Int,!Int) !*OSToolbox -> *OSToolbox
OSsetWindowViewFrameSize wPtr size tb
	= WinSetClientSize wPtr size tb

OSsetWindowSize	:: !OSWindowPtr !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetWindowSize wPtr size update tb
	= WinSetWindowSize wPtr size update tb

OSsetWindowTitle :: !OSWindowPtr !String !*OSToolbox -> *OSToolbox
OSsetWindowTitle wPtr title tb
	= WinSetWindowTitle wPtr title tb


/*	Control access operations.
*/
//	On compound controls:

OSinvalidateCompound :: !OSWindowPtr !*OSToolbox -> *OSToolbox
OSinvalidateCompound compoundPtr tb
	= WinInvalidateWindow compoundPtr tb

OSinvalidateCompoundRect :: !OSWindowPtr !Rect !*OSToolbox -> *OSToolbox
OSinvalidateCompoundRect compoundPtr rect tb
	= WinInvalidateRect compoundPtr (toTuple4 rect) tb

OSsetCompoundSliderThumb :: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetCompoundSliderThumb wMetrics compoundPtr isHorizontal thumb (maxx,maxy) redraw tb
	= WinSetScrollPos compoundPtr (if isHorizontal SB_HORZ SB_VERT) thumb maxx` maxy` extent tb
where
	(maxx`,maxy`,extent)	= if redraw (maxx,maxy,if isHorizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth) (0,0,0)

OSsetCompoundSliderThumbSize :: !OSWindowMetrics !OSWindowPtr !Bool !Int !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetCompoundSliderThumbSize wMetrics compoundPtr isHorizontal size (maxx,maxy) redraw tb
	= WinSetScrollThumbSize compoundPtr (if isHorizontal SB_HORZ SB_VERT) size maxx` maxy` extent tb
where
	(maxx`,maxy`,extent)	= if redraw (maxx,maxy,if isHorizontal wMetrics.osmHSliderHeight wMetrics.osmVSliderWidth) (0,0,0)

OSsetCompoundSlider :: !OSWindowMetrics !OSWindowPtr !Bool !(!Int,!Int,!Int,!Int) !(!Int,!Int) !*OSToolbox -> *OSToolbox
OSsetCompoundSlider wMetrics compoundPtr isHorizontal state maxcoords tb
	= setScrollRangeAndPos True True wMetrics (if isHorizontal SB_HORZ SB_VERT) state maxcoords compoundPtr tb

OSsetCompoundSelect :: !OSWindowPtr !OSWindowPtr !Rect !(!Bool,!Bool) !Bool !*OSToolbox -> *OSToolbox
OSsetCompoundSelect _ compoundPtr _ scrollInfo select tb
	= WinSetSelectStateWindow compoundPtr scrollInfo select False tb
//	= WinEnableControl compoundPtr scrollInfo select tb

OSsetCompoundShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetCompoundShow _ compoundPtr _ show tb
	= WinShowControl compoundPtr show tb

OSsetCompoundPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetCompoundPos _ (parent_x,parent_y) compoundPtr (x,y) _ update tb
	= WinSetWindowPos compoundPtr (x-parent_x,y-parent_y) update True tb

OSsetCompoundSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetCompoundSize _ _ compoundPtr _ size update tb
	= WinSetWindowSize compoundPtr size update tb

OSCompoundMovesControls :== True


//	On slider controls:

OSsetSliderThumb :: !OSWindowPtr !OSWindowPtr !Rect !Bool !(!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
OSsetSliderThumb _ cPtr _ redraw (min,thumb,max) tb
	= WinSetScrollPos cPtr SB_CTL thumb 0 0 0 tb//redraw tb

OSsetSliderControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetSliderControlSelect _ cPtr _ select tb
	= WinEnableControl cPtr select tb

OSsetSliderControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetSliderControlShow _ cPtr _ show tb
	= WinShowControl cPtr show tb

OSsetSliderControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetSliderControlPos _ (parent_x,parent_y) sliderPtr (x,y) _ update tb
	= WinSetWindowPos sliderPtr (x-parent_x,y-parent_y) update False tb

OSsetSliderControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetSliderControlSize _ _ sliderPtr _ size update tb
	= WinSetWindowSize sliderPtr size update tb


//	On radio controls:

OSsetRadioControl :: !OSWindowPtr !OSWindowPtr !OSWindowPtr !Rect !*OSToolbox -> *OSToolbox
OSsetRadioControl _ current new _ tb
	= WinCheckControl new True (WinCheckControl current False tb)

OSsetRadioControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetRadioControlSelect _ cPtr _ select tb
	= WinEnableControl cPtr select tb

OSsetRadioControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetRadioControlShow _ cPtr _ show tb
	= WinShowControl cPtr show tb

OSsetRadioControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetRadioControlPos _ (parent_x,parent_y) radioPtr (x,y) _ update tb
	= WinSetWindowPos radioPtr (x-parent_x,y-parent_y) update False tb

OSsetRadioControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetRadioControlSize _ _ radioPtr _ size update tb
	= WinSetWindowSize radioPtr size update tb


//	On check controls:

OSsetCheckControl :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetCheckControl _ cPtr _ check tb
	= WinCheckControl cPtr check tb

OSsetCheckControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetCheckControlSelect _ cPtr _ select tb
	= WinEnableControl cPtr select tb

OSsetCheckControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetCheckControlShow _ cPtr _ show tb
	= WinShowControl cPtr show tb

OSsetCheckControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetCheckControlPos _ (parent_x,parent_y) checkPtr (x,y) _ update tb
	= WinSetWindowPos checkPtr (x-parent_x,y-parent_y) update False tb

OSsetCheckControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetCheckControlSize _ _ checkPtr _ size update tb
	= WinSetWindowSize checkPtr size update tb


//	On pop up controls:

OSsetPopUpControl :: !OSWindowPtr !OSWindowPtr !Rect !Rect !Int !Int !String !Bool !*OSToolbox -> *OSToolbox
OSsetPopUpControl _ pPtr _ _ _ new _ _ tb
	= WinSelectPopupItem pPtr (new-1) tb

OSsetPopUpControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetPopUpControlSelect _ pPtr _ select tb
	= WinEnableControl pPtr select tb

OSsetPopUpControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetPopUpControlShow _ pPtr _ show tb
	= WinShowControl pPtr show tb

OSsetPopUpControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetPopUpControlPos _ (parent_x,parent_y) popupPtr (x,y) _ update tb
	= WinSetWindowPos popupPtr (x-parent_x,y-parent_y) update False tb

OSsetPopUpControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetPopUpControlSize _ _ popupPtr _ size update tb
	= WinSetWindowSize popupPtr size update tb


//	On edit controls:

OSsetEditControlText :: !OSWindowPtr !OSWindowPtr !Rect !Rect !Bool !String !*OSToolbox -> *OSToolbox
OSsetEditControlText _ ePtr _ _ _ text tb
	= WinSetWindowTitle ePtr text tb

OSgetEditControlText :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!String,!*OSToolbox) 
OSgetEditControlText _ ePtr tb
	= WinGetWindowText ePtr tb

OSsetEditControlCursor :: !OSWindowPtr !OSWindowPtr !Rect !Rect !Int !*OSToolbox -> *OSToolbox
OSsetEditControlCursor _ ePtr _ _ pos tb
	= WinSetEditSelection ePtr pos (pos+1) tb

OSsetEditControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetEditControlSelect _ ePtr _ select tb
	= WinEnableControl ePtr select tb

OSsetEditControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetEditControlShow _ ePtr _ show tb
	= WinShowControl ePtr show tb

OSsetEditControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetEditControlPos _ (parent_x,parent_y) editPtr (x,y) _ update tb
	= WinSetWindowPos editPtr (x-parent_x,y-parent_y) update False tb

OSsetEditControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetEditControlSize _ _ editPtr _ size update tb
	= WinSetWindowSize editPtr size update tb


//	On text controls:

OSsetTextControlText :: !OSWindowPtr !OSWindowPtr !Rect !Rect !Bool !String !*OSToolbox -> *OSToolbox
OSsetTextControlText _ tPtr _ _ _ text tb
	= WinSetWindowTitle tPtr text tb

OSsetTextControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetTextControlSelect _ tPtr _ select tb
	= WinEnableControl tPtr select tb

OSsetTextControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetTextControlShow _ tPtr _ show tb
	= WinShowControl tPtr show tb

OSsetTextControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetTextControlPos _ (parent_x,parent_y) textPtr (x,y) _ update tb
	= WinSetWindowPos textPtr (x-parent_x,y-parent_y) update False tb

OSsetTextControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetTextControlSize _ _ textPtr _ size update tb
	= WinSetWindowSize textPtr size update tb


//	On button controls:

OSsetButtonControlText :: !OSWindowPtr !OSWindowPtr !Rect !String !*OSToolbox -> *OSToolbox
OSsetButtonControlText _ bPtr _ text tb
	= WinSetWindowTitle bPtr text tb

OSsetButtonControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetButtonControlSelect _ bPtr _ select tb
	= WinEnableControl bPtr select tb

OSsetButtonControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetButtonControlShow _ bPtr _ show tb
	= WinShowControl bPtr show tb

OSsetButtonControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetButtonControlPos _ (parent_x,parent_y) buttonPtr (x,y) _ update tb
	= WinSetWindowPos buttonPtr (x-parent_x,y-parent_y) update False tb

OSsetButtonControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetButtonControlSize _ _ buttonPtr _ size update tb
	= WinSetWindowSize buttonPtr size update tb


//	On custom button controls:

OSsetCustomButtonControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetCustomButtonControlSelect _ cPtr _ select tb
	= WinEnableControl cPtr select tb

OSsetCustomButtonControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetCustomButtonControlShow _ cPtr _ show tb
	= WinShowControl cPtr show tb

OSsetCustomButtonControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetCustomButtonControlPos _ (parent_x,parent_y) cPtr (x,y) _ update tb
	= WinSetWindowPos cPtr (x-parent_x,y-parent_y) update False tb

OSsetCustomButtonControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetCustomButtonControlSize _ _ cPtr _ size update tb
	= WinSetWindowSize cPtr size update tb


//	On custom controls:

OSsetCustomControlSelect :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetCustomControlSelect _ cPtr _ select tb
	= WinEnableControl cPtr select tb

OSsetCustomControlShow :: !OSWindowPtr !OSWindowPtr !Rect !Bool !*OSToolbox -> *OSToolbox
OSsetCustomControlShow _ cPtr _ show tb
	= WinShowControl cPtr show tb

OSsetCustomControlPos :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetCustomControlPos _ (parent_x,parent_y) customPtr (x,y) _ update tb
	= WinSetWindowPos customPtr (x-parent_x,y-parent_y) update False tb

OSsetCustomControlSize :: !OSWindowPtr !(!Int,!Int) !OSWindowPtr !(!Int,!Int) !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
OSsetCustomControlSize _ _ customPtr _ size update tb
	= WinSetWindowSize customPtr size update tb
