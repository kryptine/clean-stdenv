implementation module windowCrossCall_12


import	StdMisc, StdTuple
import	clCrossCall_12
from	ostypes			import HWND
from	clCCall_12		import WinMakeCString, WinGetCStringAndFree, WinReleaseCString, CSTR
from	pictCCall_12	import HDC
from	rgnCCall_12		import HRGN


//	Cursor shape constants:
CURSHIDDEN			:== 6
CURSARROW			:== 5
CURSFATCROSS		:== 4
CURSCROSS			:== 3
CURSIBEAM			:== 2
CURSBUSY			:== 1

//	Constants for handling scrollbars.
SB_HORZ				:== 0
SB_VERT				:== 1
SB_CTL				:== 2
SB_BOTH				:== 3

SB_LINEUP			:== 0
SB_LINELEFT			:== 0
SB_LINEDOWN			:== 1
SB_LINERIGHT		:== 1
SB_PAGEUP			:== 2
SB_PAGELEFT			:== 2
SB_PAGEDOWN			:== 3
SB_PAGERIGHT		:== 3
SB_THUMBPOSITION	:== 4
SB_THUMBTRACK		:== 5
SB_TOP				:== 6
SB_LEFT				:== 6
SB_BOTTOM			:== 7
SB_RIGHT			:== 7
SB_ENDSCROLL		:== 8

//	PA: constants for handling window styles.
WS_OVERLAPPED		:== 0x00000000
WS_POPUP			:== 0x80000000
WS_CHILD			:== 0x40000000
WS_MINIMIZE			:== 0x20000000
WS_VISIBLE			:== 0x10000000
WS_DISABLED			:== 0x08000000
WS_CLIPSIBLINGS		:== 0x04000000
WS_CLIPCHILDREN		:== 0x02000000
WS_MAXIMIZE			:== 0x01000000
WS_CAPTION			:== 0x00C00000		/* WS_BORDER | WS_DLGFRAME  */
WS_BORDER			:== 0x00800000
WS_DLGFRAME			:== 0x00400000
WS_VSCROLL			:== 0x00200000
WS_HSCROLL			:== 0x00100000
WS_SYSMENU			:== 0x00080000
WS_THICKFRAME		:== 0x00040000
WS_GROUP			:== 0x00020000
WS_TABSTOP			:== 0x00010000

WS_MINIMIZEBOX		:== 0x00020000
WS_MAXIMIZEBOX		:== 0x00010000

WS_TILED			:== WS_OVERLAPPED
WS_ICONIC			:== WS_MINIMIZE
WS_SIZEBOX			:== WS_THICKFRAME
//	PA: end of addition.

//	PA: constants for stacking windows.
HWND_TOP			:==	0
HWND_BOTTOM			:==	1
HWND_TOPMOST		:== -1
HWND_NOTOPMOST		:==	-2
//	PA: end of addition.

//	PA: flag values for passing information about edit controls from Clean to OS.
EDITISMULTILINE		:==	1			/* PA: flag value: edit control is multi-line. */
EDITISKEYSENSITIVE	:==	2			/* PA: flag value: edit control sends keyboard events to Clean. */
//	PA: end of addition.

//	PA: values for telling Windows if a (custom)button control is OK, CANCEL, or normal. 
ISNORMALBUTTON		:==	0			/* The button is a normal button.   */
ISOKBUTTON			:==	1			/* The button is the OK button.     */
ISCANCELBUTTON		:==	2			/* The button is the CANCEL button. */
//	PA: end of addition


WinSetWindowCursor :: !HWND !Int !*OSToolbox -> *OSToolbox
WinSetWindowCursor hwnd cursorcode tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinSetWindowCursor") (Rq2Cci CcRqCHANGEWINDOWCURSOR hwnd cursorcode) tb)

WinObscureCursor :: !*OSToolbox -> *OSToolbox
WinObscureCursor tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinObscureCursor") (Rq0Cci CcRqOBSCURECURSOR) tb)

WinSetWindowTitle :: !HWND !String !*OSToolbox -> *OSToolbox
WinSetWindowTitle hwnd title tb
	# (textptr,tb)	= WinMakeCString title tb
	# (_,tb)		= IssueCleanRequest2 (ErrorCallback2 "SetWindowTitle") (Rq2Cci CcRqSETWINDOWTITLE hwnd textptr) tb
	= WinReleaseCString textptr tb

WinGetWindowText :: !HWND !*OSToolbox -> (!String, !*OSToolbox)
WinGetWindowText hwnd tb
	# (rcci,tb)	= IssueCleanRequest2 (ErrorCallback2 "WinGetWindowText") (Rq1Cci CcRqGETWINDOWTEXT hwnd) tb
	# (text,tb)	= case rcci.ccMsg of
					CcRETURN1	-> WinGetCStringAndFree rcci.p1 tb
					CcWASQUIT	-> ("",tb)
					other		-> abort "[WinGetWindowText] expected CcRETURN1 value."
	= (text,tb)

WinInvalidateWindow :: !HWND !*OSToolbox -> *OSToolbox
WinInvalidateWindow hwnd tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinInvalidateWindow") (Rq1Cci CcRqINVALIDATEWINDOW hwnd) tb)

WinInvalidateRect :: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
WinInvalidateRect hwnd (left,top, right,bottom) tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "InvalidateRect") (Rq5Cci CcRqINVALIDATERECT hwnd left top right bottom) tb)

WinValidateRect :: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
WinValidateRect hwnd (left,top, right,bottom) tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "ValidateRect") (Rq5Cci CcRqVALIDATERECT hwnd left top right bottom) tb)

WinValidateRgn :: !HWND !HRGN !*OSToolbox -> *OSToolbox
WinValidateRgn hwnd rgn tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "ValidateRgn") (Rq2Cci CcRqVALIDATERGN hwnd rgn) tb)

WinUpdateWindowRect :: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
WinUpdateWindowRect hwnd (left,top,right,bottom) tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "UpdateWindowRect") (Rq5Cci CcRqUPDATEWINDOWRECT hwnd left top right bottom) tb)

WinSetSelectStateWindow :: !HWND !(!Bool,!Bool) !Bool !Bool !*OSToolbox -> *OSToolbox
WinSetSelectStateWindow hwnd (hasHScroll,hasVScroll) toAble modalContext tb
	# selectCci	= Rq5Cci CcRqSETSELECTWINDOW hwnd (toInt hasHScroll) (toInt hasVScroll) (toInt toAble) (toInt modalContext)
	= snd (IssueCleanRequest2 (ErrorCallback2 "SetSelectStateWindow") selectCci tb)

WinBeginPaint :: !HWND !*OSToolbox -> (!HDC,!*OSToolbox) 
WinBeginPaint hwnd tb
	# (rcci,tb)	= IssueCleanRequest2 (ErrorCallback2 "BeginPaint") (Rq1Cci CcRqBEGINPAINT hwnd) tb
	  hdc		= case rcci.ccMsg of
					CcRETURN1	-> rcci.p1
					CcWASQUIT	-> 0 
					other		-> abort "[WinBeginPaint] expected CcRETURN1 value."
	= (hdc,tb)

WinEndPaint :: !HWND !(!HDC, !*OSToolbox) -> *OSToolbox
WinEndPaint hwnd (hdc,tb)
	= snd (IssueCleanRequest2 (ErrorCallback2 "EndPaint") (Rq2Cci CcRqENDPAINT hwnd hdc) tb)

WinGetDC :: !HWND !*OSToolbox -> (!HDC,!*OSToolbox)
WinGetDC _ _
	= code
	{
		.inline WinGetDC
			ccall WinGetDC "II-II"
		.end
	}

WinReleaseDC :: !HWND !(!HDC,!*OSToolbox) -> *OSToolbox
WinReleaseDC hwnd (hdc,tb)
	= code
	{
		.inline WinReleaseDC
			ccall WinReleaseDC "III-I"
		.end
	}

WinGetClientSize :: !HWND !*OSToolbox -> (!(!Int,!Int), !*OSToolbox)
WinGetClientSize hwnd tb
	# (rcci,tb)	= IssueCleanRequest2 (ErrorCallback2 "WinGetClientSize") (Rq1Cci CcRqGETCLIENTSIZE hwnd) tb
	  size		= case rcci.ccMsg of
					CcRETURN2	-> (rcci.p1,rcci.p2)
					CcWASQUIT	-> (0,0) 
					other		-> abort "[WinGetClientSize] expected CcRETURN2 value."
	= (size,tb)

WinGetWindowSize :: !HWND !*OSToolbox -> (!(!Int,!Int), !*OSToolbox)
WinGetWindowSize hwnd tb
	# (rcci,tb)	= IssueCleanRequest2 (ErrorCallback2 "WinGetWindowSize") (Rq1Cci CcRqGETWINDOWSIZE hwnd) tb
	  size		= case rcci.ccMsg of
	  				CcRETURN2	-> (rcci.p1,rcci.p2)
	  				CcWASQUIT	-> (0,0)
	  				other		-> abort "[WinGetWindowSize] expected CcRETURN2 value."
	= (size,tb)

WinSetClientSize :: !HWND !(!Int,!Int) !*OSToolbox -> *OSToolbox
WinSetClientSize hwnd (w,h) tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinSetClientSize") (Rq3Cci CcRqSETCLIENTSIZE hwnd w h) tb)

WinSetWindowSize :: !HWND !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
WinSetWindowSize hwnd (w,h) update tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinSetWindowSize") (Rq4Cci CcRqSETWINDOWSIZE hwnd w h (toInt update)) tb)

WinGetWindowPos :: !HWND !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
WinGetWindowPos hwnd tb
	# (rcci,tb)	= IssueCleanRequest2 (ErrorCallback2 "WinGetWindowPos") (Rq1Cci CcRqGETWINDOWPOS hwnd) tb
	  pos		= case rcci.ccMsg of
					CcRETURN2	-> (rcci.p1,rcci.p2)
					CcWASQUIT	-> (0,0) 
					other		-> abort "[WinGetWindowPos] expected CcRETURN2 value."
	= (pos,tb)

WinSetWindowPos :: !HWND !(!Int,!Int) !Bool !Bool !*OSToolbox -> *OSToolbox
WinSetWindowPos hwnd (x,y) update inclScrollbars tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinSetWindowPos") (Rq5Cci CcRqSETWINDOWPOS hwnd x y (toInt update) (toInt inclScrollbars)) tb)

WinSetScrollRange :: !HWND !Int !Int !Int !Bool !*OSToolbox -> *OSToolbox
WinSetScrollRange scrollHWND iBar min max redraw tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinSetScrollRange") (Rq5Cci CcRqSETSCROLLRANGE scrollHWND iBar min max (toInt redraw)) tb)
	
WinSetScrollPos :: !HWND !Int !Int !Int !Int !Int !*OSToolbox -> *OSToolbox
WinSetScrollPos scrollHWND iBar thumb maxx maxy extent tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinSetScrollPos") (Rq6Cci CcRqSETSCROLLPOS scrollHWND iBar thumb maxx maxy extent) tb)

WinSetScrollThumbSize :: !HWND !Int !Int !Int !Int !Int !*OSToolbox -> *OSToolbox
WinSetScrollThumbSize scrollHWND iBar size maxx maxy extent tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinSetScrollThumbSize") (Rq6Cci CcRqSETSCROLLSIZE scrollHWND iBar size maxx maxy extent) tb)

WinSetEditSelection :: !HWND !Int !Int !*OSToolbox -> *OSToolbox
WinSetEditSelection editHWND first last tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinSetEditSelection") (Rq3Cci CcRqSETEDITSELECTION editHWND first last) tb)

WinShowControl :: !HWND !Bool !*OSToolbox -> *OSToolbox
WinShowControl hwnd bool tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinShowControl") (Rq2Cci CcRqSHOWCONTROL hwnd (toInt bool)) tb)

WinEnableControl :: !HWND !Bool !*OSToolbox -> *OSToolbox
WinEnableControl hwnd bool tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinEnableControl") (Rq2Cci CcRqENABLECONTROL hwnd (toInt bool)) tb)

WinEnablePopupItem :: !HWND !Int !Bool !*OSToolbox -> *OSToolbox
WinEnablePopupItem hwnd pos bool tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinEnablePopupItem") (Rq3Cci CcRqENABLEPOPUPITEM hwnd pos (toInt bool)) tb)

WinCheckControl :: !HWND !Bool !*OSToolbox -> *OSToolbox
WinCheckControl hwnd bool tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinCheckControl") (Rq2Cci CcRqSETITEMCHECK hwnd (toInt bool)) tb)

WinSelectPopupItem :: !HWND !Int !*OSToolbox -> *OSToolbox
WinSelectPopupItem hwnd pos tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "WinSelectPopupItem") (Rq2Cci CcRqSELECTPOPUPITEM hwnd pos) tb)
