implementation module clipboardCrossCall_12


import	StdClass, StdInt, StdMisc
import	clCrossCall_12
from	clCCall_12	import WinMakeCString, WinGetCStringAndFree, WinReleaseCString, CSTR


//	PA: Predefined Clipboard Formats.
CF_TEXT             :==	1
CF_BITMAP           :==	2
CF_METAFILEPICT     :==	3
CF_SYLK             :==	4
CF_DIF              :==	5
CF_TIFF             :==	6
CF_OEMTEXT          :==	7
CF_DIB              :==	8
CF_PALETTE          :==	9
CF_PENDATA          :==	10
CF_RIFF             :==	11
CF_WAVE             :==	12
CF_UNICODETEXT      :==	13
CF_ENHMETAFILE      :==	14
//	PA: end of addition.


WinGetClipboardText :: !*OSToolbox -> (!String,!*OSToolbox)
WinGetClipboardText tb
	# (rcci,tb)	= IssueCleanRequest2 (ErrorCallback2 "WinGetClipboardText") (Rq0Cci CcRqGETCLIPBOARDTEXT) tb
	# (text,tb)	= case rcci.ccMsg of
					CcRETURN1	-> WinGetCStringAndFree rcci.p1 tb
					CcWASQUIT	-> ("",tb)
					other		-> abort "[WinGetClipboardText] expected CcRETURN1 value.\n"
	= (text,tb)

WinSetClipboardText :: !String !*OSToolbox -> *OSToolbox
WinSetClipboardText text tb
	# (textptr,tb)	= WinMakeCString text tb
	# (_,tb)		= IssueCleanRequest2 (ErrorCallback2 "SetClipboardText") (Rq1Cci CcRqSETCLIPBOARDTEXT textptr) tb
	= WinReleaseCString textptr tb

WinHasClipboardText :: !*OSToolbox -> (!Bool,!*OSToolbox)
WinHasClipboardText tb
	# (rcci,tb)	= IssueCleanRequest2 (ErrorCallback2 "WinHasClipboardText") (Rq0Cci CcRqCLIPBOARDHASTEXT) tb
	  ok		= case rcci.ccMsg of
	  				CcRETURN1	-> rcci.p1<>0
	  				CcWASQUIT	-> False
	  				_			-> abort "[WinHasClipboardText] expected CcRETURN1 value."
	= (ok,tb)
