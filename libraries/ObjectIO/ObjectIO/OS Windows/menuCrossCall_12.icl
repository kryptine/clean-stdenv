implementation module menuCrossCall_12


import	StdInt, StdMisc, StdTuple
import	clCrossCall_12
from	clCCall_12	import WinMakeCString, WinReleaseCString, CSTR
from	ostypes		import HWND


::	HITEM	:==  Int
::	HMENU	:==  Int


WinCreatePopupMenuHandle :: !*OSToolbox -> (!HMENU,!*OSToolbox)		// PA: check if this can be a C call
WinCreatePopupMenuHandle tb
	# (rcci,tb)	= IssueCleanRequest2 (ErrorCallback2 "CreatePopupMenuHandle ") (Rq0Cci CcRqCREATEPOPMENU) tb
	  hmenu		= case rcci.ccMsg of
					CcRETURN1	-> rcci.p1
					CcWASQUIT	-> 0
					other		-> abort "[CreatePopupMenuHandle] expected CcRETURN1 value."
	= (hmenu,tb)

WinTrackPopupMenu :: !HMENU !HWND !*OSToolbox -> (!Bool,!*OSToolbox)
WinTrackPopupMenu menu framePtr tb
	# (rcci,tb)	= IssueCleanRequest2 (ErrorCallback2 "TrackPopupMenu") (Rq2Cci CcRqTRACKPOPMENU menu framePtr) tb
	  ok		= case rcci.ccMsg of
	  				CcRETURN1	-> rcci.p1<>0
	  				CcWASQUIT	-> False
	  				other		-> abort "[TrackPopupMenu] expected CcRETURN1 value."
	= (ok,tb)

WinInsertMenu :: !String !Bool !HMENU !HMENU !Int !*OSToolbox -> *OSToolbox
WinInsertMenu text state submenu menu pos tb
	# (textptr,tb)	= WinMakeCString text tb
	# (_,tb)		= IssueCleanRequest2 (ErrorCallback2 "AppendMenu") (Rq5Cci CcRqINSERTMENU (toInt state) menu textptr submenu pos) tb
	= WinReleaseCString textptr tb

WinInsertMenuItem :: !String !Bool !Bool !HMENU !Int !*OSToolbox -> (!HITEM,!*OSToolbox)
WinInsertMenuItem text ablestate markstate menu pos tb
	# (textptr,tb)	= WinMakeCString text tb
	  insertCci		= Rq5Cci CcRqINSERTMENUITEM (toInt ablestate) menu textptr (toInt markstate) pos
	# (rcci,tb)		= IssueCleanRequest2 (ErrorCallback2 "InsertMenuItem") insertCci tb
	  hitem			= case rcci.ccMsg of
						CcRETURN1	-> rcci.p1
						CcWASQUIT	-> 0
						other		-> abort "[WinInsertMenuItem] expected CcRETURN1 value."
	# tb			= WinReleaseCString textptr tb
	= (hitem,tb)

WinInsertSeparator :: !HMENU !Int !*OSToolbox -> *OSToolbox
WinInsertSeparator menu pos tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "InsertSeparator") (Rq2Cci CcRqINSERTSEPARATOR menu pos) tb)

WinChangeMenuItemCheck :: !HMENU !HITEM !Bool !*OSToolbox -> *OSToolbox
WinChangeMenuItemCheck menu hitem state tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "CheckMenuItem") (Rq3Cci CcRqCHECKMENUITEM menu hitem (toInt state)) tb)

WinModifyMenu :: !String !HMENU !HMENU !*OSToolbox -> *OSToolbox
WinModifyMenu text submenu menu tb
	# (textptr,tb)	= WinMakeCString text tb
	# (_,tb)		= IssueCleanRequest2 (ErrorCallback2 "ModifyMenu") (Rq3Cci CcRqMODIFYMENU submenu menu textptr) tb
	= WinReleaseCString textptr tb

WinModifyMenuItem :: !String !HITEM !HMENU !*OSToolbox -> *OSToolbox
WinModifyMenuItem text hitem menu tb
	# (textptr,tb)	= WinMakeCString text tb
	# (_,tb)		= IssueCleanRequest2 (ErrorCallback2 "ModifyMenuItem") (Rq3Cci CcRqMODIFYMENUITEM hitem menu textptr) tb
	= WinReleaseCString textptr tb

WinDestroyMenu :: !HMENU !*OSToolbox -> *OSToolbox
WinDestroyMenu menu tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "DestroyMenu") (Rq1Cci CcRqDESTROYMENU menu) tb)

WinDeleteMenu :: !HMENU !HITEM !*OSToolbox -> *OSToolbox
WinDeleteMenu menu hitem tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "DeleteMenu") (Rq2Cci CcRqDELETEMENU menu hitem) tb)

WinRemoveMenuItem :: !HMENU !HITEM !*OSToolbox -> *OSToolbox
WinRemoveMenuItem menu hitem tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "RemoveMenuItem") (Rq2Cci CcRqREMOVEMENUITEM menu hitem) tb)

WinChangeItemAbility :: !HMENU !HITEM !Bool !*OSToolbox -> *OSToolbox
WinChangeItemAbility parent hitem onoff tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "ChangeItemAbility") (Rq3Cci CcRqITEMENABLE parent hitem (toInt onoff)) tb)

WinChangeMenuAbility :: !HMENU !Int !Bool !*OSToolbox -> *OSToolbox
WinChangeMenuAbility parent zIndex onoff tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "ChangeMenuAbility") (Rq3Cci CcRqMENUENABLE parent zIndex (toInt onoff)) tb)

WinDrawMenuBar :: !HWND !HWND !*OSToolbox -> *OSToolbox
WinDrawMenuBar framePtr clientPtr tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "DrawMenuBar") (Rq2Cci CcRqDRAWMBAR framePtr clientPtr) tb)

WinAddMenuShortKey :: !HWND !Int !Char !*OSToolbox -> *OSToolbox
WinAddMenuShortKey framePtr cmd key tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "AddMenuShortKey") (Rq3Cci CcRqADDMENUSHORTKEY framePtr cmd (toInt key)) tb)

WinRemoveMenuShortKey :: !HWND !Int !*OSToolbox -> *OSToolbox
WinRemoveMenuShortKey framePtr cmd tb
	= snd (IssueCleanRequest2 (ErrorCallback2 "RemoveMenuShortKey") (Rq2Cci CcRqREMOVEMENUSHORTKEY framePtr cmd) tb)
