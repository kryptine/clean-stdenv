implementation module StdClipboard


//	Clean Object I/O library, version 1.2.1


import	StdFunc, StdList, StdMisc, StdString
import	osclipboard
import	StdMaybe
from	commondef	import FatalError, StrictSeq, StrictSeqList, Remove, Cond
from	iostate		import PSt, IOSt, getIOToolbox, setIOToolbox, accIOToolbox, IOStGetClipboardState, IOStSetClipboardState, ClipboardState


StdClipboardFatalError :: String String -> .x
StdClipboardFatalError function error
	= FatalError function "StdClipboard" error


//	The clipboard item type:

::	ClipboardItem
	=	ClipboardString !String			// Support for strings
//	|	ClipboardPict	Handle			// Support for pictures (PA: not supported yet)

class Clipboard item where
	toClipboard		:: !item			-> ClipboardItem
	fromClipboard	:: !ClipboardItem	-> Maybe item

instance Clipboard {#Char} where
	toClipboard :: !{#Char} -> ClipboardItem
	toClipboard string = ClipboardString string
	
	fromClipboard :: !ClipboardItem -> Maybe {#Char}
	fromClipboard (ClipboardString string) = Just string


//	Reading and writing the value of the selection to the clipboard:

setClipboard :: ![ClipboardItem] !(PSt .l) -> PSt .l
setClipboard clipItems pState=:{io}
	# (tb,ioState)	= getIOToolbox io
	# tb			= OSinitialiseClipboard tb
	# tb			= StrictSeq (map clipboardItemToScrap singleItems) tb
	# ioState		= setIOToolbox tb ioState
	= {pState & io=ioState}
where
	singleItems		= removeDuplicateClipItems clipItems
	
	removeDuplicateClipItems :: ![ClipboardItem] -> [ClipboardItem]
	removeDuplicateClipItems [item:items]
		# (_,_,items)	= Remove (eqClipboardType item) undef items
		= [item:removeDuplicateClipItems items]
	where
		eqClipboardType :: !ClipboardItem !ClipboardItem -> Bool
		eqClipboardType (ClipboardString _) item	= case item of
														(ClipboardString _)	-> True
														_					-> False
	removeDuplicateClipItems items
		= items
	
	clipboardItemToScrap :: !ClipboardItem !*OSToolbox -> *OSToolbox
	clipboardItemToScrap (ClipboardString text) tb
		= OSsetClipboardText text tb

getClipboard :: !(PSt .l) -> (![ClipboardItem],!PSt .l)
getClipboard pState
	# (tb,ioState)		= getIOToolbox pState.io
	# tb				= OSinitialiseClipboard tb
	# (contents,tb)		= OSgetClipboardContent tb
	  contents			= filter ((==) OSClipboardText) contents
	# (clipItems,tb)	= StrictSeqList (map scrapToClipboardItem contents) tb
	# (cbs,ioState)		= IOStGetClipboardState ioState
	# (version,tb)		= OSgetClipboardVersion cbs.cbsCount tb
	# ioState			= IOStSetClipboardState {cbs & cbsCount=version} ioState
	# ioState			= setIOToolbox tb ioState
	= (clipItems,{pState & io=ioState})
where
	scrapToClipboardItem :: !Int !*OSToolbox -> (!ClipboardItem,!*OSToolbox)
	scrapToClipboardItem OSClipboardText tb
		# (text,tb)	= OSgetClipboardText tb
		= (ClipboardString text,tb)
	scrapToClipboardItem type tb
		= StdClipboardFatalError "getClipboard" ("unimplemented clipboard content of type: "+++toString type)

clipboardHasChanged :: !(PSt .l) -> (!Bool,!PSt .l)
clipboardHasChanged pState
	# (cbs,ioState)		= IOStGetClipboardState pState.io
	  oldCount			= cbs.cbsCount
	# (tb,ioState)		= getIOToolbox ioState
	# tb				= OSinitialiseClipboard tb
	# (newCount,tb)		= OSgetClipboardVersion oldCount tb
	# ioState			= setIOToolbox tb ioState
	= (oldCount<>newCount,{pState & io=ioState})
