definition module osclipboard

//	Clean Object I/O library, version 1.2

//	Clipboard operations.

import	ostoolbox

::	OSClipboardItemType
	:==	Int
OSClipboardText
	:==	1	// CF_TEXT

OShasClipboardText :: !*OSToolbox -> (!Bool,!*OSToolbox)
//	OShasClipboardText checks whether the clipboard currently contains a text item.

OSsetClipboardText :: !{#Char} !*OSToolbox -> *OSToolbox
//	OSsetClipboardText empties the clipboard and sets the text to the clipboard.
//	The return Int is the new version number.

OSgetClipboardText :: !*OSToolbox -> (!{#Char},!*OSToolbox)
//	OSgetClipboardText retrieves the current clipboard text item, which is empty if not present.

OSgetClipboardContent :: !*OSToolbox -> (![OSClipboardItemType],!*OSToolbox)
//	OSgetClipboardContent retrieves the current item types that are stored in the clipboard.

OSgetClipboardVersion :: !Int !*OSToolbox -> (!Int,!*OSToolbox)
//	OSgetClipboardVersion given the previous version number returns the new, current version number.
