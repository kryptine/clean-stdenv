implementation module osclipboard

//	Clean Object I/O library, version 1.2

//	Clipboard operations.

import StdInt
import clipboardCrossCall_12

::	OSClipboardItemType
	:==	Int
OSClipboardText
	:==	1	// CF_TEXT

OSinitialiseClipboard :: !*OSToolbox -> *OSToolbox
OSinitialiseClipboard tb
	= WinInitialiseClipboard tb

OShasClipboardText :: !*OSToolbox -> (!Bool,!*OSToolbox)
OShasClipboardText tb
	= WinHasClipboardText tb

OSsetClipboardText :: !{#Char} !*OSToolbox -> *OSToolbox
OSsetClipboardText text tb
	= WinSetClipboardText text tb

OSgetClipboardText :: !*OSToolbox -> (!{#Char},!*OSToolbox)
OSgetClipboardText tb
	= WinGetClipboardText tb

OSgetClipboardContent :: !*OSToolbox -> (![OSClipboardItemType],!*OSToolbox)
OSgetClipboardContent tb
	# (hasText,tb)	= WinHasClipboardText tb
	= (if hasText [OSClipboardText] [],tb)

OSgetClipboardVersion :: !Int !*OSToolbox -> (!Int,!*OSToolbox)
OSgetClipboardVersion nr tb
	= WinGetClipboardCount tb
