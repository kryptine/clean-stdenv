definition module osbitmap

//	Clean object I/O library, version 1.2

import ospicture

::	Bitmap
::	OSBitmap

toBitmap	:: !OSBitmap -> Bitmap
fromBitmap	:: !Bitmap -> OSBitmap

//	OSreadBitmap reads a bitmap from a file. See page 176 of Programming Windows 95 (Charles Petzold)
OSreadBitmap :: !*File -> (!Bool,!OSBitmap,!*File)

//	OSgetBitmapSize returns the size of the bitmap
OSgetBitmapSize :: !OSBitmap -> (!Int,!Int)

//	OSgetBitmapContent returns the content string of the bitmap
OSgetBitmapContent :: !OSBitmap -> {#Char}

/*	OSresizeBitmap (w,h) bitmap
		resizes the argument bitmap to the given size.
		It is assumed that w and h are not negative.
*/
OSresizeBitmap :: !(!Int,!Int) !OSBitmap -> OSBitmap

/*	OSdrawBitmap bitmap pos origin isScreenOutput pictContext
		draws the argument bitmap with the left top corner at pos, given the current origin and drawing context.
		The isScreenOutput MUST be False when producing printer output. For screen output this is not the case,
		but setting it to True is much more efficient. 
*/
OSdrawBitmap :: !OSBitmap !(!Int,!Int) !(!Int,!Int) !Bool !OSPictContext !*OSToolbox -> (!OSPictContext,!*OSToolbox)
