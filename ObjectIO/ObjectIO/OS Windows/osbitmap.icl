implementation module osbitmap


//	Clean object I/O library, version 1.2
//	PA: other version of bitmaps: create a bitmap handle instead of continuesly copying String to OS

import	StdArray, StdBool, StdChar, StdClass, StdInt, StdFile, StdTuple
import	ospicture, ostoolbox, pictCCall_12


::	Bitmap
	=	OSBitmap !OSBitmap
::	OSBitmap
	=	{	originalSize	:: !(!Int,!Int)		// The size of the bitmap
		,	reSize			:: !(!Int,!Int)		// to store values passed to resizeBitmap
		,	bitmapContents	:: !{#Char}			// The (device independent) bitmap information (for printing)
		,	bitmapHandle	:: !Int				// The handle to the screen bitmap (for screen)
		}

toBitmap :: !OSBitmap -> Bitmap
toBitmap osBitmap = OSBitmap osBitmap

fromBitmap :: !Bitmap -> OSBitmap
fromBitmap (OSBitmap osBitmap) = osBitmap

//	OSreadBitmap reads a bitmap from a file. See page 176 of Programming Windows 95 (Charles Petzold)
OSreadBitmap :: !*File -> (!Bool,!OSBitmap,!*File)
OSreadBitmap file
	# (_, c1,file) = freadc file
	# (ok,c2,file) = freadc file      // read first two bytes
	| not ok || c1<>'B' || c2<>'M'	  // are they "BM"? 
		= (False,noBitmap,file)
	# (_,  fileSize,file)	= freadi file // read file size
	# (_,  _,       file)	= freadi file // skip bfReserved1 & 2
	# (_,  _,       file)	= freadi file // skip bfOffBits
	# (_,  _,       file)	= freadi file // skip biSize
	# (_,  w,       file)	= freadi file // read width
	# (ok1,h,       file)	= freadi file // read height
	# (ok2,         file)	= fseek  file 0 FSeekSet
	| not ok1 || not ok2
		= (False,noBitmap,file)
	# (data,file)			= freads file fileSize
	| size data <> fileSize
		= (False,noBitmap,file)
	| otherwise
		# (hdc, tb)			= WinCreateScreenHDC OSNewToolbox
		# (hbmp,tb)			= WinCreateBitmap w data hdc tb
		# tb				= WinDestroyScreenHDC (hdc,tb)
		= (if (tb==OSDummyToolbox) True True,{originalSize=(w,h),reSize=(w,h),bitmapContents=data,bitmapHandle=hbmp},file)
where
	noBitmap = {originalSize=(0,0),reSize=(0,0),bitmapContents={},bitmapHandle=0}

//	OSgetBitmapSize returns the size of the bitmap.
OSgetBitmapSize :: !OSBitmap -> (!Int,!Int)
OSgetBitmapSize {reSize}
	= reSize

//	OSgetBitmapContent returns the content string of the bitmap
OSgetBitmapContent :: !OSBitmap -> {#Char}
OSgetBitmapContent {bitmapContents}
	= bitmapContents

/*	OSresizeBitmap (w,h) bitmap
		resizes the argument bitmap to the given size.
	It is assumed that w and h are not negative.
*/
OSresizeBitmap :: !(!Int,!Int) !OSBitmap -> OSBitmap
OSresizeBitmap size bitmap
	= {bitmap & reSize=size}

/*	OSdrawBitmap bitmap pos origin pictContext
		draws the argument bitmap with the left top corner at pos, given the current origin and drawing context.
*/
OSdrawBitmap :: !OSBitmap !(!Int,!Int) !(!Int,!Int) !Bool !OSPictContext !*OSToolbox -> (!OSPictContext,!*OSToolbox)
OSdrawBitmap {originalSize,reSize,bitmapContents,bitmapHandle} pos=:(px,py) origin=:(ox,oy) isScreenOutput pictContext tb
	| originalSize==reSize
		| isScreenOutput
			= WinDrawBitmap  originalSize destination bitmapHandle   (pictContext,tb)
		// otherwise
			= WinPrintBitmap originalSize destination bitmapContents (pictContext,tb) 
	| isScreenOutput
		= WinDrawResizedBitmap  originalSize destination reSize bitmapHandle   (pictContext,tb)
	| otherwise
		= WinPrintResizedBitmap originalSize destination reSize bitmapContents (pictContext,tb)
where
	destination	= (px-ox,py-oy)
