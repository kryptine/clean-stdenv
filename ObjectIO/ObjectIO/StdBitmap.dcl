definition module StdBitmap


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	StdBitmap contains functions for reading bitmap files and drawing bitmaps.
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


import	StdMaybe
from	StdFile	import FileSystem,Files
import	osbitmap
import	StdPicture
//1.3
export	FileSystem World
//3.1

openBitmap		:: !{#Char} !*env -> (!Maybe Bitmap,!*env)	| FileSystem env
/*	openBitmap reads in a bitmap from file.
	The String argument must be the file name of the bitmap.
	If the bitmap could be read, then (Just bitmap) is returned, otherwise Nothing 
	is returned.
*/

getBitmapSize	:: !Bitmap -> Size
/*	getBitmapSize returns the size of the given bitmap.
	In case the bitmap is the result of an erroneous openBitmap, then the size is 
	zero.
*/

resizeBitmap :: !Size !Bitmap -> Bitmap
/*	zooms or stretches a bitmap. The second argument is the size
	of the resulting bitmap
*/

instance Drawables Bitmap
/*	draw       bitmap
		draws the given bitmap with its left top at the current pen position.
	drawAt pos bitmap
		draws the given bitmap with its left top at the given pen position.
	undraw(At) 
		equals unfill(At) the box {box_w=w,box_h=h} with {w,h} the size of the 
		bitmap.
*/
