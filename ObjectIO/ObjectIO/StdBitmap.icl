implementation module StdBitmap


//	Clean Object I/O library, version 1.2
//	Interface functions for drawing bitmaps.


import	StdBool, StdFile, StdInt
import	osbitmap, ospicture
import	commondef
import	StdMaybe, StdPicture


openBitmap :: !{#Char} !*env -> (!Maybe Bitmap,!*env)	| FileSystem env
openBitmap name env
	# (ok,file,env)		= fopen name FReadData env
	| not ok
		= (Nothing,env)
	# (ok,osBitmap,file)= OSreadBitmap file
    # (_,env)			= fclose file env
    | not ok
    	= (Nothing,env)
    | otherwise
		= (Just (toBitmap osBitmap),env)

getBitmapSize :: !Bitmap -> Size
getBitmapSize bitmap
	= fromTuple (OSgetBitmapSize (fromBitmap bitmap))

resizeBitmap :: !Size !Bitmap -> Bitmap
resizeBitmap size=:{w,h} bitmap
	| w<0 || h<0
		= Error "resizeBitmap" "StdBitmap" "a Size record with negative components was passed"
	| otherwise
		= toBitmap (OSresizeBitmap (w,h) (fromBitmap bitmap))

instance Drawables Bitmap where
	draw :: !Bitmap !*Picture -> *Picture
	draw bitmap picture
		# (origin,pen,toScreen,pictContext,tb)	= peekPicture picture
		  (penPos,pen)							= getPenPenPos pen
		# (pictContext,tb)						= OSdrawBitmap (fromBitmap bitmap) (toTuple penPos) (toTuple origin) toScreen pictContext tb
		= unpeekPicture origin pen toScreen pictContext tb
	where
		getPenPenPos :: !*Pen -> (!Point2,!*Pen)
		getPenPenPos pen=:{penPos={x,y}} = ({x=x,y=y},pen)
	
	drawAt :: !Point2 !Bitmap !*Picture -> *Picture
	drawAt pos bitmap picture
		# (origin,pen,toScreen,pictContext,tb)	= peekPicture picture
		# (pictContext,tb)						= OSdrawBitmap (fromBitmap bitmap) (toTuple pos) (toTuple origin) toScreen pictContext tb
		= unpeekPicture origin pen toScreen pictContext tb
	
	undraw :: !Bitmap !*Picture -> *Picture
	undraw bitmap picture
		= unfill {box_w=w,box_h=h} picture
	where
		(w,h)	= OSgetBitmapSize (fromBitmap bitmap)
	
	undrawAt :: !Point2 !Bitmap !*Picture -> *Picture
	undrawAt pos bitmap picture
		= unfillAt pos {box_w=w,box_h=h} picture
	where
		(w,h)	= OSgetBitmapSize (fromBitmap bitmap)
