implementation module StdPicture


//	Clean Object I/O library, version 1.2

//	Drawing functions and other operations on Picture


import	StdBool, StdFunc, StdInt, StdList, StdMisc, StdReal, StdTuple
import	ospicture, osfont, osrgn, ostoolbox
import	commondef, StdPictureDef


//	Pen attribute functions:

setPenAttributes :: ![PenAttribute] !*Picture -> *Picture
setPenAttributes atts picture
	= StateMap2 setattribute atts picture
where
	setattribute :: !PenAttribute !*Picture -> *Picture
	setattribute (PenSize size) picture = setPenSize size picture
	setattribute (PenPos  pos)  picture = setPenPos  pos  picture
	setattribute (PenColour	c)  picture = setPenColour c  picture
	setattribute (PenBack   c)  picture = setPenBack   c  picture
	setattribute (PenFont font) picture = setPenFont font picture

getPenAttributes :: !*Picture -> (![PenAttribute],!*Picture)
getPenAttributes picture
	# (pen,picture)	= getpictpen picture
	= (getattribute pen,picture)
where
	getattribute :: !Pen -> [PenAttribute]
	getattribute {penSize,penForeColour,penBackColour,penPos,penFont}
		= [PenSize penSize,PenPos penPos,PenColour penForeColour,PenBack penBackColour,PenFont penFont]


//	Pen position attributes:
setPenPos :: !Point2 !*Picture -> *Picture
setPenPos pos picture
	= setpictpenpos pos picture

getPenPos :: !*Picture -> (!Point2,!*Picture)
getPenPos picture
	= getpictpenpos picture

class movePenPos figure	:: !figure !*Picture -> *Picture
//	Move the pen position as much as when drawing the figure.

instance movePenPos Vector2 where
	movePenPos :: !Vector2 !*Picture -> *Picture
	movePenPos v picture
		= movepictpenpos v picture

instance movePenPos Curve where
	movePenPos :: !Curve !*Picture -> *Picture
	movePenPos curve picture
		# (curpos,picture)	= getpictpenpos picture
		  (_,_,endpos)		= getcurve_rect_begin_end curpos curve
		# picture			= setpictpenpos endpos picture
		= picture


//	PenSize attributes:
setPenSize :: !Int !*Picture -> *Picture
setPenSize w picture
	= setpictpensize w picture

getPenSize :: !*Picture -> (!Int,!*Picture)
getPenSize picture
	= getpictpensize picture

setDefaultPenSize :: !*Picture -> *Picture
setDefaultPenSize picture
	= setpictpensize 1 picture


//	Colour attributes:
setPenColour :: !Colour !*Picture -> *Picture
setPenColour c picture
	= setpictpencolour c picture

getPenColour :: !*Picture -> (!Colour,!*Picture)
getPenColour picture
	= getpictpencolour picture

setPenBack :: !Colour !*Picture -> *Picture
setPenBack c picture
	= setpictbackcolour c picture

getPenBack :: !*Picture -> (!Colour,!*Picture)
getPenBack picture
	= getpictbackcolour picture

// Convert a colour to RGBColour
toRGBColour :: !Colour -> RGBColour
toRGBColour colour
	= {r=r,g=g,b=b}
where
	(r,g,b) = toRGBtriple colour

setDefaultPenColour :: !*Picture -> *Picture
setDefaultPenColour picture
	= setpictpencolour Black picture

setDefaultPenBack :: !*Picture -> *Picture
setDefaultPenBack picture
	= setpictbackcolour White picture


//	Font attributes:
setPenFont :: !Font !*Picture -> *Picture
setPenFont f picture
	= setpictpenfont f picture

getPenFont :: !*Picture -> (!Font,!*Picture)
getPenFont picture
	= getpictpenfont picture

setDefaultPenFont :: !*Picture -> *Picture
setDefaultPenFont picture
	= setpictpendefaultfont picture


//	Font operations:
openFont :: !FontDef !*Picture -> (!(!Bool,!Font),!*Picture)
openFont {fName,fStyles,fSize} picture
	# (origin,pen,toScreen,context,tb)	= peekPicture picture
	# (found,font,tb)					= OSselectfont (fName,fStyles,fSize) tb
	# picture							= unpeekPicture origin pen toScreen context tb
	= ((found,font),picture)

openDefaultFont :: !*Picture -> (!Font,!*Picture)
openDefaultFont picture
	= accpicttoolbox OSdefaultfont picture

openDialogFont :: !*Picture -> (!Font,!*Picture)
openDialogFont picture
	= accpicttoolbox OSdialogfont picture

getFontNames :: !*Picture -> (![FontName],!*Picture)
getFontNames picture
	= accpicttoolbox OSfontnames picture

getFontStyles :: !FontName	!*Picture -> (![FontStyle],!*Picture)
getFontStyles fName picture
	= accpicttoolbox (OSfontstyles fName) picture

getFontSizes :: !Int !Int !FontName	!*Picture -> (![FontSize],!*Picture)
getFontSizes sizeBound1 sizeBound2 fName picture
	= accpicttoolbox (OSfontsizes sizeBound1 sizeBound2 fName) picture

getFontDef :: !Font -> FontDef
getFontDef font
	= {fName=name,fStyles=styles,fSize=size}
where
	(name,styles,size)	= OSfontgetdef font

getFontCharWidth :: !Font !Char !*Picture -> (!Int,!*Picture)
getFontCharWidth font char picture
	# (osPictContext,picture)	= peekOSPictContext picture
	# (widths,picture)			= accpicttoolbox (OSgetfontcharwidths True osPictContext [char] font) picture
	= (hd widths,picture)

getFontCharWidths :: !Font ![Char] !*Picture -> (![Int],!*Picture)
getFontCharWidths font chars picture
	# (osPictContext,picture)	= peekOSPictContext picture
	= accpicttoolbox (OSgetfontcharwidths True osPictContext chars font) picture

getFontStringWidth :: !Font !String !*Picture -> (!Int,!*Picture)
getFontStringWidth font string picture
	# (osPictContext,picture)	= peekOSPictContext picture
	# (widths,picture)			= accpicttoolbox (OSgetfontstringwidths True osPictContext [string] font) picture
	= (hd widths,picture)

getFontStringWidths :: !Font ![String] !*Picture -> (![Int],!*Picture)
getFontStringWidths font strings picture
	# (osPictContext,picture)	= peekOSPictContext picture
	= accpicttoolbox (OSgetfontstringwidths True osPictContext strings font) picture

getFontMetrics :: !Font !*Picture -> (!FontMetrics,!*Picture)
getFontMetrics font picture
	# (osPictContext,picture)						= peekOSPictContext picture
	# ((ascent,descent,leading,maxwidth),picture)	= accpicttoolbox (OSgetfontmetrics True osPictContext font) picture
	= ({fAscent=ascent,fDescent=descent,fLeading=leading,fMaxWidth=maxwidth},picture)

getPenFontCharWidth :: !Char !*Picture -> (!Int,!*Picture)
getPenFontCharWidth char picture = getPenFontInfo (\font->getFontCharWidth font char) picture

getPenFontCharWidths :: ![Char] !*Picture -> (![Int],!*Picture)
getPenFontCharWidths chars picture = getPenFontInfo (\font->getFontCharWidths font chars) picture

getPenFontStringWidth :: !String !*Picture -> (!Int,!*Picture)
getPenFontStringWidth string picture = getPenFontInfo (\font->getFontStringWidth font string) picture

getPenFontStringWidths :: ![String] !*Picture -> (![Int],!*Picture)
getPenFontStringWidths strings picture = getPenFontInfo (\font->getFontStringWidths font strings) picture

getPenFontMetrics :: !*Picture -> (!FontMetrics,!*Picture)
getPenFontMetrics picture = getPenFontInfo (\font->getFontMetrics font) picture

getPenFontInfo :: !(Font -> *Picture -> (.x,*Picture)) !*Picture -> (!.x,!*Picture)
getPenFontInfo fontfun picture
	# (font,picture)	= getPenFont picture
	# (x,picture)		= fontfun font picture
	# picture			= setPenFont font picture
	= (x,picture)


/*	Drawing functions.
	These functions are divided into the following classes:
	Drawables:
		draw     'line-oriented' figures at the current  pen position.
		drawAt   'line-oriented' figures at the argument pen position.
		undraw     f = appPicture (draw     f o setPenColour background)
		undrawAt x f = appPicture (drawAt x f o setPenColour background)
	Fillables:
		fill     'area-oriented' figures at the current  pen position.
		fillAt   'area-oriented' figures at the argument pen position.
		unfill     f = appPicture (fill     f o setPenColour background)
		unfillAt x f = appPicture (fillAt x f o setPenColour background)
	Hilites:
		hilite	 draws figures in the appropriate 'hilite' mode at the current pen position.
		hiliteAt draws figures in the appropriate 'hilite' mode at the current pen position.
		Both functions reset the 'hilite' after drawing.
*/
class Drawables figure where
	draw	::			!figure					!*Picture -> *Picture
	drawAt	:: !Point2	!figure					!*Picture -> *Picture
	undraw	::			!figure					!*Picture -> *Picture
	undrawAt:: !Point2	!figure					!*Picture -> *Picture

class Fillables figure where
	fill	::			!figure					!*Picture -> *Picture
	fillAt	:: !Point2	!figure					!*Picture -> *Picture
	unfill	::			!figure					!*Picture -> *Picture
	unfillAt:: !Point2	!figure					!*Picture -> *Picture

class Hilites figure where
	hilite	::			!figure					!*Picture -> *Picture
	hiliteAt:: !Point2	!figure					!*Picture -> *Picture


/*	(app/acc)Picture applies the given drawing function to the given picture.
	When drawing is done, all picture attributes are set to the attribute values of the original picture.
*/
appPicture :: !.(IdFun *Picture) !*Picture -> *Picture
appPicture drawf picture
	# (pen,picture)	= getpictpen picture
	# picture		= drawf picture
	# picture		= setpictpen pen picture
	= picture

accPicture :: !.(St *Picture .x) !*Picture -> (.x,!*Picture)
accPicture drawf picture
	# (pen,picture)	= getpictpen picture
	# (x,picture)	= drawf picture
	# picture		= setpictpen pen picture
	= (x,picture)


//	Drawing in a clipping region.

::	Region
	=	{	region_shape	:: ![RegionShape]
		,	region_bound	:: !Rect
		}
::	RegionShape
	=	RegionRect		!Rect
	|	RegionPolygon	!Point2 ![Vector2]


isEmptyRegion :: !Region -> Bool
isEmptyRegion {region_shape=[]}	= True
isEmptyRegion _					= False

getRegionBound :: !Region -> Rectangle
getRegionBound {region_bound} = RectToRectangle region_bound

class toRegion area :: !area -> Region

::	PolygonAt
	=	{	polygon_pos	:: !Point2
		,	polygon		:: !Polygon
		}

instance toRegion Rectangle where
	toRegion :: !Rectangle -> Region
	toRegion rectangle
		| IsEmptyRect rect	= zero
		| otherwise			= {region_shape=[RegionRect rect],region_bound=rect}
	where
		rect				= RectangleToRect rectangle

instance toRegion PolygonAt where
	toRegion :: !PolygonAt -> Region
	toRegion {polygon_pos=p=:{x,y},polygon={polygon_shape}}
		| IsEmptyRect bound	= zero
		| otherwise			= {region_shape=[RegionPolygon p shape],region_bound=bound}
	where
		shape				= closeShape zero polygon_shape
		bound				= polybound p shape {rleft=x,rtop=y,rright=x,rbottom=y}
		
		polybound :: !Point2 ![Vector2] !Rect -> Rect
		polybound p [v:vs] {rleft=minx,rtop=miny,rright=maxx,rbottom=maxy}
			= polybound p` vs {rleft=minx`,rtop=miny`,rright=maxx`,rbottom=maxy`}
		where
			p`		= movePoint v p
			{x,y}	= p`
			minx`	= min minx x;	miny`	= min miny y;
			maxx`	= max maxx x;	maxy`	= max maxy y;
		polybound _ _ bound
			= bound
		
		closeShape :: !Vector2 ![Vector2] -> [Vector2]
		closeShape v [v`:vs]
			= [v`:closeShape (v+v`) vs]
		closeShape v []
			| v==zero	= []
			| otherwise	= [{vx=0-v.vx,vy=0-v.vy}]

instance toRegion [area] | toRegion area where
	toRegion :: ![area] -> Region	| toRegion area
	toRegion [area:areas]	= toRegion area + toRegion areas
	toRegion _				= zero

instance toRegion (:^: area1 area2)	| toRegion area1 & toRegion area2 where
	toRegion :: !(:^: area1 area2) -> Region | toRegion area1 & toRegion area2
	toRegion (r1 :^: r2) = toRegion r1 + toRegion r2

instance zero Region where
	zero :: !Region
	zero = {region_shape=[],region_bound=zero}
instance + Region where
	(+) :: !Region !Region -> Region
	(+) r1 r2
	| IsEmptyRect r1.region_bound
		= r2
	| IsEmptyRect r2.region_bound
		= r1
	| otherwise
		= {region_shape=r1.region_shape++r2.region_shape,region_bound=sumbound r1.region_bound r2.region_bound}
	where
		sumbound :: !Rect !Rect -> Rect
		sumbound {rleft=minx,rtop=miny,rright=maxx,rbottom=maxy} {rleft=minx`,rtop=miny`,rright=maxx`,rbottom=maxy`}
			= {rleft=min minx minx`,rtop=min miny miny`,rright=max maxx maxx`,rbottom=max maxy maxy`}

appClipPicture :: !Region !.(IdFun *Picture) !*Picture -> *Picture
appClipPicture region drawf picture
	= snd (accClipPicture region (\p->(undef,drawf p)) picture)

accClipPicture :: !Region !.(St *Picture .x) !*Picture -> (.x,!*Picture)
accClipPicture region drawf picture
	#! (curClipRgn,picture)				= pictgetcliprgn picture
	#! (origin,pen,toScreen,context,tb)	= peekPicture picture
	#! (newClipRgn,tb)					= osnewrgn tb
	#! (hFac,vFac,context,tb)			= getPictureScalingFactors context tb
	#! (newClipRgn,tb)					= setrgnshapes hFac vFac origin region.region_shape newClipRgn tb
	#! picture							= unpeekPicture origin pen toScreen context tb
	   (set,dispose)					= if (curClipRgn==0) (pictsetcliprgn,\_ x->x) (pictandcliprgn,osdisposergn)
	#! picture							= set newClipRgn picture
	#! (x,picture)						= drawf picture
	#! picture							= pictsetcliprgn curClipRgn picture
	#! picture							= apppicttoolbox (dispose curClipRgn o osdisposergn newClipRgn) picture
	=  (x,picture)
where
	setrgnshapes :: !(!Int,!Int) !(!Int,!Int) !Point2 ![RegionShape] !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
	setrgnshapes hFac vFac origin [shape:shapes] rgn tb
		# (rgn,tb)				= setrgnshape hFac vFac origin shape rgn tb
		= setrgnshapes hFac vFac origin shapes rgn tb
	where
		setrgnshape :: !(!Int,!Int) !(!Int,!Int) !Point2 !RegionShape !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
		setrgnshape hFac vFac {x=ox,y=oy} (RegionRect {rleft,rtop,rright,rbottom}) rgn tb
			# (rectrgn,tb)	= osnewrectrgn rect tb
			# (sumrgn, tb)	= osunionrgn rectrgn rgn tb
			# tb			= osdisposergn rectrgn tb
			# tb			= osdisposergn rgn tb
			= (sumrgn,tb)
		where
			rect			= {rleft=scale hFac (rleft-ox),rtop=scale vFac (rtop-oy),rright=scale hFac (rright-ox),rbottom=scale vFac (rbottom-oy)}
		setrgnshape hFac vFac {x=ox,y=oy} (RegionPolygon {x,y} shape) rgn tb
			# (emptyrgn,tb)	= osnewrgn tb
			# (polyrgn, tb)	= ospolyrgn (scale hFac (x-ox),scale vFac (y-oy)) (map (\{vx,vy}->(scale hFac vx,scale vFac vy)) shape) emptyrgn tb
			# (sumrgn,  tb)	= osunionrgn polyrgn rgn tb
			# tb			= osdisposergn polyrgn tb
			# tb			= osdisposergn rgn tb
			= (sumrgn,tb)
	setrgnshapes _ _ _ _ rgn tb
		= (rgn,tb)
	
	scale :: !(!Int,!Int) !Int -> Int
	scale (n,d) x = n*x/d


/*	(app/acc)XorPicture applies the given drawing function to the given picture in the platform appropriate
	xor mode. 
*/
appXorPicture :: !.(IdFun *Picture) !*Picture -> *Picture
appXorPicture drawf picture
	# picture			= setpictxormode picture
	# picture			= drawf picture
	# picture			= setpictnormalmode picture
	= picture

accXorPicture :: !.(St *Picture .x) !*Picture -> (.x,!*Picture)
accXorPicture drawf picture
	# picture			= setpictxormode picture
	# (x,picture)		= drawf picture
	# picture			= setpictnormalmode picture
	= (x,picture)


/*	Hiliting figures:
*/
instance Hilites Box where
	hilite :: !Box !*Picture -> *Picture
	hilite box picture
		# picture			= setpicthilitemode picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictfillrect (boxtorect curpos box) picture
		# picture			= setpictnormalmode picture
		= picture
	
	hiliteAt :: !Point2 !Box !*Picture -> *Picture
	hiliteAt base box picture
		# picture	= setpicthilitemode picture
		# picture	= pictfillrect (boxtorect base box) picture
		# picture	= setpictnormalmode picture
		= picture

instance Hilites Rectangle where
	hilite :: !Rectangle !*Picture -> *Picture
	hilite rectangle picture
		# picture	= setpicthilitemode picture
		# picture	= pictfillrect (RectangleToRect rectangle) picture
		# picture	= setpictnormalmode picture
		= picture
	
	hiliteAt :: !Point2 !Rectangle !*Picture -> *Picture
	hiliteAt _ rectangle picture
		# picture	= setpicthilitemode picture
		# picture	= pictfillrect (RectangleToRect rectangle) picture
		# picture	= setpictnormalmode picture
		= picture


drawPoint :: !*Picture -> *Picture
drawPoint picture
	# (curpos,picture)	= getpictpenpos picture
	# picture			= pictdrawpoint curpos picture
	# picture			= setpictpenpos {curpos & x=curpos.x+1} picture
	= picture

drawPointAt :: !Point2 !*Picture -> *Picture
drawPointAt point picture
	# (curpos,picture)	= getpictpenpos picture
	# picture			= pictdrawpoint point picture
	# picture			= setpictpenpos curpos picture
	= picture


/*	Point2 connecting drawing operations:
*/
drawLineTo :: !Point2 !*Picture -> *Picture
drawLineTo pos picture
	= pictdrawlineto pos picture

drawLine :: !Point2 !Point2 !*Picture -> *Picture
drawLine pos1 pos2 picture
	= pictdrawline pos1 pos2 picture


/*	Text drawing operations:
*/
instance Drawables Char where
	draw :: !Char !*Picture -> *Picture
	draw char picture
		= pictdrawchar char picture
	
	drawAt :: !Point2 !Char !*Picture -> *Picture
	drawAt pos char picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= setpictpenpos pos picture
		# picture			= pictdrawchar char picture
		# picture			= setpictpenpos curpos picture
		= picture
	
	undraw :: !Char !*Picture -> *Picture
	undraw char picture
		= pictundrawchar char picture
	
	undrawAt :: !Point2 !Char !*Picture -> *Picture
	undrawAt pos char picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= setpictpenpos pos picture
		# picture			= pictundrawchar char picture
		# picture			= setpictpenpos curpos picture
		= picture

instance Drawables {#Char} where
	draw :: !{#Char} !*Picture -> *Picture
	draw string picture
		= pictdrawstring string picture
	
	drawAt :: !Point2 !{#Char} !*Picture -> *Picture
	drawAt pos string picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= setpictpenpos pos picture
		# picture			= pictdrawstring string picture
		# picture			= setpictpenpos curpos picture
		= picture
	
	undraw :: !{#Char} !*Picture -> *Picture
	undraw string picture
		= pictundrawstring string picture
	
	undrawAt :: !Point2 !{#Char} !*Picture -> *Picture
	undrawAt pos string picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= setpictpenpos pos picture
		# picture			= pictundrawstring string picture
		# picture			= setpictpenpos curpos picture
		= picture


/*	Vector2 drawing operations:
*/
instance Drawables Vector2 where
	draw :: !Vector2 !*Picture -> *Picture
	draw {vx,vy} picture
		# (curpos,picture)	= getpictpenpos picture
		  endpos			= {x=curpos.x+vx,y=curpos.y+vy}
		# picture			= pictdrawlineto endpos picture
		= picture
	
	drawAt :: !Point2 !Vector2 !*Picture -> *Picture
	drawAt pos=:{x,y} {vx,vy} picture
		= pictdrawline pos {x=x+vx,y=y+vy} picture
	
	undraw :: !Vector2 !*Picture -> *Picture
	undraw {vx,vy} picture
		# (curpos,picture)	= getpictpenpos picture
		  endpos			= {x=curpos.x+vx,y=curpos.y+vy}
		# picture			= pictundrawlineto endpos picture
		= picture
	
	undrawAt :: !Point2 !Vector2 !*Picture -> *Picture
	undrawAt pos=:{x,y} {vx,vy} picture
		= pictundrawline pos {x=x+vx,y=y+vy} picture


/*	Oval drawing operations:
*/
instance Drawables Oval where
	draw :: !Oval !*Picture -> *Picture
	draw oval picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictdrawoval curpos oval picture
		= picture
	
	drawAt :: !Point2 !Oval !*Picture -> *Picture
	drawAt pos oval picture
		= pictdrawoval pos oval picture
	
	undraw :: !Oval !*Picture -> *Picture
	undraw oval picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictundrawoval curpos oval picture
		= picture
	
	undrawAt :: !Point2 !Oval !*Picture -> *Picture
	undrawAt pos oval picture
		= pictundrawoval pos oval picture

instance Fillables Oval where
	fill :: !Oval !*Picture -> *Picture
	fill oval picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictfilloval curpos oval picture
		= picture
	
	fillAt :: !Point2 !Oval !*Picture -> *Picture
	fillAt pos oval picture
		= pictfilloval pos oval picture
	
	unfill :: !Oval !*Picture -> *Picture
	unfill oval picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictunfilloval curpos oval picture
		= picture
	
	unfillAt :: !Point2 !Oval !*Picture -> *Picture
	unfillAt pos oval picture
		= pictunfilloval pos oval picture


/*	Curve drawing operations:
*/
instance Drawables Curve where
	draw :: !Curve !*Picture -> *Picture
	draw curve picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictdrawcurve True curpos curve picture
		= picture
	
	drawAt :: !Point2 !Curve !*Picture -> *Picture
	drawAt point curve picture
		= pictdrawcurve False point curve picture
	
	undraw :: !Curve !*Picture -> *Picture
	undraw curve picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictundrawcurve True curpos curve picture
		= picture
	
	undrawAt :: !Point2 !Curve !*Picture -> *Picture
	undrawAt point curve picture
		= pictundrawcurve False point curve picture

instance Fillables Curve where
	fill :: !Curve !*Picture -> *Picture
	fill curve picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictfillcurve True curpos curve picture
		= picture
	
	fillAt :: !Point2 !Curve !*Picture -> *Picture
	fillAt point curve picture
		= pictfillcurve False point curve picture

	unfill :: !Curve !*Picture -> *Picture
	unfill curve picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictunfillcurve True curpos curve picture
		= picture
	
	unfillAt :: !Point2 !Curve !*Picture -> *Picture
	unfillAt point curve picture
		= pictunfillcurve False point curve picture


/*	Box drawing operations:
*/
instance Drawables Box where
	draw :: !Box !*Picture -> *Picture
	draw box picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictdrawrect (boxtorect curpos box) picture
		= picture
	
	drawAt :: !Point2 !Box !*Picture -> *Picture
	drawAt point box picture
		= pictdrawrect (boxtorect point box) picture
	
	undraw :: !Box !*Picture -> *Picture
	undraw box picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictundrawrect (boxtorect curpos box) picture
		= picture
	
	undrawAt :: !Point2 !Box !*Picture -> *Picture
	undrawAt point box picture
		= pictundrawrect (boxtorect point box) picture

instance Fillables Box where
	fill :: !Box !*Picture -> *Picture
	fill box picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictfillrect (boxtorect curpos box) picture
		= picture
	
	fillAt :: !Point2 !Box !*Picture -> *Picture
	fillAt pos box picture
		= pictfillrect (boxtorect pos box) picture

	unfill :: !Box !*Picture -> *Picture
	unfill box picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictunfillrect (boxtorect curpos box) picture
		= picture
	
	unfillAt :: !Point2 !Box !*Picture -> *Picture
	unfillAt pos box picture
		= pictunfillrect (boxtorect pos box) picture

boxtorect :: !Point2 !Box -> Rect
boxtorect {x,y} {box_w,box_h}
	= {rleft=l,rtop=t,rright=r,rbottom=b}
where
	(l,r) = minmax x (x+box_w)
	(t,b) = minmax y (y+box_h)


/*	Rectangle drawing operations:
*/
instance Drawables Rectangle where
	draw :: !Rectangle !*Picture -> *Picture
	draw rectangle picture
		= pictdrawrect (RectangleToRect rectangle) picture
	
	drawAt :: !Point2 !Rectangle !*Picture -> *Picture
	drawAt _ rectangle picture
		= pictdrawrect (RectangleToRect rectangle) picture

	undraw :: !Rectangle !*Picture -> *Picture
	undraw rectangle picture
		= pictundrawrect (RectangleToRect rectangle) picture
	
	undrawAt :: !Point2 !Rectangle !*Picture -> *Picture
	undrawAt _ rectangle picture
		= pictundrawrect (RectangleToRect rectangle) picture

instance Fillables Rectangle where
	fill :: !Rectangle !*Picture -> *Picture
	fill rectangle picture
		= pictfillrect (RectangleToRect rectangle) picture
	
	fillAt :: !Point2 !Rectangle !*Picture -> *Picture
	fillAt _ rectangle picture
		= pictfillrect (RectangleToRect rectangle) picture

	unfill :: !Rectangle !*Picture -> *Picture
	unfill rectangle picture
		= pictunfillrect (RectangleToRect rectangle) picture
	
	unfillAt :: !Point2 !Rectangle !*Picture -> *Picture
	unfillAt _ rectangle picture
		= pictunfillrect (RectangleToRect rectangle) picture


/*	Polygon drawing operations:
*/
instance Drawables Polygon where
	draw :: !Polygon !*Picture -> *Picture
	draw polygon picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictdrawpolygon curpos polygon picture
		= picture
	
	drawAt :: !Point2 !Polygon !*Picture -> *Picture
	drawAt base polygon picture
		= pictdrawpolygon base polygon picture

	undraw :: !Polygon !*Picture -> *Picture
	undraw polygon picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictundrawpolygon curpos polygon picture
		= picture
	
	undrawAt :: !Point2 !Polygon !*Picture -> *Picture
	undrawAt base polygon picture
		= pictundrawpolygon base polygon picture

instance Fillables Polygon where
	fill :: !Polygon !*Picture -> *Picture
	fill polygon picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictfillpolygon curpos polygon picture
		= picture
	
	fillAt :: !Point2 !Polygon !*Picture -> *Picture
	fillAt base polygon picture
		= pictfillpolygon base polygon picture
	
	unfill :: !Polygon !*Picture -> *Picture
	unfill polygon picture
		# (curpos,picture)	= getpictpenpos picture
		# picture			= pictunfillpolygon curpos polygon picture
		= picture
	
	unfillAt :: !Point2 !Polygon !*Picture -> *Picture
	unfillAt base polygon picture
		= pictunfillpolygon base polygon picture


// MW...
getResolution :: !*Picture -> (!(!Int,!Int),!*Picture)
getResolution picture
	# (origin,pen,toScreen,context,tb)	= peekPicture picture
	# (res,tb)							= getResolutionC context tb
	= (res,unpeekPicture origin pen toScreen context tb)
// ... MW
