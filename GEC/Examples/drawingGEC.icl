module drawingGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import StdGecComb

// TO TEST JUST REPLACE THE EXAMPLE NAME IN THE START RULE WITH ANY OF THE EXAMPLES BELOW
// ALL EXAMPLES HAVE TO BE OF FORM pst -> pst

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world 
= 	goGui 
 	example_l1
 	world  

derive gGEC  Rectangle,Point2,Colour,RGBColour,ShapeAttributes,Shape,Oval,Box 

:: Shape =
	  Box Box
	| Oval Oval

:: ShapeAttributes =
	{ pen_colour 	:: Colour
	, pen_size	 	:: AGEC Int
	, fill_colour	:: Colour
	, x_offset		:: AGEC Int 
	, y_offset		:: AGEC Int
	}
/* UNDER CONSTRUCTION !!!!!!!
example_draw pst
#	(wid,pst) 	= openId pst
#    pst 		= snd (openWindow Void (Window "Drawings" NilLS [WindowId wid]) pst)
=	selfState_GECps (mydrawfun wid) ("Rectangle Attributes",listGEC True initstates) initstates pst
	
	where
		mydrawfun wid abs_nrects orects pst 
		# nrects = ^^ abs_nrects
		# pst = appPIO (setWindowLook wid True (True,drawfun nrects orects)) pst 
		= (abs_nrects,nrects,pst)
		
		drawfun [Box nrect<|>nattr:nrects] [Box orect<|>oattr:orects]  nx nxx
											=	drawfun nrects orects nx nxx o
												drawshape nrect nattr orect oattr
		drawfun [Oval nrect<|>nattr:nrects] [Oval orect<|>oattr:orects]  nx nxx
											=	drawfun nrects orects nx nxx o
												drawshape nrect nattr orect oattr
		drawfun _ _  _ _					=	setPenColour Black 

		drawshape nshape nattr oshape oattr =	drawAt n_offset nshape o
												setPenSize (^^ nattr.pen_size) o 
												setPenColour nattr.pen_colour o 
												fillAt n_offset nshape o 
												setPenColour nattr.fill_colour o 
												unfillAt o_offset oshape o
												undrawAt o_offset oshape 
		where
			n_offset = {x= ^^ nattr.x_offset,y= ^^ nattr.y_offset}
			o_offset = {x= ^^ oattr.x_offset,y= ^^ oattr.y_offset}
	
		initstates= [initstate]
		initstate= initbox <|> initattr
		initbox = Box {box_w=30,box_h=30}
		initattr = {pen_colour=Black,pen_size=counterGEC 1,fill_colour=White,x_offset=counterGEC 100,y_offset=counterGEC 100}
*/
