implementation module picture;

import StdClass, StdArray;
import xpicture, StdMisc, StdInt, StdBool, StdChar, StdString, StdReal;
import misc,StdFile;

:: * Picture :== (!Bool, !XPicture);
:: XPicture    :== Int;

:: DrawFunction    :== Picture -> Picture;

// The predefined figures that can be drawn:

:: Point          :== (!Int, !Int);
:: Line           :== (!Point, !Point);
:: Curve          :== (!Oval, !Int, !Int);
:: Rectangle      :== (!Point, !Point);
:: Rect           :== (!Int,!Int,!Int,!Int);
:: RoundRectangle :== (!Rectangle, !Int, !Int);
:: Oval           :== Rectangle;
:: Circle         :== (!Point, !Int);
:: Wedge          :== (!Oval, !Int, !Int);
:: Polygon        :== (!Point, !PolygonShape);

:: PolygonShape   :== [Vector];
:: Vector         :== (!Int,!Int);

// The pen attributes which influence the way figures are drawn:

:: PenSize    :== (!Int, !Int);
:: PenMode    = CopyMode | OrMode | XorMode | ClearMode | HiliteMode |
                 NotCopyMode | NotOrMode | NotXorMode | NotClearMode;
:: PenPattern = BlackPattern | DkGreyPattern | GreyPattern | LtGreyPattern |
                 WhitePattern;

// The predefined colours:

:: Colour = RGB Real Real Real
          |  BlackColour | RedColour | WhiteColour | GreenColour | BlueColour 
          |  YellowColour | CyanColour | MagentaColour;

// Fonts

:: Font :== (!Int, !FontName, !FontStyle, !FontSizeX);
:: FontName  :== String;
:: FontStyle :== String;
:: FontSize  :== Int;
:: FontInfo  :== (!Int, !Int, !Int, !Int);
 
:: FontStylesX :== (!Bool,!Bool,!Bool,!Bool,!Bool);
:: FontSizeX   :== String;

MinFontSize :== 6;
MaxFontSize :== 128;

MinRGB :== 0.0;
MaxRGB :== 1.0;

PI :== 3.1415926535898;

/* Rules internal to the I/O library:
*/
NewPicture ::    Picture;
NewPicture = (True, 0);

EmptyPicture ::    Picture;
EmptyPicture = (False, 0);

CreatePicture :: !XPicture -> Picture;
CreatePicture p =  (True, p);

MakeXPicture :: !Picture -> XPicture;
MakeXPicture (b,p) =  p;

NewXPicture :: !Int -> XPicture;
NewXPicture p =  p;

StartDrawing :: !XPicture -> XPicture;
StartDrawing p =  StartXDrawing p;

EndDrawing :: !XPicture -> XPicture;
EndDrawing p =  EndXDrawing p;


/* Rules setting the attributes of a Picture
*/
SetPenSize :: !PenSize !Picture -> Picture;
SetPenSize (width,height) (b,p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=PenSizeX width /* RWS */ height p;
		
	};

SetPenMode :: !PenMode !Picture -> Picture;
SetPenMode mode (b,p)
	#!
		strict1=SetPenMode` mode p;
	=
		(b, strict1);

SetPenMode` :: !PenMode !XPicture -> XPicture;  
SetPenMode` CopyMode     p =  PenModeX 3 p;
SetPenMode` OrMode       p =  PenModeX 7 p;
SetPenMode` XorMode      p =  PenModeX 6 p;
SetPenMode` ClearMode    p =  PenModeX 4 p;
SetPenMode` NotCopyMode  p =  PenModeX 12 p;
SetPenMode` NotOrMode    p =  PenModeX 13 p;
SetPenMode` NotXorMode   p =  PenModeX 9 p;
SetPenMode` NotClearMode p =  PenModeX 1 p;
SetPenMode` HiliteMode   p =  PenModeX 6 p;

SetPenPattern :: !PenPattern !Picture -> Picture;
SetPenPattern pattern (b,p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=SetPenPattern` pattern p;
		
	};

SetPenPattern` :: !PenPattern !XPicture -> XPicture;
SetPenPattern` BlackPattern  p =  PenPatternX 100 p;
SetPenPattern` DkGreyPattern p =  PenPatternX 75 p;
SetPenPattern` GreyPattern   p =  PenPatternX 50 p;
SetPenPattern` LtGreyPattern p =  PenPatternX 25 p;
SetPenPattern` WhitePattern  p =  PenPatternX 0 p;

SetPenNormal :: !Picture -> Picture;
SetPenNormal p
   =  SetPenSize (1,1) (SetPenMode CopyMode (SetPenPattern BlackPattern p));

SetPenColour :: !Colour !Picture -> Picture;
SetPenColour (RGB r g b) (bo,p) #!
		strict1=strict1;
		=
		(bo, strict1);
	where {
	strict1=RGBForegroundColorX r g b p;
		
	};
SetPenColour c (b,p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=ForegroundColorX (ConvertColour c) p;
		
	};

SetBackColour :: !Colour !Picture -> Picture;
SetBackColour (RGB r g b) (bo,p) #!
		strict1=strict1;
		=
		(bo, strict1);
	where {
	strict1=RGBBackgroundColorX r g b p;
		
	};
SetBackColour c (b,p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=BackgroundColorX (ConvertColour c) p;
		
	};

ConvertColour :: !Colour -> Int;
ConvertColour BlackColour   =  GetColorX 1;
ConvertColour WhiteColour   =  GetColorX 2;
ConvertColour RedColour     =  GetColorX 3;
ConvertColour GreenColour   =  GetColorX 4;
ConvertColour BlueColour    =  GetColorX 5;
ConvertColour CyanColour    =  GetColorX 6;
ConvertColour MagentaColour =  GetColorX 7;
ConvertColour YellowColour  =  GetColorX 8;

SetFont :: !Font !Picture -> Picture;
SetFont (info,name,style,size) (b,p) #!
		strict1=strict1;
		=
		(b,strict1);
	where {
	strict1=SetFontX p info name style size;
		
	};

SetFontName :: !FontName !Picture -> Picture;
SetFontName name (b,p) #!
		strict1=strict1;
		=
		(b,strict1);
	where {
	strict1=SetFontNameX p (MakeLowerCase name);
		
	};

SetFontStyle :: ![FontStyle] !Picture -> Picture;
SetFontStyle style (b,p)
   #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=SetStyleX p (ConvertFontStyles (MakeLowerCases style) 
                                          (False,False,False,False,False));
		
	};

SetFontSize :: !FontSize !Picture -> Picture;
SetFontSize size (b,p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=SetSizeX p ( toString (10 * size)  +++ "*");
		
	};

PictureCharWidth :: !Char !Picture -> (!Int, !Picture);
PictureCharWidth c (b,p)
   =  (w,(b,p`));
      where {
      (w,p`)=: GetStringWidthX p (toString c);
      };

PictureStringWidth :: !String !Picture -> (!Int, !Picture);
PictureStringWidth s (b,p) 
   #!
		p`=p`;
		=
		(w,(b,p`));
      where {
      (w,p`)=: GetStringWidthX p s;
      };

PictureFontMetrics :: !Picture -> (!FontInfo, !Picture);
PictureFontMetrics (bo,p)
   #!
		p`=p`;
		=
		((a,b,c,d),(bo, p`));
      where {
      (a,b,c,d,p`)=: GetFontInfoX p;
      };

MovePenTo :: !Point !Picture -> Picture;
MovePenTo (h,v) (b, p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=MoveToX h v p;
		
	};

MovePen :: !Vector !Picture -> Picture;
MovePen (dh,dv) (b,p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=MoveRelativeX dh dv p;
		
	};

LinePenTo :: !Point !Picture -> Picture;
LinePenTo (h,v) (b,p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=LineToX h v p;
		
	};

LinePen :: !Vector !Picture -> Picture;
LinePen (dh,dv) (b,p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=LineRelativeX dh dv p;
		
	};

DrawChar :: !Char !Picture -> Picture;
DrawChar c (b,p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=DrawStringX (toString c) p;
		
	};

DrawString :: !String !Picture -> Picture;
DrawString s (b,p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=DrawStringX s p;
		
	};

DrawPoint :: !Point !Picture -> Picture;
DrawPoint (x,y) (b,p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=DrawPointX x y p;
		
	};

DrawCPoint :: !Point !Colour !Picture -> Picture;
DrawCPoint point colour p =  DrawPoint point (SetPenColour colour p);

DrawLine :: !Line !Picture -> Picture;
DrawLine ((x1,y1),(x2,y2)) (b,p) #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=DrawLineX x1 y1 x2 y2 p;
		
	};

DrawCLine :: !Line !Colour !Picture -> Picture;
DrawCLine line colour p =  DrawLine line (SetPenColour colour p);

DrawCurve :: !Curve !Picture -> Picture;
DrawCurve (rectangle, start,arc) (b,p)
   #!	strict2=RectangleToRect rectangle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=FrameArcX x1 y1 x2 y2 start (arc - start) p;
		=
		(b, strict1);

DrawCCurve :: !Curve !Colour !Picture -> Picture;
DrawCCurve curve colour p =  DrawCurve curve (SetPenColour colour p);

DrawRectangle :: !Rectangle !Picture -> Picture;
DrawRectangle rectangle (b,p) 
   #!
		strict2=RectangleToRect rectangle;
   #   (x1,y1,x2,y2)= strict2;
   #!
      strict1=FrameRectangleX x1 y1 x2 y2 p;
		=
		(b, strict1);

FillRectangle :: !Rectangle !Picture -> Picture;
FillRectangle rectangle (b,p)
   #!
		strict2=RectangleToRect rectangle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=PaintRectangleX x1 y1 x2 y2 p;
		=
		(b, strict1);

EraseRectangle :: !Rectangle !Picture -> Picture;
EraseRectangle rectangle (b,p)
   #!
		strict2=RectangleToRect rectangle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=EraseRectangleX x1 y1 x2 y2 p;
		=
		(b, strict1);

InvertRectangle :: !Rectangle !Picture -> Picture;
InvertRectangle rectangle (b,p) 
   #!
		strict2=RectangleToRect rectangle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=InvertRectangleX x1 y1 x2 y2 p;
		=
		(b, strict1);

MoveRectangleTo :: !Rectangle !Point !Picture -> Picture;
MoveRectangleTo rectangle (x`,y`) (b,p)
   #!
		strict2=RectangleToRect rectangle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=MoveRectangleX x1 y1 x2 y2 x` y` p;
		=
		(b, strict1);

MoveRectangle :: !Rectangle !Vector !Picture -> Picture;
MoveRectangle rectangle (xv,yv) (b,p)
    #!
		strict2=RectangleToRect rectangle;
    #
      (x1,y1,x2,y2)= strict2;
    #!
      strict1=MoveRectangleX x1 y1 x2 y2 (x1 + xv) (y1 + yv) p;
		=
		(b, strict1);

CopyRectangleTo :: !Rectangle !Point !Picture -> Picture;
CopyRectangleTo rectangle (x`,y`) (b,p)
   #!
		strict2=RectangleToRect rectangle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=CopyRectangleX x1 y1 x2 y2 x` y` p;
		=
		(b, strict1);

CopyRectangle :: !Rectangle !Vector !Picture -> Picture;
CopyRectangle rectangle (xv,yv) (b,p)
    #!
		strict2=RectangleToRect rectangle;
    #
      (x1,y1,x2,y2)= strict2;
    #!
      strict1=CopyRectangleX x1 y1 x2 y2 (x1 + xv) (y1 + yv) p;
		=
		(b, strict1);
      where {
		};

RectangleToRect :: !Rectangle -> Rect;
RectangleToRect ((x,y), (x`,y`))
   | lx && ly   =  (x ,y ,x`,y`);
   | lx   =  (x ,y`,x`,y );
   | ly   =  (x`,y ,x ,y`);
   =  (x`,y`,x ,y );
	where {
	ly=:y <= y`;
		
	lx=:x <= x`;
		};

DrawRoundRectangle :: !RoundRectangle !Picture -> Picture;
DrawRoundRectangle (((x1,y1),(x2,y2)),width,height) (b,p)
   #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=FrameRoundRectangleX x1 y1 x2 y2 width height p;
		
	}; 

FillRoundRectangle :: !RoundRectangle !Picture -> Picture;
FillRoundRectangle (((x1,y1),(x2,y2)),width,height) (b,p)
   #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=PaintRoundRectangleX x1 y1 x2 y2 width height p;
		
	};

EraseRoundRectangle :: !RoundRectangle !Picture -> Picture;
EraseRoundRectangle (((x1,y1),(x2,y2)),width,height) (b,p)
   #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=EraseRoundRectangleX x1 y1 x2 y2 width height p;
		
	};

InvertRoundRectangle :: !RoundRectangle !Picture -> Picture;
InvertRoundRectangle (((x1,y1),(x2,y2)),width,height) (b,p)
   #!
		strict1=strict1;
		=
		(b, strict1);
	where {
	strict1=InvertRoundRectangleX x1 y1 x2 y2 width height p;
		
	};

DrawOval :: !Oval !Picture -> Picture;
DrawOval oval (b,p)
   #!
		strict2=RectangleToRect oval;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=FrameOvalX x1 y1 x2 y2 p;
		=
		(b, strict1);

FillOval :: !Oval !Picture -> Picture;
FillOval oval (b,p)
   #!
		strict2=RectangleToRect oval;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=PaintOvalX x1 y1 x2 y2 p;
		=
		(b, strict1);

EraseOval :: !Oval !Picture -> Picture;
EraseOval oval (b,p)
   #!
		strict2=RectangleToRect oval;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=EraseOvalX x1 y1 x2 y2 p;
		=
		(b, strict1);

InvertOval :: !Oval !Picture -> Picture;
InvertOval oval (b,p)
   #!
		strict2=RectangleToRect oval;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=InvertOvalX x1 y1 x2 y2 p;
		=
		(b, strict1);

DrawCircle :: !Circle !Picture -> Picture;
DrawCircle circle (b,p)
   #!
		strict2=CircleToRect circle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=FrameOvalX x1 y1 x2 y2 p;
		=
		(b, strict1);

FillCircle :: !Circle !Picture -> Picture;
FillCircle circle (b,p)
   #!
		strict2=CircleToRect circle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=PaintOvalX x1 y1 x2 y2 p;
		=
		(b, strict1);

EraseCircle :: !Circle !Picture -> Picture;
EraseCircle circle (b,p)
   #!
		strict2=CircleToRect circle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=EraseOvalX x1 y1 x2 y2 p;
		=
		(b, strict1);

InvertCircle :: !Circle !Picture -> Picture;
InvertCircle circle (b,p)
   #!
		strict2=CircleToRect circle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=InvertOvalX x1 y1 x2 y2 p;
		=
		(b, strict1);

CircleToRect :: !Circle -> Rect;
CircleToRect ((m,n),r) =  RectangleToRect ((m - r, n - r), (m + r, n + r));

DrawWedge :: !Wedge !Picture -> Picture;
DrawWedge (rectangle, start, arc) (b,p)
   #!
		strict2=RectangleToRect rectangle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=DrawWedge` x1 y1 x2 y2 start arc p;
		=
		(b, strict1);

DrawWedge` :: !Int !Int !Int !Int !Int !Int !XPicture -> XPicture;
DrawWedge` px py qx qy s t p
   =  DrawLineX px` py` st`_x st`_y (
         DrawLineX sr`_x sr`_y px` py` (
         FrameArcX px py qx qy s (t - s) p));
      where {
      px`    =: px +  toInt rx ;               py`    =: py +  toInt ry ;
      sr`    =:  toReal s  / 180.0;            st`    =:  toReal t  / 180.0;
      cos_sr`=: cos rads;         sin_sr`=: sin rads;
      cos_st`=: cos radt;         sin_st`=: sin radt;
      sr`_x  =:  toInt (cos_sr` * rx)  + px`; sr`_y=: py` -  toInt (sin_sr` * ry) ;
      st`_x  =:  toInt (cos_st` * rx)  + px`; st`_y=: py` -  toInt (sin_st` * ry) ;
      rx     =:  toReal (qx - px)  / 2.0;
      ry     =:  toReal (qy - py)  / 2.0;
      rads=:(sr` * PI);
		radt=:(st` * PI);
		};

FillWedge :: !Wedge !Picture -> Picture;
FillWedge (rectangle, start, arc) (b,p) 
   #!
		strict2=RectangleToRect rectangle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=PaintArcX x1 y1 x2 y2 start (arc - start) p;
		=
		(b, strict1);

EraseWedge :: !Wedge !Picture -> Picture;
EraseWedge (rectangle, start, arc) (b,p)
   #!
		strict2=RectangleToRect rectangle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=EraseArcX x1 y1 x2 y2 start (arc - start) p;
		=
		(b, strict1);

InvertWedge :: !Wedge !Picture -> Picture;
InvertWedge (rectangle, start, arc) (b,p)
   #!
		strict2=RectangleToRect rectangle;
   #
      (x1,y1,x2,y2)= strict2;
   #!
      strict1=InvertArcX x1 y1 x2 y2 start (arc - start) p;
		=
		(b, strict1);

ScalePolygon :: !Int !Polygon -> Polygon;
ScalePolygon k (position, shape) =  (position, ScaleShape shape k);
        
ScaleShape :: !PolygonShape !Int -> PolygonShape;
ScaleShape [v : vs] k #!
		strict1=strict1;
		strict2=strict2;
		=
		[strict1 : strict2];
	where {
	strict1=ScaleVector k v;
		strict2=ScaleShape vs k;
		
	};
ScaleShape vs k =  vs;

ScaleVector :: !Int !Vector -> Vector;
ScaleVector k (v,w) #!
		strict1=strict1;
		strict2=strict2;
		=
		(strict1, strict2);
	where {
	strict1=k * v;
		strict2=k * w;
		
	};

MovePolygonTo :: !Point !Polygon -> Polygon;
MovePolygonTo p` (p, shape) =  (p`, shape);
    
MovePolygon :: !Vector !Polygon -> Polygon;
MovePolygon v (position, shape) =  (TranslatePoint position v, shape);

TranslatePoint :: !Point !Vector -> Point;
TranslatePoint (x,y) (v,w) #!
		strict1=strict1;
		strict2=strict2;
		=
		(strict1, strict2);
	where {
	strict1=x + v;
		strict2=y + w;
		
	};

DrawPolygon :: !Polygon !Picture -> Picture;
DrawPolygon ((x,y),poly) (b,p)
   #
      (poly`, n)= CreatePolygon poly;
   #!
      strict1=FreePolygonX poly` (FramePolygonX poly` n x y p);
		=
		(b, strict1);

FillPolygon :: !Polygon !Picture -> Picture;
FillPolygon ((x,y),poly) (b,p)
   #
      (poly`,n)= CreatePolygon poly;
   #!
      strict1=FreePolygonX poly` (PaintPolygonX poly` n x y p);
		=
		(b, strict1);

ErasePolygon :: !Polygon !Picture -> Picture;
ErasePolygon ((x,y),poly) (b,p)
   #
      (poly`,n)= CreatePolygon poly;
   #!
      strict1=FreePolygonX poly` (ErasePolygonX poly` n x y p);
		=
		(b, strict1);

InvertPolygon :: !Polygon !Picture -> Picture;
InvertPolygon ((x,y),poly) (b,p)
   #
      (poly`,n)= CreatePolygon poly;
   #!
      strict1=FreePolygonX poly` (InvertPolygonX poly` n x y p);
		=
		(b, strict1);

CreatePolygon :: ![Vector] -> (!Int,!Int);
CreatePolygon poly
   =  (CreatePolygon` (AllocatePolygonX n) poly 1 (0,0), n);
      where {
      n=: 2 +  ListLength poly ;
      };

CreatePolygon` :: !Int ![Vector] !Int !(!Int,!Int) -> Int;
CreatePolygon` p [(x,y) : vectors] i (x`,y`)
   =  CreatePolygon` (SetPolygonPointX p i x y) vectors (inc i) (x` + x,y + y`);
CreatePolygon` p vectors i (x,y) =  SetPolygonPointX p i (0 - x) (0 - y);

ListLength :: ![x] -> Int;
ListLength [a : rest] =  inc (ListLength rest);
ListLength xs =  0;


/* The functions implementing deltaFont-functions.
*/

SelectFont :: !FontName ![FontStyle] !FontSize -> (!Bool, !Font);
SelectFont name styles size 
	| select == 0
		=  (False, BestFontMatch name` fstyles size);
		=  (True,  (select, fname, fstyles, fsize));
    where {
      select =: SelectFontX (fname +++  fstyles +++ fsize );
      fname  =: "*" +++ name`;
      name`  =: MakeLowerCase name;
      fstyles=:  ConvertFontStyles (MakeLowerCases styles) 
                                     (False,False,False,False,False)  +++
                  "-*-*-";
      fsize  =:  toString (10 * size)  +++ "-*-*-*-*-*-*";
    };

BestFontMatch :: !FontName !FontStyle !FontSize -> Font;
BestFontMatch name style size 
	| select2 == 0
		=  (def,"*courier","-medium-r-normal-*-*-","120-*-*-*-*-*-*");
	| select1 == 0
		=  (select2,fname,style`,size`);
		=  (select1,fname,style,size`);
    where {
      fname  =: "*" +++ name;
      def    =: SelectDefaultFontX 0;
      select1=: SelectFontX (fname +++  style +++ size` );
      size`  =:  toString (FitBestFontSize (FontSizes name) size)  +++ "-*-*-*-*-*-*";
      select2=: SelectFontX (fname +++  style` +++ size` );
      style` =: "-medium-r-normal-*-*-";
    };

FitBestFontSize :: ![FontSize] !FontSize -> FontSize;
FitBestFontSize [size1 : sizes] size =  FitBestFontSize` size1 sizes size;
FitBestFontSize nosizes size =  size;

ABS :: !Int -> Int;
ABS x | x > 0 =  x;
         =  0 - x;

FitBestFontSize` :: !FontSize ![FontSize] !FontSize -> FontSize;
FitBestFontSize` bestfit [size1 : sizes] size
   | better =  FitBestFontSize` size1 sizes size;
   =  FitBestFontSize` bestfit sizes size;
      where {
      better=:  ABS (size1 - size)  <  ABS (bestfit - size) ;
      };
FitBestFontSize` bestfit nosizes size =  bestfit; 

ConvertFontStyles :: ![FontStyle] !FontStylesX -> FontStyle;
ConvertFontStyles ["normal" : styles]
                     (normal, bold, demibold, italic, condensed)
   =  ConvertFontStyles styles (True,bold,demibold,italic,condensed);
ConvertFontStyles ["bold" : styles]
                     (normal, bold, demibold, italic, condensed)
   =  ConvertFontStyles styles (normal,True,demibold,italic,condensed);
ConvertFontStyles ["demibold" : styles]
                     (normal, bold, demibold, italic, condensed)
   =  ConvertFontStyles styles (normal,bold,True,italic,condensed);
ConvertFontStyles ["italic" : styles]
                     (normal, bold, demibold, italic, condensed)
   =  ConvertFontStyles styles (normal,bold,demibold,True,condensed);
ConvertFontStyles ["condensed" : styles]
                     (normal, bold, demibold, italic, condensed)
   =  ConvertFontStyles styles (normal,bold,demibold,italic,True);
ConvertFontStyles [style : styles] stylesx
   =  ConvertFontStyles styles stylesx;
ConvertFontStyles nostyles styles =  ConvertFontStyles` styles;

ConvertFontStyles` :: !FontStylesX -> FontStyle;
ConvertFontStyles` (True,b,c,d,e)         =  "-medium-r-normal";
ConvertFontStyles` (a,True,b,False,False) =  "-bold-r-normal";
ConvertFontStyles` (a,True,b,True,False)  =  "-bold-i-normal";
ConvertFontStyles` (a,True,b,False,True)  =  "-bold-r-condensed";
ConvertFontStyles` (a,True,b,True,True)   =  "-bold-i-condensed";
ConvertFontStyles` (a,b,True,False,False) =  "-demibold-r-normal";
ConvertFontStyles` (a,b,True,True,False)  =  "-demibold-i-normal";
ConvertFontStyles` (a,b,True,False,True)  =  "-demibold-r-condensed";
ConvertFontStyles` (a,b,True,True,True)   =  "-demibold-i-condensed";
ConvertFontStyles` (a,b,c,True,False)     =  "-medium-i-normal";
ConvertFontStyles` (a,b,c,True,True)      =  "-medium-i-condensed";
ConvertFontStyles` (a,b,c,d,True)         =  "-medium-r-condensed";
ConvertFontStyles` x                      =  "-medium-r-normal";

Asciia     :== 97;
AsciiA     :== 65;
AsciiDelta :== 32;

MakeLowerCases :: ![String] -> [String];
MakeLowerCases [s : rest]
   #!
		strict1=strict1;
		strict2=strict2;
		=
		[strict1 : strict2];
	where {
	strict1=MakeLowerCase` s (size s);
		strict2=MakeLowerCases rest;
		
	};
MakeLowerCases s =  s;

MakeLowerCase :: !String -> String;
MakeLowerCase s =  MakeLowerCase` s (size s);

MakeLowerCase` :: !String !Int -> String;
MakeLowerCase` s 0 =  s;
MakeLowerCase` s n
   | ascii >= Asciia =  MakeLowerCase` s n`;
   =  MakeLowerCase` (s := (n`, (toChar (ascii + AsciiDelta)))) n`;
      where {
      ascii=: toInt (s.[n`]);
      n`   =: dec n;
      };

DefaultFont ::    (!FontName, ![FontStyle], !FontSize);
DefaultFont = ("courier",[],12);

FontNames ::    [FontName];
FontNames = FontNames` (GetNumberOfFontsX 0) [];

FontNames` :: !Int ![FontName] -> [FontName];
FontNames` index fonts
   | index == 0 =  fonts;
   #
      index`= dec index;
   #!
      strict1=GetFontNameX index`;
		=
		FontNames` index` [strict1 : fonts];

FontStyles :: !FontName -> [FontStyle];
FontStyles name =  FontStyles` (GetFontStylesX ("*" +++ name)) [];

FontStyles` :: !(!Int,!Int,!Int,!Int,!Int) ![FontStyle] -> [FontStyle];
FontStyles` (1,b,c,d,e) styles =  FontStyles` (0,b,c,d,e) ["normal"  :styles];
FontStyles` (0,1,c,d,e) styles =  FontStyles` (0,0,c,d,e) ["bold"    :styles];
FontStyles` (0,0,1,d,e) styles =  FontStyles` (0,0,0,d,e) ["demibold":styles];
FontStyles` (0,0,0,1,e) styles =  FontStyles` (0,0,0,0,e) ["italic"  :styles];
FontStyles` (0,0,0,0,1) styles =  ["condensed":styles];
FontStyles` (0,0,0,0,0) styles =  styles;

FontSizes :: !FontName -> [FontSize];
FontSizes name =  FontSizes` 0 (GetFontSizesX ("*" +++ name));

FontSizes` :: !Int !Int -> [FontSize];
FontSizes` i n
   | i < n #!
		strict1=strict1;
		strict2=strict2;
		=
		[strict1 : strict2];
   =  [];
	where {
	strict1= GetOneFontSizeX i  / 10;
		strict2=FontSizes` (inc i) n;
		
	};

FontCharWidth :: !Char !Font -> Int;
FontCharWidth c (font,name,styles,size) =  GetFontStringWidthX font (toString c);

FontCharWidths :: ![Char] !Font -> [Int];
FontCharWidths [c:r] font #!
		strict1=strict1;
		strict2=strict2;
		=
		[strict1 : strict2];
	where {
	strict1=FontCharWidth c font;
		strict2=FontCharWidths r font;
		
	};
FontCharWidths r font =  [];

FontStringWidth :: !String !Font -> Int;
FontStringWidth s (font,name,styles,size) =  GetFontStringWidthX font s;

FontStringWidths :: ![String] !Font -> [Int];
FontStringWidths [s:r] font
   #!
		strict1=strict1;
		strict2=strict2;
		=
		[strict1 : strict2];
	where {
	strict1=FontStringWidth s font;
		strict2=FontStringWidths r font;
		
	};
FontStringWidths r font =  [];

FontMetrics :: !Font -> FontInfo;
FontMetrics (font,name,styles,size) =  GetFontFontInfoX font;

