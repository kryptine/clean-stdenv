implementation module osfont


//	Clean Object I/O library, version 1.2


import	StdBool, StdEnum, StdReal
import	clCrossCall_12, pictCCall_12
from	clCCall_12		import WinMakeCString, WinGetCString, CSTR, WinGetVertResolution
from	StdPictureDef	import FontName, FontSize, FontStyle, BoldStyle, ItalicsStyle, UnderlinedStyle
from	commondef		import Error, IsBetween, minmax, StateMap


::	Font
	=	{	fontdef	:: !OSFontDef	// The font requested by the program
		,	fontimp	:: !OSFont		// The font selected  by the system
		}
::	OSFont
	=	{	osfontname	:: !String	// Name of the font
		,	osfontstyles:: !Int		// Style variations of the font
		,	osfontsize	:: !Int		// Size of the font
		}
::	OSFontDef
	:==	(	!String					// Name of the font
		,	![String]				// Style variations of the font
		,	!Int					// Point size of the font
		)

instance == OSFont where
	(==) :: !OSFont !OSFont -> Bool
	(==) f1 f2 = f1.osfontsize==f2.osfontsize && f1.osfontstyles==f2.osfontstyles && f1.osfontname==f2.osfontname

OSselectfont :: !OSFontDef !*OSToolbox -> (!Bool,!Font,!*OSToolbox)
OSselectfont fdef=:(fName,fStyles,fSize) tb
	= (True,{fontdef=fdef,fontimp=fimp},tb)
where
	fimp	= {osfontname=fName,osfontstyles=SStyle2IStyle fStyles,osfontsize=fSize}

OSdefaultfont :: !*OSToolbox -> (!Font,!*OSToolbox)
OSdefaultfont tb
	= ({fontdef=def,fontimp=imp},tb)
where
	def		= (name,styles,size)
	imp		= {osfontname=name,osfontstyles=SStyle2IStyle styles,osfontsize=size}
	name	= "Times"
	styles	= []
	size	= 10

OSdialogfont :: !*OSToolbox -> (!Font,!*OSToolbox)
OSdialogfont tb
	= ({fontdef=def,fontimp=imp},tb)
where
	def		= (name,styles,size)
	imp		= {osfontname=name,osfontstyles=SStyle2IStyle styles,osfontsize=size}
	name	= "MS Sans Serif"
	styles	= []
	size	= 8

OSfontgetdef :: !Font -> OSFontDef
OSfontgetdef {fontdef}
	= fontdef

OSfontgetimp :: !Font -> OSFont
OSfontgetimp {fontimp}
	= fontimp

SStyle2IStyle :: ![FontStyle] -> Int
SStyle2IStyle styles
	= s2i styles 0
where
	s2i []                         i = i
	s2i [ BoldStyle       : rest ] i = s2i rest (i bitor iBold)
	s2i [ ItalicsStyle    : rest ] i = s2i rest (i bitor iItalic)
	s2i [ UnderlinedStyle : rest ] i = s2i rest (i bitor iUnderline)
 	s2i [ _               : rest ] i = s2i rest i

IStyle2SStyle :: !Int -> [FontStyle]
IStyle2SStyle istyle
	= idtofontstyles` istyle [iBold,iItalic,iUnderline,iStrikeOut]
where
	idtofontstyles` :: !Int ![Int] -> [String]
	idtofontstyles` 0 _
		= []
	idtofontstyles` istyle [styleflag:styleflags]
		| notStyleFlag	= styles
		| otherwise		= [style:styles]
	where
		notStyleFlag	= istyle bitand styleflag == 0
		styles			= idtofontstyles` (istyle-styleflag) styleflags
		style			= if (styleflag==iBold)      BoldStyle
						 (if (styleflag==iItalic)    ItalicsStyle
						 (if (styleflag==iUnderline) UnderlinedStyle
						                             (Error "IStyle2SStyle" "osfont"
															"Fatal error: unmatched styleflag value ("+++toString styleflag+++")"
													 )))
	idtofontstyles` _ _
		= []


OSfontnames :: !*OSToolbox -> (![String], !*OSToolbox)
OSfontnames tb
	# GetFontNamesCci		= {ccMsg=CcRqGETFONTNAMES,p1=0,p2=0,p3=0,p4=0,p5=0,p6=0}
	# (_,unsortednames,tb)	= IssueCleanRequest FontnamesCallback GetFontNamesCci [] tb
	= (SortAndRemoveDuplicates unsortednames,tb)
where
	FontnamesCallback :: !CrossCallInfo ![FontName] !*OSToolbox -> (!CrossCallInfo,![String],!*OSToolbox)
	FontnamesCallback cci names os
		# (newname,os) = WinGetCString cci.p1 os
		= (Return0Cci,[newname:names],os)

SortAndRemoveDuplicates :: !u:[a] -> u:[a] | Ord a
SortAndRemoveDuplicates [e:es]
	= insert e (SortAndRemoveDuplicates es)
where
	insert:: a !u:[a] -> u:[a] | Ord a
	insert a list=:[b:x]
		| a<b		= [a:list]
		| a>b		= [b:insert a x]
		| otherwise	= list
	insert a _
		= [a]
SortAndRemoveDuplicates _
	= []


OSfontstyles :: !String !*OSToolbox -> (![String],!*OSToolbox)
OSfontstyles fname tb
	= ([BoldStyle,ItalicsStyle,UnderlinedStyle],tb)

OSfontsizes :: !Int !Int !String !*OSToolbox -> (![Int],!*OSToolbox)
OSfontsizes between1 between2 fname tb
	# (textptr,tb)			= WinMakeCString fname tb
	  getFontSizesCci		= {ccMsg=CcRqGETFONTSIZES,p1=textptr,p2=0,p3=0,p4=0,p5=0,p6=0}
	# (_,unsortedsizes,tb)	= IssueCleanRequest FontSizesCallback getFontSizesCci [] tb
	= (SortAndRemoveDuplicates unsortedsizes,tb)
where
	(low,high)	= minmax between1 between2
	
	FontSizesCallback :: !CrossCallInfo ![FontSize] !*OSToolbox -> (!CrossCallInfo,![FontSize],!*OSToolbox)
	FontSizesCallback cci=:{p1=size,p2=0} sizes tb
		= (Return0Cci,newsizes,tb)
	where
		pts		= Height2Points size
		newsizes= if (IsBetween pts low high)
					 [pts:sizes]
					 sizes
	FontSizesCallback _ _ tb
		= (Return0Cci,[low..high],tb)

Height2Points :: !Int -> Int
Height2Points h
	= toInt points
where
	dpi		= toReal WinGetVertResolution
	phfactor= dpi / 72.0
	points	= toReal h / phfactor

/* XXX MW: probably not called anywhere
Points2Height :: !Int -> Int
Points2Height p
	= toInt height
where
	dpi		= toReal WinGetVertResolution
	phfactor= dpi / 72.0
	height	= toReal p * phfactor
*/

OSgetfontcharwidths :: !Bool !Int ![Char] !Font !*OSToolbox -> (![Int], !*OSToolbox)
OSgetfontcharwidths hdcPassed maybeHdc chars {fontimp={osfontname,osfontstyles,osfontsize}} tb
	= StateMap (\c tb->WinGetCharWidth c (osfontname,osfontstyles,osfontsize) (toInt hdcPassed) maybeHdc tb) chars tb

OSgetfontstringwidths :: !Bool !Int ![String] !Font !*OSToolbox -> (![Int], !*OSToolbox)
OSgetfontstringwidths hdcPassed maybeHdc strings {fontimp={osfontname,osfontstyles,osfontsize}} tb
	= StateMap (\s tb->WinGetStringWidth s (osfontname,osfontstyles,osfontsize) (toInt hdcPassed) maybeHdc tb) strings tb

OSgetfontmetrics :: !Bool !Int !Font !*OSToolbox -> (!(!Int,!Int,!Int,!Int),!*OSToolbox)
OSgetfontmetrics hdcPassed maybeHdc {fontimp={osfontname,osfontstyles,osfontsize}} tb
	# (ascent,descent,maxwidth,leading,tb) = WinGetFontInfo (osfontname,osfontstyles,osfontsize) (toInt hdcPassed) maybeHdc tb
	= ((ascent,descent,leading,maxwidth),tb)
