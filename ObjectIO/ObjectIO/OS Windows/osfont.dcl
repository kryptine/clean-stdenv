definition module osfont


//	Clean Object I/O library, version 1.2


from	StdOverloaded	import ==
import	StdString
from	ostoolbox		import OSToolbox


::	Font
::	OSFont
	=	{	osfontname	:: !String	// Name of the font
		,	osfontstyles:: !Int		// Style variations of the font
		,	osfontsize	:: !Int		// Point size of the font
		}
::	OSFontDef
	:==	(	!String					// Name of the font
		,	![String]				// Style variations of the font
		,	!Int					// Point size of the font
		)

instance == OSFont	// Equality on all fields

OSselectfont         :: !OSFontDef					!*OSToolbox -> (!Bool,!Font,           !*OSToolbox)
OSdefaultfont        ::								!*OSToolbox -> (      !Font,           !*OSToolbox)
OSdialogfont         ::								!*OSToolbox -> (      !Font,           !*OSToolbox)
OSfontgetdef         :: !Font                			        -> OSFontDef
OSfontgetimp         :: !Font               			        -> OSFont

OSfontnames          ::                  			!*OSToolbox -> (![String],             !*OSToolbox)
OSfontstyles         :: !String         			!*OSToolbox -> (![String],             !*OSToolbox)
OSfontsizes          :: !Int !Int !String			!*OSToolbox -> (![Int],                !*OSToolbox)

OSgetfontcharwidths  :: !Bool !Int ![Char]   !Font	!*OSToolbox -> (![Int],                !*OSToolbox)
OSgetfontstringwidths:: !Bool !Int ![String] !Font	!*OSToolbox -> (![Int],                !*OSToolbox)
OSgetfontmetrics     :: !Bool !Int           !Font	!*OSToolbox -> (!(!Int,!Int,!Int,!Int),!*OSToolbox)

/*
OSselectfont fontdef
	creates a font of the given name, style variations, and size in points.
	If successful, the Bool is True, and the Font contains a useful value.
	Otherwise, the Bool is False, and the Font is a dummy value.
OSdefaultfont 
	returns the default window text font.
OSdialogfont
	returns the default dialog text font.
OSfontgetdef
	returns the requested name, style variations, and size in points of the given font.
OSfontgetimp
	returns the internal representation of the font.

OSfontnames
	returns the set of names all currently available fonts.
OSfontstyles fontname
	returns the set of all currently available style variations for the font with the given
	fontname.
OSfontsizes x y fontname
	returns the set of all currently available sizes for the font with the given fontname 
	that lie between x and y (both inclusive).

OSgetfontcharwidths hdcPassed maybeHdc chars font
	returns the widths of all given chars in the same order of the given font.
OSgetfontstringwidths hdcPassed maybeHdc strings font
	returns the widths of all given strings in the same order of the given font.
OSgetfontmetrics hdcPassed maybeHdc font
	returns the (ascent,descent,leading,maxwidth) of the given font in that order.
*/
