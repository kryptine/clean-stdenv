implementation module systemsettings


import	StdPictureDef


/*	This module contains macro's to make the scrabble application platform customisable.
*/


//	For graphics:

//	Font information:

//font size			:== snd (SelectFont "MS Sans Serif" [] 8)
//letterfont		:== snd (SelectFont "Times" ["Bold"] 9)
//smallfont			:== snd (SelectFont "Small Fonts" [] 6)
font size			:== {SansSerifFontDef & fSize=8}
letterfont			:== {    SerifFontDef & fSize=9,fStyles=[BoldStyle]}
smallfont			:== {    SmallFontDef & fSize=6}

//	Background colour:

rbBackground		:==	LightGrey
