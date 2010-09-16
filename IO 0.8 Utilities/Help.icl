implementation module Help

import	StdString, StdInt, StdChar, StdBool, StdFile, StdArray, StdTuple, StdList
import	deltaSystem, deltaEventIO, deltaIOSystem, deltaWindow, deltaPicture, deltaFont, deltaIOState
    
::	InfoDef		:== (Int,Int,[InfoLine])
::	InfoLine	:== (InfoFontDef,Int,Int,String)
::	InfoFontDef	=	InfoFont Font Centred
				|	NoFont	 Centred
::	Centred		:== Bool
::	Fonts		:== (Font,Font,Font,Font)
::	Heights		:== (Int,Int)

HelpWdID		:== 30000
InfoFontName1	:== "Geneva"
InfoFontName2	:== "Helvetica"
InfoFontName3	:== "Times"
NormalSize1		:== 9
NormalSize2		:== 12
LargeSize1		:== 12
LargeSize2		:== 14
NormalStyle		:== []
BoldStyle		:== ["Bold"]
Margin			:== 8
AboutBegin		:== "\\About"
AboutEnd		:== "\\EndAbout"
HelpBegin		:== "\\Help"
HelpEnd			:== "\\EndHelp"
About			:== False
Help			:== True

//
//	General AboutDialog construction.
//

MakeAboutDialog :: String String (*s -> *((IOState *s) -> (*s,IOState *s))) *Files
				-> (DialogDef *s (IOState *s), *Files)
MakeAboutDialog appname infofile helpf files
	# ((xmax,ymax,text),files)	= ReadInfo About fonts AboutBegin AboutEnd infofile files
	  picture					= DrawAboutInfo nft (xmax,ymax,text)
	  aboutDialog				= AboutDialog appname ((0,0),(xmax,ymax)) picture (AboutHelp "Help" helpf)
	= (aboutDialog,files)
where
	fonts						= InfoFonts
	(nft,_,_,_)					= fonts

InfoFonts :: Fonts
InfoFonts
	= (	selectfont [(InfoFontName1,NormalSize1),(InfoFontName2,NormalSize2)] NormalStyle
	  ,	selectfont [(InfoFontName1,LargeSize1 ),(InfoFontName2,LargeSize2 )] NormalStyle
	  ,	selectfont [(InfoFontName1,NormalSize1),(InfoFontName2,NormalSize2)] BoldStyle
	  ,	selectfont [(InfoFontName1,LargeSize1 ),(InfoFontName2,LargeSize2 )] BoldStyle
	  )
where
	selectfont :: ![(String,Int)] ![FontStyle] -> Font
	selectfont [(fontname,size):preffonts] style
		# (found,font)	= SelectFont fontname style size
		| found
		= font
		| otherwise
		= selectfont preffonts style
	selectfont _ style
		= snd (SelectFont InfoFontName3 style NormalSize2)

/*	Reading and pre-processing of the file containing the about- and help-info. */

ReadInfo :: Bool Fonts String String String *Files -> ((Int,Int,[InfoLine]),*Files)
ReadInfo help fonts begin end filename files
	# (succes,file,files)	= fopen (ApplicationPath filename) FReadText files
	| not succes && help
	= ((x,y,lines),files)
	with
		(x,y,lines)		= ProcessInfoStrings fonts [errpref+++"could not be found."]
	| not succes
	= ((defaultx,defaulty,defaultlines),files)
	# (found,info,file)	= ReadInfoFile begin end file
	  (_,files)			= fclose file files
	| not found && help
	= ((x,y,lines),files)
	with
		(x,y,lines)		= ProcessInfoStrings fonts [errpref+++"does not contain help information."]
	| not found
	= ((defaultx,defaulty,defaultlines),files)
	| otherwise
	= ((x,y,lines),files)
	with
		(x,y,lines)		= ProcessInfoStrings fonts info
where
	(defaultx,defaulty,defaultlines)
						= ProcessInfoStrings fonts ["\\DThis is a Clean program."]
	errpref				= "The help file \'"+++filename+++"\' " 

ProcessInfoStrings :: Fonts [String] -> InfoDef
ProcessInfoStrings fonts=:(nft,lft,_,_) lines
	= (maxx1,maxy+Margin-lat,lines2)
where
	heights				= (nat+ndt+nld,lat+ldt+lld)
	(maxx,maxy,lines1)	= AddFontToInfoLines fonts heights 0 (Margin+lat) lines
	maxx1				= Margin+maxx+Margin
	lines2				= map (CenterInfoLine nft maxx1) lines1
	(nat,ndt,_,nld)		= FontMetrics nft
	(lat,ldt,_,lld)		= FontMetrics lft
	
	AddFontToInfoLines :: Fonts Heights Int Int [String] -> InfoDef
	AddFontToInfoLines fonts heights maxx maxy [line:rest]
		= (maxx1,maxy1,[(font,Margin,maxy,line1):rest1])
	where
		(font,wid,hgt,line1)= ParseInfoLine fonts heights line
		(maxx1,maxy1,rest1)	= AddFontToInfoLines fonts heights (max maxx wid) (maxy+hgt) rest
		
		ParseInfoLine :: Fonts Heights String -> (InfoFontDef,Int,Int,String)
		ParseInfoLine fonts=:(nft,lft,bft,dft) heights=:(nhgt,lhgt) line
			| linelen<2 || line.[0]<>'\\'
			= (NoFont False, FontStringWidth line nft, nhgt,line )
			| otherwise
			= (infofont, FontStringWidth line1 font, height,line1)
			with
				line1					= line%(2,linelen-1)
				(infofont,font,height)	= case (line.[1]) of
												'L' -> (InfoFont lft False, lft, lhgt)
												'b' -> (InfoFont bft False, bft, nhgt)
												'B' -> (InfoFont dft False, dft, lhgt)
												'c' -> (NoFont True       , nft, nhgt)
												'C' -> (InfoFont lft True , lft, lhgt)
												'd' -> (InfoFont bft True , bft, nhgt)
												'D' -> (InfoFont dft True , dft, lhgt)
												_   -> (NoFont False      , nft, nhgt)
		where
			linelen						= size line
	AddFontToInfoLines _ _ maxx maxy _
		= (maxx,maxy,[])
	
	CenterInfoLine :: Font Int InfoLine -> InfoLine
	CenterInfoLine nft maxx info=:(inft=:NoFont centered,x,y,line)
		| centered	= (inft,(maxx-FontStringWidth line nft)/2,y,line)
		| otherwise	= info
	CenterInfoLine nft maxx info=:(inft=:InfoFont font centered,x,y,line)
		| centered	= (inft,(maxx-FontStringWidth line font)/2,y,line)
		| otherwise	= info

ReadInfoFile :: String String *File -> (Bool,[String],*File)
ReadInfoFile begin end file
	# (begin_found,file)= FindInfoBegin begin file
	| not begin_found
	= (False,[],file)
	# (lines,file)		= ReadInfoUntil end file
	| otherwise
	= (True,lines,file)

FindInfoBegin :: String *File -> (Bool,*File)
FindInfoBegin begin file
	| sfend file	= (False,file)
	# (line,file)	= freadline file
	| isPrefixOf begin line
	= (True,file)
	| otherwise
	= FindInfoBegin begin file

ReadInfoUntil :: String *File -> ([String],*File)
ReadInfoUntil end file
	| sfend file
	= ([],file)
	# (line,file)	= freadline file
	| isPrefixOf end line
	= ([],file)
	# (lines,file)	= ReadInfoUntil end file
	| otherwise
	= ([StripNewline line:lines],file)


/*	The drawing of the about/help info. */

DrawAboutInfo :: Font InfoDef -> [DrawFunction]
DrawAboutInfo nft (xmax,ymax,lines)
	= [	SetFont		nft
	  ,	DrawInfo	nft 0 ymax lines
	  ]

DrawInfo :: Font Int Int [InfoLine] Picture -> Picture
DrawInfo nft top bot [(InfoFont font c,x,y,line):rest] pic
	| y>bot		= pic
	| y<top		= DrawInfo nft top bot rest pic
	| otherwise	= DrawInfo nft top bot rest (SetFont nft (DrawString line (SetFont font (MovePenTo (x,y) pic))))
DrawInfo nft top bot [(NoFont c,x,y,line):rest] pic
	| y>bot		= pic
	| y<top		= DrawInfo nft top bot rest pic
	| otherwise	= DrawInfo nft top bot rest (DrawString line (MovePenTo (x,y) pic))
DrawInfo _ _ _ _ pic
	= pic

//
//	The Help function.
//

ShowHelp :: String (IOState s) -> IOState s
ShowHelp infofile io
	# ((xmax,ymax,text),io)	= accFiles (ReadInfo Help fonts HelpBegin HelpEnd infofile) io
	  window				= FixedWindow HelpWdID (0,0) "Help" ((0,0),(xmax,ymax)) (UpdateHelpWd nft text) []
	= OpenWindows [window] io
where
	fonts					= InfoFonts
	(nft,_,_,_)				= fonts
	
	UpdateHelpWd :: Font [InfoLine] UpdateArea *s -> (*s,[DrawFunction])
	UpdateHelpWd nft lines areas s
		= (	s
		  ,	[	SetFont		nft
		  	,	RedrawAreas	nft lines areas
		  	]
		  )
	where
		RedrawAreas :: Font [InfoLine] UpdateArea Picture -> Picture
		RedrawAreas nft lines [area=:((l,t),(r,b)):rest] pic
			= RedrawAreas nft lines rest (DrawInfo nft (t-1) (b+40) lines pic)
		RedrawAreas _ _ _ pic
			= pic

/*	Support functions for the AboutDialog construction. */

isPrefixOf :: String String -> Bool
isPrefixOf prefix string
	| prefixlen>size string	= False
	| otherwise				= prefix==string%(0,prefixlen-1) 
where
	prefixlen				= size prefix

StripNewline :: String -> String
StripNewline string
	| string==""			= string
	| string.[last]<>'\n'	= string
	| otherwise				= string%(0,last-1)
where
	last					= size string-1
