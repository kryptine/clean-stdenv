implementation module htmlStyleDef

import StdEnv, ArgEnv
import htmlPrintUtil, htmlDataDef
import StdGeneric

gHpr{|HtmlStyle|} prev (CSS styles)	= prev <+ styles 

gHpr{|Style|} prev (Style name attr) = prev <+> styleCmnd name attr

//Background Style
gHpr{|StyleOpt|} prev (Background opt)		  	= prev <+> styleAttrCmnd "background"				opt
gHpr{|StyleOpt|} prev (BgAttach opt) 			= prev <+> styleAttrCmnd "background-attachment"	opt
gHpr{|StyleOpt|} prev (BgColor opt)		 	 	= prev <+> styleAttrCmnd "background-color"			opt
gHpr{|StyleOpt|} prev (BgImage opt)				= prev <+> styleAttrCmnd "background-image"			opt
gHpr{|StyleOpt|} prev (BgPosition opt)  		= prev <+> styleAttrCmnd "background-position"		opt
gHpr{|StyleOpt|} prev (BgRepeat opt)			= prev <+> styleAttrCmnd "background-repeat"		opt
//Border Style	
gHpr{|StyleOpt|} prev (Border opt)	 	 		= prev <+> styleAttrCmnd "border"					opt
gHpr{|StyleOpt|} prev (BdBottom opt)		  	= prev <+> styleAttrCmnd "border-bottom"			opt
gHpr{|StyleOpt|} prev (BdBottomColor opt)		= prev <+> styleAttrCmnd "border-bottom-color"		opt
gHpr{|StyleOpt|} prev (BdBottomStyle opt)  		= prev <+> styleAttrCmnd "border-bottom-style"		opt
gHpr{|StyleOpt|} prev (BdBottomWidth opt)	  	= prev <+> styleAttrCmnd "border-bottom-width"		opt
gHpr{|StyleOpt|} prev (BdColor opt)		  		= prev <+> styleAttrCmnd "border-color"				opt
gHpr{|StyleOpt|} prev (BdLeft opt)			  	= prev <+> styleAttrCmnd "border-left"				opt
gHpr{|StyleOpt|} prev (BdLeftColor opt)			= prev <+> styleAttrCmnd "border-left-color"		opt
gHpr{|StyleOpt|} prev (BdLeftStyle opt) 	 	= prev <+> styleAttrCmnd "border-left-style"		opt
gHpr{|StyleOpt|} prev (BdLeftWidth opt)  		= prev <+> styleAttrCmnd "border-left-width"		opt
gHpr{|StyleOpt|} prev (BdRight opt)			  	= prev <+> styleAttrCmnd "border-right"				opt
gHpr{|StyleOpt|} prev (BdRightColor opt)		= prev <+> styleAttrCmnd "border-right-color"		opt
gHpr{|StyleOpt|} prev (BdRightStyle opt)  		= prev <+> styleAttrCmnd "border-right-style"		opt
gHpr{|StyleOpt|} prev (BdRightWidth opt) 	 	= prev <+> styleAttrCmnd "border-right-width"		opt
gHpr{|StyleOpt|} prev (BdStyle opt)		  		= prev <+> styleAttrCmnd "border-style"				opt
gHpr{|StyleOpt|} prev (BdTop opt)			  	= prev <+> styleAttrCmnd "border-top"				opt
gHpr{|StyleOpt|} prev (BdTopColor opt)			= prev <+> styleAttrCmnd "border-top-color"			opt
gHpr{|StyleOpt|} prev (BdTopStyle opt)  		= prev <+> styleAttrCmnd "border-top-style"			opt
gHpr{|StyleOpt|} prev (BdTopWidth opt)  		= prev <+> styleAttrCmnd "border-top-width"			opt
gHpr{|StyleOpt|} prev (BdWidth opt)		  		= prev <+> styleAttrCmnd "border-width"				opt
//Classification Style	
gHpr{|StyleOpt|} prev (ClassClear opt)			= prev <+> styleAttrCmnd "clear"					opt
gHpr{|StyleOpt|} prev (ClassCursor opt)			= prev <+> styleAttrCmnd "cursor"					opt
gHpr{|StyleOpt|} prev (ClassDisplay opt)		= prev <+> styleAttrCmnd "display"					opt
gHpr{|StyleOpt|} prev (ClassFloat opt)  		= prev <+> styleAttrCmnd "float"					opt
gHpr{|StyleOpt|} prev (ClassPos opt)  			= prev <+> styleAttrCmnd "position"					opt
gHpr{|StyleOpt|} prev (ClassVisibility opt)	 	= prev <+> styleAttrCmnd "visibility"				opt
//Dimension Style
gHpr{|StyleOpt|} prev (DimHeight opt)  			= prev <+> styleAttrCmnd "height"					opt
gHpr{|StyleOpt|} prev (DimLineHeight opt)		= prev <+> styleAttrCmnd "line-height"				opt
gHpr{|StyleOpt|} prev (DimMaxHeight opt) 	 	= prev <+> styleAttrCmnd "max-height"				opt
gHpr{|StyleOpt|} prev (DimMaxWidth opt)			= prev <+> styleAttrCmnd "max-width"				opt
gHpr{|StyleOpt|} prev (DimMinHeight opt) 	 	= prev <+> styleAttrCmnd "min-height"				opt
gHpr{|StyleOpt|} prev (DimMinWidth opt)  		= prev <+> styleAttrCmnd "min-width"				opt
gHpr{|StyleOpt|} prev (DimWidth opt)	 	 	= prev <+> styleAttrCmnd "width"					opt
//Font Style
gHpr{|StyleOpt|} prev (FntFont opt) 		 	= prev <+> styleAttrCmnd "font"						opt
gHpr{|StyleOpt|} prev (FntFamily opt) 	 		= prev <+> styleAttrCmnd "font-family"				opt
gHpr{|StyleOpt|} prev (FntSize opt) 	 		= prev <+> styleAttrCmnd "font-size"				opt
gHpr{|StyleOpt|} prev (FntSizeAdj opt) 	 		= prev <+> styleAttrCmnd "font-size-adjust"			opt
gHpr{|StyleOpt|} prev (FntStretch opt) 		 	= prev <+> styleAttrCmnd "font-stretch"				opt
gHpr{|StyleOpt|} prev (FntStyle opt) 		 	= prev <+> styleAttrCmnd "font-style"				opt
gHpr{|StyleOpt|} prev (FntVariant opt) 	 		= prev <+> styleAttrCmnd "font-variant"				opt
gHpr{|StyleOpt|} prev (FntWeight opt) 		 	= prev <+> styleAttrCmnd "font-weight"				opt
//Generated Content Style
gHpr{|StyleOpt|} prev (GenCounterIncr opt) 		= prev <+> styleAttrCmnd "counter-increment"		opt
gHpr{|StyleOpt|} prev (GenCounterReset opt)	 	= prev <+> styleAttrCmnd "counter-reset"			opt
gHpr{|StyleOpt|} prev (GenQuotes opt) 	 		= prev <+> styleAttrCmnd "quotes"					opt
//List and Marker Style
gHpr{|StyleOpt|} prev (LstStyle opt) 			= prev <+> styleAttrCmnd "list-style"				opt
gHpr{|StyleOpt|} prev (LstStyleImg opt) 		= prev <+> styleAttrCmnd "list-style-image"			opt
gHpr{|StyleOpt|} prev (LstStylePos opt)			= prev <+> styleAttrCmnd "list-style-position"		opt
gHpr{|StyleOpt|} prev (LstStyleType opt)		= prev <+> styleAttrCmnd "list-style-type"			opt
gHpr{|StyleOpt|} prev (MarketOffset opt)		= prev <+> styleAttrCmnd "market-offset"			opt
//Margin Style
gHpr{|StyleOpt|} prev (MrgMargin opt)			= prev <+> styleAttrCmnd "margin"					opt
gHpr{|StyleOpt|} prev (MrgBottom opt)			= prev <+> styleAttrCmnd "margin-bottom"			opt
gHpr{|StyleOpt|} prev (MrgLeft opt)				= prev <+> styleAttrCmnd "margin-left"				opt
gHpr{|StyleOpt|} prev (MrgRight opt)			= prev <+> styleAttrCmnd "margin-right"				opt
gHpr{|StyleOpt|} prev (MrgTop opt)				= prev <+> styleAttrCmnd "margin-top"				opt
//Outline Style
gHpr{|StyleOpt|} prev (OlnOutline opt)			= prev <+> styleAttrCmnd "outline"					opt
gHpr{|StyleOpt|} prev (OlnColor opt)			= prev <+> styleAttrCmnd "outline-color"			opt
gHpr{|StyleOpt|} prev (OlnStyle opt)			= prev <+> styleAttrCmnd "outline-style"			opt
gHpr{|StyleOpt|} prev (OlnWidth opt)			= prev <+> styleAttrCmnd "outline-width"			opt
//Padding Style
gHpr{|StyleOpt|} prev (PadPadding opt)			= prev <+> styleAttrCmnd "padding"					opt
gHpr{|StyleOpt|} prev (PadBottom opt)			= prev <+> styleAttrCmnd "padding-bottom"			opt
gHpr{|StyleOpt|} prev (PadLeft opt)				= prev <+> styleAttrCmnd "padding-left"				opt
gHpr{|StyleOpt|} prev (PadRight opt)			= prev <+> styleAttrCmnd "padding-right"			opt
gHpr{|StyleOpt|} prev (PadTop opt)				= prev <+> styleAttrCmnd "padding-top"				opt
//Positioning Style
gHpr{|StyleOpt|} prev (PosBottom opt)			= prev <+> styleAttrCmnd "bottom"					opt
gHpr{|StyleOpt|} prev (PosClip opt)				= prev <+> styleAttrCmnd "clip"						opt
gHpr{|StyleOpt|} prev (PosLeft opt)				= prev <+> styleAttrCmnd "left"						opt
gHpr{|StyleOpt|} prev (PosOverflow opt)			= prev <+> styleAttrCmnd "overflow"					opt
gHpr{|StyleOpt|} prev (PosRight opt)			= prev <+> styleAttrCmnd "right"					opt
gHpr{|StyleOpt|} prev (PosTop opt)				= prev <+> styleAttrCmnd "top"						opt
gHpr{|StyleOpt|} prev (PosVertAlign opt)		= prev <+> styleAttrCmnd "vertical-align"			opt
gHpr{|StyleOpt|} prev (PosZIndex opt)			= prev <+> styleAttrCmnd "z-index"					opt
//Table Style
gHpr{|StyleOpt|} prev (TblBdCollapse opt)		= prev <+> styleAttrCmnd "border-collapse"			opt
gHpr{|StyleOpt|} prev (TblBdSpacing opt)		= prev <+> styleAttrCmnd "border-spacing"			opt
gHpr{|StyleOpt|} prev (TblCaptSide opt)			= prev <+> styleAttrCmnd "caption-side"				opt
gHpr{|StyleOpt|} prev (TblEmptyCells opt)		= prev <+> styleAttrCmnd "empty-cells"				opt
gHpr{|StyleOpt|} prev (TblLayout opt)			= prev <+> styleAttrCmnd "table-layout"				opt
//Text Style
gHpr{|StyleOpt|} prev (TxtColor opt)			= prev <+> styleAttrCmnd "color"					opt
gHpr{|StyleOpt|} prev (TxtDirection opt)		= prev <+> styleAttrCmnd "direction"				opt
gHpr{|StyleOpt|} prev (TxtLetterSpacing opt)	= prev <+> styleAttrCmnd "letter-spacing"			opt
gHpr{|StyleOpt|} prev (TxtAlign opt)			= prev <+> styleAttrCmnd "text-align"				opt
gHpr{|StyleOpt|} prev (TxtDecoration opt)		= prev <+> styleAttrCmnd "text-decoracion"			opt
gHpr{|StyleOpt|} prev (TxtIndent opt)			= prev <+> styleAttrCmnd "text-indent"				opt
gHpr{|StyleOpt|} prev (TxtShadow opt)			= prev <+> styleAttrCmnd "text-shadow"				opt
gHpr{|StyleOpt|} prev (TxtTransform opt)		= prev <+> styleAttrCmnd "text-transform"			opt
gHpr{|StyleOpt|} prev (TxtUnicodeBidi opt)		= prev <+> styleAttrCmnd "unicode-bidi"				opt
gHpr{|StyleOpt|} prev (TxtWhiteSpace opt)		= prev <+> styleAttrCmnd "white-space"				opt
gHpr{|StyleOpt|} prev (TxtWordSpacing opt)		= prev <+> styleAttrCmnd "word-spacing"				opt


derive gHpr AlignOpt
derive gHpr AttachOpt
derive gHpr BackgroundOpt
derive gHpr BgPosOpt
derive gHpr BgHPosOpt
derive gHpr BgVPosOpt
derive gHpr BorderOpt
derive gHpr BorderStyle
derive gHpr BorderWidth
derive gHpr ClearOpt
derive gHpr CollapseOpt
derive gHpr ColorOpt
derive gHpr CursorOpt
derive gHpr DecoraOpt
derive gHpr DisplayOpt
derive gHpr FloatOpt
derive gHpr FntSizeOpt
derive gHpr FntSizeAdjOpt
derive gHpr FntStyleOpt
derive gHpr FntWeightOpt
derive gHpr FontOpt
derive gHpr ImageOpt
derive gHpr LayoutOpt
derive gHpr LstImgOpt
derive gHpr LstPosOpt
derive gHpr LstStyleOpt
derive gHpr LstTypeOpt
derive gHpr MarginOpt
derive gHpr MktOffsetOpt
derive gHpr OutlineOpt
derive gHpr OlnColorOpt
derive gHpr OlnStyleOpt
derive gHpr OlnWidthOpt
derive gHpr OverflowOpt
derive gHpr PosOpt
derive gHpr RepeatOpt
derive gHpr ShadowOpt
derive gHpr ShowHideOpt
derive gHpr SideOpt
derive gHpr StretchOpt
derive gHpr TransformOpt
derive gHpr UnicodeOpt
derive gHpr VariantOpt
derive gHpr VisibOpt
derive gHpr WhiteSpaceOpt
derive gHpr ZIndexOpt

gHpr{|ClipOpt|} prev Cli_Auto							=	prev  <+ " auto"
gHpr{|ClipOpt|} prev (Cli_Shape val1 val2 val3 val4)	=	prev  <+ " " <+ val1 <+ " " <+ val2 <+ " " <+ val3 <+ " " <+ val4

gHpr{|CounterOpt|} prev (Cto_Ident id)			=	prev  <+ " " <+ id
gHpr{|CounterOpt|} prev (Cto_IdentInt id val)	=	prev  <+ " " <+ id <+ " " <+ val
gHpr{|CounterOpt|} prev Cto_None				=	prev  <+ " none"

/*gHpr{|CursorOpt|} prev Cuo_Auto			=	prev  <+ " auto"
gHpr{|CursorOpt|} prev Cuo_Crosshair	=	prev  <+ " crosshair"
gHpr{|CursorOpt|} prev Cuo_Default		=	prev  <+ " default"
gHpr{|CursorOpt|} prev Cuo_Eresize		=	prev  <+ " e-resize"
gHpr{|CursorOpt|} prev Cuo_Help			=	prev  <+ " help"
gHpr{|CursorOpt|} prev Cuo_Move			=	prev  <+ " move"
gHpr{|CursorOpt|} prev Cuo_Neresize		=	prev  <+ " ne-resize"
gHpr{|CursorOpt|} prev Cuo_Nresize		=	prev  <+ " n-resize"
gHpr{|CursorOpt|} prev Cuo_Nwresize		=	prev  <+ " nw-resize"
gHpr{|CursorOpt|} prev Cuo_Pointer		=	prev  <+ " pointer"
gHpr{|CursorOpt|} prev Cuo_Seresize		=	prev  <+ " se-resize"
gHpr{|CursorOpt|} prev Cuo_Sresize		=	prev  <+ " s-resize"
gHpr{|CursorOpt|} prev Cuo_Swresize		=	prev  <+ " sw-resize"
gHpr{|CursorOpt|} prev Cuo_Text			=	prev  <+ " text"
gHpr{|CursorOpt|} prev (Cuo_Url url)	=	prev  <+ "\"" <+ url <+ "\""
gHpr{|CursorOpt|} prev Cuo_Wait			=	prev  <+ " wait"
gHpr{|CursorOpt|} prev Cuo_Wresize		=	prev  <+ " w-resize"

gHpr{|DisplayOpt|} prev Dio_Block		=	prev  <+ " block"
gHpr{|DisplayOpt|} prev Dio_Compact		=	prev  <+ " compact"
gHpr{|DisplayOpt|} prev Dio_Inline		=	prev  <+ " inline"
gHpr{|DisplayOpt|} prev Dio_InlineTbl	=	prev  <+ " inline-table"
gHpr{|DisplayOpt|} prev Dio_ListItem	=	prev  <+ " list-item"
gHpr{|DisplayOpt|} prev Dio_Marker		=	prev  <+ " marker"
gHpr{|DisplayOpt|} prev Dio_None		=	prev  <+ " none"
gHpr{|DisplayOpt|} prev Dio_RunIn		=	prev  <+ " run-in"
gHpr{|DisplayOpt|} prev Dio_Table		=	prev  <+ " table"
gHpr{|DisplayOpt|} prev Dio_TblRowG		=	prev  <+ " table-row-group"
gHpr{|DisplayOpt|} prev Dio_TblHeadG	=	prev  <+ " table-head-group"
gHpr{|DisplayOpt|} prev Dio_TblFootG	=	prev  <+ " table-foot-group"
gHpr{|DisplayOpt|} prev Dio_TblRow		=	prev  <+ " table-row"
gHpr{|DisplayOpt|} prev Dio_TblColumnG	=	prev  <+ " table-column-group"
gHpr{|DisplayOpt|} prev Dio_TblColumn	=	prev  <+ " table-column"
gHpr{|DisplayOpt|} prev Dio_TblCell		=	prev  <+ " table-cell"
gHpr{|DisplayOpt|} prev Dio_TblCaption	=	prev  <+ " table-caption"

gHpr{|FntSizeOpt|} prev Fso_Large			=	prev  <+ " large"
gHpr{|FntSizeOpt|} prev Fso_Larger			=	prev  <+ " larger"
gHpr{|FntSizeOpt|} prev (Fso_Length val)	=	prev  <+ " " <+ val
gHpr{|FntSizeOpt|} prev Fso_Medium			=	prev  <+ " medium"
gHpr{|FntSizeOpt|} prev (Fso_Percent val)	=	prev  <+ " " <+ val <+ "%"
gHpr{|FntSizeOpt|} prev Fso_Small			=	prev  <+ " small"
gHpr{|FntSizeOpt|} prev Fso_Smaller			=	prev  <+ " smaller"
gHpr{|FntSizeOpt|} prev Fso_XLarge			=	prev  <+ " x-large"
gHpr{|FntSizeOpt|} prev Fso_XXLarge			=	prev  <+ " xx-large"
gHpr{|FntSizeOpt|} prev Fso_XSmall			=	prev  <+ " x-small"
gHpr{|FntSizeOpt|} prev Fso_XXSmall			=	prev  <+ " xx-small"*/

gHpr{|LengthOpt|} prev (Lgt_Percent val)	=	prev  <+ " " <+ val <+ "%"
gHpr{|LengthOpt|} prev (Lgt_Pixel val)		=	prev  <+ " " <+ val <+ "px"

gHpr{|LineHOpt|} prev (Lho_Percent val)		=	prev  <+ " " <+ val <+ "%"
gHpr{|LineHOpt|} prev (Lho_Size val)		=	prev  <+ " " <+ val

gHpr{|LineSizeOpt|} prev (Lso_Length val)	=	prev  <+ " " <+ val <+ "pt"
gHpr{|LineSizeOpt|} prev Lso_Normal			=	prev  <+ " normal"
gHpr{|LineSizeOpt|} prev (Lso_Number val)	=	prev  <+ " " <+ val
gHpr{|LineSizeOpt|} prev (Lso_Percent val)	=	prev  <+ " " <+ val <+ "%"
/*
gHpr{|LstTypeOpt|} prev Lto_Armenian		=	prev  <+ " armenian"
gHpr{|LstTypeOpt|} prev Lto_Circle			=	prev  <+ " circle"
gHpr{|LstTypeOpt|} prev Lto_Cjk				=	prev  <+ " cjk-ideographic"
gHpr{|LstTypeOpt|} prev Lto_Decimal			=	prev  <+ " decimal"
gHpr{|LstTypeOpt|} prev Lto_DecimalLZ		=	prev  <+ " decimal-leading-zero"
gHpr{|LstTypeOpt|} prev Lto_Disc			=	prev  <+ " disc"
gHpr{|LstTypeOpt|} prev Lto_Georgian		=	prev  <+ " georgian"
gHpr{|LstTypeOpt|} prev Lto_Hebrew			=	prev  <+ " hebrew"
gHpr{|LstTypeOpt|} prev Lto_Hiragana		=	prev  <+ " hiragana"
gHpr{|LstTypeOpt|} prev Lto_HiraganaIroha	=	prev  <+ " hiragana-iroha"
gHpr{|LstTypeOpt|} prev Lto_Katakana		=	prev  <+ " katakana"
gHpr{|LstTypeOpt|} prev Lto_KatakanaIroha	=	prev  <+ " katakata-iroha"
gHpr{|LstTypeOpt|} prev Lto_LowerAlpha		=	prev  <+ " lower-alpha"
gHpr{|LstTypeOpt|} prev Lto_LowerGreek		=	prev  <+ " lower-greek"
gHpr{|LstTypeOpt|} prev Lto_LowerLatin		=	prev  <+ " lower-latin"
gHpr{|LstTypeOpt|} prev Lto_LowerRoman		=	prev  <+ " lower-roman"
gHpr{|LstTypeOpt|} prev Lto_None			=	prev  <+ " none"
gHpr{|LstTypeOpt|} prev Lto_Square			=	prev  <+ " square"
gHpr{|LstTypeOpt|} prev Lto_UpperAlpha		=	prev  <+ " upper-alpha"
gHpr{|LstTypeOpt|} prev Lto_UpperLatin		=	prev  <+ " upper-latin"
gHpr{|LstTypeOpt|} prev Lto_UpperRoman		=	prev  <+ " upper-roman"*/

gHpr{|MaxSizeOpt|} prev (Mas_Length val)	=	prev  <+ " " <+ val <+ "px"
gHpr{|MaxSizeOpt|} prev Mas_None			=	prev  <+ " none"
gHpr{|MaxSizeOpt|} prev (Mas_Percent val)	=	prev  <+ " " <+ val <+ "%"

gHpr{|QuotesOpt|} prev Qto_None							=	prev  <+ " none" 
gHpr{|QuotesOpt|} prev (Qto_Quotes val1 val2 val3 val4)	=	prev  <+ " \"" <+ val1 <+ "\" \"" <+ val2 <+ "\" \"" <+ val3 <+ "\" \"" <+ val4 <+ "\""
/*
gHpr{|RepeatOpt|} prev Rop_Repeat		=	prev  <+ " repeat" 
gHpr{|RepeatOpt|} prev Rop_RepeatX		=	prev  <+ " repeat-x" 
gHpr{|RepeatOpt|} prev Rop_RepeatY		=	prev  <+ " repeat-y" 
gHpr{|RepeatOpt|} prev Rop_NoRepeat		=	prev  <+ " no-repeat" */

gHpr{|SizeOpt|} prev Szo_Auto			=	prev  <+ " auto"
gHpr{|SizeOpt|} prev (Szo_Length val)	=	prev  <+ " " <+ val <+ "px"
gHpr{|SizeOpt|} prev (Szo_Percent val)	=	prev  <+ " " <+ val <+ "%"

gHpr{|SpacingOpt|} prev (Spo_Length val)	=	prev  <+ " " <+ val <+ "px"
gHpr{|SpacingOpt|} prev Spo_Normal			=	prev  <+ " normal"

gHpr{|VertAlignOpt|} prev Vao_Baseline		=	prev  <+ " baseline"
gHpr{|VertAlignOpt|} prev Vao_Bottom		=	prev  <+ " bottom"
gHpr{|VertAlignOpt|} prev (Vao_Length val)	=	prev  <+ " " <+ val <+ "px"
gHpr{|VertAlignOpt|} prev Vao_Middle		=	prev  <+ " middle"
gHpr{|VertAlignOpt|} prev (Vao_Percent val)	=	prev  <+ " " <+ val <+ "%"
gHpr{|VertAlignOpt|} prev Vao_Sub			=	prev  <+ " sub"
gHpr{|VertAlignOpt|} prev Vao_Super			=	prev  <+ " super"
gHpr{|VertAlignOpt|} prev Vao_TextBottom	=	prev  <+ " text-bottom"
gHpr{|VertAlignOpt|} prev Vao_TextTop		=	prev  <+ " text-top"
gHpr{|VertAlignOpt|} prev Vao_Top			=	prev  <+ " top"
//gHpr{|VertAlignOpt|} prev opt		=	prev  <+ opt

/*
gHpr{|StretchOpt|} prev Sto_Condensed	=	prev  <+ " condensed"
gHpr{|StretchOpt|} prev Sto_Expanded	=	prev  <+ " expanded"
gHpr{|StretchOpt|} prev Sto_ExtraCond	=	prev  <+ " extra-condensed"
gHpr{|StretchOpt|} prev Sto_ExtraExp	=	prev  <+ " extra-expanded"
gHpr{|StretchOpt|} prev Sto_Narrower	=	prev  <+ " narrower"
gHpr{|StretchOpt|} prev Sto_Normal		=	prev  <+ " normal"
gHpr{|StretchOpt|} prev Sto_SemiCond	=	prev  <+ " semi-condensed"
gHpr{|StretchOpt|} prev Sto_SemiExp		=	prev  <+ " semi-expanded"
gHpr{|StretchOpt|} prev Sto_UltraCond	=	prev  <+ " ultra-condensed"
gHpr{|StretchOpt|} prev Sto_UltraExp	=	prev  <+ " ultra-expanded"
gHpr{|StretchOpt|} prev Sto_Wider		=	prev  <+ " wider"

gHpr{|VariantOpt|} prev Vro_Normal		=	prev  <+ " normal"
gHpr{|VariantOpt|} prev Vro_Smallcaps	=	prev  <+ " small-caps"
*/
