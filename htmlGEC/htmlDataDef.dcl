definition module htmlDataDef

// Clean ADT pendant for HTML  *** Under Construction ***
// (c) 2005 MJP

import htmlPrintUtil

derive gHpr Html	
derive gHpr Body	

:: Url			:== String
:: UniqueName 	:== String
:: NoAttr = NoAttr

None			:== [NoAttr]

// a Clean data structure representing a subset of html

:: Html 	= Head [Head_Attr] [Body]

:: Head_Attr = Hd_Base
			| Hd_Link
			| Hd_Meta
			| Hd_Script
			| Hd_Title String
										
:: Body 	= A 			[Link_Attr] 	Body	// link ancor
			| B  			String					// bold
			| Big 			String 					// bigger
			| Br									// break
			| C 			String 					// comment
			| Form 			[Form_Attr]	[Body] 		// form
			| H1 			String					// header 1
			| H2 			String					// header 2
			| H3 			String					// header 3
			| I  			String					// italic
			| Input 		[Inputs]				// inputs
			| P  			[Body]					// paragraph
			| PA 			Align [Body]			// paragraph aligned
			| Pre 			[Body] 					// literal
			| Script		[Script_Attr] String	// scripts
			| Select 		[Select_Attr] [Option]	// inputs
			| Small 		String 					// smaller
			| T  			String 					// plain text
			| Table			[Table_Attr] [[Body]]	// tables
			| Td 			[Td_Attr]   [Body]		// columns in a table
			| Tr 			[Tr_Attr] 	[Body]		// rows in a table
			| Tt 			String 					// teletype

			| Body			[Body]					// improves flexibility for code generation
			| EmptyBody								// same 


//:: Script	= Script		[Script_Attr] String	// script

:: Script_Attr
			= Scr_Type		Scr_Text
			| Scr_CharSet	CharSet
			| Scr_Defer		Defer
			| Scr_Language	Language
			| Scr_Src		Url


:: Scr_Text	= Scr_Text
:: CharSet  = CharSet		/* ???? */
:: Defer	= Defer
:: Language	= JavaScript
			| LiveScript
			| VbScript
			| Other

:: SScript :== String

:: FormElementEvents
			= OnChange		SScript					// run when element changes
			| OnSubmit		SScript					// run when form submitted
			| OnReset		SScript					// run when form is reset
			| OnSelect		SScript					// run when selected
			| OnBlur		SScript					// run when element loses focus
			| OnFocus		SScript					// run when element gets focus

:: KeyboardEvents
			= OnKeyDown		SScript					// run when key pressed
			| OnKeyPress	SScript					// run when key pressed and released
			| OnKeyUp		SScript					// run when key released

:: MouseEvents
			= OnClick		SScript					// run when mouse clicked
			| OnDClick		SScript					// run when mouse doubleclicked
			| OnMouseDown	SScript					// run when mouse button pressed
			| OnMouseMove	SScript					// run when mouse pointer moves
			| OnMouseOver	SScript					// run when mouse pointer moves over an element
			| OnMouseOut	SScript					// run when mouse pointer moves out of an element
			| OnMouseUp		SScript					// run when mouse button is released



// Here follow the type definitions in alphabetical order

:: Align	= Aln_Left 
			| Aln_Right 
			| Aln_Center
			| Aln_Justify
			| Aln_Char

:: Checked	= Checked			

:: ColorOption
			= `Col_RGB Int Int Int
			| `Col_XXX HexNum
			| `Col_Name ColorName

:: ColorName = Red

:: Disabled = Disabled

:: Form_Attr	= Frm_Action 		Url
			| Frm_Method 		GetOrPost
			| Frm_Name 			String
			| Frm_Target 		TargetOption
			| Frm_Style			String
			| `Frm_ElemEvnts	FormElementEvents

:: FrameOption
			= Frm_Void
			| Frm_Above
			| Frm_Below
			| Frm_Hsides
			| Frm_Lhs
			| Frm_Rhs
			| Frm_Vsides
			| Frm_Box
			| Frm_Border

:: GetOrPost = Get 
			|  Post

:: HeaderInfo 
			= Hdr_Col
			| Hdr_ColGroup
			| Hdr_Row
			| Hdr_RowGroup

:: Inputs 	= Inp_Type 			Types
			| Inp_Value 		Value
			| Inp_Name 			String
			| Inp_Checked		Checked
			| Inp_Size 			Int
			| Inp_Disabled		Disabled
			| Inp_ReadOnly		ReadOnly
			| `Inp_ElemEvnts	FormElementEvents
			| `Inp_MouseEvnts	MouseEvents
			| Inp_Width			String
			| Inp_Style			String

:: Link_Attr = Lnk_Href 		Url
			| Lnk_Target 		TargetOption

:: NoWrap	= NoWrap

:: Option	= Option String [Option_Attr] 

:: Option_Attr
			= Opt_Value			String
			| Opt_Selected		Selected
			| Opt_Disabled		Disabled
			| Opt_Style			String

:: RuleOption
			= Rul_None
			| Rul_Groups
			| Rul_Rows
			| Rul_Cols
			| Rul_All				

:: ReadOnly = ReadOnly

:: Selected = Selected

:: Select_Attr
			= Sel_Disable
			| Sel_Multiple
			| Sel_Name 			UniqueName
			| Sel_Size 			Int
			| Sel_Style			String
			| Sel_Disabled		Disabled
			| `Sel_ElemEvnts	FormElementEvents
			
:: Table_Attr = Tbl_Border 		Int
			| Tbl_CellPadding 	Int
			| Tbl_CellSpacing 	Int
			| Tbl_Width 		Int
			| Tbl_Frame 		FrameOption
			| Tbl_Rules 		RuleOption
			| Tbl_FrameAlign 	Align
			| Tbl_FrameBgColor 	ColorOption

:: TargetOption 
			= Trg__Blank 
			| Trg__Parent 
			| Trg__Self 
			| Trg__Top

:: Td_Attr	= Td_Align			Align			// aligment
			| Td_bgcolor		ColorOption		// backgroundcolor
			| Td_Char			Char			// which character to align to
			| Td_Charoff		Int				// ofsett to this character
			| Td_Height			Int 			// height of cell in pixels
			| Td_NoWrap			NoWrap			// enable automatic text wrapping
			| Td_Rowspan		Int				// number of rows to span
			| Td_Scope			HeaderInfo		// when this cell is used as header
			| Td_VAlign			VAlign			// vertical alignment
			| Td_Width			Int				// width of cell in pixels (depreciated)

:: Tr_Attr	= Tr_Align			Align
			| Tr_bgcolor		ColorOption
			| Tr_Char			Char
			| Tr_Charoff		Int
			| Tr_VAlign			VAlign 

:: Types	= Button
			| Checkbox
			| Hidden
			| Radio
			| Submit
			| Text

:: VAlign	= VAln_Top 
			| VAln_Middle 
			| VAln_Bottom
			| VAln_Baseline

:: Value	= SV String
			| IV Int
			| RV Real
			| BV Bool

:: HexNum = HexNum HN HN HN HN HN HN

:: HN = H_0 | H_1 | H_2 | H_3 | H_4 | H_5 | H_6 | H_7 | H_8 | H_9 | H_A | H_B | H_C | H_D | H_E | H_F


