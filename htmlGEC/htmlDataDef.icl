implementation module htmlDataDef

// Clean ADT pendant for HTML
// (c) 2005 MJP


import StdEnv, ArgEnv
import htmlPrintUtil
import StdGeneric



gHpr{|Html|} prev (Head headtags body) 
									= prev <+> htmlAttrCmnd "head" None headtags 
									 <+> htmlAttrCmnd "body" [" background = back35.jpg "] body

gHpr{|Head_Attr|} prev (Hd_Title text) = prev <+> htmlAttrCmnd "title" None text
gHpr{|Head_Attr|} prev (`Hd_Script script) = prev <+ script


gHpr{|Body|} prev (A link body)  	= prev <+> htmlAttrCmnd "a" 		[link] 		body
gHpr{|Body|} prev (B text)  		= prev <+> htmlAttrCmnd "b" 		None		text
gHpr{|Body|} prev (Big text) 		= prev <+> htmlAttrCmnd "big" 		None		text
gHpr{|Body|} prev Br				= prev <+ "<br>"
gHpr{|Body|} prev (C text) 			= prev <+ "<!-- "  <+ text <+ " -->"
gHpr{|Body|} prev (Form tags body) 	= prev <+> htmlAttrCmnd "form" 		tags 	body
gHpr{|Body|} prev (H1 text) 		= prev <+> htmlAttrCmnd "h1" 		None		text
gHpr{|Body|} prev (H2 text) 		= prev <+> htmlAttrCmnd "h2" 		None		text
gHpr{|Body|} prev (H3 text) 		= prev <+> htmlAttrCmnd "h3" 		None		text
gHpr{|Body|} prev (I text)  		= prev <+> htmlAttrCmnd "i" 		None		text
gHpr{|Body|} prev (Input inputs)	= prev <+> htmlAttrCmnd "input"		inputs 		""
gHpr{|Body|} prev (P body)  		= prev <+> htmlAttrCmnd "p"  		None		body
gHpr{|Body|} prev (PA align body)  	= prev <+> htmlAttrCmnd "p" 		[align] 	body
gHpr{|Body|} prev (Pre body) 		= prev <+> htmlAttrCmnd "pre" 		None		body
gHpr{|Body|} prev (Small text) 		= prev <+> htmlAttrCmnd "small" 	None		text
gHpr{|Body|} prev (Select seltags options)	
									= prev <+> htmlAttrCmnd "select" 	seltags 	options
gHpr{|Body|} prev (Table tabtags table) 
									= prev <+> htmlAttrCmnd "table" 	tabtags (Body (mktable table))
	where
		mktable table 	= [Tr [] (mkrow rows) \\ rows <- table]	
		mkrow rows 		= [Td [Td_VAlign VAln_Top] [row] \\ row <- rows] 

gHpr{|Body|} prev (Td tdtags body)  = prev <+> htmlAttrCmnd "td" 	tdtags body
gHpr{|Body|} prev (Tr trtags body)  = prev <+> htmlAttrCmnd "tr" 	trtags body
gHpr{|Body|} prev (T text)  		= prev <+ text
gHpr{|Body|} prev (Tt text) 		= prev <+> htmlAttrCmnd "tt" 	None text

gHpr{|Body|} prev EmptyBody			= prev
gHpr{|Body|} prev (Body body)		= prev <+ body

derive gHpr Align
derive gHpr Checked
derive gHpr Disabled
derive gHpr Form_Attr
derive gHpr FrameOption
derive gHpr GetOrPost
derive gHpr HeaderInfo
derive gHpr Inputs
derive gHpr Link_Attr
derive gHpr NoWrap
derive gHpr Option_Attr
derive gHpr RuleOption
derive gHpr Selected
derive gHpr Select_Attr
derive gHpr Table_Attr
derive gHpr TargetOption
derive gHpr Td_Attr
derive gHpr Tr_Attr
derive gHpr Types
derive gHpr VAlign

derive gHpr Script_Attr
derive gHpr Scr_Text
derive gHpr CharSet
derive gHpr Defer
derive gHpr Language
derive gHpr FormElementEvents
derive gHpr KeyboardEvents
derive gHpr MouseEvents


gHpr{|Script|} prev (Script scriptattr script)= prev <+> htmlAttrCmnd  "script" scriptattr script


gHpr{|Option|} prev (Option string tags)= prev <+> htmlAttrCmnd  "option" tags string

//	gHpr{|SelectAttr|} prev Sel_Disable				= prev <+ htmlAttr "disable"  "disable"
//	gHpr{|SelectAttr|} prev Sel_Multiple 			= prev <+ htmlAttr "multiple" "multiple"
//	gHpr{|SelectAttr|} prev (Sel_Name uniquename) 	= prev <+ htmlAttr "name" 	 uniquename <+ consChanged
//	gHpr{|SelectAttr|} prev (Sel_Size i) 			= prev <+ htmlAttr "size" 	 i

//inputChanged = "onChange = \"javascript: window.document.location.href = valueOf(name).submit\""
//inputChanged = "onChange = \"javascript: window.document.location.href = ?(help);"

//document.(name of the form).(name of the element).value

gHpr{|Value|} prev (SV string)	= prev <+ "\"" <+ string <+ "\""			
gHpr{|Value|} prev (IV int)  	= prev <+ toString int  		
gHpr{|Value|} prev (RV real) 	= prev <+ toString real		
gHpr{|Value|} prev (BV bool) 	= prev <+ toString bool		


gHpr{|ColorOption|} prev option = prev <+ option

:: ColorName = Red

gHpr{|ColorName|} prev Red	= prev <+ "red"


gHpr{|NoAttr|} prev _ = prev 

