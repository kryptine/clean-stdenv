module test

import StdEnv, ArgEnv, StdMaybe
import StdHtml


Start world  = print_to_stdout SimplePage world


FramesPage = Html (Head [] [Hd_Title "Simple Frames page test"])
		(Frameset []
		[Frame [Fra_Src "http://www.upv.es", Fra_Scrolling No]
		, Frame [Fra_Src "http://www.google.com", Fra_Noresize]
		, Frame [Fra_Src "prueba.html"]
		])

SimplePage = Html (Head [] [Hd_Title "Simple page test"])
		(Body [Batt_background "./back35.jpg"]
		[Center [] "PRUEBA DE HTML"
		, Br
		, A [Lnk_Href "http://www.google.com", Lnk_Target Trg__Blank] [Txt "Enlace a Google"]
		, Ins [Ins_Datetime "20041123"] "Insertado"
		, Script [Scr_Type TypeJavascript, Scr_Language JavaScript] "document.write('Hello World!')"
		, Br
		, Body_Object [Oba_ClassId "clsid:F08DF954-8592-11D1-B16A-00C0F0283628", `Oba_Std [Std_Id "Slider1"], Oba_Width 100, Oba_Height 50] [
		Param [Pam_Name "BorderStyle", Pam_Value (IV 1)],
		Param [Pam_Name "Enabled", Pam_Value (IV 1)],
		Param [Pam_Name "Min", Pam_Value (IV 0)],
		Param [Pam_Name "Max", Pam_Value (IV 10)]
		]
		, Q [] "PruebaQ!"
		, Hr []
		, Ol [Ola_Start 4, Ola_Type Lit_I] [Li [] [Txt "Inicion lista sin orden", Del [`Del_Std [Std_Id "com"]] "kk"],Li [Lia_Value 1] [Txt "Pruba de texto"], Dir [] [Li [Lia_Type Lit_I, Lia_Value 1] [Txt "primero"], Li [Lia_Type Lit_A, Lia_Value 2] [Txt "Segundo"]]]
		, Hr []
		, Img [Img_Src "planets.gif", Img_Usemap "#planets", /*Img_Width (Pixels 350), Img_Height (Percent 20),*/ Img_Border 1]
		, Map [Map_Name "planets", `Map_Std [Std_Id "planets"]]
		[Area [Are_Shape Sopt_Rect, Are_Coords "0,0,82,126", Are_Href "sun.htm", Are_Target Trg__Blank]
		, Area [Are_Shape Sopt_Circle, Are_Coords "90,58,3", Are_Href "mercury.htm", Are_Target Trg__Blank]]
		, Hr []
		, Dl [] [Dt [] [Txt "Prueba1", B [`Std_Attr [Std_Title "Prueba"],`Std_Events [OnClick "ScriptPrueba"]] "negrita"],Dt [] [Txt "Prueba2", I [] "Italic"], Dd [] [Txt "Desc2"],Dt [] [Txt "Prueba3"]]
		, Form [Frm_Action "prueba.php", Frm_Method Post, Frm_Name "FormPrueba", Frm_Target Trg__Blank] [Fieldset [] [Legend [Leg_Align Aln_Right] "Legend of Fieldset"
		, Select [Sel_Size 3] [Optgroup [Opg_Label "Opg1"],Option [] "Option 1"
		, Option [] "Option 2"
		, Option [] "Option 3"
		, Option [] "Option 4"
		, Optgroup [Opg_Label "Opg2"]
		, Option [] "Option 5"
		, Option [] "Option 6"
		, Option [] "Option 7"
		]
		, Hr []
		, Textarea [Txa_Cols 5, Txa_Rows 5, Txa_Disabled] "First text inside the test textarea"
		, Span [] [H1 [Hnum_Align Aln_Left] "Texto en H1", Br
		, H2 [Hnum_Align Aln_Center] "Texto en H2", Br
		, H3 [Hnum_Align Aln_Right] "Texto en H3", Br
		, H4 [Hnum_Align Aln_Right] "Texto en H4", Br]
		, H5 [Hnum_Align Aln_Center] "Texto en H5", Br
		, H6 [Hnum_Align Aln_Left] "Texto en H6"
		, Table [Tbl_Border 1] [Tr [] [Th [] "Head 1",Th [] "Head 2"],Tr [] [Td [] [Txt "Cont 1"],Td [] [Txt "Cont 2"]]]
		, Table [Tbl_Border 1] [THead [Tat_Align Aln_Center] [Tr [] [Td [Td_Colspan 2, Td_Bgcolor (`Colorname Red)] [Txt "Table Head"]]
		, TFoot [Tat_Align Aln_Right] [Tr [] [Td [Td_Colspan 2, Td_Bgcolor (`Colorname Yellow)] [B [] "Foot of the table"]]]]
		, TBody [Tat_Align Aln_Left] [Tr [] [Td [Td_Bgcolor (`Colorname Blue)] [Input [Inp_Type Inp_Text, Inp_Size 40] "Input 1"],Td [Td_Bgcolor (`HexColor (Hexnum H_2 H_F H_A H_6 H_6 H_1))] [Input [Inp_Type Inp_Text, Inp_Size 30] "Input 2"]]
		, Tr [] [Td [Td_Bgcolor (`Colorname Green)] [Input [Inp_Type Inp_Text, Inp_Size 20] "Input 3"],Td [Td_Bgcolor (`RGBColor (RGBColor 211 42 189))] [Input [Inp_Type Inp_Text, Inp_Size 40] "Input 4"]]]
		]
		, Input [Inp_Type Inp_Text, Inp_Size 40, Inp_Value (SV "Prueba"), Inp_ReadOnly ReadOnly] ""
		, Input [Inp_Type Inp_Reset, Inp_Size 400, Inp_Value (SV "Prueba")] ""
		, Br
		, Input [Inp_Type Inp_Checkbox, Inp_Name "Check1", Inp_Value (SV "Check1"), `Inp_Std [Std_Id "Check1"], Inp_Checked] ""
		, Label [Lbl_For "Check1"] "LabelCheck1"
		, Input [Inp_Type Inp_Checkbox, Inp_Name "Check2", Inp_Value (SV "Check2"), `Inp_Std [Std_Id "Check2"]] ""
		, Label [Lbl_For "Check2"] "LabelCheck2"
		, Hr []
		, Btn [Btn_Value "Value", Btn_Name "Name", Btn_Type Btn_Button, Btn_Disabled] "Click"
		, Div [Div_Align Alo_Right] [Pre [Pre_Width 6] [Txt " Probando     q de verdad lo ha   ce"]
		, P [] [Txt "Prueba     de ", B [`Std_Attr [Std_Id "TagB"]] "escritura", Big [] " en el HTML", I [] "y italica"]]
		, Br
		, Font [Fnt_Size 7, Fnt_Face "Arial", Fnt_Color (`RGBColor 	(RGBColor 0 121 59))] [Bdo [] "Texto Rtl"]]]
		, P [Pat_Align Aln_Right] [Txt "Prueba de escritura en el HTML2"]
		])

/*MyPage  = Head 
		[Hd_Title "rinus test"
		] 
		[ H1 "My Test"
		, Br
		, T "input received       : " , B GetArgs, Br 
		, T "input plain decoded  : " , B (convert GetArgs), Br 
		, T "this executable      : " , B Self, Br 
		, T "update				  : " , B update, Br 
		, T "new				  : " , B new, Br 
		, T "state				  : " , B state, Br 
		, T "who				  : " , B who, Br 
		, showClean mydata 
		,	Br
		,	showClean (updClean mydata)
//			, Form 	[Frm_Action "main.php", Frm_Name "myform", Frm_Method Post] 
//					[	showClean mydata 
//				,	Br
//				,	showClean (updClean mydata)
//	
//					]
//		, T "input analysed : " , showClean decodeInput, Br
		]
where
	mydata = Node Leaf 1 Leaf
	(update,new,state,who) = UpdateInfo
	convert s = mkString (urlDecode (mkList s))

body3  = Body [ P [B "Poep"] ]
body4  = Body  [ PA Aln_Center  [T "This text should be centered"]]
body5  = Body  [Td [Td_Width 5, Td_NoWrap NoWrap, Td_Char 'X', Td_Height 34 ] [T "wow"]]
body6  = Body [Table [Tbl_Border 2] [[T "1 ",T "2 "],[Body [T "3.1 ",T "3.2"],Form [] [T "4 "]]]]
*/
mypage = "http://www.cs.kun.nl/~rinus"

