module test

import StdEnv
import StdHtml

derive gUpd  []
derive gHGEC []

Start world  = doHtml MyPage world

submitscript :: String String -> Body
submitscript formname updatename
=	Script []
	(	" function toclean(inp)" +++
		" { document." +++
			formname  +++ "." +++
			updatename +++ ".value=inp.name+\"=\"+inp.value;" +++
			"document." +++ formname +++ ".submit(); }"
	)
	
globalstateform :: String String String Value-> Body
globalstateform formname updatename globalname globalstate
=	Form 	[ Frm_Name formname
			, Frm_Action MyPhP
			, Frm_Method Post
			]
			[ Input [ Inp_Name updatename
					, Inp_Type Hidden
					]
			, Input [ Inp_Name globalname
					, Inp_Type Hidden
					, Inp_Value globalstate
					]
			]		 

MyPage hst
= (Head 
		[ Hd_Title "Testing"]
		 
		[ submitscript    "CleanForm" "UD"
		, globalstateform "CleanForm" "UD" "GS" (SV "thisistheglobalstate") 

		
		, H1 "Counter Example"//, counter1
		, T "test"
		, Input [ Inp_Name 		 (encodeInfo "(1, UpdC \"Cons\")") //"rinus"
				, Inp_Type 		 Text
				, Inp_Value 	 (IV 23)
				, `Inp_ElemEvnts (OnChange "toclean(this)")
				]
		, Select 	[ Sel_Name ("ConsSelector")
					, `Sel_ElemEvnts (OnChange "toclean(this)")
					]
					[ Option "aap" [Opt_Value "ap"]
					, Option "noot" [Opt_Value "not"]
					, Option "mies" [Opt_Value "mies"]
					]
		 ,	Input 	[	Inp_Type Button
					, 	Inp_Value (SV "knopje")
					,	Inp_Name (encodeInfo "(2, UpdB Pressed)")
					,	Inp_Size 40  
					, 	`Inp_MouseEvnts (OnClick "toclean(this)")
					]
		, traceHtmlInput
		],hst)
