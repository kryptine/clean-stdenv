module test

import StdEnv
import StdHtml

derive gUpd   (,,), []
derive gPrint (,,)
derive gParse (,,)
derive gHpr   (,,)
derive gHGEC []

Start world  = doHtml MyPage world

MyPage hst
# (list,(listbody,hst)) = mkHGEC "list" id [1] hst
# (ni1,(counter1,hst)) 	= counterHGEC "first"   0 hst
# (ni2,(counter2,hst)) 	= counterHGEC "second"  0 hst
# (nf, (myform,hst))    = mkHGEC "addingcounters" id (ni1,ni2,ni1 + ni2) hst
= (Head 
		[ Hd_Title "Testing"
//			, `Hd_Script (Script [Scr_Language JavaScript] 
//						"globalstate = \"Hello World\") " )
		
		
		] 
		[ H1 "Counter Example"
		, T "test"
		, Form 	[Frm_Action MyPhP, Frm_Name "globalform", Frm_Method Post, Frm_Style "margin:0"] 
					[	Input	[	Inp_Type Hidden
								,	Inp_Value (SV "spelen")
								,	Inp_Name "globalinput"
								]

					 ]
		, listbody
		, counter1
		, counter2
		, myform
		, traceHtmlInput
		],hst)

// self contained counter

counterHGEC :: String Int HSt -> (Int,(Body,HSt))
counterHGEC name i hst 
# (nc, result) = mkHGEC name updCounter (toCounter i) hst
= (fromCounter nc, result)
where
	toCounter n = (n,down,up)

	fromCounter (n,_,_) = n

	updCounter (n,CHPressed,_)  = (n-1,down,up)
	updCounter (n,_,CHPressed) 	= (n+1,down,up)
	updCounter else 			= else

	up 		= CHButton defsize "+"
	down	= CHButton defsize "-"

