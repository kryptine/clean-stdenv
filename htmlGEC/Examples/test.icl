module test

import StdEnv
import StdHtml

derive gUpd  []
derive gHGEC []

Start world  = doHtml MyPage world

derive gParse (,)

MyPage hst
# (list,(listbody,hst)) = mkHGEC "list" (Edit id) [1] hst
# (ni1,(counter1,hst)) 	= counterHGEC "first"   Set 0 hst
# (ni2,(counter2,hst)) 	= counterHGEC "second"  Set 0 hst
# (nf, (myform,hst))    = mkHGEC "addingcounters" Set (ni1,ni2,ni1 + ni2) hst
= (Head 
		[ Hd_Title "Testing"]
		 
		[ H1 "Counter Example"//, counter1
		, T "test"
		, listbody
		, counter1
		, counter2
		, myform
		, T "id??:" , T CheckUpdateId, Br
		, traceHtmlInput
		],hst)
where
//	changescript s = "\"document.globalform.GlobalName.value.submit();\""  //"document.forms.globalform.submit()"
//	changescript s = "\"alert(\'kijken of dit werkt\');\""  
	changescript s = "\"document.forms.globalform.submit();\""
//	changescript s = "\"document.forms.localform.submit();\""
//	changescript s = "\"alert(" +++ "SubmitClean (\"aap\")" +++ ");\""

//	changescript s = "\"document.forms.globalform.submit(SubMitClean(\"aap\"));\""
