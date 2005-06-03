module main

import StdEnv, StdHtml

derive gForm  	[]
derive gUpd 	[]

Start world  = doHtml MyPage world

MyPage  hst
# (nval1,hst)	= mkEditForm (sFormId "edit2") 8 hst
# (nval2,hst)	= if (isEven nval1.value) (mkEditForm (sFormId "edit") [1..4] hst) (emptyForm,hst)
= mkHtml "Main Test Program"
	[ H1 [] "My Test"
	, toBody nval1,toBody nval2
	, Br
	, traceHtmlInput
	, Br
	] hst
	
emptyForm = {changed = False, value = [], form = [EmptyBody] }