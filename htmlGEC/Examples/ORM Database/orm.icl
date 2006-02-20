module orm

import StdHtml
import databaseIData

// iData definitions

Start world  = doHtmlServer myDB world

myDB hst
# (alert,hst)	= Alert (\_ -> (False,"")) hst	// reset Alert
# (dbform,hst)  = mkEditForm (Init,myDB) hst	// create an editor for the database
# (alert,hst)	= Alert id hst					// read out Alert
= mkHtmlB  "database" (OnLoadAlert alert.value)
	[ H1 [] "database Example: "
	, BodyTag dbform.form
	] hst	
where
	myDB	:: FormId MyDatabase
	myDB	= pFormId "myDB" Empty


test hst
# (dbform,hst)  = mkEditForm (Init,nFormId "text" (TextArea 4 30 "zal dit werken ???")) hst	// create an editor for the database
# (int,hst)		= mkEditForm (Init,nFormId "int" 0) hst
# (TextArea row col string) = dbform.value
= mkHtml  "database"
	[ H1 [] "database Example: "
	, BodyTag int.form
	, BodyTag dbform.form
	, Txt string
	] hst	
