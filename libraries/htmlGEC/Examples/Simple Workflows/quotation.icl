module quotation

import StdEnv, StdHtml, GenEq

derive gForm 	QForm, QState
derive gUpd 	QForm, QState
derive gParse 	QForm, QState
derive gPrint 	QForm, QState
derive gerda 	QForm, QState

derive gEq		QState

Start world = doHtmlServer (multiUserTask 2 [] (Quotation createDefault)) world

:: QForm = 	{ fromComp 			:: String
			, toComp 			:: String
			, startDate 		:: HtmlDate
			, endDate 			:: HtmlDate
			, estimatedHours 	:: Int
			, description		:: TextArea
			, price				:: Int
			}
:: QState =	Submitted | Approved | Cancelled | NeedsRework | Draft
			
Quotation :: (QState,QForm) TSt -> ((QState,QForm),TSt)
Quotation (state,form) tst
# (form,tst) 	= ([Txt "Fill in Form:",Br,Br] ?>> id (STask "Submit" form)) tst
# (state,tst) 	= (1 @:: review (state,form)) tst
# (_,tst)		= ([Txt ("Quotation " <+++ printToString state),Br,Br] ?>> STask "OK" Void) tst
| state === NeedsRework	= mkTask (Quotation (state,form)) tst
= returnV (state,form) tst
where
	review (state,form) tst
		= ( [toHtml form,Br,Br]?>>
			CTask_button
			[ ("Rework",	returnV NeedsRework)
			, ("Approved",	returnV Approved)
			, ("Cancel",	returnV Cancelled)
			] )tst

