module quotation

import StdEnv, StdHtml, GenEq

derive gForm 	QForm, QState
derive gUpd 	QForm, QState
derive gParse 	QForm, QState
derive gPrint 	QForm, QState
derive gerda 	QForm, QState

derive gEq		QState

Start world = doHtmlServer (multiUserTask 2 [] (Quotation 1 createDefault)) world

:: QForm = 	{ fromComp 			:: String
			, toComp 			:: String
			, startDate 		:: HtmlDate
			, endDate 			:: HtmlDate
			, estimatedHours 	:: Int
			, description		:: TextArea
			, price				:: Int
			}
:: QState =	Submitted | Approved | Cancelled | NeedsRework | Draft
			
Quotation :: Int (QState,QForm) -> Task (QState,QForm)
Quotation reviewer (state,form)
=					[Txt "Fill in Form:",Br,Br] ?>> id (STask "Submit" form)
	=>> \form	->	reviewer @:: review (state,form)
	=>> \state	->	[Txt ("Reviewer " <+++ reviewer <+++ " says quotation " <+++ printToString state),Br,Br] ?>> STask "OK" Void
	#>>				case state of
						NeedsRework	-> mkTask (Quotation reviewer (state,form)) 	
						else		-> returnV (state,form)
where
	review :: (QState,QForm) -> Task QState
	review (state,form) 
		= [toHtml form,Br,Br]?>>
			CTask_button
			[ ("Rework",	returnV NeedsRework)
			, ("Approved",	returnV Approved)
			, ("Cancel",	returnV Cancelled)
			]
