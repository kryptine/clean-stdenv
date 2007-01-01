module quotation

import StdEnv, StdHtml, GenEq

derive gForm 	QForm, ReviewState
derive gUpd 	QForm, ReviewState
derive gParse 	QForm, ReviewState
derive gPrint 	QForm, ReviewState
derive gerda 	QForm, ReviewState

derive gEq		ReviewState

Start world = doHtmlServer (multiUserTask 2 Quotation) world
//Start world = doHtmlServer (multiUserTask 2 (Quotation <<@ Persistent)) world

:: QForm = 	{ fromComp 			:: String
			, toComp 			:: String
			, startDate 		:: HtmlDate
			, endDate 			:: HtmlDate
			, estimatedHours 	:: Int
			, description		:: TextArea
			, price				:: Int
			}
:: ReviewState = Approved | Cancelled | NeedsRework | Draft

Quotation :: Task (QForm,ReviewState)
Quotation = reviewedTask 1 createDefault

reviewedTask :: Int (a,ReviewState) -> Task (a,ReviewState) | iData a
reviewedTask reviewer (form,state)
=					[Txt "Fill in Form:",Br,Br] 
					?>>	STask "TaskDone" form <<@ Submit
	=>> \form	->	reviewer @:: review (form,state)
	=>> \state	->	[Txt ("Reviewer " <+++ reviewer <+++ " says " <+++ printToString state),Br,Br] ?>> STask "OK" Void
	#>>				case state of
						NeedsRework	-> mkTask (reviewedTask reviewer (form,state)) 	
						else		-> returnV (form,state)
where
	review :: (a,ReviewState) -> Task ReviewState | iData a
	review (form,state) 
		= [toHtml form,Br,Br]?>>
			CTask_button
			[ ("Rework",	returnV NeedsRework)
			, ("Approved",	returnV Approved)
			, ("Cancel",	returnV Cancelled)
			]
