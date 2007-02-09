module quotation

import StdEnv, StdHtml, GenEq

derive gForm 	QForm, ReviewState
derive gUpd 	QForm, ReviewState
derive gParse 	QForm, ReviewState
derive gPrint 	QForm, ReviewState
derive gerda 	QForm, ReviewState


:: Persoonsgegevens
				=	{ naam :: String
					, e_mail :: String
					}
:: Verzendgegevens
				=	{ adres 	:: String
					, postcode 	:: String
					, plaats 	:: String
					}

Start world = doHtmlServer (multiUserTask 2 (Quotation <<@ Persistent)) world
//Start world = doHtmlServer (multiUserTask 2 (Quotation )) world

:: QForm = 	{ toComp 			:: String
			, startDate 		:: HtmlDate
			, endDate 			:: HtmlDate
			, estimatedHours 	:: Int
			, description		:: TextArea
			, price				:: Real	
			}
:: ReviewState = Approved | Cancelled | NeedsRework TextArea | Draft

Quotation :: Task (QForm,ReviewState)
Quotation = taskToReview 1 (createDefault, mytask,createDefault)
where	mytask form =	[Txt "Fill in Form:",Br,Br] 
						?>>	STask "TaskDone" form <<@ Submit

taskToReview :: Int (a,a -> Task a,ReviewState) -> Task (a,ReviewState) | iData a
taskToReview reviewer (form,task,state) = recTask "taskToReview" taskToReview`
where 
	taskToReview`	=					task form
						=>> \form	->	reviewer @:: review form
						=>> \state	->	[Txt ("Reviewer " <+++ reviewer <+++ " says "),toHtml state,Br] ?>> STask "OK" Void
						#>>				case state of
											(NeedsRework	_) 	-> taskToReview reviewer (form,task,state) 	
											else				-> returnV (form,state)

	review :: a -> Task ReviewState | iData a
	review form = [toHtml form,Br,Br]?>>
							CTask_button
							[ ("Rework",	STask "Done" (NeedsRework createDefault) <<@ Submit)
							, ("Approved",	returnV Approved)
							, ("Cancel",	returnV Cancelled)
							]
