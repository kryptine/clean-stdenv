module quotation

import StdEnv, StdHtml, GenEq

// (c) 2007 MJP

// A task is given to user 0
// When finished the result of the task is reviewed by user 1
// He can comment on the task, or approve or cancel it
// When the result needs more work, the whole process is repeated
// Otherwise the task is completed
// The task itself in the example is a quotation form that needs to be filled in

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

//Start world = doHtmlServer (multiUserTask 2 (Quotation <<@ Persistent)) world
Start world = doHtmlServer (multiUserTask 2 Quotation) world

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
						?>>	editTask "TaskDone" form <<@ Submit

taskToReview :: Int (a,a -> Task a,ReviewState) -> Task (a,ReviewState) | iData a
taskToReview reviewer (form,task,state) = newTask "taskToReview" taskToReview`
where 
	taskToReview`	=					task form
						=>> \form	->	reviewer @:: review form
						=>> \state	->	[Txt ("Reviewer " <+++ reviewer <+++ " says "),toHtml state,Br] ?>> editTask "OK" Void
						#>>				case state of
											(NeedsRework	_) 	-> taskToReview reviewer (form,task,state) 	
											else				-> return_V (form,state)

	review :: a -> Task ReviewState | iData a
	review form = [toHtml form,Br,Br]?>>
							chooseTask
							[ ("Rework",	editTask "Done" (NeedsRework createDefault) <<@ Submit)
							, ("Approved",	return_V Approved)
							, ("Cancel",	return_V Cancelled)
							]
