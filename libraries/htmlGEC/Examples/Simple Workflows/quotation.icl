module quotation

import StdEnv, StdHtml, GenEq

// (c) 2007 MJP

// A task is given to user 0
// When finished the result of the task is reviewed by user 1
// He can comment on the task, or approve or cancel it
// When the result needs more work, the whole process is repeated
// Otherwise the task is completed
// The task itself in the example is a quotation form that needs to be filled in

derive gForm 	QForm, Review, Person, Gender
derive gUpd 	QForm, Review, Person, Gender
derive gParse 	QForm, Review, Person, Gender
derive gPrint 	QForm, Review, Person, Gender
derive gerda 	QForm, Review, Person, Gender


:: Persoonsgegevens
				=	{ naam :: String
					, e_mail :: String
					}
:: Verzendgegevens
				=	{ adres 	:: String
					, postcode 	:: String
					, plaats 	:: String
					}

//Start world = doHtmlServer (multiUserTask 2 (reviewtask <<@ Persistent)) world
Start world = doHtmlServer (multiUserTask 2 reviewtask) world

:: QForm = 	{ toComp 			:: String
			, startDate 		:: HtmlDate
			, endDate 			:: HtmlDate
			, estimatedHours 	:: Int
			, description		:: TextArea
			, price				:: Real 	
			}
<<<<<<< quotation.icl
:: ReviewState = Approved | Cancelled | NeedsRework TextArea | Draft

Quotation :: Task (QForm,ReviewState)
Quotation = taskToReview 1 (createDefault, mytask,createDefault)
where	mytask form = [Txt "Fill in Form:",Br,Br] ?>>
                      editTask "TaskDone" form <<@ Submit

taskToReview :: Int (a,a -> Task a,ReviewState) -> Task (a,ReviewState) |  iData a
taskToReview reviewer (form,task,state) = newTask "taskToReview" taskToReview`
where 
	taskToReview`	=					task form                =>> \form  ->
										reviewer @:: review form =>> \state	->	
										[Txt ("Reviewer " <+++ reviewer <+++ " says "),toHtml state,Br] ?>> 
										editTask "OK" Void       #>>
										case state of
											(NeedsRework _) -> taskToReview reviewer (form,task,state) 	
											else            -> return_V (form,state)

	review :: a -> Task ReviewState | gForm{|*|} a
	review form = [toHtml form,Br,Br] ?>>
							chooseTask
							[ ("Rework",	editTask "Done" (NeedsRework createDefault) <<@ Submit)
							, ("Approved",	return_V Approved)
							, ("Cancel",	return_V Cancelled)
							]
=======
::	Person = { firstName		:: String
			 , surname			:: String
			 , dateOfBirth		:: HtmlDate
			 , gender			:: Gender
			 }
::	Gender = Male | Female
:: Review = Approved | Rejected | NeedsRework TextArea

reviewtask :: Task (Person,Review)
reviewtask = taskToReview 1 (createDefault, mytask)

mytask :: a -> *TSt -> (a,TSt) | iData a
mytask v = [Txt "Fill in Form:",Br,Br] ?>> editTask "TaskDone" v <<@ Submit

taskToReview :: Int (a,a -> Task a) -> Task (a,Review) | iData a
taskToReview reviewer (v`,task) 
= newTask "taskToReview" (
	task v`               =>> \v ->
	reviewer @:: review v =>> \r ->
	[Txt ("Reviewer " <+++ reviewer <+++ " says "),toHtml r,Br] ?>> 
	buttonTask "OK" 
	case r of
		(NeedsRework _) -> taskToReview reviewer (v,task) 	
		else            -> return_V (v,r)
  )

review :: a -> Task Review | iData a
review v
=	[toHtml v,Br,Br] ?>>
	chooseTask
		[ ("Rework",  editTask "Done" (NeedsRework createDefault) <<@ Submit)
		, ("Approved",return_V Approved)
		, ("Reject",  return_V Rejected)
		]
>>>>>>> 1.19
