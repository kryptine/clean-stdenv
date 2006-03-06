definition module stateHandling

import loginAdmin

:: ConferenceDB	:== [LoginState ConfState]
:: ConfState 	= 	{ role			:: Role  	
					, person		:: Person
					, reports		:: Reports 
					, conflict		:: [PaperNr]
					}
:: Role 		= 	ConfManager
				|	AuthorContact
				| 	Referee
:: Person 		=	{ firstName 	:: String
					, lastName		:: String
					, affiliation	:: String
					, emailAddress	:: String
					} 
:: Reports		= 	Reports [(PaperNr, Maybe Report)]
:: PaperNr 		:==	Int
:: Report		=	{ recommendation:: Recommendation
					, familiarity 	:: Familiarity 
					, commCommittee	:: CommCommittee
					, commAuthors	:: CommAuthors
					}
:: Recommendation 
				= 	StrongAccept
				| 	Accept
				| 	WeakAccept
				| 	WeakReject
				| 	Reject
				| 	StrongReject
:: Familiarity	= 	Expert
				| 	Knowledgeable
				| 	Low
:: CommCommittee:== TextArea 
:: CommAuthors	:==	TextArea 

:: Papers 		:==	[Paper]
:: Paper 		=	{ title			:: String
					, paperNr		:: Int
					, author		:: [Person]
					, abstract		:: String
					, pdf			:: String
					}
:: CurrPage 	= 	RootHomePage			// root pages
				| 	AssignPapers
				| 	AssignConflict
				| 	ModifyStates

				| 	ChangePassword			// shared pages 
				| 	ChangeInfo
				| 	ListPapers
				|	RefereeForm				

				| 	MemberHomePage			// member pages

initRootLogin 		:: (LoginStates ConfState)
initialRootState 	:: ConfState
initialRefereeState	:: Int -> ConfState
initPerson 			:: Int -> Person
initPaper  			:: Int String -> Paper

isManager			:: ConfState -> Bool
homePage 			:: Role -> CurrPage

findReports 		:: Int [ConfState] -> [(Person,Maybe Report)]
findReport 			:: Int ConfState -> (Maybe Report)
getReports 			:: Reports -> [(PaperNr, Maybe Report)]
addReport 			:: Int (Maybe Report) ConfState -> ConfState
emptyReport 		:: Report

assignPaper 		:: Int ConfState -> ConfState
deletePaper 		:: Int ConfState -> ConfState
isRefereeOf 		:: Int ConfState -> Bool
hasRefereed 		:: Int ConfState -> Bool
papersToReferee 	:: ConfState -> [PaperNr]
papersRefereed 		:: ConfState -> [PaperNr]
papersNotRefereed 	:: ConfState -> [PaperNr]

assignConflict 		:: Int ConfState -> ConfState
deleteConflict		:: Int ConfState -> ConfState
isConflict	 		:: Int ConfState -> Bool

findPaper 			:: Int Papers -> (Maybe Paper)

invariantConvDB 	:: ConferenceDB -> (Bool,String)
