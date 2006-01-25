definition module stateHandling

import loginAdmin

:: State =		{ role			:: Role  	// determines initial page when logged in
				, person		:: Person
				, papersref		:: [(PaperNr, Maybe Report)]
				, conflict		:: [PaperNr]
				}

:: Role 		= ConfManager
				| Referee

:: CurrPage 	= RootHomePage			// root pages
				| AssignPapers
				| AssignConflict
				| ModifyStates

				| ChangePassword		// shared pages 
				| ChangeInfo
				| ListPapers
				
				| MemberHomePage		// member pages

:: Person =		{ firstName 	:: String
				, lastName		:: String
				, affiliation	:: String
				, emailAddress	:: String
				} 
				
:: PaperNr :==	Int

:: Report =		{ recommendation:: Recommendation
				, familiarity 	:: Familiarity 
				, commCommittee	:: CommCommittee
				, commAuthors	:: CommAuthors
				}

:: Recommendation 
				= StrongAccept
				| Accept
				| WeakAccept
				| WeakReject
				| Reject
				| StrongReject
					
:: Familiarity	= Expert
				| Knowladgeble
				| Low

:: CommCommittee :== String

:: CommAuthors	:== String


:: Papers :==		[Paper]

:: Paper =		{ title			:: String
				, author		:: [Person]
				, abstract		:: String
				, pdf			:: String
				}


initRootLogin 		:: (LoginStates State)
initialRootState 	:: State
initialRefereeState	:: State
initPerson 			:: Person
initPaper  			:: String -> Paper

isManager		:: State -> Bool
homePage 		:: Role -> CurrPage

findReports 	:: Int [State] -> [(Person,Maybe Report)]

assignPaper 	:: Int State -> State
deletePaper 	:: Int State -> State
isRefereeOf 	:: Int State -> Bool
hasRefereed 	:: Int State -> Bool

assignConflict 	:: Int State -> State
deleteConflict	:: Int State -> State
isConflict	 	:: Int State -> Bool
