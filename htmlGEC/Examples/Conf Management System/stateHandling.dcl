definition module stateHandling

import loginAdmin, htmlFormlib

// The Information to maintain:

:: ConfAccounts	:== [ConfAccount]
:: ConfAccount	:== Account Member

// Shared Information:

:: RefPerson	=	RefPerson   	(Refto Person)
:: RefPaper		=	RefPaper 		(Refto Paper)	
:: RefReport	=	RefReport		(Refto MaybeReport)
:: RefDiscussion=	RefDiscussion	(Refto Discussion)

// Information maintained by the Conference Manager

:: Member 		= 	ConfManager		ManagerInfo			
				|	Authors			PaperInfo							
				| 	Referee 		RefereeInfo
				|	Guest			Person
				
:: ManagerInfo	=	{ person		:: RefPerson
					}
:: PaperInfo	=	{ person		:: RefPerson
					, nr			:: PaperNr
					, paper			:: RefPaper
					, status		:: PaperStatus
					, discussion	:: RefDiscussion
					}
:: PaperNr 		:==	Int
:: PaperStatus	=	Accepted
				|	CondAccepted
				|	Rejected
				|	UnderDiscussion
				|	Submitted

:: RefereeInfo	=	{ person		:: RefPerson  
					, conflicts		:: Conflicts 
					, reports		:: Reports
					} 
:: Reports		=	Reports			[(PaperNr, RefReport)]

:: Conflicts	=	Conflicts 		[PaperNr]
 
// Information maintained by a referee

:: MaybeReport	:==	Maybe Report

:: Report		=	{ recommendation:: Recommendation
					, familiarity 	:: Familiarity 
					, commCommittee	:: CommCommittee
					, commAuthors	:: CommAuthors
					}
:: Recommendation 
				= 	StrongAccept
				| 	Accept
				| 	WeakAccept
				|	Discuss
				| 	WeakReject
				| 	Reject
				| 	StrongReject
:: Familiarity	= 	Expert
				| 	Knowledgeable
				| 	Low
:: CommCommittee:== String // TextArea 
:: CommAuthors	:==	String // TextArea 

// Information maintained by the Conference Manager *or* a Referee *or* an Author

:: Person 		=	{ firstName 	:: String
					, lastName		:: String
					, affiliation	:: String
					, emailAddress	:: String
					} 

// Information maintained by the Conference Manager *or* a Referee *or* an Author

:: Discussion	=	Discussion	[(String,String)]

// Information submitted by an author

:: Paper		=	{ title			:: String
					, first_author	:: Person
					, co_authors	:: Co_authors
					, abstract		:: String
					, pdf			:: String
					}
:: Co_authors 	=	Co_authors [Person]					

// Access functions on these data structures:

initManagerLogin 	:: Login
initManagerAccount 	:: Login 		-> ConfAccount

isConfManager 		:: ConfAccount	-> Bool
isReferee			:: ConfAccount -> Bool
isAuthor			:: ConfAccount -> Bool
isGuest				:: ConfAccount -> Bool

getRefPerson 		:: Member 		-> (Maybe RefPerson)

getPaperNumbers 	:: ConfAccounts -> [Int]
getRefPapers 		:: ConfAccounts -> [(Int,RefPaper)]
getPaperInfo 		:: Int ConfAccounts -> Maybe PaperInfo
getAssignments 		:: ConfAccounts -> [(RefPerson,[Int])]
getConflicts 		:: ConfAccounts -> [(RefPerson,[Int])]
getConflictsAssign	:: ConfAccounts -> [(RefPerson,[Int],[Int])]
getRefReports 		:: ConfAccounts -> [(Int,[(RefPerson, RefReport)])]
getMyRefReports 	:: ConfAccount ConfAccounts -> [(Int,[(RefPerson, RefReport)])]

getMyReports 		:: ConfAccount -> [(Int, RefReport)]
addMyReport 		:: (Int, RefReport) ConfAccount ConfAccounts -> ConfAccounts

hasAssignment 		:: Int RefPerson ConfAccounts -> Bool
hasConflict 		:: Int RefPerson ConfAccounts -> Bool

addAssignment 		:: Int RefPerson ConfAccounts -> ConfAccounts
removeAssignment 	:: Int RefPerson ConfAccounts -> ConfAccounts
addConflict 		:: Int RefPerson ConfAccounts -> ConfAccounts
removeConflict 		:: Int RefPerson ConfAccounts -> ConfAccounts


instance == RefPerson, RefPaper, RefReport

// invariants testing and setting

invariantConfAccounts 	:: String ConfAccounts 	-> Judgement
invariantPerson 		:: String Person 		-> Judgement
invariantPersons 		:: String [Person] 		-> Judgement
invariantPaper 			:: String Paper 		-> Judgement
invariantReport 		:: String Report 		-> Judgement

setInvariantAccounts 	:: ConfAccounts -> ConfAccounts


