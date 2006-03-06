implementation module stateHandling

import StdEnv, StdHtml

import loginAdmin

// state stored in login administration

initRootLogin :: (LoginStates ConfState)
initRootLogin 		= 	[(mkLogin "root" "secret",initialRootState)]

initialRootState :: ConfState
initialRootState	= 	{ role 		= ConfManager
						, person	= initPerson 0
						, reports	= Reports []
						, conflict	= []
						} 

initialRefereeState :: Int -> ConfState
initialRefereeState	i = 	{ role 		= Referee
							, person	= initPerson i
							, reports	= Reports []
							, conflict	= []
							} 

initPerson :: Int -> Person
initPerson i = 	{ firstName 	= "firstname" <+++ i
				, lastName		= "lastname" <+++ i
				, affiliation 	= "affiliation" <+++ i
				, emailAddress	= "emailaddress" <+++ i
				}

initPaper 	:: Int String -> Paper
initPaper i s =	{ title		= "paper " +++ s
				, author	= [initPerson i]
				, abstract	= "type in abstract here"
				, pdf		= "download pdf here"
				, paperNr	= i
				}

isManager :: ConfState -> Bool
isManager state = case state.role of
					ConfManager -> True
					_ ->  False

homePage :: Role -> CurrPage
homePage ConfManager 	= RootHomePage
homePage Referee		= MemberHomePage

findReports :: Int [ConfState] -> [(Person,Maybe Report)]
findReports papernr states 
	= [ (state.person,report) 	\\ 	state <- states 
								, 	(paperdone, report) <- getReports state.reports
								|	paperdone == papernr ]

emptyReport :: Report
emptyReport = 		{ recommendation	= StrongReject
					, familiarity 		= Low  
					, commCommittee		= TextArea 4 70 ""
					, commAuthors		= TextArea 10 70 "Please enter your report"
					}

getReports :: Reports -> [(PaperNr, Maybe Report)]
getReports (Reports report) = report

isRefereeOf :: Int ConfState -> Bool
isRefereeOf papernr state
	= foldr (||) False [ papertodo == papernr \\ (papertodo, _) <- getReports state.reports ]

hasRefereed :: Int ConfState -> Bool
hasRefereed papernr state
	= foldr (||) False [ papertodo == papernr \\ (papertodo,Just report) <- getReports state.reports ]

assignPaper :: Int ConfState -> ConfState
assignPaper i state
| isRefereeOf i state = state
= {state & reports = Reports [(i,Nothing):getReports state.reports]}

deletePaper :: Int ConfState -> ConfState
deletePaper i state = {state & reports = Reports [(j,report) \\ (j,report) <- getReports state.reports | j <> i]}

assignConflict :: Int ConfState -> ConfState
assignConflict i state
| isMember i state.conflict = state
= {state & conflict = [i:state.conflict]}

deleteConflict :: Int ConfState -> ConfState
deleteConflict i state = {state & conflict = [j \\ j <- state.conflict | j <> i]}

isConflict:: Int ConfState -> Bool
isConflict i state = isMember i  state.conflict

papersToReferee :: ConfState -> [PaperNr]
papersToReferee state = [nr \\ (nr,_) <- getReports state.reports]

papersRefereed :: ConfState -> [PaperNr]
papersRefereed state = [nr \\ (nr,Just _) <- getReports state.reports]

papersNotRefereed :: ConfState -> [PaperNr]
papersNotRefereed state = [nr \\ (nr,Nothing) <- getReports state.reports]

findReport :: Int ConfState -> (Maybe Report)
findReport i state 
| isRefereeOf i state =  (hd [report \\ (j,report) <- getReports state.reports | j == i])
= Nothing

addReport :: Int (Maybe Report) ConfState -> ConfState
addReport i new state
| isRefereeOf i state = {state & reports = Reports [(nr,if (nr==i) new old) \\ (nr,old) <- getReports state.reports ]}
= state

findPaper :: Int Papers -> (Maybe Paper)
findPaper i [] = Nothing
findPaper i [x=:{paperNr}:xs]
| i == paperNr = Just x
= findPaper i xs

invariantConvDB :: ConferenceDB -> (Bool,String)
invariantConvDB cdb  
= invariant [(state,i) \\ (_,state) <- cdb & i <- [1..]]
where
	invariant [] 							= invariantLogin cdb
	invariant [({person,reports,conflict},i):cdbs]
	| person.firstName		== "" 			= error " : first name is not specified!"
	| person.lastName		== "" 			= error " : last name is not specified!"
	| person.affiliation	== "" 			= error " : affiliation is not specified!"
	| person.emailAddress	== "" 			= error " : email address is not specified!"
	| isAnyMember papernrs conflict			= error " : conflict with papers assigend!"
	= invariant cdbs
	where
		papernrs = [papernr \\ (papernr,_) <- getReports reports]
		error message = (True,"record " <+++ i <+++ message)
	










