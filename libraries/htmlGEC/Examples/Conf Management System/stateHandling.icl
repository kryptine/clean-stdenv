implementation module stateHandling

import StdEnv, StdHtml

import loginAdmin

// state stored in login administration

initRootLogin :: (LoginStates State)
initRootLogin 		= 	[(mkLogin "root" "secret",initialRootState)]


initialRootState :: State
initialRootState	= 	{ role 		= ConfManager
						, person	= initPerson
						, papersref	= []
						, conflict	= []
						} 

initialRefereeState :: State
initialRefereeState	= 	{ role 		= Referee
						, person	= initPerson
						, papersref	= []
						, conflict	= []
						} 


initPerson :: Person
initPerson = 	{ firstName 	= ""
				, lastName		= ""
				, affiliation 	= ""
				, emailAddress	= ""
				}

initPaper 	:: String -> Paper
initPaper s =	{ title		= "paper " +++ s
				, author	= [initPerson]
				, abstract	= "type in abstract here"
				, pdf		= "download pdf here"
				}

isManager :: State -> Bool
isManager state = case state.role of
					ConfManager -> True
					_ ->  False

homePage :: Role -> CurrPage
homePage ConfManager 	= RootHomePage
homePage Referee		= MemberHomePage

findReports :: Int [State] -> [(Person,Maybe Report)]
findReports papernr states 
	= [ (state.person,report) 	\\ 	state <- states 
								, 	(paperdone, report) <- state.papersref
								|	paperdone == papernr ]

isRefereeOf :: Int State -> Bool
isRefereeOf papernr state
	= foldr (||) False [ papertodo == papernr \\ (papertodo, _) <- state.papersref ]

hasRefereed :: Int State -> Bool
hasRefereed papernr state
	= foldr (||) False [ papertodo == papernr \\ (papertodo,Just report) <- state.papersref ]

assignPaper :: Int State -> State
assignPaper i state
| isRefereeOf i state = state
= {state & papersref = [(i,Nothing):state.papersref]}

deletePaper :: Int State -> State
deletePaper i state = {state & papersref = [(j,report) \\ (j,report) <- state.papersref | j <> i]}

assignConflict :: Int State -> State
assignConflict i state
| isMember i state.conflict = state
= {state & conflict = [i:state.conflict]}

deleteConflict :: Int State -> State
deleteConflict i state = {state & conflict = [j \\ j <- state.conflict | j <> i]}

isConflict:: Int State -> Bool
isConflict i state = isMember i  state.conflict