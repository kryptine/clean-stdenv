implementation module stateHandling

import StdEnv, StdHtml, confIData

import loginAdmin

initManagerLogin :: Login
initManagerLogin	
= (mkLogin "root" "secret")

initManagerAccount :: Login -> ConfAccount
initManagerAccount	login 
= mkAccount login (ConfManager {ManagerInfo | person = RefPerson (uniquePerson login.loginName)})

initRefereeAccount :: Login -> ConfAccount
initRefereeAccount	login 
= mkAccount login (Referee 	{ person		= RefPerson (uniquePerson login.loginName)
							, conflicts		= Conflicts []
							, reports		= Reports []
							}) 

initAuthorsAccount :: Login Int -> ConfAccount
initAuthorsAccount login i
= mkAccount login (Authors 	{ nr			= i
							, paper			= RefPaper  (uniquePaper  login.loginName)
							, person		= RefPerson (uniquePerson login.loginName)
							})


initPaper :: String -> Paper
initPaper name
=	{ title			= "paper of " +++ name
	, first_author	= RefPerson (uniquePerson name)
	, co_authors	= []
	, abstract		= "type in abstract here"
	, pdf			= "download pdf here"
	}

initReport :: Report
initReport 
= 	{ recommendation	= StrongReject
	, familiarity 		= Low  
	, commCommittee		= TextArea 4 70 ""
	, commAuthors		= TextArea 10 70 "Please enter your report"
	}

isConfManager :: ConfAccount -> Bool
isConfManager account 
= case account.state of
	ConfManager _ -> True
	_ ->  False

getRefPerson :: Member -> (Refto Person)
getRefPerson (ConfManager managerInfo) 	= deRefPerson managerInfo.ManagerInfo.person
getRefPerson (Referee refereeInfo)		= deRefPerson refereeInfo.RefereeInfo.person							
getRefPerson (Authors paperInfo)		= deRefPerson paperInfo.PaperInfo.person

deRefPerson (RefPerson refto) = refto

invariantPerson :: Person -> Judgement
invariantPerson {firstName,lastName,affiliation,emailAddress}
| firstName		== "" 			= (False,"first name is not specified!")
| lastName		== "" 			= (False,"last name is not specified!")
| affiliation	== "" 			= (False,"affiliation is not specified!")
| emailAddress	== "" 			= (False,"email address is not specified!")
= OK

setInvariantAccounts :: ConfAccounts -> ConfAccounts
setInvariantAccounts confaccounts
	= map setInvariantAccount confaccounts

setInvariantAccount :: ConfAccount -> ConfAccount
setInvariantAccount account
=	case account.state of
		(ConfManager managerInfo) -> 
			{account & state = ConfManager {managerInfo & ManagerInfo.person = RefPerson (uniquePerson account.login.loginName)}}
		(Referee refereeInfo) ->
			{account & state = Referee {refereeInfo & RefereeInfo.person = RefPerson (uniquePerson account.login.loginName)}}
		(Authors paperInfo) ->
			{account & state = Authors {paperInfo & PaperInfo.person = RefPerson (uniquePerson account.login.loginName)}}


/*
FetchReports :: Int ConfAccounts -> ([(Person,Maybe Report)],*Hst)
FetchReports papernr accounts hst
	= [ (state.person,report) 	\\ 	{state = Referee info} <- accounts 
								, 	(paperdone, report) <- getReports info.reports
								|	paperdone == papernr ]

*/
/*
invariantAccounts :: ConfAccounts -> (Bool,String)
invariantAccounts accounts  
= invariant [(state,i) \\ state <- states & i <- [1..]]
where
	invariant [({person,reports,conflict},i):states]
	| person.firstName		== "" 			= error " : first name is not specified!"
	| person.lastName		== "" 			= error " : last name is not specified!"
	| person.affiliation	== "" 			= error " : affiliation is not specified!"
	| person.emailAddress	== "" 			= error " : email address is not specified!"
	| isAnyMember papernrs conflict			= error " : conflict with papers assigend!"
	= invariant states
	where
		papernrs = [papernr \\ (papernr,_) <- getReports reports]
		error message = (True,"record " <+++ i <+++ message)

getReports :: Reports -> [(PaperNr, Maybe Report)]
getReports (Reports report) = report

isRefereeOf :: Int ConfAccount -> Bool
isRefereeOf papernr state
	= foldr (||) False [ papertodo == papernr \\ (papertodo, _) <- getReports state.reports ]

hasRefereed :: Int ConfAccount -> Bool
hasRefereed papernr state
	= foldr (||) False [ papertodo == papernr \\ (papertodo,Just report) <- getReports state.reports ]

assignPaper :: Int ConfAccount -> ConfAccount
assignPaper i state
| isRefereeOf i state = state
= {state & reports = Reports [(i,Nothing):getReports state.reports]}

deletePaper :: Int ConfAccount -> ConfAccount
deletePaper i state = {state & reports = Reports [(j,report) \\ (j,report) <- getReports state.reports | j <> i]}

assignConflict :: Int ConfAccount -> ConfAccount
assignConflict i state
| isMember i state.conflict = state
= {state & conflict = [i:state.conflict]}

deleteConflict :: Int ConfAccount -> ConfAccount
deleteConflict i state = {state & conflict = [j \\ j <- state.conflict | j <> i]}

isConflict:: Int ConfAccount -> Bool
isConflict i state = isMember i  state.conflict

papersToReferee :: ConfAccount -> [PaperNr]
papersToReferee state = [nr \\ (nr,_) <- getReports state.reports]

papersRefereed :: ConfAccount -> [PaperNr]
papersRefereed state = [nr \\ (nr,Just _) <- getReports state.reports]

papersNotRefereed :: ConfAccount -> [PaperNr]
papersNotRefereed state = [nr \\ (nr,Nothing) <- getReports state.reports]

findReport :: Int ConfAccount -> (Maybe Report)
findReport i state 
| isRefereeOf i state =  (hd [report \\ (j,report) <- getReports state.reports | j == i])
= Nothing

addReport :: Int (Maybe Report) ConfAccount -> ConfAccount
addReport i new state
| isRefereeOf i state = {state & reports = Reports [(nr,if (nr==i) new old) \\ (nr,old) <- getReports state.reports ]}
= state

findPaper :: Int Papers -> (Maybe Paper)
findPaper i [] = Nothing
findPaper i [x=:{paperNr}:xs]
| i == paperNr = Just x
= findPaper i xs

	

*/








