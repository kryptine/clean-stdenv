implementation module stateHandling

import StdEnv, StdHtml, confIData

import loginAdmin

initManagerLogin :: Login
initManagerLogin	
= (mkLogin "root" "secret")

initManagerAccount :: Login -> ConfAccount
initManagerAccount	login 
= mkAccount login (ConfManager {ManagerInfo | person = RefPerson (Refto "")})

instance == RefPerson
where
	(==) (RefPerson rp1) (RefPerson rp2) = rp1 == rp2
instance == RefPaper
where
	(==) (RefPaper rp1) (RefPaper rp2) = rp1 == rp2
instance == RefReport
where
	(==) (RefReport rp1) (RefReport rp2) = rp1 == rp2
instance == (Refto a)
where
	(==) (Refto rp1) (Refto rp2) = rp1 == rp2

isConfManager :: ConfAccount -> Bool
isConfManager account 
= case account.state of
	ConfManager _ -> True
	_ ->  False

isReferee:: ConfAccount -> Bool
isReferee account 
= case account.state of
	Referee _ -> True
	_ ->  False

isAuthor:: ConfAccount -> Bool
isAuthor account 
= case account.state of
	Authors _ -> True
	_ ->  False

getRefPerson :: Member -> RefPerson
getRefPerson (ConfManager managerInfo) 	= managerInfo.ManagerInfo.person
getRefPerson (Referee refereeInfo)		= refereeInfo.RefereeInfo.person							
getRefPerson (Authors paperInfo)		= paperInfo.PaperInfo.person

getRefPapers :: ConfAccounts -> [(Int,RefPaper)]
getRefPapers accounts = [(nr,refpapers) 
						\\ {state = Authors {nr,paper = refpapers}} <- accounts]

getPaperNumbers :: ConfAccounts -> [Int]
getPaperNumbers accounts = sort [nr \\ {state = Authors {nr}} <- accounts]

getAssignments :: ConfAccounts -> [(RefPerson,[Int])]
getAssignments accounts = [(person,map fst reportslist) 
						 \\ {state = Referee {person,reports = Reports reportslist}} 	<- accounts]

getRefReports :: ConfAccounts -> [(Int,[(RefPerson, RefReport)])]
getRefReports accounts = [(nr,	[ (person,refreports) 
								\\ {state = Referee {person,reports = Reports reportslist}} <- accounts
						 		, (rnr,refreports) 									<- reportslist
						 		| rnr == nr ])
							\\ nr <- getPaperNumbers accounts]


getMyRefReports :: ConfAccount ConfAccounts -> [(Int,[(RefPerson, RefReport)])]
getMyRefReports account accounts
# me = getRefPerson account.state 
=  [(i,reports) 			\\ (i,reports) <- getRefReports accounts
							| not (hasConflict i me [account])]

getMyReports :: ConfAccount -> [(Int, RefReport)]
getMyReports {state = Referee {reports = Reports allreports}} =  allreports
getMyReports _ = []

addMyReport :: (Int,RefReport) ConfAccount ConfAccounts -> ConfAccounts
addMyReport myreport acc=:{state = Referee refinfo=:{reports = Reports oldreports}} accounts 
# account = {acc & state = Referee {refinfo & reports = Reports (addreport myreport oldreports)}}
=  changeAccount account accounts
where
	addreport (i,mbrep)[] = [] 
	addreport (i,mbrep)[(j,oldrep):reps] 
	| i == j = [(i,mbrep):reps]
	= [(j,oldrep):addreport (i,mbrep) reps]

getConflicts :: ConfAccounts -> [(RefPerson,[Int])]
getConflicts accounts = [(person,nrs) 
						 \\ {state = Referee {person,conflicts = Conflicts nrs}} 		<- accounts]

getConflictsAssign :: ConfAccounts -> [(RefPerson,[Int],[Int])]
getConflictsAssign accounts = [(person,nrs,map fst reportslist) 
						 	\\ {state = Referee {person,conflicts = Conflicts nrs
							 ,  reports = Reports reportslist}} 						<- accounts]


hasAssignment :: Int RefPerson ConfAccounts -> Bool
hasAssignment nr sperson [] = False
hasAssignment nr sperson [acc=:{state = Referee ref}:accs] 
# person 			= ref.RefereeInfo.person
# (Reports reports) = ref.RefereeInfo.reports
| sperson == person = isMember nr (map fst reports)
hasAssignment nr sperson [acc:accs] = hasAssignment nr sperson accs

hasConflict :: Int RefPerson ConfAccounts -> Bool
hasConflict nr sperson [] = False
hasConflict nr sperson [acc=:{state = Referee ref}:accs] 
# person 				= ref.RefereeInfo.person
# (Conflicts conflicts) = ref.RefereeInfo.conflicts
| sperson == person = isMember nr conflicts
hasConflict nr sperson [acc:accs] = hasConflict nr sperson accs


addAssignment :: Int RefPerson ConfAccounts -> ConfAccounts
addAssignment nr sperson [] = []
addAssignment nr sperson [acc=:{state = Referee ref}:accs] 
# person 			= ref.RefereeInfo.person
# (Reports reports) = ref.RefereeInfo.reports
| sperson == person = [{acc & state  = Referee {ref & reports = Reports [(nr,RefReport (Refto "")):reports]}}:accs]
= [acc:addAssignment nr sperson accs]
addAssignment nr sperson [acc:accs] = [acc:addAssignment nr sperson accs]


removeAssignment :: Int RefPerson ConfAccounts -> ConfAccounts
removeAssignment nr sperson [] = []
removeAssignment nr sperson [acc=:{state = Referee ref}:accs] 
# person 			= ref.RefereeInfo.person
# (Reports reports) = ref.RefereeInfo.reports
| sperson == person = [{acc & state  = Referee {ref & reports = Reports (remove nr reports)}}:accs]
with
	remove nr [] = []
	remove nr [(pnr,report):reports]
	| nr == pnr = reports
	= [(pnr,report):remove nr reports]
= [acc:removeAssignment nr sperson accs]
removeAssignment nr sperson [acc:accs] = [acc:removeAssignment nr sperson accs]

addConflict :: Int RefPerson ConfAccounts -> ConfAccounts
addConflict nr sperson [] = []
addConflict nr sperson [acc=:{state = Referee ref}:accs] 
# person 				= ref.RefereeInfo.person
# (Conflicts conflicts) = ref.RefereeInfo.conflicts
| sperson == person = [{acc & state  = Referee {ref & conflicts = Conflicts [nr:conflicts]}}:accs]
= [acc:addConflict nr sperson accs]
addConflict nr sperson [acc:accs] = [acc:addConflict nr sperson accs]

removeConflict :: Int RefPerson ConfAccounts -> ConfAccounts
removeConflict nr sperson [] = []
removeConflict nr sperson [acc=:{state = Referee ref}:accs] 
# person 				= ref.RefereeInfo.person
# (Conflicts conflicts) = ref.RefereeInfo.conflicts
| sperson == person = [{acc & state  = Referee {ref & conflicts = Conflicts (remove nr conflicts)}}:accs]
with
	remove nr [] = []
	remove nr [cnr:conflicts]
	| nr == cnr = conflicts
	= [cnr:remove nr conflicts]
= [acc:removeConflict nr sperson accs]
removeConflict nr sperson [acc:accs] = [acc:removeConflict nr sperson accs]

invariantPerson :: String Person -> Judgement
invariantPerson id {firstName,lastName,affiliation,emailAddress}
| firstName		== ""	= (False,id +++ " : first name is not specified!")
| lastName		== ""	= (False,id +++ " : last name is not specified!")
| affiliation	== ""	= (False,id +++ " : affiliation is not specified!")
| emailAddress	== ""	= (False,id +++ " : email address is not specified!")
= OK

invariantPaper :: String Paper -> Judgement
invariantPaper id {title,first_author,co_authors = Co_authors authors,abstract,pdf}
| title			== ""	= (False,id +++ " : title of paper not specified!")
| abstract		== ""	= (False,id +++ " : no abstract of paper specified!")
| pdf			== ""	= (False,id +++ " : no pdf given!")
# judgementFirst_author	= invariantPerson (id +++ " first author") first_author
# judgementCo_authors	= foldl (+) OK (map (invariantPerson (id +++ " co_authors")) authors)
= judgementFirst_author + judgementCo_authors

invariantReport :: String Report -> Judgement
invariantReport id {commCommittee,commAuthors}
| commAuthors	== ""	= (False,id +++ " : You have to make some remarks to the authors")
| commCommittee	== ""	= (False,id +++ " : You have to make some remarks to the committee")
= OK

invariantConfAccounts :: ConfAccounts -> Judgement
invariantConfAccounts accounts 
# allpapernrs 		= [nr \\ (nr,refpapers) <- getRefPapers accounts]
| isMember 0 allpapernrs 					= (False,"paper number has to be a postive number")
| not (allUnique allpapernrs) 				= (False,"paper number already in use")
# uniqueconflicts 	= and [allUnique nrs \\ (_,nrs) <- getConflicts accounts] 
| not uniqueconflicts						= (False,"conflict already assigned to referee")
# uniqueassigment 	= and [allUnique nrs  \\ (_,nrs) <- getAssignments accounts] 
| not uniqueassigment						= (False,"paper already assigned to referee")
# uniqueassignconfl = or [isAnyMember conflnrs asnrs \\ (_,conflnrs,asnrs) <- getConflictsAssign accounts] 
| uniqueassignconfl							= (False,"paper assigned conflicts with conflict assigned")
# allreportnrs		= [nr \\ (_,nrs) <- getAssignments accounts, nr <- nrs]
| not (allMembers allreportnrs allpapernrs)	= (False,"assignment refers to non existing paper number")
# allconflicts		= [nr \\ (_,nrs) <- getConflicts accounts, nr <- nrs]
| not (allMembers allconflicts allpapernrs)	= (False,"conflict refers to non existing paper number")
= OK

allUnique :: [a] -> Bool | Eq a
allUnique list = length list == length (removeDup list)

allMembers [] list = True
allMembers [x:xs] list = isMember x list && allMembers xs list


setInvariantAccounts :: ConfAccounts -> ConfAccounts
setInvariantAccounts confaccounts
	= map setInvariantAccount confaccounts
where
	setInvariantAccount :: ConfAccount -> ConfAccount
	setInvariantAccount account
	=	case account.state of
			(ConfManager managerInfo) -> 
				{account & state = ConfManager 	{managerInfo 
												& ManagerInfo.person 	= RefPerson (Refto uniquename)}}
			(Referee refereeInfo) ->
				{account & state = Referee 		{refereeInfo 
												& RefereeInfo.person 	= RefPerson (Refto uniquename)
												, RefereeInfo.reports 	= setInvariantReports refereeInfo.reports}}
			(Authors paperInfo) ->
				{account & state = Authors 		{paperInfo 
												& PaperInfo.person 		= RefPerson (Refto uniquename)
												, PaperInfo.paper 		= RefPaper(Refto (uniquePaper paperInfo.nr uniquename))}}
	where
		uniquename = uniquePerson account.login.loginName
	
		setInvariantReports (Reports reports) = Reports (setInvariantReport reports)
		
		setInvariantReport :: [(PaperNr, RefReport)] -> [(PaperNr, RefReport)]
		setInvariantReport [] = []
		setInvariantReport [(nr, (RefReport (Refto _))):reports]
						 = [(nr, (RefReport (Refto (uniqueReport nr uniquename)))):setInvariantReport reports]
	
		setInvariantReport [report:reports]
						 = [report:setInvariantReport reports]



/*
assignPaper :: Int ConfAccount -> ConfAccount
assignPaper i state
| isRefereeOf i state = state
= {state & reports = Reports [(i,Nothing):getReports state.reports]}
*/
/*


FetchReports :: Int ConfAccounts -> ([(Person,Maybe Report)],*Hst)
FetchReports papernr accounts hst
	= [ (state.person,report) 	\\ 	{state = Referee info} <- accounts 
								, 	(paperdone, report) <- getReports info.reports
								|	paperdone == papernr ]

*/
/*

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

initPaper :: String -> Paper
initPaper name
=	{ title			= "paper of " +++ name
	, first_author	= RefPerson (Refto (uniquePerson name))
	, co_authors	= Co_authors []
	, abstract		= "type in abstract here"
	, pdf			= "download pdf here"
	}

initReport :: Report
initReport 
= 	{ recommendation	= StrongReject
	, familiarity 		= Low  
	, commCommittee		= "committee remarks" // TextArea 4 70 ""
	, commAuthors		= "author remarks" //TextArea 10 70 "Please enter your report"
	}
	

*/








