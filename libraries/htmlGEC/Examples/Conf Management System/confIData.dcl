definition module confIData

// In this module basic editors are either derived or specialized for all types used

import htmlFormData, loginAdmin, stateHandling

derive gForm 	Maybe, [], Ref2
derive gUpd  	Maybe, [], Ref2
derive gPrint 	Maybe, Ref2
derive gParse 	Maybe, Ref2

derive gForm 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors, RefDiscussion,
				PaperStatus, Discussion, DiscussionStatus, Message
derive gUpd 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors, RefDiscussion,
				PaperStatus, Discussion, DiscussionStatus, Message
derive gPrint 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors, RefDiscussion,
				PaperStatus, Discussion, DiscussionStatus, Message
derive gParse 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors, RefDiscussion,
				PaperStatus, Discussion, DiscussionStatus, Message

// Naming convention of shared persistent information

uniqueDBname				:== "conferenceDBS"							// accounts database
uniquePerson  	 name 		:== name									// personnel information
uniqueReport  	 int name 	:== name +++ ".report." +++ toString int	// report of paper submiited by referee
uniquePaper   	 int name 	:== name +++ ".paper."  +++ toString int	// submitted paper
uniqueDiscussion int name	:== "discuss."  +++ (uniquePaper int name)	// discussion about submitted paper

// Persistent conf account database

AccountsDB			:: !Init !ConfAccounts   *HSt -> (ConfAccounts,!*HSt)	// confaccounts db
PaperNrStore 		:: !(Int -> Int) *HSt -> (Int,!*HSt) 					// paper counter

// editors for referenced types

editorRefPerson 	:: !(InIDataId RefPerson) 		!*HSt -> (Form RefPerson,!*HSt)
editorRefPaper 		:: !(InIDataId RefPaper) 		!*HSt -> (Form RefPaper,!*HSt)
editorRefReport 	:: !(InIDataId RefReport) 		!*HSt -> (Form RefReport,!*HSt)
editorRefDiscussion :: !(InIDataId RefDiscussion)	!*HSt -> (Form RefDiscussion,!*HSt)


