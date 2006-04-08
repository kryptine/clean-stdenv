definition module confIData

// In this module basic editors are either derived or specialized for all types used

import htmlFormData, loginAdmin, stateHandling

derive gForm 	Maybe, []
derive gUpd  	Maybe, []
derive gPrint 	Maybe
derive gParse 	Maybe

derive gForm 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors, RefDiscussion, PaperStatus, Discussion 
derive gUpd 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors, RefDiscussion, PaperStatus, Discussion 
derive gPrint 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors, RefDiscussion, PaperStatus, Discussion 
derive gParse 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo, Co_authors, RefDiscussion, PaperStatus, Discussion 

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

editorRefPerson 	:: !(InIDataId RefPerson) 		!*HSt -> (Form Person,!*HSt)
editorRefPaper 		:: !(InIDataId RefPaper) 		!*HSt -> (Form Paper,!*HSt)
editorRefReport 	:: !(InIDataId RefReport) 		!*HSt -> (Form (Maybe Report),!*HSt)
editorRefDiscussion :: !(InIDataId RefDiscussion)	!*HSt -> (Form Discussion,!*HSt)


