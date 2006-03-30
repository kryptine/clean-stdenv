definition module confIData

// In this module editors are derived or specialized for all types used

import htmlFormData, loginAdmin, stateHandling

derive gForm 	Maybe, []
derive gUpd  	Maybe, []
derive gPrint 	Maybe
derive gParse 	Maybe

derive gForm 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo 
derive gUpd 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo 
derive gPrint 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo 
derive gParse 	
				Login, Account, Member, ManagerInfo, RefereeInfo, Conflicts,
				RefPerson, Person,
				Reports, RefReport, Report, Recommendation, Familiarity, 
				RefPaper, Paper, PaperInfo 

// Naming convention of shared persistent information

uniqueDBname			:== "conferenceDBS"							// accounts database
uniquePerson  name 		:== name									// personnel information
uniqueReport  int name 	:== name +++ ".report." +++ toString int	// report of paper
uniquePaper   int name 	:== name +++ ".paper."  +++ toString int	// submitted paper information

// Persistent conf account database

AccountsDB			:: !Init !ConfAccounts   *HSt -> (ConfAccounts,!*HSt)

// editors for referenced types

editorRefPerson 	:: !(InIDataId RefPerson) !*HSt -> (Form Person,!*HSt)
editorRefPaper 		:: !(InIDataId RefPaper) !*HSt -> (Form Paper,!*HSt)
editorRefReport 	:: !(InIDataId RefReport) !*HSt -> (Form (Maybe Report),!*HSt)


