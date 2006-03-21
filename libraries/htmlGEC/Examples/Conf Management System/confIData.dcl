definition module confIData

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

uniqueDBname			:== "conferenceDBS"
uniquePerson  name 		:== name +++ ".person"
uniqueReport  int name 	:== name +++ ".report" +++ "." +++ toString int
uniquePaper   int name 	:== name +++ ".paper"  +++ "." +++ toString int

// The used persistent global i-Data editors and stores

AccountsDB		:: !Mode !Init !ConfAccounts   *HSt -> (Form ConfAccounts,!*HSt)



