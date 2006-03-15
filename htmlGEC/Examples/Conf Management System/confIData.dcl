definition module confIData

import htmlHandler, loginAdmin, stateHandling

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

uniqueDBname		:== "confDBS"
uniquePerson  name 	:== Refto (name +++ ".person")
uniqueReport  name 	:== Refto (name +++ ".report")
uniquePaper   name 	:== Refto (name +++ ".paper")

// The used persistent i-Data stores

editAccounts :: !Mode !Init !ConfAccounts *HSt -> (!Form ConfAccounts,!*HSt)

editPerson 	:: (!Mode,!Init,(!Refto Person,!Mode,!Init)) *HSt -> (Form (Refto Person),Form Person,!*HSt)

ReportStore :: (Judgement -> Judgement) *HSt -> (Judgement,!*HSt)
//getPerson 	:: !RefPerson *HSt -> (!Person,!*HSt)

//editReport	:: (!Mode,!Init,(!RefReport,!Mode,!Init)) *HSt -> (Form RefReport,Form Report,!*HSt)
//getReport 	:: !RefReport *HSt -> (!Report,!*HSt)

//editPaper 	:: (!Mode,!Init,(!RefPaper,!Mode,!Init)) *HSt -> (Form RefPaper,Form Paper,!*HSt)
//getPaper 	:: !RefPaper *HSt -> (!Paper,!*HSt)




