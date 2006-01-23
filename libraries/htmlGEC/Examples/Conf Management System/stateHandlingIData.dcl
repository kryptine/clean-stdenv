definition module stateHandlingIData

import stateHandling
import StdHtml

derive gForm 	CurrPage, State, Person, Paper, Report, Recommendation, Familiarity, Maybe
derive gUpd 	CurrPage, State, Person, Paper, Report, Recommendation, Familiarity, Maybe
derive gPrint 	CurrPage, State, Person, Paper, Report, Recommendation, Familiarity, Maybe 
derive gParse 	CurrPage, State, Person, Paper, Report, Recommendation, Familiarity, Maybe

showPapersPage :: !(LoginState State) !(InIDataId Papers) [State] !*HSt -> (![BodyTag],!*HSt)

assignPapersPage :: !(LoginStates State) !Papers *HSt -> (!LoginStates State,![BodyTag],!*HSt)

assignConflictsPage :: !(LoginStates State) !Papers *HSt -> (!LoginStates State,![BodyTag],!*HSt)

refereeStatusPage :: !Int (LoginState State) !(LoginStates State) !Papers *HSt -> (!LoginStates State,![BodyTag],!*HSt)