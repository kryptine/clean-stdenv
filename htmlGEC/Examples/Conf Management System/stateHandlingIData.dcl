definition module stateHandlingIData

import stateHandling
import StdHtml

derive gForm 	CurrPage, ConfState, Person, Paper, Report, Recommendation, Familiarity
derive gUpd 	CurrPage, ConfState, Person, Paper, Report, Recommendation, Familiarity
derive gPrint 	CurrPage, ConfState, Person, Paper, Report, Recommendation, Familiarity 
derive gParse 	CurrPage, ConfState, Person, Paper, Report, Recommendation, Familiarity

changeInfo 			:: !(LoginState ConfState) !(LoginStates ConfState) !*HSt -> (LoginStates ConfState,[BodyTag],*HSt)
showPapersPage ::  !Papers !(LoginState ConfState)!(LoginStates ConfState) !*HSt -> (![BodyTag],!*HSt)

assignPapersPage 	:: !Papers !(LoginStates ConfState) !*HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)
assignConflictsPage :: !Papers !(LoginStates ConfState) !*HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)

refereeStatusPage 	:: !Papers !(LoginState ConfState) !(LoginStates ConfState)  !*HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)

modifyStatesPage 	:: !(LoginStates ConfState) !*HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)
