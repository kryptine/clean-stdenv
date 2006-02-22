definition module stateHandlingIData

import stateHandling
import StdHtml

derive gForm 	CurrPage, ConfState, Person, Paper, Report, Recommendation, Familiarity, Maybe
derive gUpd 	CurrPage, ConfState, Person, Paper, Report, Recommendation, Familiarity, Maybe
derive gPrint 	CurrPage, ConfState, Person, Paper, Report, Recommendation, Familiarity, Maybe 
derive gParse 	CurrPage, ConfState, Person, Paper, Report, Recommendation, Familiarity, Maybe

showPapersPage :: !(LoginState ConfState) !(InIDataId Papers) [ConfState] !*HSt -> (![BodyTag],!*HSt)

assignPapersPage :: !(LoginStates ConfState) !Papers *HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)

assignConflictsPage :: !(LoginStates ConfState) !Papers *HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)

refereeStatusPage :: (LoginState ConfState) !(LoginStates ConfState) !Papers *HSt -> (!LoginStates ConfState,![BodyTag],!*HSt)

modifyStatesPage :: !(LoginState state) !(LoginStates state) !*HSt -> (!LoginStates state,![BodyTag],!*HSt)
 					| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC state
