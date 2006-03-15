definition module stateHandlingIData

import stateHandling
import StdHtml

changeInfo 			:: !ConfAccount !*HSt -> ([BodyTag],!*HSt)
modifyStatesPage 	:: !ConfAccount !ConfAccounts !*HSt -> (Judgement,(ConfAccount,ConfAccounts),[BodyTag],!*HSt)

//changeInfo 			:: !(Login ConfState) !(Logins ConfState) !*HSt -> (Logins ConfState,[BodyTag],*HSt)
//showPapersPage ::  !Papers !ConfState !ConfStates !*HSt -> (![BodyTag],!*HSt)

// assignPapersPage 		:: !Papers !(Logins ConfStates,ConfStates) !*HSt -> (!ConfStates,![BodyTag],!*HSt)
//assignConflictsPage :: !Papers !(Logins ConfState) !*HSt -> (!Logins ConfState,![BodyTag],!*HSt)

//refereeStatusPage 	:: !Papers !(Login ConfState) !(Logins ConfState)  !*HSt -> (!Logins ConfState,![BodyTag],!*HSt)

//modifyStatesPage 	:: !(Logins ConfState) !*HSt -> (!Logins ConfState,![BodyTag],!*HSt)
