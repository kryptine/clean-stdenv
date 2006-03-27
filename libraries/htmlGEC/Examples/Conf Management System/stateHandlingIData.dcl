definition module stateHandlingIData

import stateHandling
import StdHtml

// Conference Manager Pages 

modifyStatesPage 			:: !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
assignPapersConflictsPage 	:: !ConfAccounts !*HSt -> ([BodyTag],!*HSt)

// Showing information

showPapersPage 				:: !ConfAccounts !*HSt -> ([BodyTag],!*HSt)
showReportsPage 			:: !ConfAccount !ConfAccounts !*HSt -> ([BodyTag],!*HSt)

// Changing user settings

changeInfo 					:: !ConfAccount !*HSt -> ([BodyTag],!*HSt)
submitPaperPage 			:: !ConfAccount !*HSt -> ([BodyTag],!*HSt)

// Changes made by a referee

submitReportPage 			:: !ConfAccount !ConfAccounts !*HSt -> ([BodyTag],!*HSt)

