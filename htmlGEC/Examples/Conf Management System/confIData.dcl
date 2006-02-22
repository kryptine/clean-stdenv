definition module confIData

import htmlHandler, loginAdmin, stateHandling

derive gForm []
derive gUpd  []

// global stores

LoginStatesStore 	:: !((LoginStates ConfState) -> (LoginStates ConfState)) *HSt -> (!Form (LoginStates ConfState),!*HSt) // login administration database
papersStore 		:: !Papers !(Papers -> Papers) *HSt -> (!Form Papers,!*HSt)										// papers to referee				
currPageStore 		:: !CurrPage  !(CurrPage -> CurrPage) *HSt -> (!Form CurrPage,!*HSt)							// current page to display
Exception			:: ((Bool,String) -> (Bool,String)) -> (*HSt -> *((Form (Bool,String)),*HSt))
