definition module confIData

import htmlHandler, loginAdmin, stateHandling

derive gForm []
derive gUpd  []

// global stores

LoginStatesStore :: !((LoginStates State) -> (LoginStates State)) *HSt -> (!Form (LoginStates State),!*HSt) // login administration database

currPageStore :: !CurrPage  !(CurrPage -> CurrPage) *HSt -> (!Form CurrPage,!*HSt)							// current page to display

papersStore :: !Papers !(Papers -> Papers) *HSt -> (!Form Papers,!*HSt)										// papers to referee				

