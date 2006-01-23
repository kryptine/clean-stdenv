implementation module confIData

import htmlHandler, StdList
import loginAdmin, stateHandling
import stateHandlingIData, loginAdminIData

gForm {|[]|} gHa formid hst 
= case formid.ival of
	[x:xs]
	# (x,hst) 	= gHa (reuseFormId formid x) hst
	# (xs,hst) 	= gForm {|*->*|} gHa (subFormId formid (toString (length xs)) xs) hst
	= ({changed = x.changed||xs.changed,form = x.form ++ xs.form,value = [x.value:xs.value]},hst)
	[] 
	= ({changed = False,form = [],value = []},hst)


derive gUpd []


LoginStatesStore :: !((LoginStates State) -> (LoginStates State)) *HSt -> (!Form (LoginStates State),!*HSt) // login administration database
LoginStatesStore upd hst = mkStoreForm (Init,pFormId "loginDatabase" initState) upd hst

currPageStore :: !CurrPage  !(CurrPage -> CurrPage) *HSt -> (!Form CurrPage,!*HSt)							// current page to display
currPageStore currpage cbf hst = mkStoreForm (Init, sFormId "currPage" currpage) cbf hst 

papersStore :: !Papers !(Papers -> Papers) *HSt -> (!Form Papers,!*HSt)										// papers to referee				
papersStore papers cbf hst = mkStoreForm (Init, pFormId "papersDatabase" papers) cbf hst 
