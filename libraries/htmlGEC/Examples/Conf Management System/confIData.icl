implementation module confIData

import htmlHandler, StdList
import loginAdmin, stateHandling
import stateHandlingIData, loginAdminIData

derive gUpd []

gForm {|[]|} gHa formid hst 
= case formid.ival of
	[x:xs]
	# (x,hst) 	= gHa (subFormId formid (toString (length xs)) x) hst
	# (xs,hst) 	= gForm {|*->*|} gHa (reuseFormId formid xs) hst
	= ({changed = x.changed||xs.changed,form = x.form ++ xs.form,value = [x.value:xs.value]},hst)
	[] 
	= ({changed = False,form = [],value = []},hst)

/*


gForm {|[]|} gHa formid hst 
= case formid.ival of
	[x:xs]
	# (x,hst) 	= gHa (reuseFormId formid x) hst
	# (xs,hst) 	= gForm {|*->*|} gHa (subFormId formid (toString (length xs)) xs) hst
	= ({changed = x.changed||xs.changed,form = x.form ++ xs.form,value = [x.value:xs.value]},hst)
	[] 
	= ({changed = False,form = [],value = []},hst)
*/
//derive gForm []

LoginStatesStore :: !((LoginStates ConfState) -> (LoginStates ConfState)) *HSt -> (!Form (LoginStates ConfState),!*HSt) // login administration database
LoginStatesStore upd hst = mkStoreForm (Init,pFormId "cf_loginDatabase" initRootLogin) upd hst

currPageStore :: !CurrPage  !(CurrPage -> CurrPage) *HSt -> (!Form CurrPage,!*HSt)							// current page to display
currPageStore currpage cbf hst = mkStoreForm (Init, sFormId "cf_currPage" currpage) cbf hst 

papersStore :: !Papers !(Papers -> Papers) *HSt -> (!Form Papers,!*HSt)										// papers to referee				
papersStore papers cbf hst = mkStoreForm (Init, pFormId "cf_papersDatabase" papers) cbf hst 

Exception :: ((Bool,String) -> (Bool,String)) -> (*HSt -> *((Form (Bool,String)),*HSt))
Exception fun = mkStoreForm (Init,sdFormId "cf_alert" (False,"")) fun
