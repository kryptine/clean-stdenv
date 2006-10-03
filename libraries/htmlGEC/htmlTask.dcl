definition module htmlTask

// *experimantal* library for controlling interactive Tasks (iTask) based on iData 

import StdHtml

:: *TSt										// task state
:: Task a		:== St *TSt a				// an interactive task
:: IDataFun a	:== St *HSt (Form a)		// an iData Form

/*
startTask		:: lift iData to iTask domain
mkTask			:: promote TSt state function to an interactive Task, i.e. task will only be called when it is its turn

STask			:: a Sequential iTask
STask_button	:: do corresponding iTask when button pressed
STasks			:: do all iTasks one after another, task completed when all done

CTask_button	:: Choose one iTask from list, depending on button pressed
CTask_pdmenu	:: Choose one iTask from list, depending on pulldownmenu item selected

MCTask_ckbox	:: Multiple Choice of iTasks, depending on marked checkboxes

PCTask2			:: do both iTasks in any order (paralel), task completed as soon as first one done
PCTasks			:: do all  iTasks in any order (paralel), task completed as soon as first one done

PTask2			:: do both iTasks in any order (paralel), task completed when both done
PTask			:: do all  iTasks in any order (paralel), task completed when all  done

returnTask		:: return the value and show it, no IO action from the user required
returnVF		:: return the value and show the code, no IO action from the user required
returnV			:: return the value, no IO action from the user required
returnF			:: add html code

appIData		:: lift iData editors to iTask domain
*/

startTask 		:: (Task a) *HSt -> ([BodyTag],HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
mkTask 			:: (*TSt -> *(a,*TSt)) 	-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a

STask 			:: String a 			-> (Task a)		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
STask_button	:: String (Task a)		-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
STasks			:: [(String,Task a)] 	-> (Task [a])	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 

CTask_button 	:: [(String,Task a)] 	-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
CTask_pdmenu 	:: [(String,Task a)] 	-> (Task a)	 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a

MCTask_ckbox 	:: [(String,Task a)] 	-> (Task [a]) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a

PCTask2			:: (Task a,Task a) 		-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
PCTasks			:: [(String,Task a)] 	-> (Task a)		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 

PTask2 			:: (Task a,Task b) 		-> (Task (a,b)) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a & gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
PTasks 			:: [(String,Task a)]	-> (Task [a])	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 

returnTask 		:: a 					-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
returnVF 		:: a [BodyTag] 			-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
returnV 		:: a 					-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
returnF 		:: [BodyTag] 			-> TSt -> TSt

appIData 		:: (IDataFun a) 		-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a

