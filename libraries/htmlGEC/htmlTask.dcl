definition module htmlTask

// *experimantal* library for controlling interactive Tasks (iTask) based on iData 

import StdHtml

:: *TSt										// task state
:: Task a		:== St *TSt a				// an interactive task
:: IDataFun a	:== St *HSt (Form a)		// an iData Form

/*
startTask		:: lift iData to iTask domain

doSTask			:: a sequential iTask
doSTasks		:: a list of sequential iTask that will be performed one after another
doCbuttonTask	:: choose one iTask depending on button pressed
buttonTask		:: do task when button pressed
doCpdmenuTask	:: choose one iTask from the (pulldown) list
doPandTask		:: do both iTasks in any order, finished if both done
doPorTask		:: do both iTasks in any order, finished if first one done

mkTask			:: promote TSt state function to an interactive Task, i.e. task will only be called when it is its turn

returnTask		:: return the value and show it, no IO action from the user required
returnVF		:: return the value and show the code, no IO action from the user required
returnV			:: return the value, no IO action from the user required
returnF			:: add html code

appIData		:: lift iData editors to iTask domain
*/

startTask 		:: (Task a) *HSt -> ([BodyTag],HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 

doSTask 		:: String a 			-> (Task a)		| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
doSTasks 		:: [(String,Task a)] 	-> (Task [a])	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
doCbuttonTask 	:: [(String,Task a)] 	-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
buttonTask 		:: String (Task a)		-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
doCpdmenuTask 	:: [(String,Task a)] 	-> (Task a)	 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
doMCcheckTask 	:: [(String,Task a)] 	-> (Task [a]) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
doPandTask 		:: (Task a,Task b) 		-> (Task (a,b)) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a & gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
doPorTask 		:: (Task a,Task a) 		-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 

mkTask 			:: (*TSt -> *(a,*TSt)) 	-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a

returnTask 		:: a 					-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
returnVF 		:: a [BodyTag] 			-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
returnV 		:: a 					-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a 
returnF 		:: [BodyTag] 			-> TSt -> TSt

appIData 		:: (IDataFun a) 		-> (Task a) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a

