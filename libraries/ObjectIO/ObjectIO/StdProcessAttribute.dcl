definition module StdProcessAttribute


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdProcessAttribute specifies which ProcessAttributes are valid for each of the
//	standard interactive processes.
//	Basic comparison operations and retrieval functions are also included.
//	********************************************************************************


import StdProcessDef


/*	The following function specifies the valid attributes for each standard 
	interactive process, specialised by its DocumentInterface.
*/

isProcessKindAttribute :: !DocumentInterface !(ProcessAttribute .st) -> Bool 
/*	(y = valid, . = invalid)
	ProcessActivate		NDI SDI MDI | ProcessToolbar		SDI MDI
	ProcessClose		NDI SDI MDI | ProcessWindowPos		SDI MDI
	ProcessDeactivate	NDI SDI MDI | ProcessWindowResize	SDI MDI
	ProcessNoWindowMenu	        MDI	| ProcessWindowSize		SDI MDI
	ProcessOpenFiles		SDI MDI
*/


/*	The following functions return True only iff the attribute equals the 
	indicated name.
*/
isProcessActivate			:: !(ProcessAttribute .st) -> Bool
isProcessClose				:: !(ProcessAttribute .st) -> Bool
isProcessDeactivate			:: !(ProcessAttribute .st) -> Bool
isProcessNoWindowMenu		:: !(ProcessAttribute .st) -> Bool
isProcessOpenFiles			:: !(ProcessAttribute .st) -> Bool
isProcessToolbar			:: !(ProcessAttribute .st) -> Bool
isProcessWindowPos			:: !(ProcessAttribute .st) -> Bool
isProcessWindowResize		:: !(ProcessAttribute .st) -> Bool
isProcessWindowSize			:: !(ProcessAttribute .st) -> Bool


/*	The following functions return the attribute value if appropriate. 
	THESE ARE PARTIAL FUNCTIONS! They are only defined on the corresponding
	attribute.
*/
getProcessActivateFun		:: !(ProcessAttribute .st) -> IdFun .st
getProcessCloseFun			:: !(ProcessAttribute .st) -> IdFun .st
getProcessDeactivateFun		:: !(ProcessAttribute .st) -> IdFun .st
getProcessOpenFilesFun		:: !(ProcessAttribute .st) -> ProcessOpenFilesFunction .st
getProcessToolbarAtt		:: !(ProcessAttribute .st) -> [ToolbarItem .st]
getProcessWindowPosAtt		:: !(ProcessAttribute .st) -> ItemPos
getProcessWindowResizeFun	:: !(ProcessAttribute .st) -> ProcessWindowResizeFunction .st
getProcessWindowSizeAtt		:: !(ProcessAttribute .st) -> Size
