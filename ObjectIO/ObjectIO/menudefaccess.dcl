definition module menudefaccess


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Access functions to MenuDefinitions
//	Author: Peter Achten
//	Modified: 8 October 2001 for Clean 2.0
//	********************************************************************************


import	StdMenuAttribute


menuDefGetMenuId		:: !(Menu m .ls .pst)                -> (!Maybe Id,   !Menu m .ls .pst)
menuDefGetSelectState	:: !(Menu m .ls .pst)                -> (!SelectState,!Menu m .ls .pst)
menuDefGetTitle			:: !(Menu m .ls .pst)                -> (!Title,      !Menu m .ls .pst)
menuDefGetIndex			:: !(Menu m .ls .pst)                -> (!Maybe Index,!Menu m .ls .pst)
menuDefSetAbility		:: !(Menu m .ls .pst) !SelectState   -> Menu m .ls .pst
