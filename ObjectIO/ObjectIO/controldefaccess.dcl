definition module controldefaccess


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Access functions to ControlDefinitions
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


import	StdControlAttribute


controlAttributesHaveThisId	:: !Id ![ControlAttribute .st]	-> Bool

sameControlAttribute		:: !(ControlAttribute .st) !(ControlAttribute .st) -> Bool
