definition module controldefaccess


//	Clean Object I/O library, version 1.2

//	Access functions to ControlDefinitions

import	StdControlAttribute


ControlAttributesHaveThisId	:: !Id ![ControlAttribute .st]	-> Bool

sameControlAttribute		:: !(ControlAttribute .st) !(ControlAttribute .st) -> Bool
