definition module StdAGEC

import genericgecs

// BimapGEC:  make an a-value with a b-editor

derive gGEC BimapGEC								

:: BimapGEC a b 	= 	{ toGEC   :: a (Current b) -> b		// specify how to convert a into b, possibly using previous b settings
						, fromGEC :: b -> a					// specify how to convert b back to a
						, updGEC  :: b -> b					// will be called each time a b-value has been changed
						, value   :: a						// store initial a-value, will automatically always contain latest a-value made with b-editor
						}
:: Current a		=	Undefined | Defined a				// Undefined for a new-editor, Defined when a new value is set in an existing editor

mkBimapGEC  		:: (a (Current b) -> b) (b -> b) (b -> a) a -> (BimapGEC a b)

// abstract editors

derive gGEC AGEC

:: AGEC a		// abstract GEC for an a-value maintained with a b-editor

mkAGEC  	:: !(BimapGEC a b) !String -> AGEC a |  gGEC{|*|} b
^^			:: (AGEC a) -> a
(^=) infixl	:: (AGEC a) a -> (AGEC a)
