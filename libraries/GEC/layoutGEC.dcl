definition module layoutGEC

import genericgecs

// simple lay out macro's

derive gGEC (,), (,,)						// A tuple-editor is used to place things next to each other
											// A PAIR-editor by default places things below each other

(<->) infixr 3	//:: a b -> (a,b)			// Place a and b next to each other	
(<->) x y :== (x,y)
(<|>) infixr 2	//:: a b -> (PAIR a b)		// Place a above b
(<|>) x y :== PAIR x y

:: <|> x y :== PAIR x y
:: <-> x y :== (x,y)
