definition module StdAGEC

import genericgecs

// lay out macro's

derive gGEC (,) 							// A tuple-editor is used to place things next to each other
											// A PAIR-editor by default places things below each other

(<->) infixr 3	//:: a b -> (a,b)			// Place a and b next to each other	
(<->) x y :== (x,y)
(<|>) infixr 2	//:: a b -> (PAIR a b)		// Place a above b
(<|>) x y :== PAIR x y

// various buttons

derive gGEC Button, UpDown

:: Button  = 	Button String | Pressed
:: UpDown  = 	UpPressed | DownPressed | Neutral

// Mode = Display (non editable) ; Hide (invisable) ; Edit (regular)

derive gGEC Mode

:: Mode a 	= Display a
			| Hide a
			| Edit a

// Timer editor: will cause an automatic update when it is part of any datastructure displayed in a gGEC

derive gGEC Timed

:: Timed = Timed (Int ->Int) Int		// function will be called automatically after given timeout in miliseconds						
										// function will receive the time left and should set the new timeout time

// BimapGEC:  make an a-value with a b-editor

derive gGEC BimapGEC								

:: BimapGEC a b 	= 	{ toGEC   :: a (Current b) -> b		// specify how to convert a into b, possibly using previous b settings
						, fromGEC :: b -> a					// specify how to convert b back to a
						, updGEC  :: b -> b					// will be called each time a b-value has been changed
						, value   :: a						// store initial a-value, will automatically always contain latest a-value made with b-editor
						}
:: Current a		=	Undefined | Defined a				// Undefined for a new-editor, Defined when a new value is set in an existing editor

// abstract editors

derive gGEC AGEC

:: AGEC a		// abstract GEC for an a-value maintained with a b-editor

mkAGEC		:: (BimapGEC a b) -> AGEC a | gGEC{|*|} b & gGEC{|*|} a
^^			:: (AGEC a) -> a
(^=) infixl	:: (AGEC a) a -> (AGEC a)

// examples of abstract editors

idGEC 			:: a   					-> AGEC a 		| gGEC {|*|} a				// identity editor  
hidGEC 			:: a 					-> AGEC a 		| gGEC {|*|} a 				// identity, no editor created
applyAGEC 		:: (b -> a) (AGEC b) 	-> AGEC a 		| gGEC {|*|} a & gGEC {|*|} b // apply fba; show both b and a
horlistGEC 		:: [a] 					-> AGEC [a]		| gGEC {|*|} a 				// all elements of a list displayed in a row
vertlistGEC 	:: [a] 					-> AGEC [a] 	| gGEC {|*|} a 				// all elements of a list displayed in a column
tableGEC 		:: [[a]] 				-> AGEC [[a]] 	| gGEC {|*|} a  			// a vertical list of horizontal lists

counterGEC 		:: a   					-> AGEC a 		| IncDec a & gGEC {|*|} a	// counter with UpDown buttons 
calcGEC 		:: a [[(Button,a->a)]] 	-> AGEC a 		| gGEC {|*|} a 				// apply pressed function to argument
listGEC 		:: Bool [a] 			-> AGEC [a] 	| gGEC {|*|} a				// list editor, set True for finite lists  

intcalcGEC		:: Int 					-> AGEC Int									// create Int calculator with calculator buttons
realcalcGEC 	:: Real 				-> AGEC Real								// create Real calculator with calculator buttons

