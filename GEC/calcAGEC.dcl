definition module calcAGEC

import StdClass
import StdAGEC, buttonAGEC

calcAGEC 	 :: a [[(Button,a->a)]] 	-> AGEC a | gGEC {|*|} a // apply pressed function to argument
intcalcAGEC  :: Int 					-> AGEC Int
realcalcAGEC :: Real 				-> AGEC Real
