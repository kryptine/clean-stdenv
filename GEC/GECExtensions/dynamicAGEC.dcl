definition module dynamicAGEC

import StdAGEC

dynamicAGEC  :: a -> AGEC a | TC a & gGEC {|*|} a  // shows typed in expression, resulting value + type	 					
dynamicAGEC2 :: a -> AGEC a | TC a & gGEC {|*|} a  // only shows typed in expression	 					
				
derive gGEC (->), DynString

:: DynString = DynStr Dynamic String
