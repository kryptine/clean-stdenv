definition module StdDynamicGEC

import StdAGEC

dynamicGEC :: a -> AGEC a | TC a & gGEC {|*|} a	 					
				
derive gGEC (->), DynString

:: DynString = DynStr Dynamic String
