implementation module GenDefault

import testable
import iostate, StdPSt

gDefaultVal :: !*env -> (!t,!*env) | generate{|*|} t & TimeEnv env
gDefaultVal env
	# (rs,env)			= randomStream env
	# (result,_,_,_)	= generate{|*|} emptyTrace rs
	= (result,env)

GenDefaultValIfNoValue :: !(Maybe t) !*env -> (!t,!*env) | generate{|*|} t & TimeEnv env
GenDefaultValIfNoValue maybev env
	= case maybev of
		Just v  = (v,env)
		nothing = gDefaultVal env
