definition module GenDefaultVal

import testable, StdMaybe
from   iostate import :: PSt{..}, :: IOSt

gDefaultVal :: !*env -> (!t,!*env) | generate{|*|} t & TimeEnv env

GenDefaultValIfNoValue :: !(Maybe t) !*env -> (!t,!*env) | generate{|*|} t & TimeEnv env
