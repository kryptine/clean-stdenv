implementation module StdFileSelect

//	Clean Object I/O library, version 1.2.1

import StdFunc, StdMaybe, StdTuple
import osfileselect
import scheduler

class FileSelectEnv env where
	selectInputFile ::                 !*env -> (!Maybe String,!*env)
	selectOutputFile:: !String !String !*env -> (!Maybe String,!*env)
	selectDirectory ::                 !*env -> (!Maybe String,!*env)

instance FileSelectEnv World where
	selectInputFile :: !*World -> (!Maybe String,!*World)
	selectInputFile world
		# (initContext,tb)			= initContext` world
		# tb						= OSinitialiseFileSelectors tb
		# (ok,name,doneContext,tb)	= OSselectinputfile handleOSEvent initContext tb
		= (if ok (Just name) Nothing,closeContext doneContext tb)
	
	selectOutputFile :: !String !String !*World -> (!Maybe String,!*World)
	selectOutputFile prompt filename world
		# (initContext,tb)			= initContext` world
		# tb						= OSinitialiseFileSelectors tb
		# (ok,name,doneContext,tb)	= OSselectoutputfile handleOSEvent initContext prompt filename tb
		= (if ok (Just name) Nothing,closeContext doneContext tb)
	
	selectDirectory :: !*World -> (!Maybe String,!*World)
	selectDirectory world
		# (initContext,tb)			= initContext` world
		# tb						= OSinitialiseFileSelectors tb
		# (ok,name,doneContext,tb)	= OSselectdirectory handleOSEvent initContext tb
		= (if ok (Just name) Nothing,closeContext doneContext tb)

handleOSEvent :: !OSEvent !Context -> Context
handleOSEvent osEvent context
	= snd (handleContextOSEvent osEvent context)
