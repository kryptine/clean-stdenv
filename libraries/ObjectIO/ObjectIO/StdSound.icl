implementation module StdSound


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.1
//	
//	StdSound specifies sound playing functions.
//	********************************************************************************


import	StdString
from	clCCall_12	import winPlaySound
import	ostoolbox


class playSoundFile env :: !String !*env -> (!Bool,!*env)

instance playSoundFile World where
	playSoundFile :: !String !*World -> (!Bool,!*World)
	playSoundFile soundFileName world
		# (tb,world)	= worldGetToolbox world
		# (ok,tb)		= winPlaySound soundFileName tb
	    # world			= worldSetToolbox tb world
	    = (ok,world)
