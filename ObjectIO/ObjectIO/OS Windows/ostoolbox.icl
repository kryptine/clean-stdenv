implementation module ostoolbox

//	Clean Object I/O library, version 1.2

import StdMisc, StdInt, StdClass
import clCrossCall_12

::	OSToolbox
	:==	Int
	
// OSNewToolbox :: *OSToolbox
OSNewToolbox :== 0

// RWS ??? add success bool
OSInitToolbox :: *OSToolbox -> *OSToolbox
OSInitToolbox toolbox
	|	toolbox <> 0
		=	abort "OSInitToolbox reinitialised\n"
	# (ok, toolbox)
		=	WinInitOs
	| not ok
		=	toolbox // PA: don't abort, otherwise you can't do startIO twice. 
	//	=	abort "OSInitToolbox failed\n"
	// otherwise
		=	WinStartOsThread toolbox	// PA: WinStartOsThread added

// RWS ??? ugly
// OSDummyToolbox :: *OSToolbox
OSDummyToolbox :== 0

// PA: moved from world to ostoolbox
WorldGetToolbox :: !*World -> (!*OSToolbox,!*World)
WorldGetToolbox world
	= (OSNewToolbox,world)

WorldSetToolbox :: !*OSToolbox !*World -> *World
WorldSetToolbox _ world
	= world
