implementation module ostoolbox

//	Clean Object I/O library, version 1.2

import	StdBool, StdClass, StdInt, StdMisc, StdTuple
import	clCrossCall_12
import	code from "cCrossCallFont_121.obj", "cCrossCallTCP_121.obj"


::	OSToolbox
	:==	Int
	
// OSNewToolbox :: *OSToolbox
OSNewToolbox :== 0

// RWS ??? add success bool
OSInitToolbox :: *OSToolbox -> *OSToolbox
OSInitToolbox toolbox
	| toolbox<>0
		= abort "OSInitToolbox reinitialised\n"
	# (ok,toolbox)	= WinInitOs
	| not ok
		= toolbox // PA: don't abort, otherwise you can't do startIO twice. 
	//	= abort "OSInitToolbox failed\n"
	| otherwise
		# toolbox	= WinStartOsThread toolbox	// PA: WinStartOsThread added
		# toolbox	= OSinstallFont toolbox		// Install font info cross call handling
		# toolbox	= OSinstallTCP  toolbox		// Install tcp cross call handling
		= toolbox

OSinstallFont :: !*OSToolbox -> *OSToolbox
OSinstallFont _
	= code
	{
		.inline InstallCrossCallFont
			ccall InstallCrossCallFont "I-I"
		.end
	}

OSinstallTCP :: !*OSToolbox -> *OSToolbox
OSinstallTCP tb
	= snd (IssueCleanRequest2 (\_ tb->(Return0Cci,tb)) (Rq0Cci CcRqCREATETCPWINDOW) (osInstallTCP tb))

osInstallTCP :: !*OSToolbox -> *OSToolbox
osInstallTCP _
	= code
	{
		.inline InstallCrossCallTCP
			ccall InstallCrossCallTCP "I-I"
		.end
	}

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
