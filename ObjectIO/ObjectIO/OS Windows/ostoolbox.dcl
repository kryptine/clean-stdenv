definition module ostoolbox

//	Clean Object I/O library, version 1.2

::	OSToolbox 
	:==	Int

// OSNewToolbox :: *OSToolbox
OSNewToolbox :== 0

// RWS ??? add success bool
OSInitToolbox :: *OSToolbox -> *OSToolbox

// RWS ??? ugly
// OSDummyToolbox :: *OSToolbox
OSDummyToolbox :== 0

// PA: moved from world to ostoolbox
WorldGetToolbox :: !*World -> (!*OSToolbox,!*World)
WorldSetToolbox :: !*OSToolbox !*World -> *World
