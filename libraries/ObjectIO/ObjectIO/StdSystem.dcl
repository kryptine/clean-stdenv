definition module StdSystem


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdSystem defines platform dependent constants and functions. 
//	********************************************************************************


import	StdIOCommon


//	System dependencies concerning the file system.

dirseparator	:: Char		// Separator between folder- and filenames in a pathname
homepath		:: !String -> String
applicationpath	:: !String -> String
/*	dirseparator
		is the separator symbol used between folder- and filenames in a file path.
	homepath
		prefixes the 'home' directory file path to the given file name.
	applicationpath
		prefixes the 'application' directory file path to the given file name.
	Use these directories to store preference/options/help files of an application.
*/


//	System dependencies concerning the time resolution

ticksPerSecond	:: Int
/*	ticksPerSecond returns the maximum timer resolution per second.
*/


//	System dependencies concerning the screen resolution.

mmperinch		:== 25.4

hmm				:: !Real -> Int
vmm				:: !Real -> Int
hinch			:: !Real -> Int
vinch			:: !Real -> Int
/*	h(mm/inch) convert millimeters/inches into pixels, horizontally. 
	v(mm/inch) convert millimeters/inches into pixels, vertically.
*/

maxScrollWindowSize :: Size
maxFixedWindowSize  :: Size
/*	maxScrollWindowSize
		yields the range at which scrollbars are inactive.
	maxFixedWindowSize
		yields the range at which a window still fits on the screen. 
*/
