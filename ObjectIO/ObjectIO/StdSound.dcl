definition module StdSound


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	StdSound specifies sound playing functions.
//	NOTE: This is an experimental extension of the Object I/O library.
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************

import	StdString


class playSoundFile env :: !String !*env -> (!Bool,!*env)

/*	playSoundFile filename 
		opens the sound file at filename and plays it synchronously. 
		The Boolean result indicates whether the sound file could be succesfully 
		played.
*/

instance playSoundFile World
