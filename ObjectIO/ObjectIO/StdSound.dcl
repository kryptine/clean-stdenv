definition module StdSound


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdSound specifies sound playing functions.
//	********************************************************************************

from	StdString	import String


class playSoundFile env :: !String !*env -> (!Bool,!*env)

/*	playSoundFile filename 
		opens the sound file at filename and plays it synchronously. 
		The Boolean result indicates whether the sound file could be succesfully 
		played.
*/

instance playSoundFile World
