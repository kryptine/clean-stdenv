definition module Highscore

/*
	General utility for reading/writing high scores to Files and displaying current high scores.
	This module uses the 0.8 I/O library.
*/

from	StdFile			import	:: Files
from	deltaEventIO	import	:: IOState
from	deltaIOSystem	import	:: DialogId

::	HiScores
	:== [HiScore]
::	HiScore
	=	{	name	:: !String
		,	score	:: !Int
		}

// Read high score file from disk:
ReadHiScores	:: !String !*Files				-> (!(!*File,!HiScores),!*Files)

// Write high scores to disk:
WriteHiScores	:: !*File  !HiScores !*Files	-> *Files

//	Determine whether, given the number of high scores, a given score is actually a new high score:
ItsAHighScore	:: !Int !Int !HiScores			-> Bool

//	Add, given the number of high scores, a HiScore to the current list of high scores:
AddScore		:: !Int !HiScore !HiScores		-> HiScores

// Display current high scores to user in a modal dialog with given DialogId:
ShowHiScores	:: DialogId String !HiScores !*s !(IOState *s)	-> (!*s,!IOState *s)
