definition module Highscore

//	**************************************************************************************************
//
//	General utility for reading/writing high scores to Files and displaying current high scores.
//
//	This module has been written in Clean 1.3.1 and uses the Clean Standard Object I/O library 1.0.2
//	
//	**************************************************************************************************

from	StdFile			import Files
from	StdString		import String
from	StdPSt			import PSt, IOSt
from	StdId			import Id

::	HiScores
	:== [HiScore]
::	HiScore
	=	{	name	:: !String
		,	score	:: !Int
		}

readHiScores	:: !String !*Files					-> (!(!*File,!HiScores),!*Files)
//	Reads high score file from disk.

writeHiScores	:: !*File  !HiScores !*Files		-> *Files
//	Writes high scores to disk.

itsAHighScore	:: !Int !Int !HiScores				-> Bool
//	Determines whether, given the number of high scores, a given score is actually a new high score.

addScore		:: !Int !HiScore !HiScores			-> HiScores
//	Add, given the number of high scores, a HiScore to the current list of high scores.

showHiScores	:: String !HiScores !(PSt .l .p)	-> PSt .l .p
//	Display current high scores to user in a modal dialog with given Id.
