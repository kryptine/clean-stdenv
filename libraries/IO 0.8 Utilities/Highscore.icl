implementation module Highscore


/*	General utility for reading/writing high scores to Files and displaying current high scores.
	This module uses the 0.8 I/O library.
*/

import	StdBool, StdString, StdFile, StdEnum, StdTuple, StdList, StdMisc, StdFunc
from	deltaSystem	import HomePath
import	deltaIOSystem, deltaDialog

::	HiScores
	:== [HiScore]
::	HiScore
	=	{	name	:: !String
		,	score	:: !Int
		}

//	Read in the high scores:
ReadHiScores :: !String !*Files -> (!(!*File,!HiScores),!*Files)
ReadHiScores fname files
	# (exists,file,files)	= fopen fpath FReadData files
	| exists
	= ((file1,highs),files)
	with
		(highs,file1)		= ReadHighs file
	# (_,create,files)	= fopen fpath FWriteData files
	| otherwise
	= ((create,[]),files)
where
	fpath = HomePath fname
	
	ReadHighs :: !*File -> (!HiScores,!*File)
	ReadHighs file
		| sfend file
		= ([],file)
		# (name, file) = freads file 13
		  (ok,hi,file) = freadi file
		| not ok
		= ([],file)
		# (ok,_, file) = freadc file
		| not ok
		= ([],file)
		# (rest, file) = ReadHighs file
		| otherwise
		= ([{name=name,score=hi}:rest],file)

//	Write the high scores:
WriteHiScores :: !*File !HiScores !*Files -> *Files
WriteHiScores file highs files
	# (ok,file)	= freopen file FWriteData
	| not ok
	= abort "Could not reopen file.\n"
	# (_,files)	= fclose (file<<<highs) files
	| otherwise
	= files

instance <<< HiScore where
	(<<<) :: !*File !HiScore -> *File
	(<<<) f {name,score} = f<<<take13 name<<<score<<<'\n'
	where
		take13 :: !String -> String
		take13 string = (string+++"             ")%(0,12)

instance <<< [x] | <<< x where
	(<<<) :: !*File ![x] -> *File | <<< x
	(<<<) f [x:xs]	= f<<<x<<<xs
	(<<<) f _		= f


//	Determine whether, given the number of high scores, a given score is actually a new high score:
ItsAHighScore :: !Int !Int !HiScores -> Bool
ItsAHighScore nrOfHiScores score scores
	| score==0						= False
	| length scores<nrOfHiScores	= True
	| otherwise						= IsItReallyAHighScore score scores
where
	IsItReallyAHighScore :: !Int !HiScores -> Bool
	IsItReallyAHighScore score` [{score}:hiscores]
		= score`>score || IsItReallyAHighScore score` hiscores
	IsItReallyAHighScore _ _
		= False


//	Add a HiScore to the current list of high scores:
AddScore :: !Int !HiScore !HiScores -> HiScores
AddScore nrOfHighScores hi hiscores
	= take nrOfHighScores (addscore hi hiscores)
where
	addscore :: !HiScore !HiScores -> HiScores
	addscore hi` hiscores=:[hi:his]
		| hi.score>hi`.score	= [hi  : addscore hi` his]
		| otherwise				= [hi` : hiscores]
	addscore hi` _
		= [hi`]


//	Display high scores in a modal dialog to the user:
ShowHiScores :: DialogId String !HiScores !*s !(IOState *s) -> (!*s,!IOState *s)
ShowHiScores _ _ [] state io
	# (_,state,io)	= OpenNotice (Notice ["No high scores available."] (NoticeButton 1 "OK") []) state io
	= (state,io)
ShowHiScores id header highs state io
	= OpenModalDialog dialog state io
where
	dialog	= CommandDialog id "High Scores" [] okId
				[	StaticText 1 Center header
				:	flatten
				[	
				[	DynamicText id		(YOffset (id-2) (Pixel 2)) (MM 65.0) (toString ((id-2)/2)+++". "+++name)
				,	StaticText (id+1)	(XOffset id (MM 0.0)) (toString score)
				]	\\ (id,{name,score}) <- zip2 [4,6..] highs
				]
				++	
				[	DialogButton okId Center "OK" Able (\_ state io -> (state, CloseActiveDialog io))
				]
				]
	okId	= 2*(length highs+1)
