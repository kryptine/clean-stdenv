/*
	Debug functions.

	Version 1.0.4
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
definition module ClosureDebug


// generic show function
import Debug

:: ClosureOption
	=	ClosureEvaluate
	|	ClosureShow
	|	ClosureHide
	
closureDebugShowWithOptions :: ClosureOption [DebugShowOption] .a -> [{#Char}]

