/*
	Ronny's syntax and options for debug functions.

	Version 1.0.3
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
implementation module RWSDebug

import Debug

show
	=	debugShowWithOptions [DebugMaxChars 79, DebugMaxDepth 5,  DebugMaxBreadth 20]

(<<-) infix 0 :: .a !.b -> .a
(<<-) value debugValue
	=	debugBefore debugValue show value

(->>) infix 0 :: !.a .b -> .a
(->>) value debugValue
	=	debugAfter debugValue show value

<<->> :: !.a -> .a
<<->> value
	=	debugValue show value

