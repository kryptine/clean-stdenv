/*
	Debug functions.

	Version 1.0
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
definition module ShowDebug

:: DebugShowFunction a :== a -> [{#Char}]

// print (show a), then evaluate b
debugBefore :: !.a !(DebugShowFunction .a) .b -> .b
// evaluate b, then print (show a)
debugAfter :: .a !(DebugShowFunction .a) !.b -> .b
// evaluate and print (show a)
debugValue :: !(DebugShowFunction .a) !.a -> .a

// generic show function
debugShow :: [DebugShowOption] .a -> [{#Char}]

:: DebugShowOption 
	=	DebugMaxDepth !Int			// default no limit
	|	DebugMaxBreadth !Int		// default no limit
	|	DebugMaxChars !Int			// default no limit
	|	DebugTerminator !{#Char}	// default "\n"
