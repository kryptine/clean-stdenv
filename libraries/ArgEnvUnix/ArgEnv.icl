/*
	Version 1.0.3
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
implementation module ArgEnv

import StdEnv

:: CString :== Int
NULL :== 0

:: EnvironmentVariable
    =	EnvironmentVariableUndefined
    |   EnvironmentVariable !.{#Char}

getEnv :: !{#Char} -> (!Int, !CString)
getEnv _
	= code inline {
			ccall ArgEnvGetEnvironmentVariableC "S-II"
	}

getEnvironmentVariable :: !{#Char} -> *EnvironmentVariable
getEnvironmentVariable name
	| cString == NULL
		=	EnvironmentVariableUndefined
	| otherwise
		=	EnvironmentVariable (copy size cString)
	where
		(size, cString)
			=	getEnv (name +++ "\0")


copy :: !Int !CString -> {#.Char}
copy length cString
	= code inline {
			create_array_	CHAR 0 1

			push_a	0
			ccall	ArgEnvCopyCStringToCleanStringC "IS-I"
			pop_b	1
		.end
	}

getCommandLineCount :: Int
getCommandLineCount 
	= code inline {
			ccall ArgEnvGetCommandLineCountC "-I"
	}

getCommandLineArgument :: !Int -> (!Int, !Int)
getCommandLineArgument _
	= code inline {
			ccall ArgEnvGetCommandLineArgumentC "I-II"
	}

getArg :: !Int -> {#.Char}
getArg i
	=	copy size cString
	where
		(size, cString)
			=	getCommandLineArgument i

getCommandLine :: {.{#Char}}
getCommandLine
	=	{getArg i \\ i <- [0 .. getCommandLineCount-1]}
