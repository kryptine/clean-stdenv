 
implementation module ArgEnv

import StdEnv

:: CString :== Int
NULL :== 0

:: EnvironmentVariable
    =	EnvironmentVariableUndefined
    |   EnvironmentVariable !.{#Char}

getEnv :: !{#Char} -> (!Int, !CString)
getEnv _
	= code {
		.inline getEnv
			ccall ArgEnvGetEnvironmentVariableC "S-II"
		.end
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
	= code {
		.inline copy
			| Clean 1.1: use create_array
			| pushC	'\000'
			| push_b	1
			| update_b	1 2
			| update_b	0 1
			| pop_b	1
			| create_array	CHAR 0 1

			| Clean 1.2 and later: use create_array_
			create_array_	CHAR 0 1

			push_a	0
			ccall	ArgEnvCopyCStringToCleanStringC "IS-I"
			pop_b	1
		.end
	}

getCommandLineCount :: Int
getCommandLineCount 
	= code {
		.inline getCommandLineCount
			ccall ArgEnvGetCommandLineCountC "-I"
		.end
	}

getCommandLineArgument :: !Int -> (!Int, !Int)
getCommandLineArgument _
	= code {
		.inline getCommandLineArgument
			ccall ArgEnvGetCommandLineArgumentC "I-II"
		.end
	}

getArg :: !Int -> {#.Char}
getArg i
	=	copy size cString
	where
		(size, cString)
			=	getCommandLineArgument i

// Clean 1.1: getCommandLine :: {{#Char}}
getCommandLine :: {.{#Char}}
getCommandLine
	=	{getArg i \\ i <- [0 .. getCommandLineCount-1]}
