definition module ostypes

//	Clean Object I/O library, version 1.2

//	Standard types for the OS

::	Rect						// A Rect is supposed to be an ordered rectangle with
	=	{	rleft	:: !Int		// rleft<=rright && rtop<=rbottom
		,	rtop	:: !Int
		,	rright	:: !Int
		,	rbottom	:: !Int
		}
::	OSWindowPtr
	:== HWND
::	HWND
	:==	Int
