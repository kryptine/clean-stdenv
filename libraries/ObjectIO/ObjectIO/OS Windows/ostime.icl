implementation module ostime

//	Clean Object I/O library, version 1.2

import	StdClass, StdInt, StdOverloaded
import	ostoolbox

::	OSTime
	=	OSTime !Int

OSMaxTickCount	:==	2^31-1

OSMaxTime :: OSTime
OSMaxTime = OSTime OSMaxTickCount

OSGetTime :: !*OSToolbox -> (!OSTime,!*OSToolbox)
OSGetTime tb
	# (tickcount,tb)	= GetMessageTime tb
	= (OSTime tickcount,tb)
where
	GetMessageTime :: !*OSToolbox -> (!Int,!*OSToolbox)
	GetMessageTime tb = WinGetTickCount tb

OSWait :: !Int .x !*OSToolbox -> (.x,!*OSToolbox)
OSWait delay x tb
	= (x,WinWait delay tb)

OSGetBlinkInterval :: !*OSToolbox -> (!Int,!*OSToolbox)
OSGetBlinkInterval tb
	= WinGetBlinkTime tb

OSGetCurrentTime :: !*OSToolbox -> (!(!Int,!Int,!Int),!*OSToolbox)
OSGetCurrentTime tb
	= WinGetTime tb

OSGetCurrentDate :: !*OSToolbox -> (!(!Int,!Int,!Int,!Int),!*OSToolbox)
OSGetCurrentDate tb
	= WinGetDate tb

instance - OSTime where
	(-) :: !OSTime !OSTime -> OSTime
	(-) (OSTime new) (OSTime old)
		| old<=new
			= OSTime (new-old)
			= OSTime (OSMaxTickCount-old+new)

instance < OSTime where
	(<) :: !OSTime !OSTime -> Bool
	(<) (OSTime t1) (OSTime t2)
		= t1<t2

instance toInt OSTime where
	toInt :: !OSTime -> Int
	toInt (OSTime t) = t

instance fromInt OSTime where
	fromInt :: !Int -> OSTime
	fromInt t = OSTime (max 0 t)


WinGetTime :: !*OSToolbox -> (!(!Int,!Int,!Int),!*OSToolbox)
WinGetTime tb
	= code
	{
		.inline WinGetTime
			ccall WinGetTime "I-IIII"
		.end
	}

WinGetDate :: !*OSToolbox -> (!(!Int,!Int,!Int,!Int),!*OSToolbox)
WinGetDate tb
	= code
	{
		.inline WinGetDate
			ccall WinGetDate "I-IIIII"
		.end
	}

WinWait :: !Int !*OSToolbox -> *OSToolbox
WinWait i tb
	= code
	{
		.inline WinWait
			ccall WinWait "II-I"
		.end
	}

WinGetBlinkTime :: !*OSToolbox -> (!Int,!*OSToolbox)
WinGetBlinkTime tb
	= code
	{
		.inline WinGetBlinkTime
			ccall WinGetBlinkTime "I-II"
		.end
	}

WinGetTickCount ::  !*OSToolbox -> (!Int, !*OSToolbox)
WinGetTickCount _
	= code
	{
		.inline WinGetTickCount
			ccall WinGetTickCount "I-II"
		.end
	}
