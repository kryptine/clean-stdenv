implementation module clCCall_12

//	Clean Object I/O library, version 1.2

from ostoolbox import OSToolbox
import code from "cCCallSystem_121.obj", 
				 "cCCallWindows_121.obj",
				 "cCrossCallWindows_121.obj", 
				 "cCrossCall_121.obj", 
				 "cdebug_121.obj", 
				 "cpicture_121.obj",
				 "util_121.obj"


::	CSTR	:==  Int
::	ACCLPTR	:==  Int

MaxRand				:== 32767

WinHelpKey			:== 5
WinEscapeKey		:== 27
WinReturnKey		:== 13
WinTabKey			:== 9
WinDelKey			:== 127
WinBackSpKey		:== 8
WinEndKey			:== 4
WinBeginKey			:== 1
WinPgDownKey		:== 12
WinPgUpKey			:== 11
WinRightKey			:== 29
WinLeftKey			:== 28
WinDownKey			:== 31
WinUpKey			:== 30
WinF1Key			:==	1001
WinF2Key			:==	1002
WinF3Key			:==	1003
WinF4Key			:==	1004
WinF5Key			:==	1005
WinF6Key			:==	1006
WinF7Key			:==	1007
WinF8Key			:==	1008
WinF9Key			:==	1009
WinF10Key			:==	1010
WinF11Key			:==	1011
WinF12Key			:==	1012

CTRLBIT				:== 4
ALTBIT				:== 2
SHIFTBIT			:== 1

KEYREPEAT			:== 4
KEYUP				:== 2
KEYDOWN				:== 1

BUTTONUP			:== 50
BUTTONSTILLDOWN		:== 40
BUTTONTRIPLEDOWN	:== 3
BUTTONDOUBLEDOWN	:== 2
BUTTONDOWN			:== 1
BUTTONSTILLUP		:== 0		/* PA: new constant for passing mouse move events. */


WinLaunchApp ::  !{#Char} !Bool !*OSToolbox -> ( !Bool, !*OSToolbox)
WinLaunchApp _ _ _
	= code
	{
		.inline WinLaunchApp
			ccall WinLaunchApp "SII-II"
		.end
	}

WinLaunchApp2 :: !{#Char} !{#Char} !Bool !*OSToolbox -> ( !Bool, !*OSToolbox)
WinLaunchApp2 _ _ _ _
	= code
	{
		.inline WinLaunchApp2
			ccall WinLaunchApp2 "SSII-II"
		.end
	}

WinCallProcess ::  !CSTR !CSTR !CSTR !CSTR !CSTR !CSTR !*OSToolbox -> ( !Bool, !Int, !*OSToolbox)
WinCallProcess _ _ _ _ _ _ _
	= code
	{
		.inline WinCallProcess
			ccall WinCallProcess "IIIIIII-III"
		.end
	}

WinGetModulePath ::  {#Char}
WinGetModulePath
	= code
	{
		.inline WinGetModulePath
			ccall WinGetModulePath "-S"
		.end
	}

WinFileModifiedDate ::  !{#Char} -> ( !Bool, !Int, !Int, !Int, !Int, !Int, !Int)
WinFileModifiedDate _
	= code
	{
		.inline WinFileModifiedDate
			ccall WinFileModifiedDate "S-IIIIIII"
		.end
	}

WinFileExists ::  !{#Char} ->  Bool
WinFileExists _
	= code
	{
		.inline WinFileExists
			ccall WinFileExists "S-I"
		.end
	}

WinBeep :: !*OSToolbox -> *OSToolbox
WinBeep tb
	= code
	{
		.inline WinBeep
			ccall WinBeep "I-I"
		.end
	}

Rand ::  Int
Rand
	= code
	{
		.inline Rand
			ccall Rand "-I"
		.end
	}

WinReleaseCString ::  !CSTR !*OSToolbox ->  *OSToolbox
WinReleaseCString _ _
	= code
	{
		.inline WinReleaseCString
			ccall WinReleaseCString "II-I"
		.end
	}

WinGetCStringAndFree ::  !CSTR !*OSToolbox -> ( !{#Char}, !*OSToolbox)
WinGetCStringAndFree _ _
	= code
	{
		.inline WinGetCStringAndFree
			ccall WinGetCStringAndFree "II-SI"
		.end
	}

WinGetCString ::  !CSTR !*OSToolbox -> ( !{#Char}, !*OSToolbox)
WinGetCString _ _
	= code
	{
		.inline WinGetCString
			ccall WinGetCString "II-SI"
		.end
	}

WinMakeCString ::  !{#Char} !*OSToolbox -> ( !CSTR, !*OSToolbox)
WinMakeCString _ _
	= code
	{
		.inline WinMakeCString
			ccall WinMakeCString "SI-II"
		.end
	}

WinGetAppPath ::  CSTR
WinGetAppPath
	= code
	{
		.inline WinGetAppPath
			ccall WinGetAppPath "-I"
		.end
	}

WinSetDoubleDownDist ::  !Int !*OSToolbox ->  *OSToolbox
WinSetDoubleDownDist _ _
	= code
	{
		.inline WinSetDoubleDownDist
			ccall WinSetDoubleDownDist "II-I"
		.end
	}

WinGetHorzResolution ::  Int
WinGetHorzResolution
	= code
	{
		.inline WinGetHorzResolution
			ccall WinGetHorzResolution "-I"
		.end
	}

WinGetVertResolution ::  Int
WinGetVertResolution
	= code
	{
		.inline WinGetVertResolution
			ccall WinGetVertResolution "-I"
		.end
	}

WinMaxFixedWindowSize :: ( !Int, !Int)
WinMaxFixedWindowSize
	= code
	{
		.inline WinMaxFixedWindowSize
			ccall WinMaxFixedWindowSize "-II"
		.end
	}

WinMaxScrollWindowSize :: ( !Int, !Int)
WinMaxScrollWindowSize
	= code
	{
		.inline WinMaxScrollWindowSize
			ccall WinMaxScrollWindowSize "-II"
		.end
	}

//	PA: interface added for determining screen width and height.
WinScreenYSize :: !*OSToolbox -> (!Int,!*OSToolbox)
WinScreenYSize _
	= code
	{	
		.inline WinScreenYSize
			ccall WinScreenYSize "I-II"
		.end
	}

WinScreenXSize :: !*OSToolbox -> (!Int,!*OSToolbox)
WinScreenXSize _
	= code
	{	
		.inline WinScreenXSize
			ccall WinScreenXSize "I-II"
		.end
	}

WinMinimumWinSize :: ( !Int, !Int)
WinMinimumWinSize
	= code
	{
		.inline WinMinimumWinSize
			ccall WinMinimumWinSize "-II"
		.end
	}

//	PA: function added to get system metrics for width and height of scrollbars.
WinScrollbarSize :: !*OSToolbox -> ( !Int, !Int, !*OSToolbox )
WinScrollbarSize _
	= code
	{	
		.inline WinScrollbarSize
			ccall WinScrollbarSize "I-III"
		.end
	}

/*	PA: two new routines (Win(M/S)DIClientToOuterSizeDims added to convert between the
		client and outer size of (M/S)DI windows. The Int argument contains the style flags 
		of the window.
*/
WinMDIClientToOuterSizeDims :: !Int !*OSToolbox -> (!Int,!Int,!*OSToolbox)
WinMDIClientToOuterSizeDims _ _
	= code
	{
		.inline WinMDIClientToOuterSizeDims
			ccall WinMDIClientToOuterSizeDims "II-III"
		.end
	}

WinSDIClientToOuterSizeDims :: !Int !*OSToolbox -> (!Int,!Int,!*OSToolbox)
WinSDIClientToOuterSizeDims _ _
	= code
	{
		.inline WinSDIClientToOuterSizeDims
			ccall WinSDIClientToOuterSizeDims "II-III"
		.end
	}


WinPlaySound :: !{#Char} !*OSToolbox -> (!Bool,!*OSToolbox)
WinPlaySound _ _
	= code
	{
		.inline WinPlaySound
			ccall WinPlaySound "SI-II"
		.end
	}
