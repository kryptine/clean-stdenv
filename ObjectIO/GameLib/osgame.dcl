definition module osgame

//      Version 1.0

from	StdIOBasic	import Point2
import	gamehandle, gameintrface_12

::  OSGameData gs
    =   {   scroll    :: [(MAPID, Movement)]        // The layer movement functions
        ,   gamest    :: gs                         // The game state
        ,   gamehnd   :: GameHandle gs              // Complete game definition
        }

OSBinaryIntStr :: !Int -> {#Char}

OSBinaryBoolStr :: !Bool -> {#Char}

OSIntListArrayToString :: ![{#Int}] -> {#Char}

OSInitGameBitmap :: !BID !{#Char} !Int !Int !Int !Int !*OSToolbox -> (!GRESULT,!*OSToolbox)

OSGameBitmapDone :: !BID !*OSToolbox -> (!GRESULT,!*OSToolbox)

OSClearAllGameBitmaps :: !*OSToolbox -> (!GRESULT,!*OSToolbox)

OSSetTransparentColor :: !BID !Int !Int !*OSToolbox -> (!GRESULT,!*OSToolbox)

OSInitBlockSequence :: !BID !SEQID !{#Char} !*OSToolbox -> (!GRESULT,!*OSToolbox)

OSInitGameLayerMap :: !MAPID !BID !{#Char} !Int !Int !Bool !*OSToolbox -> (!GRESULT,!*OSToolbox)

OSGameLayerMapDone :: !MAPID !*OSToolbox -> (!GRESULT,!*OSToolbox)

OSRunGameEngine :: !(OSGameData .gs) !*OSToolbox -> (.gs,!*OSToolbox)

OSSetGameBoundMap :: !Int !Int !{#Char} !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT,!*OSToolbox)

OSMoveScreenTo :: !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

OSInitSpriteAnimation :: !BID !{#Char} !Bool !*OSToolbox -> (!GRESULT,!*OSToolbox)

OSInitGameObject :: !ObjectCode !SubCode !Point2 !*OSToolbox -> (!GRESULT,!*OSToolbox)

OSSetObjectFocus :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT,!*OSToolbox)

// modified 01/11/99
OSCreateUserEvent :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT,!*OSToolbox)

/*
OSShowStatistic :: !Int !Int !{#Char} !Int !Colour !{#Char} !Int !Bool !Bool  !Bool !Int !Int !RGBcolor !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
*/

OSPlayMusic :: !{#Char} !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)

OSStopMusic :: !*OSToolbox -> (!GRESULT, !*OSToolbox)

OSGameLevelOptions :: !Bool !Colour !Bool !Bool !Bool !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)

OSInitSoundSample :: !Int !{#Char} !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

OSPlaySoundSample :: !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

OSGetBoundMap :: !Int !Int !*OSToolbox -> (!(!Int,!GRESULT), !*OSToolbox)

OSSetBoundMap :: !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
