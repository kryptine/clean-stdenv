definition module GameFunctions

//      Version 1.0

import	gameintrface_12, osgame
from	StdIOBasic	import Point2

// game result codes (GRESULT)
GR_OK                  :==  0
GR_FAILED              :== -1  // very unlikely errors
GR_OS_ERROR            :== -2  // OS specific error
GR_INVALID_BITMAP_ID   :== -3  // bitmap ID doesn't exist or already used
GR_INVALID_SPRITE_ID   :== -4  // sprite ID not found
GR_INVALID_MAP_ID      :== -5  // layer map ID is invalid
GR_NOT_FOUND           :== -6  // file or resource not found

InitGameBitmap :: !BID !{#Char} !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

GameBitmapDone :: !BID !*OSToolbox -> (!GRESULT, !*OSToolbox)

ClearAllGameBitmaps :: !*OSToolbox -> (!GRESULT, !*OSToolbox)

SetTransparentColor :: !BID Point2 !*OSToolbox -> (!GRESULT, !*OSToolbox)

InitBlockSequence :: !BID (!SEQID, [(Int,Int)]) !*OSToolbox -> (!GRESULT, !*OSToolbox)

InitGameLayerMap :: !MAPID !BID [{#Int}] !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)

GameLayerMapDone :: !MAPID !*OSToolbox -> (!GRESULT, !*OSToolbox)

// OSGameData gs should only contain the current level here!
RunGameEngine :: (OSGameData gs) !*OSToolbox -> (gs, !*OSToolbox)

SetGameBoundMap :: !Int !Int [{#Int}] !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

MoveScreenTo :: Point2 !*OSToolbox -> (!GRESULT, !*OSToolbox)

InitSpriteAnimation :: !BID [(Int,Int)] !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)

InitGameObject :: !ObjectType !SubType !Point2 !*OSToolbox -> (!GRESULT, !*OSToolbox)

SetObjectFocus :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

// modified 01/11/99
CreateUserEvent :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

/*
ShowStatistic :: !Int !Int !{#Char} !Int !Colour !{#Char} !Int !Bool !Bool  !Bool !Int !Int !Colour !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
*/

PlayMusic :: !{#Char} !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)

StopMusic :: !*OSToolbox -> (!GRESULT, !*OSToolbox)

